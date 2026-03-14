-- | Module Anatomy Visualization
-- |
-- | Decomposes a single module's declaration call graph using biconnected
-- | components. Reveals: tightly coupled function clusters, articulation-point
-- | "god functions", cross-module coupling edges, and refactoring difficulty.
module CE2.Component.ModuleAnatomyViz
  ( component
  , Input
  , DeclInfo
  , FunctionCall
  , Output(..)
  , Query
  , Slot
  -- Reusable HATS rendering functions
  , callGraphTree
  , concernClusteredTree
  , blockColor
  ) where

import Prelude

import Data.Array as Array
import Data.Array (mapWithIndex, sortBy, (!!))
import Data.Foldable (foldl)
import Data.Int as Int
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number as Number
import Data.Set as Set
import Data.String as String
import Data.Tuple (Tuple(..))
import Data.Either (Either(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import Hylograph.HATS (Tree, elem, staticStr, staticNum)
import Hylograph.HATS.InterpreterTick (clearContainer, rerender)
import Hylograph.Internal.Element.Types (ElementType(..))

import CE2.Data.Decomposition as Dec
import CE2.Data.SubDeclarationAnalysis as SDA
import CE2.Data.Loader as Loader
import DataViz.Layout.BlockCutTree as BCT

-- =============================================================================
-- Types
-- =============================================================================

type Input =
  { packageName :: String
  , moduleName :: String
  , declarations :: Array DeclInfo
  , functionCalls :: Array FunctionCall
  , crossModuleCalls :: Array FunctionCall  -- calls to/from other modules
  , moduleSourceMap :: Map.Map String String  -- moduleName → package source ("workspace"|"registry"|"extra")
  , siblingModules :: Array String  -- other modules in same package, for compare picker
  }

-- | Simplified declaration info
type DeclInfo =
  { name :: String
  , kind :: String  -- "value" | "data" | "type_class" | etc
  }

-- | Function call edge
type FunctionCall =
  { callerName :: String
  , calleeModule :: String
  , calleeName :: String
  , isCrossModule :: Boolean
  , callCount :: Int
  }

data Output
  = NavigateToDeclaration String  -- declaration name
  | CompareWith String            -- compare current module with this sibling module

data Query (a :: Type)

type Slot = H.Slot Query Output

data ViewMode = DeclarationView | SubDeclarationView

type State =
  { input :: Input
  , decompInfo :: Maybe Dec.DecompInfo
  , graph :: Maybe (Dec.SimpleGraph String)
  , crossModuleEdges :: Array { from :: String, to :: String, toModule :: String }
  , viewMode :: ViewMode
  , subDeclAnalysis :: Maybe SDA.SubDeclAnalysis
  , subDeclDecomp :: Maybe Dec.DecompInfo
  , subDeclGraph :: Maybe (Dec.SimpleGraph String)
  , selectedGroup :: Maybe Int  -- Index of selected concern group for Tier 4 preview
  }

data Action
  = Initialize
  | Receive Input
  | SwitchView ViewMode
  | SelectConcernGroup Int
  | PickCompareModule String  -- user selected a module from the compare dropdown

-- =============================================================================
-- Block colors
-- =============================================================================

blockColors :: Array String
blockColors =
  [ "#4e9a6d", "#c05a4e", "#5a8ec0", "#c09a4e", "#8e5ac0"
  , "#c04e8e", "#4ec0c0", "#a0a040", "#e07048", "#4888c0"
  , "#50a060", "#c06090", "#6080b0", "#b08040", "#7060a0"
  ]

blockColor :: Int -> String
blockColor i = fromMaybe "#888" (blockColors !! (i `mod` Array.length blockColors))

kindColor :: String -> String
kindColor = case _ of
  "value" -> "#3b82f6"
  "data" -> "#10b981"
  "newtype" -> "#10b981"
  "type_synonym" -> "#8b5cf6"
  "type_class" -> "#f59e0b"
  "foreign" -> "#6b7280"
  _ -> "#6b7280"

-- =============================================================================
-- Force layout for group centers
-- =============================================================================

type ForceNode = { x :: Number, y :: Number, vx :: Number, vy :: Number }

-- | Simple force-directed layout for N group centers.
-- | Attraction between linked groups (weighted by cross-edge count),
-- | repulsion between all pairs, collision avoidance by group radius,
-- | and centering force.
forceLayoutGroups
  :: { width :: Number, height :: Number, centerX :: Number, centerY :: Number }
  -> Map.Map (Tuple Int Int) Int  -- cross-group edge weights
  -> Array Number                 -- group radii
  -> Array ForceNode              -- initial positions
  -> Int                          -- iterations
  -> Array ForceNode
forceLayoutGroups bounds weights radii initial iterations =
  let
    n = Array.length initial
    alpha0 = 1.0
    decay = alpha0 / Int.toNumber iterations

    step :: Number -> Array ForceNode -> Array ForceNode
    step alpha nodes =
      let
        -- Repulsion: all pairs push apart
        repelled = mapWithIndex (\i ni ->
          foldl (\acc j ->
            if i == j then acc
            else case nodes Array.!! j of
              Nothing -> acc
              Just nj ->
                let
                  dx = ni.x - nj.x
                  dy = ni.y - nj.y
                  dist = max 1.0 (Number.sqrt (dx * dx + dy * dy))
                  -- Collision: minimum distance = sum of radii + gap
                  ri = fromMaybe 30.0 (radii Array.!! i)
                  rj = fromMaybe 30.0 (radii Array.!! j)
                  minDist = ri + rj + 60.0
                  force = if dist < minDist
                    then 2000.0 / (dist * dist) + (minDist - dist) * 1.5
                    else 2000.0 / (dist * dist)
                in acc { vx = acc.vx + dx / dist * force * alpha
                       , vy = acc.vy + dy / dist * force * alpha
                       }
          ) ni (Array.range 0 (n - 1))
        ) nodes

        -- Attraction: linked groups pull together, weighted
        attracted = mapWithIndex (\i ni ->
          foldl (\acc (Tuple (Tuple gi gj) w) ->
            let targetIdx = if gi == i then gj else if gj == i then gi else -1
            in if targetIdx < 0 then acc
               else case nodes Array.!! targetIdx of
                 Nothing -> acc
                 Just nj ->
                   let
                     dx = nj.x - ni.x
                     dy = nj.y - ni.y
                     dist = max 1.0 (Number.sqrt (dx * dx + dy * dy))
                     strength = Int.toNumber w * 0.08
                   in acc { vx = acc.vx + dx / dist * strength * alpha
                          , vy = acc.vy + dy / dist * strength * alpha
                          }
          ) ni (Map.toUnfoldable weights :: Array (Tuple (Tuple Int Int) Int))
        ) repelled

        -- Centering force
        centered = map (\ni ->
          ni { vx = ni.vx + (bounds.centerX - ni.x) * 0.01 * alpha
             , vy = ni.vy + (bounds.centerY - ni.y) * 0.01 * alpha
             }
        ) attracted

        -- Apply velocity with damping, clamp to bounds
        margin = 60.0
        moved = map (\ni ->
          let
            x = max margin (min (bounds.width - margin) (ni.x + ni.vx * 0.4))
            y = max margin (min (bounds.height - margin) (ni.y + ni.vy * 0.4))
          in ni { x = x, y = y, vx = ni.vx * 0.6, vy = ni.vy * 0.6 }
        ) centered
      in moved

    -- Run iterations with decaying alpha
    result = foldl (\nodes i ->
      let alpha = max 0.01 (alpha0 - Int.toNumber i * decay)
      in step alpha nodes
    ) initial (Array.range 0 (iterations - 1))
  in result

-- =============================================================================
-- Component
-- =============================================================================

component :: forall m. MonadAff m => H.Component Query Input Output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval
        { initialize = Just Initialize
        , handleAction = handleAction
        , receive = Just <<< Receive
        }
    }

initialState :: Input -> State
initialState input =
  { input
  , decompInfo: Nothing
  , graph: Nothing
  , crossModuleEdges: []
  , viewMode: DeclarationView
  , subDeclAnalysis: Nothing
  , subDeclDecomp: Nothing
  , subDeclGraph: Nothing
  , selectedGroup: Nothing
  }

-- =============================================================================
-- Render
-- =============================================================================

render :: forall m. MonadAff m => State -> H.ComponentHTML Action () m
render state =
  let
    pillStyle active = "padding: 6px 16px; border: 1px solid "
      <> (if active then "#555; background: #555; color: #fff;" else "#C0BDB4; background: #fff; color: #555;")
      <> " cursor: pointer; font-size: 12px; font-weight: 500; font-family: 'Courier New', monospace;"
    isDeclView = case state.viewMode of
      DeclarationView -> true
      _ -> false
    isSubDeclView = case state.viewMode of
      SubDeclarationView -> true
      _ -> false
  in
  HH.div
    [ HP.style "width: 100%; height: 100%; overflow-y: auto; background: #fafaf8; color: #333; font-family: 'Courier New', Courier, monospace;" ]
    [ -- Heading + toggle bar
      HH.div
        [ HP.style "padding: 24px 32px 0; max-width: 1200px; margin: 0 auto;" ]
        [ HH.div
            [ HP.style "display: flex; align-items: baseline; gap: 16px; margin: 0 0 16px; flex-wrap: wrap;" ]
            [ HH.h1
                [ HP.style "font-size: 20px; font-weight: bold; margin: 0; letter-spacing: 0.5px;" ]
                [ HH.text $ "Anatomy of " <> shortModuleName state.input.moduleName ]
            , HH.span [ HP.style "font-size: 12px; color: #888;" ]
                [ HH.text $ show (Array.length state.input.declarations) <> " declarations, "
                    <> show (Array.length state.input.functionCalls) <> " internal calls, "
                    <> show (Array.length state.input.crossModuleCalls) <> " cross-module"
                ]
            , renderComparePicker state.input.moduleName state.input.siblingModules
            , HH.div
                [ HP.style "display: flex; gap: 0; margin-left: auto;" ]
                [ HH.button
                    [ HP.style $ pillStyle isDeclView <> " border-radius: 4px 0 0 4px;"
                    , HE.onClick \_ -> SwitchView DeclarationView
                    ]
                    [ HH.text "Declarations" ]
                , HH.button
                    [ HP.style $ pillStyle isSubDeclView <> " border-radius: 0 4px 4px 0; border-left: none;"
                    , HE.onClick \_ -> SwitchView SubDeclarationView
                    ]
                    [ HH.text "Sub-Declarations" ]
                ]
            ]
        ]
    -- Main content
    , HH.div
        [ HP.style "max-width: 1200px; margin: 0 auto; padding: 0 32px 24px;" ]
        [ case state.viewMode of
        SubDeclarationView -> renderSubDeclarationView state
        DeclarationView ->
          case state.decompInfo, state.graph of
            Just info, Just _graph ->
              HH.div [ HP.style "display: flex; gap: 16px; flex: 1; min-height: 0;" ]
                [ -- Main: graph + declaration list
                  HH.div [ HP.style "flex: 1; display: flex; flex-direction: column; gap: 16px; overflow-y: auto;" ]
                    [ -- Call graph decomposition
                      HH.div [ HP.style "border: 1px solid #ddd; border-radius: 4px; padding: 12px;" ]
                        [ HH.h3 [ HP.style "margin: 0 0 8px; font-size: 14px; font-weight: 600; color: #333;" ]
                            [ HH.text "Declaration Call Graph" ]
                        , HH.p [ HP.style "margin: 0 0 8px; font-size: 11px; color: #888;" ]
                            [ HH.text "Biconnected components of intra-module function calls. Dashed = cross-module." ]
                        , HH.div [ HP.id "module-structure-graph", HP.style "width: 100%; min-height: 300px;" ] []
                        ]
                    -- Declaration cluster list
                    , renderClusterList info state
                    -- Cross-module coupling
                    , if Array.length state.crossModuleEdges > 0
                      then renderCrossModuleCoupling state
                      else HH.text ""
                    ]
                -- Sidebar: metrics + refactoring difficulty
                , HH.div [ HP.style "width: 260px; flex-shrink: 0;" ]
                    [ renderMetrics info state
                    , renderRefactoringDifficulty info state
                    , renderLegend
                    ]
                ]
            _, _ ->
              HH.div [ HP.style "display: flex; align-items: center; justify-content: center; flex: 1; color: #888;" ]
                [ HH.text "Computing declaration structure..." ]
        ]
    ]

-- =============================================================================
-- Compare picker
-- =============================================================================

renderComparePicker :: forall m. MonadAff m => String -> Array String -> H.ComponentHTML Action () m
renderComparePicker currentMod siblings =
  if Array.null siblings then HH.text ""
  else
    let
      -- Modules sharing a prefix with the current module (e.g. SceneCoordinator.Pure for SceneCoordinator)
      currentPrefix = currentMod <> "."
      parentPrefix = case String.lastIndexOf (String.Pattern ".") currentMod of
        Just idx -> String.take (idx + 1) currentMod
        Nothing -> ""
      isRelated m = String.take (String.length currentPrefix) m == currentPrefix
                 || (parentPrefix /= "" && String.take (String.length parentPrefix) m == parentPrefix)
      sorted = Array.sortBy (\a b -> compare a b) siblings
      related = Array.filter isRelated sorted
      others = Array.filter (not <<< isRelated) sorted
      optionEl m = HH.option [ HP.value m ] [ HH.text (shortModuleName m) ]
      separator = HH.option [ HP.value "", HP.disabled true ] [ HH.text "───────────" ]
    in
    HH.select
      [ HP.style "padding: 3px 8px; font-size: 11px; border: 1px solid #ccc; border-radius: 3px; color: #555; background: #fff; cursor: pointer;"
      , HE.onValueInput PickCompareModule
      ]
      ( [ HH.option [ HP.value "" ] [ HH.text "Compare with\x2026" ] ]
      <> (if Array.null related then [] else (related <#> optionEl) <> [separator])
      <> (others <#> optionEl)
      )

-- =============================================================================
-- Sub-declaration view
-- =============================================================================

renderSubDeclarationView :: forall m. MonadAff m => State -> H.ComponentHTML Action () m
renderSubDeclarationView state =
  case state.subDeclAnalysis of
    Nothing ->
      HH.div [ HP.style "display: flex; align-items: center; justify-content: center; flex: 1; color: #888;" ]
        [ HH.text "Loading sub-declaration analysis..." ]
    Just analysis ->
      HH.div [ HP.style "display: flex; gap: 16px; flex: 1; min-height: 0;" ]
        [ -- Main column
          HH.div [ HP.style "flex: 1; display: flex; flex-direction: column; gap: 16px; overflow-y: auto;" ]
            [ -- Branch call graph
              HH.div [ HP.style "border: 1px solid #ddd; border-radius: 4px; padding: 12px;" ]
                [ HH.h3 [ HP.style "margin: 0 0 8px; font-size: 14px; font-weight: 600; color: #333;" ]
                    [ HH.text "Case Branch Call Graph" ]
                , HH.p [ HP.style "margin: 0 0 8px; font-size: 11px; color: #888;" ]
                    [ HH.text "Each node is a case branch (action handler). Connected by shared state fields and action dispatches." ]
                , HH.div [ HP.id "module-structure-graph", HP.style "width: 100%; min-height: 300px;" ] []
                ]
            -- Case expression summary
            , renderCaseExprSummary analysis
            -- Concern groups
            , if Array.length analysis.concernGroups > 0
              then renderConcernGroups analysis state.selectedGroup
              else HH.text ""
            -- Refactoring preview (Tier 4) — shown when a group is selected
            , case state.selectedGroup of
                Just idx -> renderRefactoringPreview analysis idx
                Nothing -> HH.text ""
            ]
        -- Sidebar
        , HH.div [ HP.style "width: 280px; flex-shrink: 0;" ]
            [ renderSubDeclMetrics analysis state
            , case state.subDeclDecomp of
                Just info -> renderRefactoringDifficultySubDecl info analysis
                Nothing -> HH.text ""
            , renderSubDeclLegend
            ]
        ]

renderCaseExprSummary :: forall m. MonadAff m => SDA.SubDeclAnalysis -> H.ComponentHTML Action () m
renderCaseExprSummary analysis =
  HH.div [ HP.style "border: 1px solid #ddd; border-radius: 4px; padding: 12px;" ]
    ( [ HH.h3 [ HP.style "margin: 0 0 8px; font-size: 14px; font-weight: 600; color: #333;" ]
          [ HH.text "Case Expressions" ]
      ]
      <> (analysis.caseExpressions <#> \ce ->
        HH.div [ HP.style "padding: 6px 8px; margin-bottom: 4px; background: #f8f8f8; border-radius: 3px;" ]
          [ HH.div [ HP.style "font-size: 13px; font-weight: 600; color: #333;" ]
              [ HH.text $ ce.functionName <> " (" <> show (Array.length ce.branches) <> " branches)" ]
          , HH.div [ HP.style "font-size: 11px; color: #888; margin-top: 2px;" ]
              [ HH.text $ "line " <> show ce.lineStart <> " — branches: "
                  <> String.joinWith ", " (Array.take 8 (ce.branches <#> _.name))
                  <> if Array.length ce.branches > 8 then ", ..." else ""
              ]
          ]
      )
    )

renderConcernGroups :: forall m. MonadAff m => SDA.SubDeclAnalysis -> Maybe Int -> H.ComponentHTML Action () m
renderConcernGroups analysis selectedGroup =
  HH.div [ HP.style "border: 1px solid #e0d4f5; border-radius: 4px; padding: 12px; background: #faf5ff;" ]
    ( [ HH.h3 [ HP.style "margin: 0 0 4px; font-size: 14px; font-weight: 600; color: #6b21a8;" ]
          [ HH.text "Concern Groups" ]
      , HH.p [ HP.style "margin: 0 0 8px; font-size: 11px; color: #888;" ]
          [ HH.text "Click a group to preview extraction. Groups = branches sharing state fields." ]
      ]
      <> mapWithIndex (\i group ->
        let
          isSelected = selectedGroup == Just i
          bgColor = if isSelected then "#f0f0ff" else "#fff"
          borderWidth = if isSelected then "4px" else "3px"
        in
        HH.div
          [ HP.style $ "padding: 8px; margin-bottom: 6px; border-left: " <> borderWidth <> " solid " <> blockColor i
              <> "; background: " <> bgColor <> "; border-radius: 2px; cursor: pointer;"
              <> (if isSelected then " outline: 2px solid " <> blockColor i <> "; outline-offset: -1px;" else "")
          , HE.onClick \_ -> SelectConcernGroup i
          ]
          [ HH.div [ HP.style "display: flex; justify-content: space-between; align-items: baseline;" ]
              [ HH.div [ HP.style "font-size: 13px; font-weight: 600; color: #333;" ]
                  [ HH.text group.name ]
              , HH.span [ HP.style "font-size: 11px; color: #888;" ]
                  [ HH.text $ show (Set.size group.branches) <> " branches, "
                      <> show (Set.size group.allFields) <> " fields"
                  ]
              ]
          , HH.div [ HP.style "font-size: 11px; color: #555; margin-top: 4px;" ]
              [ HH.text $ "Branches: " <> String.joinWith ", " (Set.toUnfoldable group.branches :: Array String) ]
          , if Set.size group.sharedFields > 0
            then HH.div [ HP.style "font-size: 11px; color: #6b21a8; margin-top: 2px;" ]
              [ HH.text $ "Shared fields: " <> String.joinWith ", " (Set.toUnfoldable group.sharedFields :: Array String) ]
            else HH.text ""
          ]
      ) analysis.concernGroups
    )

-- =============================================================================
-- Tier 4: Refactoring Preview
-- =============================================================================

renderRefactoringPreview :: forall m. MonadAff m => SDA.SubDeclAnalysis -> Int -> H.ComponentHTML Action () m
renderRefactoringPreview analysis idx =
  case analysis.concernGroups !! idx of
    Nothing -> HH.text ""
    Just group ->
      let
        -- Branches moving to the new module
        movingBranches = Array.filter (\b -> Set.member b.name group.branches) analysis.allBranches
        remainingBranches = Array.filter (\b -> not (Set.member b.name group.branches)) analysis.allBranches

        -- State fields: exclusive to this group vs shared with remaining
        remainingFields = foldl (\acc b -> Set.union acc (Set.union b.stateReads b.stateWrites)) Set.empty remainingBranches
        exclusiveFields = Set.difference group.allFields remainingFields
        sharedWithRemaining = Set.intersection group.allFields remainingFields

        -- Action dispatches between groups (would become cross-module)
        movingNames = group.branches
        dispatchesOut = foldl (\acc b ->
          Set.union acc (Set.difference b.actionDispatches movingNames)
        ) Set.empty movingBranches
        dispatchesIn = foldl (\acc b ->
          Set.union acc (Set.intersection b.actionDispatches movingNames)
        ) Set.empty remainingBranches

        crossModuleDispatches = Set.union dispatchesOut dispatchesIn

        -- Lines of code
        movingLines = foldl (\acc b -> acc + b.lineCount) 0 movingBranches
        totalLines = foldl (\acc b -> acc + b.lineCount) 0 analysis.allBranches

        -- Severity assessment
        hasSharedFields = Set.size sharedWithRemaining > 0
        hasCrossDispatches = Set.size crossModuleDispatches > 0
        extractionCleanness =
          if not hasSharedFields && not hasCrossDispatches then "Clean"
          else if hasSharedFields && hasCrossDispatches then "Requires refactoring"
          else "Minor adjustments needed"
        cleannessColor =
          if not hasSharedFields && not hasCrossDispatches then "#0d904f"
          else if hasSharedFields && hasCrossDispatches then "#c62828"
          else "#e65100"
      in
      HH.div [ HP.style "border: 2px solid #2563eb; border-radius: 6px; padding: 16px; background: #f0f7ff;" ]
        [ HH.h3 [ HP.style "margin: 0 0 12px; font-size: 15px; font-weight: 700; color: #1e40af;" ]
            [ HH.text $ "Extract: " <> group.name ]

        -- Summary bar
        , HH.div [ HP.style "display: flex; gap: 16px; margin-bottom: 12px; flex-wrap: wrap;" ]
            [ previewBadge (show (Set.size group.branches) <> " branches") "#2563eb"
            , previewBadge (show movingLines <> " / " <> show totalLines <> " lines") "#2563eb"
            , previewBadge (show (Set.size exclusiveFields) <> " exclusive fields") "#0d904f"
            , previewBadge extractionCleanness cleannessColor
            ]

        -- What moves
        , HH.div [ HP.style "margin-bottom: 12px;" ]
            [ HH.div [ HP.style "font-size: 12px; font-weight: 600; color: #1e40af; margin-bottom: 4px;" ]
                [ HH.text "Branches moving to new module:" ]
            , HH.div [ HP.style "font-size: 12px; color: #333; line-height: 1.6;" ]
                [ HH.text $ String.joinWith ", " (Set.toUnfoldable group.branches :: Array String) ]
            ]

        -- State field analysis
        , HH.div [ HP.style "margin-bottom: 12px;" ]
            ( [ HH.div [ HP.style "font-size: 12px; font-weight: 600; color: #1e40af; margin-bottom: 4px;" ]
                  [ HH.text "State field impact:" ]
              ]
              <> (if Set.size exclusiveFields > 0
                  then [ HH.div [ HP.style "font-size: 12px; color: #0d904f; margin-bottom: 2px;" ]
                           [ HH.text $ "Move to new module (" <> show (Set.size exclusiveFields) <> "): "
                               <> String.joinWith ", " (Set.toUnfoldable exclusiveFields :: Array String)
                           ] ]
                  else [])
              <> (if Set.size sharedWithRemaining > 0
                  then [ HH.div [ HP.style "font-size: 12px; color: #e65100; margin-bottom: 2px;" ]
                           [ HH.text $ "Shared with remaining (" <> show (Set.size sharedWithRemaining) <> "): "
                               <> String.joinWith ", " (Set.toUnfoldable sharedWithRemaining :: Array String)
                           ] ]
                  else [ HH.div [ HP.style "font-size: 12px; color: #0d904f;" ]
                           [ HH.text "No shared state fields — clean state separation!" ] ])
            )

        -- Cross-dispatch analysis
        , if Set.size crossModuleDispatches > 0
          then HH.div [ HP.style "margin-bottom: 12px;" ]
            [ HH.div [ HP.style "font-size: 12px; font-weight: 600; color: #c62828; margin-bottom: 4px;" ]
                [ HH.text "Cross-module action dispatches (become message passing):" ]
            , HH.div [ HP.style "font-size: 12px; color: #c62828;" ]
                [ HH.text $ String.joinWith ", " (Set.toUnfoldable crossModuleDispatches :: Array String) ]
            ]
          else HH.div [ HP.style "font-size: 12px; color: #0d904f; margin-bottom: 12px;" ]
            [ HH.text "No cross-group action dispatches — clean extraction!" ]

        -- Verdict
        , HH.div [ HP.style $ "padding: 8px 12px; border-radius: 4px; font-size: 13px; font-weight: 600; color: " <> cleannessColor <> "; background: #fff;" ]
            [ HH.text $ case extractionCleanness of
                "Clean" -> "This group can be extracted as a standalone sub-component or module with no coupling to the parent."
                "Requires refactoring" -> "Extraction needs shared state to be lifted into a common record or passed via props/messages. Cross-dispatches become inter-component messages."
                _ -> "Extraction is feasible with minor state field sharing. Consider a shared sub-record for the " <> show (Set.size sharedWithRemaining) <> " shared fields."
            ]
        ]

previewBadge :: forall w i. String -> String -> HH.HTML w i
previewBadge label color =
  HH.span [ HP.style $ "padding: 2px 8px; border-radius: 10px; font-size: 11px; font-weight: 600; color: " <> color <> "; background: " <> color <> "14; border: 1px solid " <> color <> "40;" ]
    [ HH.text label ]

renderSubDeclMetrics :: forall m. MonadAff m => SDA.SubDeclAnalysis -> State -> H.ComponentHTML Action () m
renderSubDeclMetrics analysis _state =
  let
    nBranches = Array.length analysis.allBranches
    nFields = Set.size analysis.allStateFields
    nGroups = Array.length analysis.concernGroups
    maxBranchLines = fromMaybe 0 $ map _.lineCount $ Array.head $
      sortBy (\a b -> compare b.lineCount a.lineCount) analysis.allBranches
  in HH.div [ HP.style "border: 1px solid #ddd; border-radius: 4px; padding: 12px; margin-bottom: 12px;" ]
    [ HH.h3 [ HP.style "margin: 0 0 12px; font-size: 14px; font-weight: 600; color: #333;" ]
        [ HH.text "Sub-Declaration Metrics" ]
    , metricRow "Case branches" (show nBranches)
    , metricRow "State fields touched" (show nFields)
    , metricRow "Concern groups" (show nGroups)
    , metricRow "Largest branch" (show maxBranchLines <> " lines")
    , metricRow "Case expressions" (show (Array.length analysis.caseExpressions))
    ]

renderRefactoringDifficultySubDecl :: forall m. MonadAff m => Dec.DecompInfo -> SDA.SubDeclAnalysis -> H.ComponentHTML Action () m
renderRefactoringDifficultySubDecl info analysis =
  let
    m = info.metrics
    nGroups = Array.length analysis.concernGroups
    independentGroups = Array.filter (\g -> Set.size g.sharedFields == 0) analysis.concernGroups
    nIndependent = Array.length independentGroups

    -- Difficulty assessment
    difficulty =
      (if m.treelikeness > 0.8 then 0 else if m.treelikeness > 0.4 then 1 else 2) +
      (if nGroups > 4 then 1 else 0)
    difficultyLabel = case difficulty of
      0 -> { text: "Low — clean separation", color: "#0d904f", bg: "#f1f8f1", border: "#c8e6c9" }
      1 -> { text: "Moderate — some tangling", color: "#e65100", bg: "#fff8f0", border: "#ffe0b2" }
      _ -> { text: "High — deeply tangled", color: "#c62828", bg: "#fff8f8", border: "#ffcdd2" }
  in
  HH.div [ HP.style $ "border: 1px solid " <> difficultyLabel.border <> "; border-radius: 4px; padding: 12px; margin-bottom: 12px; background: " <> difficultyLabel.bg <> ";" ]
    [ HH.div [ HP.style "display: flex; justify-content: space-between; align-items: baseline; margin-bottom: 8px;" ]
        [ HH.h3 [ HP.style "margin: 0; font-size: 14px; font-weight: 600; color: #333;" ]
            [ HH.text "Extraction Difficulty" ]
        , HH.span [ HP.style $ "font-size: 13px; font-weight: 700; color: " <> difficultyLabel.color <> ";" ]
            [ HH.text difficultyLabel.text ]
        ]
    , HH.div [ HP.style "font-size: 12px; color: #555; margin-bottom: 6px;" ]
        [ HH.text $ show nGroups <> " concern groups found. "
            <> show nIndependent <> " independent (no shared state with others)."
        ]
    , HH.div [ HP.style "font-size: 12px; color: #555;" ]
        [ HH.text $ "Branch-level treelikeness: " <> showPercent m.treelikeness
            <> " — " <> (if m.treelikeness > 0.8 then "branches are mostly independent"
                         else if m.treelikeness > 0.4 then "some branch clusters share state"
                         else "many branches share state fields")
        ]
    ]

renderSubDeclLegend :: forall m. H.ComponentHTML Action () m
renderSubDeclLegend =
  HH.div [ HP.style "border: 1px solid #ddd; border-radius: 4px; padding: 12px; margin-bottom: 12px;" ]
    [ HH.div [ HP.style "font-size: 12px; margin-bottom: 6px;" ]
        [ HH.span [ HP.style "display: inline-block; width: 12px; height: 12px; border-radius: 50%; background: #3b82f6; vertical-align: middle; margin-right: 6px;" ] []
        , HH.text "Case branch"
        ]
    , HH.div [ HP.style "font-size: 12px; margin-bottom: 6px;" ]
        [ HH.span [ HP.style "display: inline-block; width: 20px; height: 0; border-top: 2px solid #999; vertical-align: middle; margin-right: 6px;" ] []
        , HH.text "Shared state field"
        ]
    , HH.div [ HP.style "font-size: 12px; margin-bottom: 6px;" ]
        [ HH.span [ HP.style "display: inline-block; width: 20px; height: 0; border-top: 2px dashed #c05a4e; vertical-align: middle; margin-right: 6px;" ] []
        , HH.text "Action dispatch"
        ]
    , HH.div [ HP.style "font-size: 12px;" ]
        [ HH.span [ HP.style "display: inline-block; width: 12px; height: 12px; border-radius: 3px; background: #faf5ff; border: 1px solid #e0d4f5; vertical-align: middle; margin-right: 6px;" ] []
        , HH.text "Concern group cluster"
        ]
    ]

-- =============================================================================
-- Original declaration view helpers
-- =============================================================================

renderMetrics :: forall m. MonadAff m => Dec.DecompInfo -> State -> H.ComponentHTML Action () m
renderMetrics info state =
  let
    m = info.metrics
    nDecls = Array.length state.input.declarations
    nInternal = Array.length state.input.functionCalls
    nCross = Array.length state.crossModuleEdges
    -- Structural shape label
    shape = if nInternal == 0 then "flat (no internal calls)"
            else if m.treelikeness > 0.95 then "tree"
            else if m.treelikeness < 0.3 then "tangled"
            else "mixed"
  in HH.div [ HP.style "border: 1px solid #ddd; border-radius: 4px; padding: 12px; margin-bottom: 12px;" ]
    [ HH.h3 [ HP.style "margin: 0 0 12px; font-size: 14px; font-weight: 600; color: #333;" ]
        [ HH.text "Metrics" ]
    , metricRow "Declarations" (show nDecls)
    , metricRow "Internal calls" (show nInternal)
    , metricRow "Cross-module calls" (show nCross)
    , metricRow "Shape" shape
    , metricRow "Treelikeness" (showPercent m.treelikeness)
    , metricRow "Clusters" (show m.biconnectedComponentCount)
    , metricRow "Largest cluster" (show m.maxBlockSize <> " decls")
    , metricRow "Bridges" (show m.bridgeCount)
    ]

renderRefactoringDifficulty :: forall m. MonadAff m => Dec.DecompInfo -> State -> H.ComponentHTML Action () m
renderRefactoringDifficulty info state =
  let
    m = info.metrics
    apList = Array.sort (Set.toUnfoldable info.aps :: Array String)
    nInternalCalls = Array.length state.input.functionCalls

    -- Structural classification
    isFlat = nInternalCalls == 0
    isTree = m.treelikeness > 0.95
    isTangled = m.treelikeness < 0.5 && m.articulationPointCount > 0
    hasLargeCluster = m.maxBlockSize > 5

    -- Tangled hubs: APs that are inside non-bridge blocks (genuinely hard)
    tangledAPs = if isTree then []
      else Array.filter (\name ->
        case Map.lookup name info.nodeBlock of
          Just blockIdx ->
            case Array.find (\b -> b.index == blockIdx) info.blocks of
              Just block -> not block.isBridge && Set.size block.nodes > 2
              Nothing -> false
          Nothing -> false
      ) apList

    -- Cross-module coupling
    crossModuleCallerCounts = foldl (\acc call ->
      Map.alter (Just <<< (_ + 1) <<< fromMaybe 0) call.callerName acc
    ) Map.empty state.input.crossModuleCalls
    highCoupling = sortBy (\a b -> compare (snd b) (snd a)) $
      Array.filter (\(Tuple _ c) -> c > 1)
        (Map.toUnfoldable crossModuleCallerCounts :: Array (Tuple String Int))

    -- Composite difficulty score (0-3)
    difficulty =
      (if isTangled then 2 else if isTree || isFlat then 0 else 1) +
      (if Array.length highCoupling > 3 then 1 else 0)
    difficultyLabel = case difficulty of
      0 -> { text: "Low", color: "#0d904f", bg: "#f1f8f1", border: "#c8e6c9" }
      1 -> { text: "Moderate", color: "#e65100", bg: "#fff8f0", border: "#ffe0b2" }
      _ -> { text: "High", color: "#c62828", bg: "#fff8f8", border: "#ffcdd2" }
  in
  HH.div [ HP.style $ "border: 1px solid " <> difficultyLabel.border <> "; border-radius: 4px; padding: 12px; margin-bottom: 12px; background: " <> difficultyLabel.bg <> ";" ]
    ( [ HH.div [ HP.style "display: flex; justify-content: space-between; align-items: baseline; margin-bottom: 8px;" ]
          [ HH.h3 [ HP.style "margin: 0; font-size: 14px; font-weight: 600; color: #333;" ]
              [ HH.text "Refactoring Difficulty" ]
          , HH.span [ HP.style $ "font-size: 13px; font-weight: 700; color: " <> difficultyLabel.color <> ";" ]
              [ HH.text difficultyLabel.text ]
          ]
      ]
      -- Structural diagnosis
      <> (if isFlat
          then [ HH.div [ HP.style "font-size: 12px; color: #0d904f; margin-bottom: 6px;" ]
                   [ HH.text "Independent declarations — no internal call dependencies. Can freely move any declaration." ] ]
          else if isTree
          then [ HH.div [ HP.style "font-size: 12px; color: #0d904f; margin-bottom: 6px;" ]
                   [ HH.text $ "Tree-structured (" <> show m.bridgeCount <> " bridges). Can split at any bridge edge — "
                       <> show m.articulationPointCount <> " routing points are not tangled." ] ]
          else if isTangled
          then [ HH.div [ HP.style "font-size: 12px; color: #c62828; margin-bottom: 6px;" ]
                   [ HH.text $ "Tangled structure (" <> showPercent m.treelikeness <> " treelikeness). "
                       <> show (Array.length tangledAPs) <> " declarations embedded in cycles — moving them cascades." ] ]
          else [ HH.div [ HP.style "font-size: 12px; color: #555; margin-bottom: 6px;" ]
                   [ HH.text $ "Mixed structure: " <> showPercent m.treelikeness <> " treelikeness, "
                       <> show m.biconnectedComponentCount <> " clusters." ] ])
      -- Tangled hub functions (only shown when genuinely tangled)
      <> (if Array.length tangledAPs > 0
          then [ HH.div [ HP.style "margin-bottom: 8px;" ]
                   ( [ HH.div [ HP.style "font-size: 11px; color: #c62828; font-weight: 600; margin-bottom: 4px;" ]
                         [ HH.text "Tangled hub functions:" ]
                     ]
                     <> (Array.take 10 tangledAPs <#> \name ->
                       HH.div [ HP.style "font-size: 12px; color: #333; padding: 1px 0;" ]
                         [ HH.text name ]
                     )
                     <> (if Array.length tangledAPs > 10
                         then [ HH.div [ HP.style "font-size: 11px; color: #888;" ]
                                  [ HH.text $ "... and " <> show (Array.length tangledAPs - 10) <> " more" ] ]
                         else [])
                   )
               ]
          else [])
      -- Large clusters (concerning if they exist)
      <> (if hasLargeCluster
          then [ HH.div [ HP.style "font-size: 11px; color: #e65100; margin-bottom: 6px;" ]
                   [ HH.text $ "Largest tightly-coupled cluster: " <> show m.maxBlockSize
                       <> " declarations that cannot be separated without breaking cycles." ] ]
          else [])
      -- Cross-module coupling
      <> (if Array.length highCoupling > 0
          then [ HH.div []
                   ( [ HH.div [ HP.style "font-size: 11px; color: #e65100; font-weight: 600; margin-bottom: 4px;" ]
                         [ HH.text "High cross-module coupling:" ]
                     ]
                     <> (Array.take 5 highCoupling <#> \(Tuple name count) ->
                       HH.div [ HP.style "font-size: 12px; color: #333; padding: 1px 0;" ]
                         [ HH.text $ name <> " (" <> show count <> " external calls)" ]
                     )
                   )
               ]
          else [])
    )

renderClusterList :: forall m. MonadAff m => Dec.DecompInfo -> State -> H.ComponentHTML Action () m
renderClusterList info _state =
  let
    nonBridge = Array.filter (not <<< _.isBridge) info.blocks
    sorted = sortBy (\a b -> compare (Set.size b.nodes) (Set.size a.nodes)) nonBridge
  in
    if Array.length sorted == 0 then HH.text ""
    else
    HH.div [ HP.style "border: 1px solid #ddd; border-radius: 4px; padding: 12px;" ]
      ( [ HH.h3 [ HP.style "margin: 0 0 8px; font-size: 14px; font-weight: 600; color: #333;" ]
            [ HH.text "Function Clusters" ]
        , HH.p [ HP.style "margin: 0 0 8px; font-size: 11px; color: #888;" ]
            [ HH.text "Tightly coupled groups of declarations (biconnected components)" ]
        ]
        <> (sorted <#> \block ->
          let
            members = Array.sort (Set.toUnfoldable block.nodes :: Array String)
            n = Set.size block.nodes
          in
          HH.div [ HP.style $ "padding: 8px; margin-bottom: 6px; border-left: 3px solid " <> blockColor block.index <> "; background: #fafafa; border-radius: 2px;" ]
            [ HH.div [ HP.style "font-size: 12px; font-weight: 600; color: #333; margin-bottom: 4px;" ]
                [ HH.text $ show n <> " declarations" ]
            , HH.div [ HP.style "font-size: 11px; color: #555; line-height: 1.6;" ]
                [ HH.text $ String.joinWith ", " members ]
            ]
        )
      )

-- | Classify a module by its package source
data CouplingCategory = Architectural | Library | Infrastructure

classifyModule :: Map.Map String String -> String -> CouplingCategory
classifyModule sourceMap modName =
  case Map.lookup modName sourceMap of
    Just "workspace" -> Architectural
    Just "extra" -> Library
    Just "registry" -> Infrastructure
    Just _ -> Infrastructure
    Nothing -> Infrastructure  -- Unknown modules are likely registry

renderCrossModuleCoupling :: forall m. MonadAff m => State -> H.ComponentHTML Action () m
renderCrossModuleCoupling state =
  let
    sourceMap = state.input.moduleSourceMap

    -- Group by target module
    byModule = foldl (\acc edge ->
      Map.alter (Just <<< Array.cons edge <<< fromMaybe []) edge.toModule acc
    ) Map.empty state.crossModuleEdges
    allEntries = Map.toUnfoldable byModule :: Array (Tuple String (Array { from :: String, to :: String, toModule :: String }))

    -- Categorize
    architectural = sortBy (\a b -> compare (Array.length (snd b)) (Array.length (snd a))) $
      Array.filter (\(Tuple modName _) -> case classifyModule sourceMap modName of
        Architectural -> true
        _ -> false) allEntries
    library = sortBy (\a b -> compare (Array.length (snd b)) (Array.length (snd a))) $
      Array.filter (\(Tuple modName _) -> case classifyModule sourceMap modName of
        Library -> true
        _ -> false) allEntries
    infrastructure = sortBy (\a b -> compare (Array.length (snd b)) (Array.length (snd a))) $
      Array.filter (\(Tuple modName _) -> case classifyModule sourceMap modName of
        Infrastructure -> true
        _ -> false) allEntries

    renderEntry (Tuple modName edges) =
      HH.div [ HP.style "padding: 3px 0; font-size: 12px;" ]
        [ HH.span [ HP.style "font-weight: 500; color: #333;" ]
            [ HH.text $ shortModuleName modName ]
        , HH.span [ HP.style "color: #888;" ]
            [ HH.text $ " (" <> show (Array.length edges) <> " calls)" ]
        ]

    renderSection title color entries maxShow =
      if Array.length entries == 0 then []
      else
        [ HH.div [ HP.style $ "font-size: 11px; font-weight: 600; color: " <> color <> "; margin: 8px 0 4px; text-transform: uppercase; letter-spacing: 0.5px;" ]
            [ HH.text $ title <> " (" <> show (Array.length entries) <> ")" ]
        ]
        <> (Array.take maxShow entries <#> renderEntry)
        <> (if Array.length entries > maxShow
            then [ HH.div [ HP.style "font-size: 11px; color: #888; padding: 2px 0;" ]
                     [ HH.text $ "... and " <> show (Array.length entries - maxShow) <> " more" ] ]
            else [])
  in
  HH.div [ HP.style "border: 1px solid #e3d5b0; border-radius: 4px; padding: 12px; background: #fdf8ef;" ]
    ( [ HH.h3 [ HP.style "margin: 0 0 4px; font-size: 14px; font-weight: 600; color: #8b6914;" ]
          [ HH.text "Cross-Module Coupling" ]
      ]
      <> renderSection "Sibling modules" "#2563eb" architectural 8
      <> renderSection "Library" "#7c3aed" library 5
      <> renderSection "Infrastructure" "#6b7280" infrastructure 3
    )

renderLegend :: forall m. H.ComponentHTML Action () m
renderLegend =
  HH.div [ HP.style "border: 1px solid #ddd; border-radius: 4px; padding: 12px;" ]
    [ HH.div [ HP.style "display: flex; align-items: center; gap: 6px; margin-bottom: 6px; font-size: 12px;" ]
        [ HH.span [ HP.style "display: inline-block; width: 12px; height: 12px; background: #fff; border: 2px solid #333; transform: rotate(45deg);" ] []
        , HH.text "Hub function (articulation point)"
        ]
    , HH.div [ HP.style "display: flex; align-items: center; gap: 6px; margin-bottom: 6px; font-size: 12px;" ]
        [ HH.span [ HP.style "display: inline-block; width: 20px; height: 0; border-top: 2px dashed #999;" ] []
        , HH.text "Cross-module call"
        ]
    , HH.div [ HP.style "display: flex; align-items: center; gap: 6px; font-size: 12px;" ]
        [ HH.span [ HP.style "display: inline-block; width: 12px; height: 12px; border-radius: 50%; background: #3b82f6;" ] []
        , HH.text "value"
        , HH.span [ HP.style "display: inline-block; width: 12px; height: 12px; border-radius: 50%; background: #10b981; margin-left: 8px;" ] []
        , HH.text "data/newtype"
        , HH.span [ HP.style "display: inline-block; width: 12px; height: 12px; border-radius: 50%; background: #f59e0b; margin-left: 8px;" ] []
        , HH.text "class"
        ]
    ]

metricRow :: forall w i. String -> String -> HH.HTML w i
metricRow label value =
  HH.div [ HP.style "display: flex; justify-content: space-between; padding: 3px 0; font-size: 13px;" ]
    [ HH.span [ HP.style "color: #666;" ] [ HH.text label ]
    , HH.span [ HP.style "font-weight: 600; color: #333;" ] [ HH.text value ]
    ]

showPercent :: Number -> String
showPercent n =
  let pct = n * 100.0
      rounded = Int.round (pct * 10.0)
  in show (Int.toNumber rounded / 10.0) <> "%"

snd :: forall a b. Tuple a b -> b
snd (Tuple _ b) = b

shortModuleName :: String -> String
shortModuleName name =
  fromMaybe name $ Array.last (String.split (String.Pattern ".") name)

-- =============================================================================
-- Actions
-- =============================================================================

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  Initialize -> computeAndRender
  Receive input -> do
    state <- H.get
    when (input.moduleName /= state.input.moduleName) do
      H.modify_ _ { input = input, subDeclAnalysis = Nothing, subDeclDecomp = Nothing, subDeclGraph = Nothing }
      computeAndRender
  SwitchView mode -> do
    H.modify_ _ { viewMode = mode, selectedGroup = Nothing }
    case mode of
      SubDeclarationView -> loadSubDeclarationAnalysis
      DeclarationView -> computeAndRender
  SelectConcernGroup idx -> do
    state <- H.get
    let newIdx = case state.selectedGroup of
          Just i | i == idx -> Nothing  -- Toggle off
          _ -> Just idx
    H.modify_ _ { selectedGroup = newIdx }
  PickCompareModule modName ->
    when (modName /= "") do
      H.raise (CompareWith modName)

loadSubDeclarationAnalysis :: forall m. MonadAff m => H.HalogenM State Action () Output m Unit
loadSubDeclarationAnalysis = do
  state <- H.get
  -- Check if already loaded
  case state.subDeclAnalysis of
    Just analysis -> renderSubDeclGraph analysis
    Nothing -> do
      -- Fetch module source
      result <- liftAff $ Loader.fetchModuleSource state.input.moduleName
      case result of
        Left err -> do
          log $ "[SubDecl] Failed to fetch source for " <> state.input.moduleName <> ": " <> err
        Right src -> do
          let analysis = SDA.analyzeModuleSource src.source
          log $ "[SubDecl] " <> state.input.moduleName <> ": "
              <> show (Array.length analysis.allBranches) <> " branches, "
              <> show (Set.size analysis.allStateFields) <> " state fields, "
              <> show (Array.length analysis.concernGroups) <> " concern groups"

          -- Convert to graph and run decomposition
          let { declarations, internalCalls } = SDA.branchesToDeclGraph analysis.allBranches
          let declNames = Set.fromFoldable $ declarations <#> _.name
          let edges = foldl (\acc call ->
                if Set.member call.callerName declNames && Set.member call.calleeName declNames
                then
                  Map.alter (Just <<< Set.insert call.calleeName <<< fromMaybe Set.empty) call.callerName
                    (Map.alter (Just <<< Set.insert call.callerName <<< fromMaybe Set.empty) call.calleeName acc)
                else acc
              ) Map.empty internalCalls
          let graph = { nodes: Set.toUnfoldable declNames :: Array String, edges }
          let info = Dec.analyzeGraph graph

          H.modify_ _ { subDeclAnalysis = Just analysis, subDeclDecomp = Just info, subDeclGraph = Just graph }
          renderSubDeclGraph analysis

renderSubDeclGraph :: forall m. MonadAff m => SDA.SubDeclAnalysis -> H.HalogenM State Action () Output m Unit
renderSubDeclGraph analysis = do
  state <- H.get
  case state.subDeclGraph of
    Just graph -> do
      liftEffect do
        clearContainer "#module-structure-graph"
        _ <- rerender "#module-structure-graph" (concernClusteredTree graph analysis.caseExpressions)
        pure unit
    _ -> pure unit

computeAndRender :: forall m. MonadAff m => H.HalogenM State Action () Output m Unit
computeAndRender = do
  state <- H.get
  let { graph, crossEdges } = buildCallGraph state.input
  let info = Dec.analyzeGraph graph

  log $ "[ModuleAnatomy] " <> state.input.moduleName <> ": "
      <> show (Array.length graph.nodes) <> " decls, "
      <> show info.metrics.biconnectedComponentCount <> " clusters, "
      <> show info.metrics.articulationPointCount <> " hubs, "
      <> show (Array.length crossEdges) <> " cross-module"

  H.modify_ _ { decompInfo = Just info, graph = Just graph, crossModuleEdges = crossEdges }

  liftEffect do
    clearContainer "#module-structure-graph"
    let kindMap = foldl (\acc d -> Map.insert d.name d.kind acc) Map.empty state.input.declarations
    _ <- rerender "#module-structure-graph" (callGraphTree graph info kindMap)
    pure unit

-- =============================================================================
-- Build call graph from function calls
-- =============================================================================

buildCallGraph :: Input -> { graph :: Dec.SimpleGraph String, crossEdges :: Array { from :: String, to :: String, toModule :: String } }
buildCallGraph input =
  let
    -- Internal calls: both caller and callee are in this module
    internalCalls = Array.filter (not <<< _.isCrossModule) input.functionCalls

    -- All declaration names in this module
    declNames = Set.fromFoldable $ input.declarations <#> _.name

    -- Build undirected edges from internal calls
    edges = foldl (\acc call ->
      if Set.member call.callerName declNames && Set.member call.calleeName declNames
      then
        Map.alter (Just <<< Set.insert call.calleeName <<< fromMaybe Set.empty) call.callerName
          (Map.alter (Just <<< Set.insert call.callerName <<< fromMaybe Set.empty) call.calleeName acc)
      else acc
    ) Map.empty internalCalls

    nodes = Set.toUnfoldable declNames :: Array String
    graph = { nodes, edges }

    -- Cross-module edges (for display)
    crossEdges = Array.concatMap (\call ->
      if call.isCrossModule
      then [{ from: call.callerName, to: call.calleeName, toModule: call.calleeModule }]
      else []
    ) input.functionCalls
    -- Also include incoming cross-module calls
    incomingCross = Array.concatMap (\call ->
      [{ from: call.callerName, to: call.calleeName, toModule: call.calleeModule }]
    ) input.crossModuleCalls
  in
    { graph, crossEdges: crossEdges <> incomingCross }

-- =============================================================================
-- HATS Rendering: Concern-Clustered Graph
-- =============================================================================

-- | Render branches clustered by parent case expression.
-- | Each case expression gets a circle of nodes; cross-group edges show the seams.
concernClusteredTree :: Dec.SimpleGraph String -> Array SDA.CaseExprInfo -> Tree
concernClusteredTree graph caseExprs =
  let
    nGroups = Array.length caseExprs
    width = 900.0
    height = 600.0
    centerX = width / 2.0
    centerY = height / 2.0

    -- Build branch → group index map for edge weight computation
    branchGroup = foldl (\acc (Tuple gi ce) ->
      foldl (\a branch -> Map.insert branch.name gi a) acc ce.branches
    ) Map.empty (mapWithIndex Tuple caseExprs)

    -- Compute cross-group edge weights: how many edges between group i and group j
    crossGroupWeights = foldl (\acc name ->
      let targets = fromMaybe Set.empty (Map.lookup name graph.edges)
      in foldl (\a tgt ->
        if name < tgt then
          case Map.lookup name branchGroup, Map.lookup tgt branchGroup of
            Just gi, Just gj | gi /= gj ->
              let key = Tuple (min gi gj) (max gi gj)
              in Map.alter (Just <<< (_ + 1) <<< fromMaybe 0) key a
            _, _ -> a
        else a
      ) acc (Set.toUnfoldable targets :: Array String)
    ) (Map.empty :: Map.Map (Tuple Int Int) Int) graph.nodes

    -- Group radii (needed for collision avoidance)
    groupRadii = mapWithIndex (\_ ce ->
      max 25.0 (Number.sqrt (Int.toNumber (Array.length ce.branches)) * 18.0)
    ) caseExprs

    -- Force-directed layout for group centers
    -- Initialize in a circle, then iterate
    mainRadius = min (width * 0.32) (height * 0.32)
    initialPositions = mapWithIndex (\i _ce ->
      let angle = 2.0 * Number.pi * Int.toNumber i / Int.toNumber (max nGroups 1) - Number.pi / 2.0
      in { x: centerX + mainRadius * Number.cos angle
         , y: centerY + mainRadius * Number.sin angle
         , vx: 0.0, vy: 0.0
         }
    ) caseExprs

    -- Run 200 iterations of force simulation
    groupCenters = map (\p -> { x: p.x, y: p.y }) $
      forceLayoutGroups { width, height, centerX, centerY }
        crossGroupWeights groupRadii initialPositions 200

    -- Build node positions: each group's nodes in a circle around its center
    nodePositions = foldl (\acc (Tuple gi ce) ->
      let
        center = fromMaybe { x: centerX, y: centerY } (groupCenters Array.!! gi)
        n = Array.length ce.branches
        r = max 25.0 (Number.sqrt (Int.toNumber n) * 18.0)
      in foldl (\a (Tuple ni branch) ->
        let
          angle = 2.0 * Number.pi * Int.toNumber ni / Int.toNumber (max n 1) - Number.pi / 2.0
          x = center.x + r * Number.cos angle
          y = center.y + r * Number.sin angle
        in Map.insert branch.name { x, y, group: gi } a
      ) acc (mapWithIndex Tuple ce.branches)
    ) Map.empty (mapWithIndex Tuple caseExprs)

    -- Group background circles (hulls)
    groupBgs = Array.mapMaybe (\(Tuple gi ce) ->
      case groupCenters Array.!! gi of
        Nothing -> Nothing
        Just center ->
          let
            n = Array.length ce.branches
            r = max 25.0 (Number.sqrt (Int.toNumber n) * 18.0) + 14.0
          in Just $ elem Circle
            [ staticNum "cx" center.x, staticNum "cy" center.y
            , staticNum "r" r
            , staticStr "fill" (blockColor gi)
            , staticNum "fill-opacity" 0.08
            , staticStr "stroke" (blockColor gi)
            , staticNum "stroke-opacity" 0.2
            , staticNum "stroke-width" 1.5
            ] []
    ) (mapWithIndex Tuple caseExprs)

    -- Group labels (case expression function name)
    groupLabels = Array.mapMaybe (\(Tuple gi ce) ->
      case groupCenters Array.!! gi of
        Nothing -> Nothing
        Just center ->
          let
            n = Array.length ce.branches
            r = max 25.0 (Number.sqrt (Int.toNumber n) * 18.0) + 22.0
          in Just $ elem Text
            [ staticNum "x" center.x, staticNum "y" (center.y - r)
            , staticStr "text-anchor" "middle", staticStr "font-size" "11px"
            , staticStr "font-weight" "600"
            , staticStr "fill" (blockColor gi), staticStr "font-family" "system-ui, sans-serif"
            , staticStr "textContent" $ ce.functionName <> " (" <> show n <> ")"
            ] []
    ) (mapWithIndex Tuple caseExprs)

    -- Edges: intra-group = curves through group center; inter-group = straight lines
    edgeElems = Array.concatMap (\name ->
      let targets = fromMaybe Set.empty (Map.lookup name graph.edges)
      in Array.mapMaybe (\tgt ->
        if name < tgt then
          case Map.lookup name nodePositions, Map.lookup tgt nodePositions of
            Just p1, Just p2 ->
              let sameGroup = p1.group == p2.group
              in if sameGroup then
                -- Intra-group: quadratic bezier curved through group center
                case groupCenters Array.!! p1.group of
                  Just gc ->
                    let
                      -- Control point biased toward group center
                      cpx = (p1.x + p2.x) / 2.0 * 0.4 + gc.x * 0.6
                      cpy = (p1.y + p2.y) / 2.0 * 0.4 + gc.y * 0.6
                      d = "M" <> show p1.x <> "," <> show p1.y
                        <> " Q" <> show cpx <> "," <> show cpy
                        <> " " <> show p2.x <> "," <> show p2.y
                    in Just $ elem Path
                      [ staticStr "d" d
                      , staticStr "fill" "none"
                      , staticStr "stroke" (blockColor p1.group)
                      , staticNum "stroke-width" 0.5
                      , staticNum "stroke-opacity" 0.12
                      ] []
                  Nothing -> Nothing
              else
                -- Inter-group: straight line, colored by source group
                Just $ elem Line
                  [ staticNum "x1" p1.x, staticNum "y1" p1.y
                  , staticNum "x2" p2.x, staticNum "y2" p2.y
                  , staticStr "stroke" (blockColor p1.group)
                  , staticNum "stroke-width" 1.0
                  , staticNum "stroke-opacity" 0.25
                  ] []
            _, _ -> Nothing
        else Nothing
      ) (Set.toUnfoldable targets :: Array String)
    ) graph.nodes

    -- Node circles (no labels — hover to be added later)
    nodeElems = Array.mapMaybe (\name ->
      case Map.lookup name nodePositions of
        Nothing -> Nothing
        Just pos ->
          Just $ elem Circle
            [ staticNum "cx" pos.x, staticNum "cy" pos.y, staticNum "r" 5.0
            , staticStr "fill" (blockColor pos.group)
            , staticStr "stroke" "#fff", staticNum "stroke-width" 0.8
            ] []
    ) graph.nodes
  in
    elem SVG
      [ staticStr "viewBox" $ "0 0 " <> show width <> " " <> show height
      , staticStr "width" "100%"
      , staticStr "preserveAspectRatio" "xMidYMid meet"
      , staticStr "style" "background: transparent; border-radius: 4px;"
      ]
      (groupBgs <> edgeElems <> nodeElems <> groupLabels)

-- =============================================================================
-- HATS Rendering: Call Graph (Declaration view)
-- =============================================================================

callGraphTree :: Dec.SimpleGraph String -> Dec.DecompInfo -> Map.Map String String -> Tree
callGraphTree graph info kindMap =
  let
    -- Layout: pure computation from hylograph-layout
    bctLayout = BCT.layout BCT.defaultConfig graph
    width = bctLayout.width
    height = bctLayout.height

    -- Block background circles
    blockBgs = Array.mapMaybe (\(Tuple blockIdx bl) ->
      if bl.isBridge then Nothing
      else Just $ elem Circle
        [ staticNum "cx" bl.x, staticNum "cy" bl.y
        , staticNum "r" (bl.radius + 10.0)
        , staticStr "fill" (blockColor blockIdx)
        , staticNum "fill-opacity" 0.06
        , staticStr "stroke" (blockColor blockIdx)
        , staticNum "stroke-opacity" 0.15
        , staticNum "stroke-width" 1.0
        ] []
    ) (Map.toUnfoldable bctLayout.blocks :: Array (Tuple Int BCT.BlockLayout))

    -- Internal edges
    edgeElems = Array.concatMap (\name ->
      let targets = fromMaybe Set.empty (Map.lookup name graph.edges)
      in Array.mapMaybe (\tgt ->
        if name < tgt then
          case Map.lookup name bctLayout.nodes, Map.lookup tgt bctLayout.nodes of
            Just p1, Just p2 ->
              let
                isBridge = Set.member (Tuple name tgt) info.bridgeSet
                edgeColor = case Map.lookup (Tuple name tgt) info.edgeBlock of
                  Just bi -> blockColor bi
                  Nothing -> "#ccc"
              in Just $ elem Line
                [ staticNum "x1" p1.x, staticNum "y1" p1.y
                , staticNum "x2" p2.x, staticNum "y2" p2.y
                , staticStr "stroke" edgeColor
                , staticNum "stroke-width" (if isBridge then 2.0 else 1.0)
                , staticStr "stroke-dasharray" (if isBridge then "5,3" else "")
                , staticNum "stroke-opacity" (if isBridge then 0.6 else 0.3)
                ] []
            _, _ -> Nothing
        else Nothing
      ) (Set.toUnfoldable targets :: Array String)
    ) graph.nodes

    -- Node elements
    nodeElems = Array.mapMaybe (\name ->
      case Map.lookup name bctLayout.nodes of
        Nothing -> Nothing
        Just nl ->
          let
            kind = fromMaybe "value" (Map.lookup name kindMap)
            fill = if nl.isArticulationPoint then "#fff" else kindColor kind
            r = if nl.isArticulationPoint then 7.0 else 5.0
          in Just $
            if nl.isArticulationPoint then
              elem Group []
                [ elem Rect
                    [ staticNum "x" (nl.x - r), staticNum "y" (nl.y - r)
                    , staticNum "width" (r * 2.0), staticNum "height" (r * 2.0)
                    , staticStr "transform" $ "rotate(45," <> show nl.x <> "," <> show nl.y <> ")"
                    , staticStr "fill" "#fff", staticStr "stroke" "#333", staticNum "stroke-width" 1.5
                    ] []
                , elem Text
                    [ staticNum "x" nl.x, staticNum "y" (nl.y - r - 4.0)
                    , staticStr "text-anchor" "start", staticStr "font-size" "9px"
                    , staticStr "font-weight" "600"
                    , staticStr "transform" $ "rotate(-45," <> show nl.x <> "," <> show (nl.y - r - 4.0) <> ")"
                    , staticStr "fill" "#c62828", staticStr "font-family" "system-ui, sans-serif"
                    , staticStr "textContent" name
                    ] []
                ]
            else
              elem Group []
                [ elem Circle
                    [ staticNum "cx" nl.x, staticNum "cy" nl.y, staticNum "r" r
                    , staticStr "fill" fill, staticStr "stroke" "#fff", staticNum "stroke-width" 0.5
                    ] []
                , elem Text
                    [ staticNum "x" nl.x, staticNum "y" (nl.y - r - 2.0)
                    , staticStr "text-anchor" "start", staticStr "font-size" "8px"
                    , staticStr "transform" $ "rotate(-45," <> show nl.x <> "," <> show (nl.y - r - 2.0) <> ")"
                    , staticStr "fill" "#555", staticStr "font-family" "system-ui, sans-serif"
                    , staticStr "textContent" name
                    ] []
                ]
    ) graph.nodes
  in
    elem SVG
      [ staticStr "viewBox" $ "0 0 " <> show width <> " " <> show height
      , staticStr "width" "100%"
      , staticStr "preserveAspectRatio" "xMidYMid meet"
      , staticStr "style" "background: transparent; border-radius: 4px;"
      ]
      (blockBgs <> edgeElems <> nodeElems)
