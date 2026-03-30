-- | Edge Bundle Panel Component
-- |
-- | Standalone Halogen component showing the internal call structure of a module
-- | as a hierarchical edge bundling diagram. Declarations are arranged radially,
-- | with bundled curves showing call relationships. Uses the hylograph-layout
-- | EdgeBundle algorithm.
module CE2.Component.EdgeBundlePanel
  ( component
  , Input
  , Output(..)
  , Query(..)
  , Slot
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldMap)
import Data.Int (toNumber) as Int
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (abs) as Num
import Data.Number (pi)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import CE2.Data.Loader as Loader
import CE2.Util.SVG (svgElem, sa)
import CE2.Viz.DeclarationArcDiagram (isEffectful)
import DataViz.Layout.Hierarchy.EdgeBundle as EdgeBundle

-- =============================================================================
-- Types
-- =============================================================================

type Input =
  { moduleName :: String
  , declarations :: Array Loader.V2Declaration
  , functionCalls :: Map.Map Int (Array Loader.V2FunctionCall)
  }

data Output
  = DeclarationClicked String

data Query a = NoQuery a

type Slot = H.Slot Query Output

type State =
  { lastInput :: Input
  , bundleResult :: Maybe (EdgeBundle.EdgeBundleResult DeclNode)
  , hoveredNode :: Maybe String
  }

-- | Intermediate node type for the edge bundle
type DeclNode =
  { name :: String
  , kind :: String
  , callees :: Array String
  , effectful :: Boolean
  }

data Action
  = Initialize
  | Receive Input
  | NodeHovered (Maybe String)
  | NodeClicked String

-- =============================================================================
-- Component
-- =============================================================================

component :: forall m. MonadAff m => H.Component Query Input Output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , receive = Just <<< Receive
        , initialize = Just Initialize
        }
    }

initialState :: Input -> State
initialState input =
  { lastInput: input
  , bundleResult: Nothing
  , hoveredNode: Nothing
  }

-- =============================================================================
-- Render
-- =============================================================================

render :: forall m. State -> H.ComponentHTML Action () m
render state = case state.bundleResult of
  Nothing ->
    HH.div [ HP.style "padding: 24px; color: #999; font-size: 12px; text-align: center;" ]
      [ HH.text "No internal call structure \x2014 declarations do not call each other" ]
  Just result
    | Array.null result.links ->
        HH.div [ HP.style "padding: 24px; color: #999; font-size: 12px; text-align: center;" ]
          [ HH.text "No internal call structure \x2014 declarations do not call each other" ]
    | otherwise ->
        let
          leafNodes = Array.filter _.isLeaf result.nodes
          radius = 300.0
          viewSize = radius * 2.0 + 120.0
          viewBox = show (-viewSize / 2.0) <> " " <> show (-viewSize / 2.0) <> " " <> show viewSize <> " " <> show viewSize
        in
        HH.div [ HP.style "padding: 8px;" ]
          [ HH.div [ HP.style "font-size: 11px; color: #888; margin-bottom: 6px;" ]
              [ HH.text $ "Edge bundle \x2014 " <> show (Array.length leafNodes) <> " declarations, "
                  <> show (Array.length result.links) <> " calls" ]
          , svgElem "svg"
              [ sa "viewBox" viewBox
              , sa "width" "100%"
              , sa "preserveAspectRatio" "xMidYMid meet"
              , HP.style "display: block; border: 1px solid #d5d0c4; border-radius: 4px; background: #faf8f3; max-height: 600px;"
              ]
              ( -- Links first (behind nodes)
                (result.links <#> renderLink state)
                -- Nodes
                <> (leafNodes <#> renderNode state)
                -- Labels
                <> (leafNodes <#> renderLabel state)
              )
          ]

-- =============================================================================
-- SVG Rendering
-- =============================================================================

renderLink :: forall w. State -> EdgeBundle.BundledLink -> HH.HTML w Action
renderLink state link =
  let
    isHighlighted = case state.hoveredNode of
      Just name -> name == link.source || name == link.target
      Nothing -> false
    opacity = case state.hoveredNode of
      Nothing -> "0.15"
      Just _ -> if isHighlighted then "0.7" else "0.03"
    strokeWidth = if isHighlighted then "1.5" else "0.8"
    color = if isHighlighted then "#e07020" else "#c8b890"
  in
  svgElem "path"
    [ sa "d" link.path
    , sa "fill" "none"
    , sa "stroke" color
    , sa "stroke-width" strokeWidth
    , sa "stroke-opacity" opacity
    , sa "class" "edge-bundle-link"
    , HP.style "transition: stroke-opacity 150ms ease, stroke 150ms ease;"
    ] []

renderNode :: forall w. State -> EdgeBundle.PositionedNode DeclNode -> HH.HTML w Action
renderNode state node =
  let
    isHovered = state.hoveredNode == Just node.shortName
    r = if isHovered then 5.0 else 3.5
    fillColor = case node.data_ of
      Just d -> kindColor d.kind d.effectful
      Nothing -> "#999"
    strokeColor = if isHovered then "#333" else "rgba(0,0,0,0.2)"
  in
  svgElem "circle"
    [ sa "cx" (show node.cartX)
    , sa "cy" (show node.cartY)
    , sa "r" (show r)
    , sa "fill" fillColor
    , sa "stroke" strokeColor
    , sa "stroke-width" (if isHovered then "1.5" else "0.5")
    , sa "class" "edge-bundle-node"
    , HP.style "cursor: pointer; transition: r 100ms ease;"
    , HE.onMouseEnter \_ -> NodeHovered (Just node.shortName)
    , HE.onMouseLeave \_ -> NodeHovered Nothing
    , HE.onClick \_ -> NodeClicked node.shortName
    ] []

renderLabel :: forall w. State -> EdgeBundle.PositionedNode DeclNode -> HH.HTML w Action
renderLabel state node =
  let
    isHovered = state.hoveredNode == Just node.shortName
    -- Rotate label so it reads outward from center
    angleDeg = node.x * 180.0 / pi
    -- Flip text on the left side so it's always readable
    isLeftSide = node.x > pi / 2.0 && node.x < 3.0 * pi / 2.0
    textAnchor = if isLeftSide then "end" else "start"
    labelRotation = if isLeftSide then angleDeg + 180.0 else angleDeg
    -- Offset from node
    labelRadius = node.y + 8.0
    labelX = labelRadius
    labelY = 0.0
    transform = "rotate(" <> show angleDeg <> ") translate(" <> show labelX <> ",0) rotate(" <> show (if isLeftSide then 180.0 else 0.0) <> ")"
    opacity = case state.hoveredNode of
      Nothing -> if isHovered then "1" else "0.6"
      Just _ -> if isHovered then "1" else "0.2"
  in
  svgElem "text"
    [ sa "transform" transform
    , sa "text-anchor" textAnchor
    , sa "dominant-baseline" "middle"
    , sa "font-size" (if isHovered then "10" else "8")
    , sa "font-family" "'Fira Code', 'SF Mono', monospace"
    , sa "fill" "#555"
    , sa "opacity" opacity
    , HP.style "cursor: pointer; transition: opacity 150ms ease;"
    , HE.onMouseEnter \_ -> NodeHovered (Just node.shortName)
    , HE.onMouseLeave \_ -> NodeHovered Nothing
    , HE.onClick \_ -> NodeClicked node.shortName
    ] [ HH.text node.shortName ]

-- =============================================================================
-- Colors
-- =============================================================================

kindColor :: String -> Boolean -> String
kindColor kind effectful
  | effectful = "#e67e22"
  | otherwise = case kind of
      "value" -> "#4e79a7"
      "data" -> "#59a14f"
      "newtype" -> "#76b7b2"
      "class" -> "#f28e2b"
      "synonym" -> "#edc948"
      "foreign" -> "#e15759"
      _ -> "#999"

-- =============================================================================
-- Action Handlers
-- =============================================================================

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  Initialize -> do
    state <- H.get
    computeBundle state.lastInput

  Receive input -> do
    state <- H.get
    let changed = input.moduleName /= state.lastInput.moduleName
              || Array.length input.declarations /= Array.length state.lastInput.declarations
    H.modify_ _ { lastInput = input }
    when changed do
      computeBundle input

  NodeHovered mName ->
    H.modify_ _ { hoveredNode = mName }

  NodeClicked name ->
    H.raise (DeclarationClicked name)

-- =============================================================================
-- Layout Computation
-- =============================================================================

computeBundle :: forall m. MonadAff m => Input -> H.HalogenM State Action () Output m Unit
computeBundle input = do
  let
    moduleName = input.moduleName

    -- Extract intra-module calls
    allCalls :: Array Loader.V2FunctionCall
    allCalls = foldMap identity input.functionCalls

    intraCalls :: Array { from :: String, to :: String }
    intraCalls = allCalls
      # Array.filter (\c -> not c.isCrossModule)
      # Array.filter (\c -> c.calleeModule == moduleName)
      # Array.filter (\c -> c.callerName /= c.calleeName)
      # Array.nubBy (\a b -> compare (Tuple a.callerName a.calleeName) (Tuple b.callerName b.calleeName))
      # map (\c -> { from: c.callerName, to: c.calleeName })

  if Array.null intraCalls then
    H.modify_ _ { bundleResult = Nothing }
  else do
    let
      -- Names participating in calls
      callNames = intraCalls
        # foldMap (\c -> Set.insert c.from (Set.singleton c.to))

      -- Build adjacency map: caller -> [callees]
      calleeMap = Array.foldl (\acc c ->
        Map.alter (Just <<< Array.cons c.to <<< fromMaybe []) c.from acc
      ) Map.empty intraCalls

      -- Build kind lookup
      declKindMap = Map.fromFoldable $
        input.declarations <#> \d -> Tuple d.name d.kind
      declSigMap = Map.fromFoldable $
        input.declarations <#> \d -> Tuple d.name d.typeSignature

      -- Create nodes for the edge bundle
      declNodes :: Array DeclNode
      declNodes = (Set.toUnfoldable callNames :: Array String) <#> \name ->
        { name
        , kind: fromMaybe "value" (Map.lookup name declKindMap)
        , callees: fromMaybe [] (Map.lookup name calleeMap)
        , effectful: isEffectful (join $ Map.lookup name declSigMap)
        }

      -- Run edge bundle layout
      result = EdgeBundle.edgeBundle
        { getName: _.name
        , getImports: _.callees
        , beta: 0.85
        , innerRadius: 80.0
        , outerRadius: 280.0
        }
        declNodes

    H.modify_ _ { bundleResult = Just result }
