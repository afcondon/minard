-- | Module Planet — Unified Module View
-- |
-- | Lightroom-style panel layout combining all module-level views:
-- | Signatures, Structure (layers/concerns/cutpoints), Dependencies,
-- | Annotations, and Overview. Panels toggle open/closed independently.
-- | On large displays, multiple panels visible simultaneously.
module CE2.Component.ModulePlanetViz
  ( component
  , Input
  , Output(..)
  , Query(..)
  , Slot
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))

import CE2.Component.ModuleAnnotationsViz as AnnotationsViz
import CE2.Component.ModuleSignaturesViz as SignaturesViz
import CE2.Viz.BlameRibbon as BlameRibbon
import CE2.Viz.CommitSparkline as Spark
import CE2.Component.DeclarationUsageGraph as UsageGraphViz
import CE2.Data.Loader as Loader

-- =============================================================================
-- Types
-- =============================================================================

type Input =
  { packageName :: String
  , moduleName :: String
  , declarations :: Array Loader.V2Declaration
  , annotations :: Array Loader.V2Annotation
  , functionCalls :: Map.Map Int (Array Loader.V2FunctionCall)
  , moduleNameToId :: Map.Map String Int
  }

data Output
  = DeclarationClicked String String String  -- pkg, mod, decl
  | AnnotationStatusChanged Int String
  | AnnotationReplyCreated
      { targetType :: String
      , targetId :: String
      , kind :: String
      , value :: String
      , supersedes :: Int
      }
  | CompareSnapshotsClicked

type Slot = H.Slot Query Output

data Query a = NoQuery a

-- | Which panels are currently open
data Panel = PanelSignatures | PanelDependencies | PanelAnnotations

derive instance eqPanel :: Eq Panel
derive instance ordPanel :: Ord Panel

type ChildSlots =
  ( signatures :: SignaturesViz.Slot Unit
  , dependencies :: UsageGraphViz.Slot Unit
  , annotations :: AnnotationsViz.Slot Unit
  )

_signatures :: Proxy "signatures"
_signatures = Proxy

_dependencies :: Proxy "dependencies"
_dependencies = Proxy

_annotations :: Proxy "annotations"
_annotations = Proxy

type State =
  { lastInput :: Input
  , openPanels :: Set Panel
  , focusedDeclaration :: Maybe String
  , blameData :: Maybe Loader.BlameResult
  , blameLoading :: Boolean
  , sparklineBars :: Array Spark.SparklineBar
  }

data Action
  = Initialize
  | Receive Input
  | TogglePanel Panel
  | FocusDeclaration (Maybe String)
  | HandleSignaturesOutput SignaturesViz.Output
  | HandleDependenciesOutput UsageGraphViz.Output
  | HandleAnnotationsOutput AnnotationsViz.Output

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
  , openPanels: Set.fromFoldable [PanelSignatures]
  , focusedDeclaration: Nothing
  , blameData: Nothing
  , blameLoading: false
  , sparklineBars: []
  }

-- =============================================================================
-- Render
-- =============================================================================

render :: forall m. MonadAff m => State -> H.ComponentHTML Action ChildSlots m
render state =
  HH.div
    [ HP.style "display: flex; flex-direction: column; width: 100%; height: 100%; overflow: hidden;" ]
    [ -- Sparkline + panel toggle bar
      renderPanelBar state
    -- Main area: blame ribbon (left) + panels (right)
    , HH.div
        [ HP.style "flex: 1; min-height: 0; display: flex;" ]
        [ -- Left edge: blame ribbon (persistent)
          HH.div
              [ HP.style "flex-shrink: 0; overflow-y: auto;" ]
              [ BlameRibbon.renderBlameRibbon
                  { blameData: state.blameData
                  , loading: state.blameLoading
                  , onLineClick: \_ -> TogglePanel PanelSignatures -- no-op for now
                  }
              ]
        -- Right: scrollable panel stack
        , HH.div
            [ HP.style "flex: 1; min-width: 0; overflow-y: auto;" ]
            ( Array.catMaybes
                [ if isPanelOpen PanelSignatures state
                    then Just (renderSignaturesPanel state)
                    else Nothing
                , if isPanelOpen PanelDependencies state || state.focusedDeclaration /= Nothing
                    then Just (renderDependenciesPanel state)
                    else Nothing
                , if isPanelOpen PanelAnnotations state
                    then Just (renderAnnotationsPanel state)
                    else Nothing
                ]
            )
        ]
    ]

-- | Top bar with panel toggle buttons
renderPanelBar :: forall m. State -> H.ComponentHTML Action ChildSlots m
renderPanelBar state =
  HH.div
    [ HP.style "display: flex; align-items: center; gap: 2px; padding: 6px 12px; background: #e8e4d8; border-bottom: 1px solid #d8d0bc; flex-shrink: 0;" ]
    [ HH.span
        [ HP.style "font-size: 12px; font-weight: 700; color: #555; margin-right: 12px; font-family: 'Courier New', Courier, monospace;" ]
        [ HH.text $ shortModuleName state.lastInput.moduleName ]
    , panelToggle "Signatures" PanelSignatures state
    , panelToggle "Dependencies" PanelDependencies state
    , panelToggle "Annotations" PanelAnnotations state
    , -- Sparkline (fills available space)
      Spark.renderSparkline state.sparklineBars
    , HH.span
        [ HP.style "font-size: 10px; color: #888; white-space: nowrap; margin-left: 8px;" ]
        [ HH.text $ show (Array.length state.lastInput.declarations) <> " decls"
            <> (if Array.length state.sparklineBars > 0 then " \x00B7 " <> show (Array.length state.sparklineBars) <> " commits" else "")
        ]
    ]

panelToggle :: forall m. String -> Panel -> State -> H.ComponentHTML Action ChildSlots m
panelToggle label panel state =
  let isOpen = isPanelOpen panel state
      style = if isOpen
        then "font-size: 11px; font-weight: 600; color: #333; background: #fff; border: 1px solid #c8c0a8; border-radius: 3px; padding: 3px 10px; cursor: pointer;"
        else "font-size: 11px; font-weight: 500; color: #888; background: transparent; border: 1px solid transparent; border-radius: 3px; padding: 3px 10px; cursor: pointer;"
  in
  HH.span
    [ HP.style style
    , HE.onClick \_ -> TogglePanel panel
    ]
    [ HH.text label ]

isPanelOpen :: Panel -> State -> Boolean
isPanelOpen panel state = Set.member panel state.openPanels

-- =============================================================================
-- Panel Rendering
-- =============================================================================

renderSignaturesPanel :: forall m. MonadAff m => State -> H.ComponentHTML Action ChildSlots m
renderSignaturesPanel state =
  let input = state.lastInput
  in
  HH.div
    [ HP.style "border-bottom: 2px solid #e8e4d8; position: relative; height: 80vh;" ]
    [ HH.slot _signatures unit SignaturesViz.component
        { packageName: input.packageName
        , moduleName: input.moduleName
        , declarations: input.declarations
        , functionCalls: input.functionCalls
        }
        HandleSignaturesOutput
    ]

renderDependenciesPanel :: forall m. MonadAff m => State -> H.ComponentHTML Action ChildSlots m
renderDependenciesPanel state =
  let
    input = state.lastInput
    declName = fromMaybe (firstValueDecl input.declarations) state.focusedDeclaration
    mDecl = Array.find (\d -> d.name == declName) input.declarations
    modId = Map.lookup input.moduleName input.moduleNameToId
    calls = fromMaybe [] (modId >>= \mid -> Map.lookup mid input.functionCalls)
  in
  HH.div
    [ HP.style "border-bottom: 2px solid #e8e4d8;" ]
    [ -- Header with close button
      HH.div
        [ HP.style "display: flex; align-items: center; justify-content: space-between; padding: 8px 16px; background: #f8f6f0; border-bottom: 1px solid #e8e4d8;" ]
        [ HH.span
            [ HP.style "font-size: 12px; font-weight: 600; color: #555; font-family: 'Courier New', Courier, monospace;" ]
            [ HH.text $ "Dependencies: " <> declName ]
        , HH.span
            [ HP.style "font-size: 14px; color: #999; cursor: pointer; padding: 2px 6px;"
            , HE.onClick \_ -> FocusDeclaration Nothing
            ]
            [ HH.text "\x00D7" ]
        ]
    -- Usage graph
    , HH.div
        [ HP.style "height: 500px; overflow: hidden;" ]
        [ HH.slot _dependencies unit UsageGraphViz.component
            { packageName: input.packageName
            , moduleName: input.moduleName
            , declarationName: declName
            , focusTypeSignature: mDecl >>= _.typeSignature
            , declarations: input.declarations
            , moduleCalls: calls
            , allCalls: input.functionCalls
            , moduleNameToId: input.moduleNameToId
            }
            HandleDependenciesOutput
        ]
    ]

renderAnnotationsPanel :: forall m. MonadAff m => State -> H.ComponentHTML Action ChildSlots m
renderAnnotationsPanel state =
  let input = state.lastInput
      declNames = input.declarations <#> _.name
  in
  HH.div
    [ HP.style "padding: 12px 16px;" ]
    [ HH.slot _annotations unit AnnotationsViz.component
        { moduleName: input.moduleName
        , annotations: input.annotations
        , declarationNames: declNames
        }
        HandleAnnotationsOutput
    ]

-- =============================================================================
-- Helpers
-- =============================================================================

shortModuleName :: String -> String
shortModuleName name =
  case Array.last (String.split (String.Pattern ".") name) of
    Just short -> short
    Nothing -> name

firstValueDecl :: Array Loader.V2Declaration -> String
firstValueDecl decls =
  case Array.find (\d -> d.kind == "value") decls of
    Just d -> d.name
    Nothing -> case Array.head decls of
      Just d -> d.name
      Nothing -> "component"

-- =============================================================================
-- Action Handlers
-- =============================================================================

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action ChildSlots Output m Unit
handleAction = case _ of
  Initialize -> do
    state <- H.get
    loadModuleData state.lastInput

  Receive input -> do
    state <- H.get
    let moduleChanged = input.moduleName /= state.lastInput.moduleName
    H.modify_ _ { lastInput = input }
    when moduleChanged do
      H.modify_ _ { blameData = Nothing, blameLoading = false, sparklineBars = [], focusedDeclaration = Nothing }
      loadModuleData input

  TogglePanel panel -> do
    state <- H.get
    let newPanels = if Set.member panel state.openPanels
          then Set.delete panel state.openPanels
          else Set.insert panel state.openPanels
    H.modify_ _ { openPanels = newPanels }

  FocusDeclaration mDecl -> do
    H.modify_ _ { focusedDeclaration = mDecl }
    -- Ensure dependencies panel is open when focusing
    case mDecl of
      Just _ -> do
        state <- H.get
        when (not $ Set.member PanelDependencies state.openPanels) do
          H.modify_ _ { openPanels = Set.insert PanelDependencies state.openPanels }
      Nothing -> pure unit

  HandleSignaturesOutput output -> case output of
    SignaturesViz.DeclarationClicked pkgName modName declName -> do
      log $ "[ModulePlanet] Signature clicked: " <> declName
      state <- H.get
      if modName == state.lastInput.moduleName
        then handleAction (FocusDeclaration (Just declName))
        else H.raise (DeclarationClicked pkgName modName declName)
    SignaturesViz.NavigateToStructure ->
      pure unit  -- Already on the planet page, no-op

  HandleDependenciesOutput output -> case output of
    UsageGraphViz.NodeClicked _pkgName modName declName -> do
      state <- H.get
      if modName == state.lastInput.moduleName
        then handleAction (FocusDeclaration (Just declName))
        else H.raise (DeclarationClicked state.lastInput.packageName modName declName)
    UsageGraphViz.ModuleClicked modName -> do
      state <- H.get
      H.raise (DeclarationClicked state.lastInput.packageName modName "")
    UsageGraphViz.OpenFocusInEditor -> do
      -- Let parent handle VS Code opening
      state <- H.get
      case state.focusedDeclaration of
        Just declName -> H.raise (DeclarationClicked state.lastInput.packageName state.lastInput.moduleName declName)
        Nothing -> pure unit
    UsageGraphViz.ViewModuleSignatures _ -> do
      -- Already visible — ensure signatures panel is open
      state <- H.get
      when (not $ Set.member PanelSignatures state.openPanels) do
        H.modify_ _ { openPanels = Set.insert PanelSignatures state.openPanels }
    UsageGraphViz.ViewPackage pkgName -> do
      H.raise (DeclarationClicked pkgName "" "")

  HandleAnnotationsOutput output -> case output of
    AnnotationsViz.AnnotationStatusChanged annId newStatus -> do
      H.raise (AnnotationStatusChanged annId newStatus)
    AnnotationsViz.AnnotationReplyCreated reply -> do
      H.raise (AnnotationReplyCreated reply)
    AnnotationsViz.DeclarationClicked declName -> do
      handleAction (FocusDeclaration (Just declName))

-- | Fetch blame and sparkline data for the current module
loadModuleData :: forall m. MonadAff m => Input -> H.HalogenM State Action ChildSlots Output m Unit
loadModuleData input = do
  -- Fetch blame
  H.modify_ _ { blameLoading = true }
  void $ H.fork do
    blameResult <- liftAff $ Loader.fetchModuleBlame input.moduleName
    case blameResult of
      Right blame -> H.modify_ _ { blameData = Just blame, blameLoading = false }
      Left _ -> H.modify_ _ { blameLoading = false }
  -- Fetch sparkline
  void $ H.fork do
    -- Find the package name for numstat fetch
    let pkgName = input.packageName
    numstatResult <- liftAff $ Loader.fetchModuleNumstat 500 pkgName
    case numstatResult of
      Right commits -> do
        let bars = Spark.prepareData input.moduleName commits
        H.modify_ _ { sparklineBars = bars }
      Left _ -> pure unit
