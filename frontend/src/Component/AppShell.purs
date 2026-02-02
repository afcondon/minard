-- | Application Shell Component
-- |
-- | Owns application lifecycle:
-- | - Model data loading from unified v2 API
-- | - Package set and timeline data loading (on demand)
-- |
-- | Renders SceneCoordinator as single child slot, passing loaded data down.
-- | Scene-based navigation replaces the legacy ViewCoordinator.
-- |
-- | Note: This is simplified from the legacy version - no project/snapshot selection.
-- | The v2 API serves data from a unified database.
module CE2.Component.AppShell where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))

import CE2.Data.Loader as Loader
import CE2.Component.SceneCoordinator as SceneCoordinator
import CE2.Scene (Scene(..))

-- =============================================================================
-- Types
-- =============================================================================

-- | Application lifecycle phases
data AppPhase
  = Loading
  | Ready
  | Error String

derive instance eqAppPhase :: Eq AppPhase

-- | Loaded model data (immutable after load)
type ModelData = Loader.LoadedModel

-- | V2 data for specialized visualizations (beeswarm, etc.)
type V2Data =
  { packages :: Array Loader.V2Package
  , modules :: Array Loader.V2ModuleListItem
  , imports :: Array Loader.V2ModuleImports
  }

-- | Application shell state - owns data loading
type State =
  { -- Lifecycle
    phase :: AppPhase

    -- Loaded data (passed down to SceneCoordinator)
  , modelData :: Maybe ModelData
  , v2Data :: Maybe V2Data  -- Raw V2 data for specialized views
  , packageSetData :: Maybe Loader.PackageSetData
  , currentScene :: Scene  -- Track current scene for header display
  }

-- | Actions handled at the shell level
data Action
  = Initialize
  | DataLoaded Loader.LoadedModelWithV2
  | DataFailed String
  | PackageSetLoaded Loader.PackageSetData
  | HandleSceneCoordinatorOutput SceneCoordinator.Output

-- | Child slots
type Slots = ( sceneCoordinator :: SceneCoordinator.Slot Unit )

_sceneCoordinator :: Proxy "sceneCoordinator"
_sceneCoordinator = Proxy

-- =============================================================================
-- Component
-- =============================================================================

component :: forall query input output m. MonadAff m => H.Component query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }

initialState :: forall input. input -> State
initialState _ =
  { phase: Loading
  , modelData: Nothing
  , v2Data: Nothing
  , packageSetData: Nothing
  , currentScene: GalaxyTreemap  -- Start at GalaxyTreemap (blueprint view)
  }

-- =============================================================================
-- Render
-- =============================================================================

render :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
render state =
  HH.div
    [ HP.class_ (HH.ClassName "explorer-app") ]
    [ -- Header with status
      renderHeader state

      -- Main content area
    , case state.phase of
        Loading ->
          HH.div [ HP.class_ (HH.ClassName "loading") ]
            [ HH.text "Loading from unified database..." ]
        Error msg ->
          HH.div [ HP.class_ (HH.ClassName "error") ]
            [ HH.text $ "Error: " <> msg ]
        Ready ->
          -- SceneCoordinator gets the loaded data
          HH.slot _sceneCoordinator unit SceneCoordinator.component
            (mkSceneCoordinatorInput state)
            HandleSceneCoordinatorOutput
    ]

-- | Build input for SceneCoordinator from shell state
mkSceneCoordinatorInput :: State -> SceneCoordinator.Input
mkSceneCoordinatorInput state =
  { modelData: state.modelData
  , v2Data: state.v2Data
  , packageSetData: state.packageSetData
  , initialScene: GalaxyTreemap  -- Start at GalaxyTreemap (blueprint view)
  }

-- | Render the header with status info
renderHeader :: forall m. State -> H.ComponentHTML Action Slots m
renderHeader state =
  HH.div
    [ HP.class_ (HH.ClassName "status-panel") ]
    [ HH.h2_ [ HH.text "Code Explorer v2" ]

    , HH.p_ [ HH.text $ "Phase: " <> showPhase state.phase ]

    , case state.modelData of
        Nothing -> HH.p_ [ HH.text "Loading data..." ]
        Just model -> HH.p_
          [ HH.text $ "Loaded: " <> show model.moduleCount <> " modules, "
              <> show model.packageCount <> " packages"
          ]
    ]

showPhase :: AppPhase -> String
showPhase Loading = "Loading..."
showPhase Ready = "Ready"
showPhase (Error msg) = "Error: " <> msg

-- =============================================================================
-- Action Handlers
-- =============================================================================

handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action Slots output m Unit
handleAction = case _ of
  Initialize -> do
    log "[AppShell] Initializing with unified v2 API..."

    -- Load model from v2 API (with raw data for specialized views)
    void $ H.fork do
      result <- H.liftAff $ Loader.loadModelFromV2WithRaw
      case result of
        Left err -> handleAction (DataFailed err)
        Right loaded -> handleAction (DataLoaded loaded)

  DataLoaded loaded -> do
    log $ "[AppShell] Data loaded: " <> show loaded.model.moduleCount <> " modules, "
        <> show loaded.model.packageCount <> " packages"
        <> " (with " <> show (Array.length loaded.v2Packages) <> " v2 packages)"
    H.modify_ _
      { phase = Ready
      , modelData = Just loaded.model
      , v2Data = Just
          { packages: loaded.v2Packages
          , modules: loaded.v2Modules
          , imports: loaded.v2Imports
          }
      }

  DataFailed err -> do
    log $ "[AppShell] Data load failed: " <> err
    H.modify_ _ { phase = Error err }

  PackageSetLoaded psData -> do
    log $ "[AppShell] Package set loaded: " <> psData.packageSet.name
        <> " with " <> show (Array.length psData.packages) <> " packages"
    H.modify_ _ { packageSetData = Just psData }

  HandleSceneCoordinatorOutput output -> case output of
    SceneCoordinator.RequestPackageSetData -> do
      state <- H.get
      case state.packageSetData of
        Just _ -> pure unit  -- Already have it
        Nothing -> do
          log "[AppShell] Fetching package set data from V2 API..."
          void $ H.fork do
            result <- H.liftAff $ Loader.fetchPackageSetFromV2
            case result of
              Left err -> log $ "[AppShell] Failed to fetch package set: " <> err
              Right psData -> handleAction (PackageSetLoaded psData)

    SceneCoordinator.SceneChanged newScene -> do
      log $ "[AppShell] Scene changed to: " <> show newScene
      H.modify_ _ { currentScene = newScene }
