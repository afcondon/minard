-- | Package Module Beeswarm Visualization Component
-- |
-- | Two-layer visualization for a single package's modules:
-- | - Background: module treemap (paperwhite)
-- | - Foreground: module beeswarm (large packages) or bubblepack (small packages)
-- |
-- | The small/large threshold determines which overlay to use:
-- | - Small packages (< 200 modules): bubblepack showing declaration categories
-- | - Large packages (>= 200 modules): simple beeswarm with topo ordering
module CE2.Component.PkgModuleBeeswarmViz
  ( component
  , Input
  , V2DataSlice
  , Output
  , Query
  , Slot
  ) where

import Prelude

import Data.Array as Array
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

import Hylograph.HATS.InterpreterTick (clearContainer)

import CE2.Containers as C
import CE2.Data.Loader as Loader
import CE2.Viz.ModuleTreemap as ModuleTreemap
import CE2.Viz.ModuleBeeswarm as ModuleBeeswarm
import CE2.Viz.ModuleBubblePack as ModuleBubblePack

-- =============================================================================
-- Types
-- =============================================================================

-- | Input from parent
type Input =
  { packageName :: String
  , v2Data :: V2DataSlice
  , declarationStats :: Maybe (Map.Map Int Loader.V2ModuleDeclarationStats)
  }

-- | Subset of V2Data needed by this component
type V2DataSlice =
  { packages :: Array Loader.V2Package
  , modules :: Array Loader.V2ModuleListItem
  , imports :: Array Loader.V2ModuleImports
  }

-- | No output events
data Output

-- | No queries
data Query (a :: Type)

-- | Slot type for parent component (Void = no outputs)
type Slot = H.Slot Query Void

-- | Module count threshold for choosing overlay type
smallPackageThreshold :: Int
smallPackageThreshold = 200

-- | Component state
type State =
  { lastInput :: Input
  }

-- | Actions
data Action
  = Initialize
  | Receive Input
  | Finalize

-- =============================================================================
-- Component
-- =============================================================================

component :: forall m. MonadAff m => H.Component Query Input Void m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , receive = Just <<< Receive
        , initialize = Just Initialize
        , finalize = Just Finalize
        }
    }

initialState :: Input -> State
initialState input =
  { lastInput: input
  }

-- =============================================================================
-- Render
-- =============================================================================

render :: forall m. State -> H.ComponentHTML Action () m
render _state =
  HH.div
    [ HP.class_ (HH.ClassName "pkg-module-beeswarm-viz")
    , HP.style "position: relative; width: 100%; height: 100%;"
    ]
    [ -- Treemap layer (below)
      HH.div
        [ HP.id C.pkgModuleBeeswarmTreemapContainerId
        , HP.class_ (HH.ClassName "pkg-treemap")
        , HP.style "position: absolute; top: 0; left: 0; width: 100%; height: 100%; z-index: 1;"
        ]
        []
    -- Beeswarm/BubblePack layer (above)
    , HH.div
        [ HP.id C.pkgModuleBeeswarmContainerId
        , HP.class_ (HH.ClassName "pkg-module-beeswarm")
        , HP.style "position: absolute; top: 0; left: 0; width: 100%; height: 100%; z-index: 2;"
        ]
        []
    ]

-- =============================================================================
-- Action Handlers
-- =============================================================================

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action () Void m Unit
handleAction = case _ of
  Initialize -> do
    state <- H.get
    let input = state.lastInput
    log $ "[PkgModuleBeeswarmViz] Initializing for " <> input.packageName
    renderVisualization input

  Receive input -> do
    state <- H.get
    let lastInput = state.lastInput
        packageChanged = input.packageName /= lastInput.packageName
        -- Declaration stats arriving (was Nothing, now Just)
        statsArrived = case lastInput.declarationStats, input.declarationStats of
          Nothing, Just _ -> true
          _, _ -> false

    H.modify_ _ { lastInput = input }

    when (packageChanged || statsArrived) do
      log $ "[PkgModuleBeeswarmViz] Input changed"
          <> (if packageChanged then " (package)" else "")
          <> (if statsArrived then " (stats arrived)" else "")
          <> ", re-rendering"
      renderVisualization input

  Finalize -> do
    log "[PkgModuleBeeswarmViz] Finalizing"
    liftEffect $ clearContainer C.pkgModuleBeeswarmTreemapContainer
    liftEffect $ clearContainer C.pkgModuleBeeswarmContainer

-- =============================================================================
-- Rendering
-- =============================================================================

-- | Render both layers of the visualization
renderVisualization :: forall m. MonadAff m => Input -> H.HalogenM State Action () Void m Unit
renderVisualization input = do
  let v2 = input.v2Data
      pkgModules = Array.filter (\m -> m.package.name == input.packageName) v2.modules
      pkgModuleIds = Set.fromFoldable $ map _.id pkgModules
      pkgImports = Array.filter (\imp -> Set.member imp.moduleId pkgModuleIds) v2.imports
      moduleCount = Array.length pkgModules
      isSmallPackage = moduleCount < smallPackageThreshold

  log $ "[PkgModuleBeeswarmViz] Rendering: " <> input.packageName
      <> " (" <> show moduleCount <> " modules, "
      <> (if isSmallPackage then "small -> bubblepack" else "large -> beeswarm") <> ")"

  -- Background: module treemap
  liftEffect $ ModuleTreemap.render
    { containerSelector: C.pkgModuleBeeswarmTreemapContainer
    , width: 1650.0
    , height: 900.0
    , packageName: input.packageName
    , onModuleClick: Nothing
    }
    pkgModules

  -- Foreground: bubblepack or beeswarm
  if isSmallPackage
    then do
      let statsMap = case input.declarationStats of
            Just stats -> stats
            Nothing -> Map.empty
      _ <- liftEffect $ ModuleBubblePack.render
        { containerSelector: C.pkgModuleBeeswarmContainer
        , width: 1650.0
        , height: 900.0
        , packages: v2.packages
        }
        pkgModules
        statsMap
      log "[PkgModuleBeeswarmViz] Module bubblepack rendered"
    else do
      _ <- liftEffect $ ModuleBeeswarm.renderSinglePackage
        { containerSelector: C.pkgModuleBeeswarmContainer
        , width: 1650.0
        , height: 900.0
        , packageName: input.packageName
        }
        pkgModules
        pkgImports
      log "[PkgModuleBeeswarmViz] Module beeswarm rendered"
