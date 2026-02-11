-- | Dependency Adjacency Matrix Visualization Component
-- |
-- | A Halogen slot component wrapping DependencyAdjacency.render.
-- | Shared by SolarSwarm (package-level) and PkgTreemap (module-level).
-- | Parent computes DependencyData, this component renders it.
module CE2.Component.DependencyAdjacencyViz
  ( component
  , Input
  , Query
  , Slot
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

import Hylograph.HATS.InterpreterTick (clearContainer)

import CE2.Containers as C
import CE2.Viz.DependencyAdjacency as DependencyAdjacency
import CE2.Viz.DependencyMatrix (DependencyData)

-- =============================================================================
-- Types
-- =============================================================================

-- | Input from parent
type Input =
  { depData :: DependencyData
  , containerId :: String   -- e.g. "package-adjacency-container" or "module-adjacency-container"
  , width :: Number
  , height :: Number
  , cellSize :: Number
  , labelWidth :: Number
  , labelHeight :: Number
  , matrixMode :: Boolean
  }

-- | No queries
data Query (a :: Type)

-- | Slot type for parent component (Void = no outputs)
type Slot = H.Slot Query Void

-- | Component state — for change detection only
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
render state =
  HH.div
    [ HP.id state.lastInput.containerId
    , HP.class_ (HH.ClassName "dependency-adjacency")
    , HP.style "position: absolute; top: 0; left: 0; width: 100%; height: 100%; background: #F0F0F0;"
    ]
    []

-- =============================================================================
-- Action Handlers
-- =============================================================================

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action () Void m Unit
handleAction = case _ of
  Initialize -> do
    state <- H.get
    let input = state.lastInput
    log $ "[DependencyAdjacencyViz] Initializing, entities=" <> show (Array.length input.depData.names)
    renderMatrix input

  Receive input -> do
    state <- H.get
    let lastInput = state.lastInput
        namesChanged = Array.length input.depData.names /= Array.length lastInput.depData.names
        connectionsChanged = input.depData.totalConnections /= lastInput.depData.totalConnections
        containerChanged = input.containerId /= lastInput.containerId

    H.modify_ _ { lastInput = input }

    when (namesChanged || connectionsChanged || containerChanged) do
      log $ "[DependencyAdjacencyViz] Input changed, re-rendering"
      renderMatrix input

  Finalize -> do
    state <- H.get
    let selector = C.toSelector state.lastInput.containerId
    log $ "[DependencyAdjacencyViz] Finalizing, clearing " <> selector
    liftEffect $ clearContainer selector

-- | Render the adjacency matrix
renderMatrix :: forall m. MonadAff m => Input -> H.HalogenM State Action () Void m Unit
renderMatrix input = do
  let selector = C.toSelector input.containerId
      config =
        { containerSelector: selector
        , width: input.width
        , height: input.height
        , cellSize: input.cellSize
        , labelWidth: input.labelWidth
        , labelHeight: input.labelHeight
        , matrixMode: input.matrixMode
        }
  log $ "[DependencyAdjacencyViz] Rendering matrix: " <> show (Array.length input.depData.names)
      <> " entities, " <> show input.depData.totalConnections <> " connections"
  result <- liftEffect $ DependencyAdjacency.render config input.depData
  case result of
    DependencyAdjacency.Success _ ->
      log "[DependencyAdjacencyViz] Adjacency matrix rendered successfully"
    DependencyAdjacency.Overloaded n ->
      log $ "[DependencyAdjacencyViz] Adjacency overloaded: " <> show n <> " entities"
