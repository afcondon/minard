-- | Dependency Chord Diagram Visualization Component
-- |
-- | A Halogen slot component wrapping DependencyChord.render.
-- | Shared by SolarSwarm (package-level) and PkgTreemap (module-level).
-- | Parent computes DependencyData, this component renders it.
module CE2.Component.DependencyChordViz
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
import CE2.Viz.DependencyChord as DependencyChord
import CE2.Viz.DependencyMatrix (DependencyData)

-- =============================================================================
-- Types
-- =============================================================================

-- | Input from parent
type Input =
  { depData :: DependencyData
  , containerId :: String   -- e.g. "package-chord-container" or "module-chord-container"
  , width :: Number
  , height :: Number
  , innerRadius :: Number
  , outerRadius :: Number
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
    , HP.class_ (HH.ClassName "dependency-chord")
    , HP.style "position: absolute; top: 0; left: 0; width: 100%; height: 100%;"
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
    log $ "[DependencyChordViz] Initializing, entities=" <> show (Array.length input.depData.names)
    renderChord input

  Receive input -> do
    state <- H.get
    let lastInput = state.lastInput
        namesChanged = Array.length input.depData.names /= Array.length lastInput.depData.names
        connectionsChanged = input.depData.totalConnections /= lastInput.depData.totalConnections
        containerChanged = input.containerId /= lastInput.containerId

    H.modify_ _ { lastInput = input }

    when (namesChanged || connectionsChanged || containerChanged) do
      log $ "[DependencyChordViz] Input changed, re-rendering"
      renderChord input

  Finalize -> do
    state <- H.get
    let selector = C.toSelector state.lastInput.containerId
    log $ "[DependencyChordViz] Finalizing, clearing " <> selector
    liftEffect $ clearContainer selector

-- | Render the chord diagram
renderChord :: forall m. MonadAff m => Input -> H.HalogenM State Action () Void m Unit
renderChord input = do
  let selector = C.toSelector input.containerId
      config =
        { containerSelector: selector
        , width: input.width
        , height: input.height
        , innerRadius: input.innerRadius
        , outerRadius: input.outerRadius
        }
  log $ "[DependencyChordViz] Rendering chord: " <> show (Array.length input.depData.names)
      <> " entities, " <> show input.depData.totalConnections <> " connections"
  result <- liftEffect $ DependencyChord.render config input.depData
  case result of
    DependencyChord.Success _ ->
      log "[DependencyChordViz] Chord diagram rendered successfully"
    DependencyChord.Overloaded n ->
      log $ "[DependencyChordViz] Chord overloaded: " <> show n <> " entities"
