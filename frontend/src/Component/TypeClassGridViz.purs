-- | Type Class Grid Visualization Component
-- |
-- | A Halogen component wrapper for the TypeClassGrid visualization.
-- | Simple render-only component: receives stats, renders grid.
-- | No interactivity beyond rendering.
module CE2.Component.TypeClassGridViz
  ( component
  , Input
  , Query
  , Slot
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

import Hylograph.HATS.InterpreterTick (clearContainer)

import CE2.Data.Loader as Loader
import CE2.Types (ViewTheme)
import CE2.Viz.TypeClassGrid as TypeClassGrid

-- =============================================================================
-- Types
-- =============================================================================

-- | Input from parent
type Input =
  { typeClassStats :: Loader.TypeClassStats
  , theme :: ViewTheme
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

-- | Container ID (not in CE2.Containers since TypeClassGrid was ad-hoc)
containerId :: String
containerId = "type-class-grid-container"

containerSelector :: String
containerSelector = "#" <> containerId

render :: forall m. State -> H.ComponentHTML Action () m
render _state =
  HH.div
    [ HP.id containerId
    , HP.class_ (HH.ClassName "type-class-grid")
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
    log $ "[TypeClassGridViz] Initializing with " <> show input.typeClassStats.count <> " type classes"
    renderGrid input

  Receive input -> do
    state <- H.get
    let lastInput = state.lastInput
        statsChanged = input.typeClassStats.count /= lastInput.typeClassStats.count
        themeChanged = input.theme /= lastInput.theme

    H.modify_ _ { lastInput = input }

    when (statsChanged || themeChanged) do
      log $ "[TypeClassGridViz] Input changed, re-rendering"
      renderGrid input

  Finalize -> do
    log "[TypeClassGridViz] Finalizing"
    liftEffect $ clearContainer containerSelector

-- | Render the grid visualization
renderGrid :: forall m. MonadAff m => Input -> H.HalogenM State Action () Void m Unit
renderGrid input = do
  let config =
        { containerSelector
        , width: 1650.0
        , height: 900.0
        , theme: input.theme
        }
  liftEffect $ TypeClassGrid.render config input.typeClassStats
