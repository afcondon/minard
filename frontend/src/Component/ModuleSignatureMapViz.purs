-- | Module Signature Map Visualization Component
-- |
-- | A Halogen component wrapper for the ModuleSignatureMap visualization.
-- | Full-screen treemap of a module's type signatures, sized by visual
-- | complexity of each signature.
module CE2.Component.ModuleSignatureMapViz
  ( component
  , Input
  , Output(..)
  , Query(..)
  , Slot
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS

import CE2.Containers as C
import CE2.Data.Loader as Loader
import CE2.Viz.ModuleSignatureMap as ModuleSignatureMap

-- =============================================================================
-- Types
-- =============================================================================

type Input =
  { packageName :: String
  , moduleName :: String
  , declarations :: Array Loader.V2Declaration
  }

data Output
  = DeclarationClicked String String String  -- pkg, mod, decl

type Slot = H.Slot Query Output

data Query a = NoQuery a

type State =
  { initialized :: Boolean
  , actionListener :: Maybe (HS.Listener Action)
  , lastInput :: Input
  , sigMapHandle :: Maybe ModuleSignatureMap.SignatureMapHandle
  }

data Action
  = Initialize
  | Receive Input
  | Finalize
  | HandleDeclarationClick String String String

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
        , finalize = Just Finalize
        }
    }

initialState :: Input -> State
initialState input =
  { initialized: false
  , actionListener: Nothing
  , lastInput: input
  , sigMapHandle: Nothing
  }

-- =============================================================================
-- Render
-- =============================================================================

render :: forall m. State -> H.ComponentHTML Action () m
render _state =
  HH.div
    [ HP.id C.moduleSignatureMapContainerId
    , HP.class_ (HH.ClassName "module-signature-map")
    , HP.style "position: absolute; top: 0; left: 0; width: 100%; height: 100%;"
    ]
    []

-- =============================================================================
-- Action Handlers
-- =============================================================================

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  Initialize -> do
    state <- H.get
    let input = state.lastInput
    log $ "[ModuleSignatureMapViz] Initializing: " <> input.moduleName
        <> ", " <> show (Array.length input.declarations) <> " declarations"

    { emitter, listener } <- liftEffect HS.create
    void $ H.subscribe emitter
    H.modify_ _ { actionListener = Just listener, initialized = true }

    renderSignatureMap input

  Receive input -> do
    state <- H.get
    let changed = input.moduleName /= state.lastInput.moduleName
              || Array.length input.declarations /= Array.length state.lastInput.declarations
    H.modify_ _ { lastInput = input }
    when (changed && state.initialized) do
      -- Cleanup previous
      case state.sigMapHandle of
        Just handle -> liftEffect handle.cleanup
        Nothing -> pure unit
      H.modify_ _ { sigMapHandle = Nothing }
      renderSignatureMap input

  Finalize -> do
    log "[ModuleSignatureMapViz] Finalizing"
    state <- H.get
    case state.sigMapHandle of
      Just handle -> liftEffect handle.cleanup
      Nothing -> pure unit

  HandleDeclarationClick pkgName modName declName -> do
    log $ "[ModuleSignatureMapViz] Declaration clicked: " <> declName
    H.raise (DeclarationClicked pkgName modName declName)

-- | Render the signature map into the container
renderSignatureMap :: forall m. MonadAff m => Input -> H.HalogenM State Action () Output m Unit
renderSignatureMap input = do
  state <- H.get
  let onDeclClick = makeDeclarationClickCallback state.actionListener
  handle <- liftEffect $ ModuleSignatureMap.render
    { containerSelector: C.moduleSignatureMapContainer
    , moduleName: input.moduleName
    , packageName: input.packageName
    , onDeclarationClick: Just onDeclClick
    }
    input.declarations
  H.modify_ _ { sigMapHandle = Just handle }

-- | Create a declaration click callback that notifies the Halogen listener
makeDeclarationClickCallback :: Maybe (HS.Listener Action) -> String -> String -> String -> Effect Unit
makeDeclarationClickCallback mListener pkgName modName declName = case mListener of
  Just listener -> HS.notify listener (HandleDeclarationClick pkgName modName declName)
  Nothing -> log $ "[ModuleSignatureMapViz] No listener for decl click: " <> pkgName <> "/" <> modName <> "/" <> declName
