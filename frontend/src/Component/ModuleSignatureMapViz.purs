-- | Module Signature Map Visualization Component
-- |
-- | A Halogen component that renders a category-lane layout of a module's
-- | type signatures. SVGs are pre-rendered and measured, then placed into
-- | a Halogen HTML layout with CSS flexbox shelf packing.
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
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS

import CE2.Data.Loader as Loader
import CE2.Viz.ModuleSignatureMap as MSM

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
  , lanes :: Array MSM.Lane
  }

data Action
  = Initialize
  | Receive Input
  | Finalize
  | HandleDeclarationClick String String String
  | CellClicked (Effect Unit)

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
  , lanes: []
  }

-- =============================================================================
-- Render
-- =============================================================================

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div
    [ HP.class_ (HH.ClassName "module-signature-map")
    , HP.style "overflow-y: auto; padding: 12px 16px; position: absolute; top: 0; left: 0; width: 100%; height: 100%; box-sizing: border-box;"
    ]
    if Array.null state.lanes && state.initialized then
      [ HH.div
          [ HP.style "display:flex;align-items:center;justify-content:center;height:100%;color:#999;font-size:14px;" ]
          [ HH.text "No declarations" ]
      ]
    else
      (state.lanes <#> renderLane)

renderLane :: forall m. MSM.Lane -> H.ComponentHTML Action () m
renderLane lane =
  let
    cellsStyle = if lane.column
      then "display:grid; grid-template-columns:1fr 1fr; gap:6px; align-items:start; justify-items:start;"
      else "display:flex; flex-wrap:wrap; gap:6px; align-items:flex-start;"
  in
  HH.div [ HP.style "margin-bottom: 16px;" ]
    [ HH.div
        [ HP.style $ "display:flex; align-items:center; gap:8px; padding:4px 0; margin-bottom:6px; border-bottom: 2px solid " <> lane.accent <> ";" ]
        [ HH.span
            [ HP.style $ "font-family: 'Courier New', Courier, monospace; font-size:11px; font-weight:700; color:" <> lane.accent <> "; text-transform:uppercase; letter-spacing:0.5px;" ]
            [ HH.text lane.label ]
        , HH.span
            [ HP.style $ "font-size:9px; padding:1px 5px; border-radius:8px; background:" <> lane.accent <> "; color:white; font-weight:600;" ]
            [ HH.text (show (Array.length lane.cells)) ]
        ]
    , HH.div
        [ HP.style cellsStyle ]
        (lane.cells <#> renderCell)
    ]

renderCell :: forall m. MSM.MeasuredCell -> H.ComponentHTML Action () m
renderCell cell =
  HH.div
    [ HP.id ("sig-cell-" <> cell.name)
    , HP.style $ "width:" <> show cell.cellWidth <> "px;"
        <> " height:" <> show cell.cellHeight <> "px;"
        <> " overflow:auto;"
        <> " padding:" <> show MSM.cellPad <> "px;"
        <> " box-sizing:border-box;"
        <> " background:" <> MSM.kindBackground cell.kind <> ";"
        <> " border:1px solid " <> MSM.kindBorder cell.kind <> ";"
        <> " border-radius:3px;"
        <> " cursor:pointer;"
        <> " transition:box-shadow 0.15s ease, transform 0.15s ease;"
        <> " flex-shrink:0;"
    , HE.onClick \_ -> CellClicked cell.onClick
    ]
    (case cell.svg of
      Just _ -> []  -- SVGs inserted by Effect after render
      Nothing ->
        [ HH.div
            [ HP.style "font-size:11px; color:#333; font-family:'Fira Code','SF Mono',monospace;" ]
            [ HH.text (cell.name <> if cell.sig == "" then "" else " :: " <> cell.sig) ]
        ])

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
      H.modify_ _ { lanes = [] }
      renderSignatureMap input

  Finalize -> do
    log "[ModuleSignatureMapViz] Finalizing"

  HandleDeclarationClick pkgName modName declName -> do
    log $ "[ModuleSignatureMapViz] Declaration clicked: " <> declName
    H.raise (DeclarationClicked pkgName modName declName)

  CellClicked handler -> do
    liftEffect handler

-- | Pre-render SVGs, measure, group into lanes, then insert SVGs after render.
renderSignatureMap :: forall m. MonadAff m => Input -> H.HalogenM State Action () Output m Unit
renderSignatureMap input = do
  state <- H.get
  let onDeclClick = makeDeclarationClickCallback state.actionListener
  measured <- liftEffect $ MSM.prepareCells
    { containerSelector: ""
    , moduleName: input.moduleName
    , packageName: input.packageName
    , onDeclarationClick: Just onDeclClick
    }
    input.declarations
  let newLanes = MSM.groupIntoLanes measured
  H.modify_ _ { lanes = newLanes }
  -- After Halogen re-renders with the new lanes, insert SVGs into cell divs
  liftEffect $ MSM.insertSVGsIntoCells measured

-- | Create a declaration click callback that notifies the Halogen listener
makeDeclarationClickCallback :: Maybe (HS.Listener Action) -> String -> String -> String -> Effect Unit
makeDeclarationClickCallback mListener pkgName modName declName = case mListener of
  Just listener -> HS.notify listener (HandleDeclarationClick pkgName modName declName)
  Nothing -> log $ "[ModuleSignatureMapViz] No listener for decl click: " <> pkgName <> "/" <> modName <> "/" <> declName
