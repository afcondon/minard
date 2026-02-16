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
import CE2.Viz.SignatureTree as SigTree

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
  HH.div [ HP.style "margin-bottom: 16px;" ]
    [ renderLaneHeader lane
    , HH.div
        [ HP.style "columns: 440px; column-gap: 8px;" ]
        (lane.cells <#> renderFullCell)
    ]

-- | Shared lane header
renderLaneHeader :: forall m. MSM.Lane -> H.ComponentHTML Action () m
renderLaneHeader lane =
  HH.div
    [ HP.style $ "display:flex; align-items:center; gap:8px; padding:4px 0; margin-bottom:6px; border-bottom: 2px solid " <> lane.accent <> ";" ]
    [ HH.span
        [ HP.style $ "font-family: 'Courier New', Courier, monospace; font-size:11px; font-weight:700; color:" <> lane.accent <> "; text-transform:uppercase; letter-spacing:0.5px;" ]
        [ HH.text lane.label ]
    , HH.span
        [ HP.style $ "font-size:9px; padding:1px 5px; border-radius:8px; background:" <> lane.accent <> "; color:white; font-weight:600;" ]
        [ HH.text (show (Array.length lane.cells)) ]
    ]

-- =============================================================================
-- Cell renderers
-- =============================================================================

-- | Render a full-size cell for all declaration kinds
-- | Content inserted post-render via insertHtmlIntoCells
renderFullCell :: forall m. MSM.MeasuredCell -> H.ComponentHTML Action () m
renderFullCell cell =
  HH.div
    [ HP.id ("sig-cell-" <> cell.name)
    , HP.class_ (HH.ClassName "sigmap-cell")
    , HP.style $ "break-inside:avoid;"
        <> " margin-bottom:6px;"
        <> " overflow:auto;"
        <> " padding:" <> show MSM.cellPad <> "px;"
        <> " box-sizing:border-box;"
        <> " background:" <> MSM.kindBackground cell.kind <> ";"
        <> " border:1px solid " <> MSM.kindBorder cell.kind <> ";"
        <> " border-radius:3px;"
        <> " cursor:pointer;"
    , HE.onClick \_ -> CellClicked cell.onClick
    ]
    -- All content inserted post-render by insertHtmlIntoCells
    -- Only plain-text fallback for cells with no structured data and no AST
    (case cell.dataDecl of
      Just _ -> []
      Nothing -> case cell.classDecl of
        Just _ -> []
        Nothing -> case cell.typeSynonym of
          Just _ -> []
          Nothing -> case cell.ast of
            Just _ -> []
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

-- | Prepare cells, group into lanes, then insert HTML content after render.
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
  -- Insert HTML for all cell types (data decls, class decls, values, type synonyms)
  liftEffect $ insertHtmlIntoCells measured

-- | Create a declaration click callback that notifies the Halogen listener
makeDeclarationClickCallback :: Maybe (HS.Listener Action) -> String -> String -> String -> Effect Unit
makeDeclarationClickCallback mListener pkgName modName declName = case mListener of
  Just listener -> HS.notify listener (HandleDeclarationClick pkgName modName declName)
  Nothing -> log $ "[ModuleSignatureMapViz] No listener for decl click: " <> pkgName <> "/" <> modName <> "/" <> declName

-- | Insert HTML content into all cell types after Halogen render.
-- | Data decls → renderDataDeclInto, class decls → renderClassDeclInto,
-- | value cells → adaptive sigil/siglet, type synonyms → full-size signature.
insertHtmlIntoCells :: Array MSM.MeasuredCell -> Effect Unit
insertHtmlIntoCells cells =
  Array.foldM (\_ cell ->
    let cellSelector = "#sig-cell-" <> cell.name
    in case cell.dataDecl of
      Just dd ->
        SigTree.renderDataDeclInto cellSelector
          { name: cell.name, typeParams: dd.typeParams, constructors: dd.constructors, keyword: dd.keyword }
      Nothing -> case cell.classDecl of
        Just cd -> do
          SigTree.renderClassDeclInto cellSelector
            { name: cell.name, typeParams: cd.typeParams, superclasses: cd.superclasses, methods: cd.methods }
          -- Append instances section if any
          when (not (Array.null cd.instances)) do
            SigTree.appendHtmlInto cellSelector (renderInstancesHtml cd.instances)
        Nothing -> case cell.typeSynonym of
          Just ts ->
            SigTree.renderTypeSynonymInto cellSelector
              { name: cell.name, typeParams: ts.typeParams, body: ts.body }
          Nothing -> case cell.ast of
            Just ast ->
              if cell.foreignImport then
                -- Foreign imports: dedicated renderer
                SigTree.renderForeignImportInto cellSelector
                  { name: cell.name, ast }
              else
                -- Value cells: full sigil rendering
                SigTree.renderSignatureInto cellSelector
                  { name: cell.name, sig: cell.sig, ast, typeParams: [], className: Nothing }
            Nothing -> pure unit
  ) unit cells

-- | Build HTML for the instances section of a class card.
renderInstancesHtml :: Array { name :: String, sig :: Maybe String } -> String
renderInstancesHtml instances =
  let
    count = Array.length instances
    instanceItems = Array.foldl (\acc inst ->
      acc <> "<li class=\"sig-class-instance\">"
        <> "<code class=\"sig-class-instance-name\">" <> inst.name <> "</code>"
        <> "</li>"
    ) "" instances
  in
    "<div class=\"sig-class-instances\">"
    <> "<div class=\"sig-class-instances-header\">"
    <> show count <> " instance" <> (if count == 1 then "" else "s")
    <> "</div>"
    <> "<ul class=\"sig-class-instance-list\">" <> instanceItems <> "</ul>"
    <> "</div>"
