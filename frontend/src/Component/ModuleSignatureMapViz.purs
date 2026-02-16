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
import Data.Array.NonEmpty as NEA
import Data.Maybe (Maybe(..))
import Data.String as String
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
import CE2.Viz.TypeSignature as TS
import CE2.Viz.TypeSignature.TypeAST (elideAST)

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
  | ShowTooltip String  -- cell name
  | HideTooltip

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
-- Constants
-- =============================================================================

tooltipId :: String
tooltipId = "sigmap-tooltip"

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
      (state.lanes <#> renderLane) <>
      [ -- Fixed-position tooltip (outside columns, rendered at end)
        HH.div
          [ HP.id tooltipId
          , HP.class_ (HH.ClassName "sigmap-tooltip")
          ]
          []
      ]

renderLane :: forall m. MSM.Lane -> H.ComponentHTML Action () m
renderLane lane =
  if lane.key == "values"
    then renderValuesLane lane
    else renderStandardLane lane

-- | Standard lane for types, data, classes etc — full-size SVG cards in columns
renderStandardLane :: forall m. MSM.Lane -> H.ComponentHTML Action () m
renderStandardLane lane =
  HH.div [ HP.style "margin-bottom: 16px;" ]
    [ renderLaneHeader lane
    , HH.div
        [ HP.style "columns: 300px; column-gap: 6px;" ]
        (lane.cells <#> renderFullCell)
    ]

-- | Values lane — siglet cells, alphabetically grouped when large
renderValuesLane :: forall m. MSM.Lane -> H.ComponentHTML Action () m
renderValuesLane lane =
  let
    sorted = Array.sortBy (comparing _.name) lane.cells
    groups = groupByFirstLetter sorted
    useGrouping = Array.length lane.cells >= 12
  in
  HH.div [ HP.style "margin-bottom: 16px;" ]
    [ renderLaneHeader lane
    , HH.div
        [ HP.style "column-count: 1; column-gap: 6px;" ]
        (if useGrouping
          then Array.concatMap renderLetterGroup groups
          else sorted <#> renderSigletCell)
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
-- Alphabetical grouping
-- =============================================================================

type LetterGroup = { letter :: String, cells :: Array MSM.MeasuredCell }

groupByFirstLetter :: Array MSM.MeasuredCell -> Array LetterGroup
groupByFirstLetter sorted =
  let
    firstLetter c = String.toUpper (String.take 1 c.name)
  in
    Array.groupBy (\a b -> firstLetter a == firstLetter b) sorted
      # map (\group ->
          { letter: firstLetter (NEA.head group)
          , cells: NEA.toArray group
          })

renderLetterGroup :: forall m. LetterGroup -> Array (H.ComponentHTML Action () m)
renderLetterGroup group =
  [ renderLetterHeader group.letter ] <> (group.cells <#> renderSigletCell)

renderLetterHeader :: forall m. String -> H.ComponentHTML Action () m
renderLetterHeader letter =
  HH.div
    [ HP.style "column-span: all; font-size: 13px; font-weight: 700; color: #aaa; padding: 10px 0 2px; letter-spacing: 0.5px;" ]
    [ HH.text letter ]

-- =============================================================================
-- Cell renderers
-- =============================================================================

-- | Render a value cell: name header + empty containers filled by HATS post-render
renderSigletCell :: forall m. MSM.MeasuredCell -> H.ComponentHTML Action () m
renderSigletCell cell =
  HH.div
    [ HP.id ("sigmap-cell-" <> cell.name)
    , HP.classes [ HH.ClassName "sigmap-cell", HH.ClassName "sigmap-cell-sparkline" ]
    , HP.style $ "break-inside:avoid;"
        <> " margin-bottom:6px;"
        <> " overflow:visible;"
        <> " padding:" <> show MSM.cellPad <> "px;"
        <> " box-sizing:border-box;"
        <> " background:" <> MSM.kindBackground cell.kind <> ";"
        <> " border:1px solid " <> MSM.kindBorder cell.kind <> ";"
        <> " border-radius:3px;"
        <> " cursor:pointer;"
    , HE.onClick \_ -> CellClicked cell.onClick
    , HE.onMouseEnter \_ -> ShowTooltip cell.name
    , HE.onMouseLeave \_ -> HideTooltip
    ]
    [ -- Name header (Halogen)
      HH.div
        [ HP.style "font-family:'Fira Code','SF Mono',monospace; font-size:12px; font-weight:700; color:#333; white-space:nowrap; overflow:hidden; text-overflow:ellipsis;" ]
        [ HH.text cell.name ]
    -- Siglet container (filled by HATS after render)
    , HH.div [ HP.id ("sig-sparkline-" <> cell.name) ] []
    -- Hidden full-size for tooltip (filled by HATS after render)
    , HH.div
        [ HP.id ("sig-cell-" <> cell.name)
        , HP.style "display:none;"
        ]
        []
    ]

-- | Render a regular full-size cell (types, data, classes, type synonyms)
-- | Content inserted post-render: SVG for ADT/ClassDef, HATS HTML for others
renderFullCell :: forall m. MSM.MeasuredCell -> H.ComponentHTML Action () m
renderFullCell cell =
  HH.div
    [ HP.id ("sig-cell-" <> cell.name)
    , HP.class_ (HH.ClassName "sigmap-cell")
    , HP.style $ "break-inside:avoid;"
        <> " margin-bottom:6px;"
        <> (case cell.svg of
              Just _ -> " height:" <> show cell.cellHeight <> "px;"
              Nothing -> "")
        <> " overflow:auto;"
        <> " padding:" <> show MSM.cellPad <> "px;"
        <> " box-sizing:border-box;"
        <> " background:" <> MSM.kindBackground cell.kind <> ";"
        <> " border:1px solid " <> MSM.kindBorder cell.kind <> ";"
        <> " border-radius:3px;"
        <> " cursor:pointer;"
    , HE.onClick \_ -> CellClicked cell.onClick
    ]
    -- All content inserted post-render (SVG or HATS HTML)
    -- Only plain-text fallback for cells with no SVG and no AST
    (case cell.svg of
      Just _ -> []  -- SVG inserted by insertSVGsIntoCells
      Nothing -> case cell.ast of
        Just _ -> []  -- HATS HTML inserted by insertHtmlIntoCells
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

  ShowTooltip name -> do
    liftEffect $ TS.showSigletTooltip tooltipId ("sig-cell-" <> name) ("sigmap-cell-" <> name)

  HideTooltip -> do
    liftEffect $ TS.hideSigletTooltip tooltipId

-- | Pre-render SVGs, measure, group into lanes, then insert content after render.
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
  -- After Halogen re-renders, insert SVGs for ADT/ClassDef cells
  liftEffect $ MSM.insertSVGsIntoCells measured
  -- Insert HATS HTML trees for value/type synonym cells
  liftEffect $ insertHtmlIntoCells measured

-- | Create a declaration click callback that notifies the Halogen listener
makeDeclarationClickCallback :: Maybe (HS.Listener Action) -> String -> String -> String -> Effect Unit
makeDeclarationClickCallback mListener pkgName modName declName = case mListener of
  Just listener -> HS.notify listener (HandleDeclarationClick pkgName modName declName)
  Nothing -> log $ "[ModuleSignatureMapViz] No listener for decl click: " <> pkgName <> "/" <> modName <> "/" <> declName

-- | Insert HATS HTML trees into cells that have AST but no SVG.
-- | Value cells use adaptive rendering: try full sigil body first, fall back
-- | to siglet if the rendered height exceeds ~40px (roughly 2 lines).
-- | Type synonym/foreign cells get full-size rendering.
insertHtmlIntoCells :: Array MSM.MeasuredCell -> Effect Unit
insertHtmlIntoCells cells =
  Array.foldM (\_ cell ->
    case cell.svg of
      Just _ -> pure unit  -- SVG cells handled by insertSVGsIntoCells
      Nothing -> case cell.ast of
        Just ast -> do
          -- Value cells: adaptive sigil/siglet rendering
          when (cell.kind == "value") do
            let sparkSelector = "#sig-sparkline-" <> cell.name
            -- 1. Render full sigil body (no name header)
            SigTree.renderSigilBodyInto sparkSelector { ast }
            -- 2. Measure rendered height
            h <- TS.measureElementHeight sparkSelector
            -- 3. If too tall (>40px ≈ 2 lines), clear and fall back to siglet
            when (h > 40.0) do
              TS.clearElement sparkSelector
              SigTree.renderSigletInto sparkSelector
                { ast: elideAST ast, maxWidth: 360.0 }
          -- All non-SVG cells with AST: render full-size signature (for tooltip)
          SigTree.renderSignatureInto ("#sig-cell-" <> cell.name)
            { name: cell.name, sig: cell.sig, ast, typeParams: [], className: Nothing }
        Nothing -> pure unit
  ) unit cells
