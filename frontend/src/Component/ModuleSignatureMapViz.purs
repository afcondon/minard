-- | Module Signature Map Visualization Component
-- |
-- | A Halogen component that renders a category-lane layout of a module's
-- | type signatures. SVGs are pre-rendered and measured, then placed into
-- | a Halogen HTML layout with CSS flexbox shelf packing.
-- |
-- | Includes an arc diagram of intra-module function calls between the
-- | annotation header and the lane cards.
module CE2.Component.ModuleSignatureMapViz
  ( component
  , Input
  , Output(..)
  , Query(..)
  , Slot
  ) where

import Prelude

import Data.Array as Array
import Data.Int (toNumber) as Int
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Number (min) as Num
import Data.String.Common as SC
import Data.String.CodeUnits as SCU
import Data.String.Pattern (Pattern(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core (AttrName(..), ElemName(..), Namespace(..), PropName(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS

import CE2.Data.Loader as Loader
import CE2.Viz.DeclarationArcDiagram as ArcDiagram
import CE2.Viz.DOMHelpers as DOMHelpers
import CE2.Viz.ModuleSignatureMap as MSM
import CE2.Viz.SignatureTree as SigTree

-- =============================================================================
-- Types
-- =============================================================================

type Input =
  { packageName :: String
  , moduleName :: String
  , declarations :: Array Loader.V2Declaration
  , annotations :: Array Loader.V2Annotation
  , functionCalls :: Map Int (Array Loader.V2FunctionCall)
  }

data Output
  = DeclarationClicked String String String  -- pkg, mod, decl
  | AnnotationStatusChanged Int String       -- annId, newStatus

type Slot = H.Slot Query Output

data Query a = NoQuery a

type State =
  { initialized :: Boolean
  , actionListener :: Maybe (HS.Listener Action)
  , lastInput :: Input
  , lanes :: Array MSM.Lane
  , annotations :: Array Loader.V2Annotation
  , measuredCells :: Array MSM.MeasuredCell
  , arcLayout :: Maybe ArcDiagram.ArcLayout
  , hoveredArcNode :: Maybe String
  }

data Action
  = Initialize
  | Receive Input
  | Finalize
  | HandleDeclarationClick String String String
  | CellClicked (Effect Unit)
  | ArcNodeHovered (Maybe String)
  | ArcNodeClicked String
  | ScrollToLanes
  | OpenInEditor
  | ConfirmAnnotation Int
  | DisputeAnnotation Int

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
  , annotations: input.annotations
  , measuredCells: []
  , arcLayout: Nothing
  , hoveredArcNode: Nothing
  }

-- =============================================================================
-- SVG helpers
-- =============================================================================

svgNS :: Namespace
svgNS = Namespace "http://www.w3.org/2000/svg"

svgElem :: forall r w i. String -> Array (HH.IProp r i) -> Array (HH.HTML w i) -> HH.HTML w i
svgElem name = HH.elementNS svgNS (ElemName name)

sa :: forall r i. String -> String -> HH.IProp r i
sa name val = HP.attr (AttrName name) val

-- =============================================================================
-- Render
-- =============================================================================

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div
    [ HP.class_ (HH.ClassName "module-signature-map")
    , HP.style "overflow-y: auto; padding: 12px 16px; position: absolute; top: 0; left: 0; width: 100%; height: 100%; box-sizing: border-box;"
    ]
    -- Stable three-child structure: annotation container + arc diagram + lanes container.
    -- This prevents Halogen's index-based VDOM diff from patching lane cells
    -- (which use innerHTML prop) into annotation columns when annotations
    -- arrive asynchronously and shift the children array.
    [ HH.div [] (renderAnnotationHeader state.annotations state.measuredCells)
    , renderArcDiagram state
    , if Array.null state.lanes && state.initialized then
        HH.div
          [ HP.style "display:flex;align-items:center;justify-content:center;height:100%;color:#999;font-size:14px;" ]
          [ HH.text "No declarations" ]
      else
        HH.div [] (state.lanes <#> renderLane)
    ]

-- =============================================================================
-- Arc Diagram renderer
-- =============================================================================

renderArcDiagram :: forall m. State -> H.ComponentHTML Action () m
renderArcDiagram state = case state.arcLayout of
  Nothing ->
    -- Empty placeholder to maintain three-child structure
    HH.div [] []
  Just layout
    | Array.null layout.edges ->
        HH.div [] []
    | otherwise ->
        let declCount = Array.length state.lastInput.declarations
        in HH.div
          [ HP.style "margin: 8px 0 12px 0;" ]
          [ svgElem "svg"
              [ sa "viewBox" ("0 0 " <> show layout.width <> " " <> show layout.height)
              , sa "width" "100%"
              , sa "preserveAspectRatio" "xMidYMid meet"
              , HP.style "display: block;"
              ]
              ( (layout.edges <#> renderArcEdge state layout)
              <> (layout.nodes <#> renderArcNode state layout)
              <> (layout.nodes <#> renderArcLabel state layout)
              )
          , renderCtaBar declCount
          ]

renderArcEdge :: forall m. State -> ArcDiagram.ArcLayout -> ArcDiagram.ArcEdge -> H.ComponentHTML Action () m
renderArcEdge state _layout edge =
  let
    strokeW = Num.min 3.0 (0.75 + Int.toNumber edge.count * 0.5)
    isConnected = case state.hoveredArcNode of
      Nothing -> true
      Just hovered -> edge.fromName == hovered || edge.toName == hovered
    opacity = if isConnected then "0.7" else "0.1"
  in
    svgElem "path"
      [ sa "d" edge.pathD
      , sa "fill" "none"
      , sa "stroke" edge.color
      , sa "stroke-width" (show strokeW)
      , sa "opacity" opacity
      , HP.style "transition: opacity 150ms ease;"
      ]
      []

renderArcNode :: forall m. State -> ArcDiagram.ArcLayout -> ArcDiagram.ArcNode -> H.ComponentHTML Action () m
renderArcNode state layout node =
  let
    isHovered = state.hoveredArcNode == Just node.name
    isConnected = case state.hoveredArcNode of
      Nothing -> true
      Just hovered -> hovered == node.name || nodeConnected hovered node.name state.arcLayout
    r = if isHovered then "6" else "4"
    opacity = if isConnected then "1" else "0.2"
    fillColor = ArcDiagram.heatColor node.heat
    strokeColor = ArcDiagram.heatColor (Num.min 1.0 (node.heat + 0.15))
  in
    svgElem "circle"
      [ sa "cx" (show node.x)
      , sa "cy" (show layout.baselineY)
      , sa "r" r
      , sa "fill" fillColor
      , sa "stroke" strokeColor
      , sa "stroke-width" "1.5"
      , sa "opacity" opacity
      , HP.style "transition: opacity 150ms ease, r 150ms ease; cursor: pointer;"
      , HE.onMouseEnter \_ -> ArcNodeHovered (Just node.name)
      , HE.onMouseLeave \_ -> ArcNodeHovered Nothing
      , HE.onClick \_ -> ArcNodeClicked node.name
      ]
      []

renderArcLabel :: forall m. State -> ArcDiagram.ArcLayout -> ArcDiagram.ArcNode -> H.ComponentHTML Action () m
renderArcLabel state layout node =
  let
    isConnected = case state.hoveredArcNode of
      Nothing -> true
      Just hovered -> hovered == node.name || nodeConnected hovered node.name state.arcLayout
    opacity = if isConnected then "1" else "0.15"
    label = if SCU.length node.name > 16 then SCU.take 15 node.name <> "\x2026" else node.name
    labelY = layout.baselineY + 10.0
    labelColor = if node.effectful then "#d97706" else "#2563eb"
  in
    svgElem "text"
      [ sa "x" (show node.x)
      , sa "y" (show labelY)
      , sa "text-anchor" "start"
      , sa "font-family" "'Fira Code', 'SF Mono', monospace"
      , sa "font-size" "8"
      , sa "fill" labelColor
      , sa "opacity" opacity
      , sa "transform" ("rotate(45 " <> show node.x <> " " <> show labelY <> ")")
      , HP.style "transition: opacity 150ms ease; cursor: pointer; user-select: none;"
      , HE.onMouseEnter \_ -> ArcNodeHovered (Just node.name)
      , HE.onMouseLeave \_ -> ArcNodeHovered Nothing
      , HE.onClick \_ -> ArcNodeClicked node.name
      ]
      [ HH.text label ]

-- | CTA bar shown below the arc diagram with scroll hint and editor stub.
renderCtaBar :: forall m. Int -> H.ComponentHTML Action () m
renderCtaBar declCount =
  HH.div
    [ HP.style "display: flex; justify-content: space-between; align-items: center; margin: 4px 0 0 0;" ]
    [ HH.span
        [ HP.style "font-family: 'Fira Code', monospace; font-size: 10px; color: #999; cursor: pointer; transition: color 150ms ease;"
        , HE.onMouseEnter \_ -> ArcNodeHovered Nothing
        , HE.onClick \_ -> ScrollToLanes
        ]
        [ HH.text ("\x2193 " <> show declCount <> " declarations below") ]
    , HH.span
        [ HP.style "font-family: 'Fira Code', monospace; font-size: 10px; color: #999; cursor: pointer; transition: color 150ms ease;"
        , HE.onClick \_ -> OpenInEditor
        ]
        [ HH.text "Open in editor" ]
    ]

-- | Check if two nodes are connected by an edge in the arc layout.
nodeConnected :: String -> String -> Maybe ArcDiagram.ArcLayout -> Boolean
nodeConnected a b = case _ of
  Nothing -> false
  Just layout -> Array.any (\e ->
    (e.fromName == a && e.toName == b) || (e.fromName == b && e.toName == a)
    ) layout.edges

-- =============================================================================
-- Annotation header
-- =============================================================================

renderAnnotationHeader :: forall m. Array Loader.V2Annotation -> Array MSM.MeasuredCell -> Array (H.ComponentHTML Action () m)
renderAnnotationHeader anns _
  | Array.null anns = []
renderAnnotationHeader anns cells =
  let sorted = anns # Array.sortBy (comparing _.kind)
  in
  [ HH.div
      [ HP.style "margin-bottom: 0; display: grid; grid-template-columns: repeat(auto-fit, minmax(280px, 1fr)); gap: 0;" ]
      (Array.mapWithIndex (\annIdx ann ->
        let borderColor = statusBorderColor ann.status
        in HH.div
          [ HP.style $ "padding: 10px 16px; border-right: 1px solid #e0e0e0; border-left: 3px solid " <> borderColor <> "; overflow-wrap: break-word;" ]
          [ HH.div
              [ HP.style "font-weight: 600; color: #999; text-transform: uppercase; font-size: 9px; letter-spacing: 1px; margin-bottom: 6px;" ]
              [ HH.text ann.kind ]
          , HH.ul
              [ HP.style "margin: 0; padding: 0 0 0 16px; list-style: disc; color: #444; font-size: 12px; line-height: 1.5;" ]
              (splitSentences ann.value <#> \sentence ->
                HH.li
                  [ HP.style "margin-bottom: 3px;" ]
                  (annotateText annIdx sentence cells)
              )
          , renderAnnotationFooter ann
          ]
      ) sorted)
  , HH.div [ HP.style "border-bottom: 2px solid #e0e0e0; margin: 8px 0 16px 0;" ] []
  ]

-- | Border color based on annotation status
statusBorderColor :: String -> String
statusBorderColor = case _ of
  "confirmed" -> "#4caf50"
  "rejected"  -> "#e53935"
  "stale"     -> "#f57c00"
  _           -> "#bdbdbd"

-- | Status-dependent footer for an annotation card
renderAnnotationFooter :: forall m. Loader.V2Annotation -> H.ComponentHTML Action () m
renderAnnotationFooter ann = case ann.status of
  "proposed" ->
    HH.div
      [ HP.style "margin-top: 6px; display: flex; gap: 8px;" ]
      [ HH.span
          [ HP.style "font-family: 'Fira Code', monospace; font-size: 9px; color: #4caf50; cursor: pointer; padding: 1px 6px; border: 1px solid #4caf50; border-radius: 2px;"
          , HE.onClick \_ -> ConfirmAnnotation ann.id
          ]
          [ HH.text "Confirm" ]
      , HH.span
          [ HP.style "font-family: 'Fira Code', monospace; font-size: 9px; color: #e53935; cursor: pointer; padding: 1px 6px; border: 1px solid #e53935; border-radius: 2px;"
          , HE.onClick \_ -> DisputeAnnotation ann.id
          ]
          [ HH.text "Dispute" ]
      ]
  "confirmed" ->
    HH.div
      [ HP.style "margin-top: 6px; font-family: 'Fira Code', monospace; font-size: 9px; color: #4caf50;" ]
      [ HH.text "\x2713 Confirmed" ]
  "rejected" ->
    HH.div
      [ HP.style "margin-top: 6px; font-family: 'Fira Code', monospace; font-size: 9px; color: #e53935;" ]
      [ HH.text "\x2717 Disputed" ]
  "stale" ->
    HH.div
      [ HP.style "margin-top: 6px; font-family: 'Fira Code', monospace; font-size: 9px; color: #f57c00;" ]
      [ HH.text "\x26a0 May be outdated" ]
  _ ->
    HH.div [] []

-- | Split annotation text into sentences (on ". " boundaries).
-- | Preserves trailing periods on each sentence.
splitSentences :: String -> Array String
splitSentences text =
  let parts = SC.split (Pattern ". ") text
      len = Array.length parts
  in parts # Array.mapWithIndex (\i s ->
    if i < len - 1 then s <> "." else s)
    # Array.filter (\s -> SCU.length s > 0)

-- | A match of a declaration name found in annotation text.
type TextMatch = { pos :: Int, cell :: MSM.MeasuredCell }

-- | Find cells whose names appear in the text. Returns non-overlapping matches
-- | sorted by position, capped at 6.
findTextMatches :: String -> Array MSM.MeasuredCell -> Array TextMatch
findTextMatches text cells =
  let
    candidates = cells
      # Array.filter (\c -> SCU.length c.name >= 4)
      # Array.mapMaybe (\c -> case SCU.indexOf (Pattern c.name) text of
          Just pos -> Just { pos, cell: c }
          Nothing -> Nothing)
      # Array.sortBy (comparing _.pos)
    removeOverlaps = Array.foldl (\acc m ->
      case Array.last acc of
        Nothing -> [m]
        Just prev ->
          if m.pos < prev.pos + SCU.length prev.cell.name
          then acc
          else Array.snoc acc m
    ) [] candidates
  in Array.take 6 removeOverlaps

-- | Render annotation text with inline siglet placeholders where declaration
-- | names appear. Returns a mixed array of HH.text and inline spans.
annotateText :: forall m. Int -> String -> Array MSM.MeasuredCell -> Array (H.ComponentHTML Action () m)
annotateText annIdx text cells =
  let
    matches = findTextMatches text cells
    go :: Int -> Int -> Array TextMatch -> Array (H.ComponentHTML Action () m)
    go cursor refIdx remaining = case Array.uncons remaining of
      Nothing ->
        let rest = SCU.drop cursor text
        in if SCU.length rest > 0 then [HH.text rest] else []
      Just { head: m, tail: ms } ->
        let
          before = SCU.take (m.pos - cursor) (SCU.drop cursor text)
          nameLen = SCU.length m.cell.name
          beforeEls = if SCU.length before > 0 then [HH.text before] else []
          matchEl = renderInlineRef annIdx refIdx m.cell
        in beforeEls <> [matchEl] <> go (m.pos + nameLen) (refIdx + 1) ms
  in go 0 0 matches

-- | Render an inline reference span for a declaration in annotation text.
-- | Shows the declaration name as a styled label. (Siglet rendering via
-- | innerHTML is used for lane cards but not here — inline-flex siglets
-- | inside flowing text can collapse parent layout in some browsers.)
renderInlineRef :: forall m. Int -> Int -> MSM.MeasuredCell -> H.ComponentHTML Action () m
renderInlineRef _annIdx _refIdx cell =
  HH.span
    [ HP.style $ "padding: 1px 4px; border-radius: 3px;"
        <> " background: " <> MSM.kindBackground cell.kind <> ";"
        <> " border: 1px solid " <> MSM.kindBorder cell.kind <> ";"
        <> " cursor: pointer; font-family: 'Fira Code','SF Mono', monospace; font-size: 10px;"
    , HE.onClick \_ -> CellClicked cell.onClick
    ]
    [ HH.text cell.name ]

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

-- | Render a full-size cell for all declaration kinds.
-- | Structured content is set via the innerHTML DOM property so that
-- | Halogen applies it during its normal VDOM-to-DOM patch — no post-render
-- | injection or timing hacks required.
renderFullCell :: forall m. MSM.MeasuredCell -> H.ComponentHTML Action () m
renderFullCell cell =
  let
    baseProps =
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
  in case cellHtml cell of
    Just html ->
      HH.div (baseProps <> [ HP.prop (PropName "innerHTML") html ]) []
    Nothing ->
      HH.div baseProps
        [ HH.div
            [ HP.style "font-size:11px; color:#333; font-family:'Fira Code','SF Mono',monospace;" ]
            [ HH.text (cell.name <> if cell.sig == "" then "" else " :: " <> cell.sig) ]
        ]

-- | Generate the HTML string for a cell's content. Returns Nothing for
-- | plain-text-only cells (no structured data, no AST).
cellHtml :: MSM.MeasuredCell -> Maybe String
cellHtml cell = case cell.dataDecl of
  Just dd -> Just $ SigTree.renderDataDecl
    { name: cell.name, typeParams: dd.typeParams, constructors: dd.constructors, keyword: dd.keyword }
  Nothing -> case cell.classDecl of
    Just cd ->
      let
        classHtml = SigTree.renderClassDecl
          { name: cell.name, typeParams: cd.typeParams, superclasses: cd.superclasses, methods: cd.methods }
        instancesHtml =
          if Array.null cd.instances then ""
          else renderInstancesHtml cd.instances
      in Just (classHtml <> instancesHtml)
    Nothing -> case cell.typeSynonym of
      Just ts -> Just $ SigTree.renderTypeSynonym
        { name: cell.name, typeParams: ts.typeParams, body: ts.body }
      Nothing -> case cell.ast of
        Just ast ->
          if cell.foreignImport
          then Just $ SigTree.renderForeignImport { name: cell.name, ast }
          else Just $ SigTree.renderSignature
            { name: cell.name, sig: cell.sig, ast, typeParams: [], className: Nothing }
        Nothing -> Nothing

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
    let callsChanged = Map.size input.functionCalls /= Map.size state.lastInput.functionCalls
    H.modify_ _ { lastInput = input, annotations = input.annotations }
    when ((changed || callsChanged) && state.initialized) do
      H.modify_ _ { lanes = [], measuredCells = [], arcLayout = Nothing }
      renderSignatureMap input

  Finalize -> do
    log "[ModuleSignatureMapViz] Finalizing"

  HandleDeclarationClick pkgName modName declName -> do
    log $ "[ModuleSignatureMapViz] Declaration clicked: " <> declName
    H.raise (DeclarationClicked pkgName modName declName)

  CellClicked handler -> do
    liftEffect handler

  ArcNodeHovered mName -> do
    H.modify_ _ { hoveredArcNode = mName }

  ArcNodeClicked declName -> do
    liftEffect $ DOMHelpers.scrollElementIntoView ("sig-cell-" <> declName)

  ScrollToLanes -> do
    state <- H.get
    case Array.head state.lanes >>= (_.cells >>> Array.head) of
      Just firstCell ->
        liftEffect $ DOMHelpers.scrollElementIntoView ("sig-cell-" <> firstCell.name)
      Nothing -> pure unit

  OpenInEditor -> do
    state <- H.get
    log $ "[ModuleSignatureMapViz] Open in editor (stub): " <> state.lastInput.moduleName

  ConfirmAnnotation annId -> do
    H.raise (AnnotationStatusChanged annId "confirmed")

  DisputeAnnotation annId -> do
    H.raise (AnnotationStatusChanged annId "rejected")

-- | Prepare cells, group into lanes, compute arc layout, then update state.
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
  let arcLay = ArcDiagram.computeLayout
        { moduleName: input.moduleName
        , declarations: input.declarations
        , functionCalls: input.functionCalls
        , layoutWidth: 900.0
        }
  let mArcLayout = if Array.null arcLay.edges then Nothing else Just arcLay
  H.modify_ _ { lanes = newLanes, measuredCells = measured, arcLayout = mArcLayout }

-- | Create a declaration click callback that notifies the Halogen listener
makeDeclarationClickCallback :: Maybe (HS.Listener Action) -> String -> String -> String -> Effect Unit
makeDeclarationClickCallback mListener pkgName modName declName = case mListener of
  Just listener -> HS.notify listener (HandleDeclarationClick pkgName modName declName)
  Nothing -> log $ "[ModuleSignatureMapViz] No listener for decl click: " <> pkgName <> "/" <> modName <> "/" <> declName

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
