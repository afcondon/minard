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
import Data.Foldable (foldl, foldMap)
import Data.Int (toNumber) as Int
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (min) as Num
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Data.String.Common as SC
import Data.String.CodeUnits as SCU
import Data.String.Pattern (Pattern(..))
import Effect (Effect)
import Data.Either (Either(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core (PropName(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS

import CE2.Data.Decomposition as Dec
import CE2.Data.Loader as Loader
import CE2.Data.SubDeclarationAnalysis as SDA
import CE2.Component.ModuleAnatomyViz as StructViz
import CE2.Util.SVG (svgElem, sa)
import CE2.Viz.CommitSparkline as Spark
import CE2.Viz.DeclarationArcDiagram as ArcDiagram
import CE2.Viz.ModuleTreemapEnriched (DeclarationCircle, ChildCircle, kindColor, childKindColor, packDeclarations)
import CE2.Viz.DeclarationLayerDiagram as LayerDiagram
import CE2.Viz.DOMHelpers as DOMHelpers
import CE2.Viz.ModuleSignatureMap as MSM
import CE2.Viz.SignatureTree as SigTree

import Hylograph.HATS.InterpreterTick (clearContainer, rerender)

-- | Open a URI in the browser (used for vscode:// links)
foreign import openUri :: String -> Effect Unit

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
  | AnnotationReplyCreated                   -- reply annotation created
      { targetType :: String
      , targetId :: String
      , kind :: String
      , value :: String
      , supersedes :: Int
      }
  | CompareSnapshotsClicked                  -- User wants cross-snapshot comparison

type Slot = H.Slot Query Output

data Query a = NoQuery a

data DiagramMode = LayerView | ArcView | DeclStructureView | ConcernClusterView

derive instance eqDiagramMode :: Eq DiagramMode

type State =
  { initialized :: Boolean
  , actionListener :: Maybe (HS.Listener Action)
  , lastInput :: Input
  , lanes :: Array MSM.Lane
  , annotations :: Array Loader.V2Annotation
  , measuredCells :: Array MSM.MeasuredCell
  , arcLayout :: Maybe ArcDiagram.ArcLayout
  , layerLayout :: Maybe LayerDiagram.LayerLayout
  , diagramMode :: DiagramMode
  , diagramReason :: String
  , hoveredArcNode :: Maybe String
  , hoveredLayerNode :: Maybe String
  , declGraph :: Maybe (Dec.SimpleGraph String)
  , declDecomp :: Maybe Dec.DecompInfo
  , subDeclAnalysis :: Maybe SDA.SubDeclAnalysis
  , subDeclGraph :: Maybe (Dec.SimpleGraph String)
  , structureRendered :: Boolean   -- track if HATS structure views need re-rendering
  , replyingTo :: Maybe Int
  , replyText :: String
  , collapsedThreads :: Set Int
  , sparklineBars :: Array Spark.SparklineBar
  }

data Action
  = Initialize
  | Receive Input
  | Finalize
  | HandleDeclarationClick String String String
  | CellClicked (Effect Unit)
  | ArcNodeHovered (Maybe String)
  | ArcNodeClicked String
  | LayerNodeHovered (Maybe String)
  | SwitchDiagramMode DiagramMode
  | ScrollToLanes
  | OpenInEditor
  | ConfirmAnnotation Int
  | DisputeAnnotation Int
  | StartReply Int
  | CancelReply
  | UpdateReplyText String
  | SubmitReply
  | ToggleThreadCollapse Int
  | CompareSnapshots

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
  , layerLayout: Nothing
  , diagramMode: LayerView
  , diagramReason: ""
  , hoveredArcNode: Nothing
  , hoveredLayerNode: Nothing
  , declGraph: Nothing
  , declDecomp: Nothing
  , subDeclAnalysis: Nothing
  , subDeclGraph: Nothing
  , structureRendered: false
  , replyingTo: Nothing
  , replyText: ""
  , collapsedThreads: Set.empty
  , sparklineBars: []
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
    -- Stable child structure: annotation container + sparkline + arc diagram + lanes container.
    -- This prevents Halogen's index-based VDOM diff from patching lane cells
    -- (which use innerHTML prop) into annotation columns when annotations
    -- arrive asynchronously and shift the children array.
    [ HH.div [] (renderAnnotationHeader state)
    , renderSparklineRow state
    , renderDiagramSection state
    , if Array.null state.lanes && state.initialized then
        HH.div
          [ HP.style "display:flex;align-items:center;justify-content:center;height:100%;color:#999;font-size:14px;" ]
          [ HH.text "No declarations" ]
      else
        HH.div [] (state.lanes <#> renderLane)
    ]

-- =============================================================================
-- Diagram section: tabs + selected view
-- =============================================================================

renderDiagramSection :: forall m. State -> H.ComponentHTML Action () m
renderDiagramSection state =
  let
    hasArc = case state.arcLayout of
      Just l -> not (Array.null l.edges)
      Nothing -> false
    hasLayer = case state.layerLayout of
      Just l -> not (Array.null l.nodes)
      Nothing -> false
    hasCalls = hasArc || hasLayer  -- module has internal call structure
    declCount = Array.length state.lastInput.declarations
  in
    if not hasCalls then
      HH.div [] []  -- placeholder for stable child structure
    else
      HH.div [ HP.style "margin: 8px 0 12px 0;" ]
        [ -- Tab bar + reason
          HH.div [ HP.style "display: flex; align-items: baseline; gap: 0; margin-bottom: 8px; border-bottom: 1px solid #ddd;" ]
            [ renderDiagramTab "Layers" LayerView state.diagramMode
            , renderDiagramTab "Declarations" DeclStructureView state.diagramMode
            , renderDiagramTab "Concerns" ConcernClusterView state.diagramMode
            , if state.diagramReason /= "" then
                HH.span [ HP.style "margin-left: 12px; font-size: 10px; color: #999; font-style: italic;" ]
                  [ HH.text state.diagramReason ]
              else HH.text ""
            ]
        , -- Active diagram
          case state.diagramMode of
            LayerView -> renderLayerDiagram state
            ArcView -> renderLayerDiagram state  -- Arc tab removed; fall back to layers
            DeclStructureView ->
              HH.div [ HP.id "decl-structure-container", HP.style "min-height: 200px;" ] []
            ConcernClusterView ->
              HH.div [ HP.id "concern-cluster-container", HP.style "min-height: 200px;" ] []
        , renderCtaBar declCount
        ]

renderDiagramTab :: forall m. String -> DiagramMode -> DiagramMode -> H.ComponentHTML Action () m
renderDiagramTab label mode activeMode =
  let
    isActive = mode == activeMode
    style = "padding: 4px 12px; font-size: 11px; font-weight: 600; cursor: pointer; border: 1px solid "
      <> (if isActive then "#888; background: #fff; color: #333; border-bottom: 1px solid #fff; margin-bottom: -1px; z-index: 1;"
          else "#ddd; background: #f5f5f5; color: #888; border-bottom: 1px solid #ddd;")
      <> " border-radius: 3px 3px 0 0;"
  in
    HH.div
      [ HP.style style
      , HE.onClick \_ -> SwitchDiagramMode mode
      ]
      [ HH.text label ]

-- =============================================================================
-- Arc Diagram renderer
-- =============================================================================

renderArcDiagram :: forall m. State -> H.ComponentHTML Action () m
renderArcDiagram state = case state.arcLayout of
  Nothing -> emptyMessage "No intra-module function calls between exported declarations"
  Just layout
    | Array.null layout.edges -> emptyMessage "No intra-module function calls between exported declarations"
    | otherwise ->
        HH.div []
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
          ]

-- =============================================================================
-- Layer Diagram renderer
-- =============================================================================

emptyMessage :: forall m w. String -> HH.HTML w m
emptyMessage msg = HH.div [ HP.style "padding: 24px; color: #999; font-size: 12px; text-align: center;" ] [ HH.text msg ]

renderLayerDiagram :: forall m. State -> H.ComponentHTML Action () m
renderLayerDiagram state = case state.layerLayout of
  Nothing -> emptyMessage "No internal call hierarchy — declarations do not call each other"
  Just layout
    | Array.null layout.nodes -> emptyMessage "No internal call hierarchy — declarations do not call each other"
    | otherwise ->
        HH.div []
          [ svgElem "svg"
              [ sa "viewBox" ("0 0 " <> show layout.width <> " " <> show layout.height)
              , sa "width" "100%"
              , sa "preserveAspectRatio" "xMidYMid meet"
              , HP.style "display: block; border: 1px solid #e5e5e5; border-radius: 4px; background: #fafafa;"
              ]
              ( renderLayerBands layout
              <> (layout.edges <#> renderLayerEdge state layout)
              <> (layout.nodes <#> renderLayerNode state layout)
              <> (layout.nodes <#> renderLayerLabel state layout)
              )
          ]

-- | Background bands for each layer
renderLayerBands :: forall m w. LayerDiagram.LayerLayout -> Array (HH.HTML w m)
renderLayerBands layout =
  layout.layers <#> \l ->
    let
      y = 30.0 + Int.toNumber (layout.maxLayer - l.layer) * 60.0
      isEven = l.layer `mod` 2 == 0
    in svgElem "rect"
      [ sa "x" "0", sa "y" (show y)
      , sa "width" (show layout.width), sa "height" "60"
      , sa "fill" (if isEven then "#f8f8f8" else "#fff")
      , sa "stroke" "none"
      ] []

renderLayerEdge :: forall m w. State -> LayerDiagram.LayerLayout -> LayerDiagram.LayerEdge -> HH.HTML w m
renderLayerEdge state _layout edge =
  let
    isConnected = case state.hoveredLayerNode of
      Nothing -> true
      Just hovered -> edge.fromName == hovered || edge.toName == hovered
    opacity = if isConnected then "0.3" else "0.05"
    color = if edge.crossesLayers > 1 then "#c05a4e" else "#94a3b8"
  in svgElem "line"
    [ sa "x1" (show edge.fromX), sa "y1" (show edge.fromY)
    , sa "x2" (show edge.toX), sa "y2" (show edge.toY)
    , sa "stroke" color
    , sa "stroke-width" "0.8"
    , sa "stroke-opacity" opacity
    ] []

renderLayerNode :: forall m. State -> LayerDiagram.LayerLayout -> LayerDiagram.LayerNode -> H.ComponentHTML Action () m
renderLayerNode state _layout node =
  let
    isHovered = state.hoveredLayerNode == Just node.name
    isConnected = case state.hoveredLayerNode of
      Nothing -> true
      Just hovered -> hovered == node.name || layerNodeConnected hovered node.name state.layerLayout
    r = if isHovered then show (node.r + 2.0) else show node.r
    opacity = if isConnected then "1" else "0.2"
    -- Use concern group color if available, otherwise kind-based color
    fillColor = case concernGroupForDecl node.name state.subDeclAnalysis of
      Just gi -> StructViz.blockColor gi
      Nothing -> layerKindColor node.kind node.effectful
  in svgElem "circle"
    [ sa "cx" (show node.x), sa "cy" (show node.y)
    , sa "r" r
    , sa "fill" fillColor
    , sa "stroke" "#fff", sa "stroke-width" "0.8"
    , sa "opacity" opacity
    , sa "cursor" "pointer"
    , HE.onMouseEnter \_ -> LayerNodeHovered (Just node.name)
    , HE.onMouseLeave \_ -> LayerNodeHovered Nothing
    ] []

renderLayerLabel :: forall m. State -> LayerDiagram.LayerLayout -> LayerDiagram.LayerNode -> H.ComponentHTML Action () m
renderLayerLabel state _layout node =
  let
    isConnected = case state.hoveredLayerNode of
      Nothing -> true
      Just hovered -> hovered == node.name || layerNodeConnected hovered node.name state.layerLayout
    opacity = if isConnected then "1" else "0.15"
    label = if SCU.length node.name > 18 then SCU.take 17 node.name <> "\x2026" else node.name
    labelY = node.y + node.r + 12.0
    labelColor = case concernGroupForDecl node.name state.subDeclAnalysis of
      Just gi -> StructViz.blockColor gi
      Nothing -> if node.effectful then "#d97706" else "#2563eb"
  in svgElem "text"
    [ sa "x" (show node.x), sa "y" (show labelY)
    , sa "text-anchor" "start"
    , sa "font-size" "8px"
    , sa "font-family" "system-ui, sans-serif"
    , sa "fill" labelColor
    , sa "opacity" opacity
    , sa "pointer-events" "none"
    , sa "transform" ("rotate(-45," <> show node.x <> "," <> show labelY <> ")")
    ]
    [ HH.text label ]

-- | Look up the concern group index for a declaration name.
-- | First checks if the declaration contains a case expression (direct match).
-- | Then checks which concern group's branches reference this declaration most.
concernGroupForDecl :: String -> Maybe SDA.SubDeclAnalysis -> Maybe Int
concernGroupForDecl _name Nothing = Nothing
concernGroupForDecl name (Just analysis) =
  -- Direct match: declaration contains a case expression
  case Array.findIndex (\ce -> ce.functionName == name) analysis.caseExpressions of
    Just i -> Just i
    Nothing ->
      -- Indirect: find which group's branches reference this declaration most
      let
        groupRefs = Array.mapWithIndex (\i ce ->
          { group: i
          , refs: foldl (\acc br -> if Set.member name br.identifierRefs then acc + 1 else acc) 0 ce.branches
          }) analysis.caseExpressions
        best = foldl (\acc gr -> if gr.refs > acc.refs then gr else acc) { group: 0, refs: 0 } groupRefs
      in if best.refs > 0 then Just best.group else Nothing

-- | Color by declaration kind, with effectful distinction
layerKindColor :: String -> Boolean -> String
layerKindColor kind effectful
  | effectful = "#d97706"  -- amber for effectful
  | otherwise = case kind of
      "value" -> "#3b82f6"
      "data" -> "#10b981"
      "newtype" -> "#10b981"
      "type_synonym" -> "#8b5cf6"
      "type_class" -> "#f59e0b"
      _ -> "#6b7280"

-- | Check if two nodes are connected in the layer layout
layerNodeConnected :: String -> String -> Maybe LayerDiagram.LayerLayout -> Boolean
layerNodeConnected a b = case _ of
  Nothing -> false
  Just layout -> Array.any (\e ->
    (e.fromName == a && e.toName == b) || (e.fromName == b && e.toName == a)
    ) layout.edges

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
    , HH.div [ HP.style "display: flex; gap: 12px;" ]
        [ HH.span
            [ HP.style "font-family: 'Fira Code', monospace; font-size: 10px; color: #999; cursor: pointer; transition: color 150ms ease;"
            , HE.onClick \_ -> CompareSnapshots
            ]
            [ HH.text "Compare snapshots" ]
        , HH.span
            [ HP.style "font-family: 'Fira Code', monospace; font-size: 10px; color: #999; cursor: pointer; transition: color 150ms ease;"
            , HE.onClick \_ -> OpenInEditor
            ]
            [ HH.text "Open in editor" ]
        ]
    ]

-- | Check if two nodes are connected by an edge in the arc layout.
nodeConnected :: String -> String -> Maybe ArcDiagram.ArcLayout -> Boolean
nodeConnected a b = case _ of
  Nothing -> false
  Just layout -> Array.any (\e ->
    (e.fromName == a && e.toName == b) || (e.fromName == b && e.toName == a)
    ) layout.edges

-- =============================================================================
-- Commit Sparkline
-- =============================================================================

renderSparklineRow :: forall m. State -> H.ComponentHTML Action () m
renderSparklineRow state
  | Array.null state.sparklineBars && Array.null state.lastInput.declarations = HH.div [] []  -- placeholder for stable child structure
renderSparklineRow state =
  let nBars = Array.length state.sparklineBars
      hasSparkline = nBars > 0
      hasBubblepack = not (Array.null state.lastInput.declarations)
  in HH.div
    [ HP.style "margin: 0 0 12px 0; padding: 8px 0; border-bottom: 1px solid #e8e8e8;" ]
    [ HH.div
        [ HP.style "display: flex; align-items: center; gap: 8px; margin-bottom: 4px;" ]
        [ HH.span
            [ HP.style "font-size: 9px; color: #aaa; font-weight: 600; letter-spacing: 0.5px; text-transform: uppercase;" ]
            [ HH.text (if hasSparkline then show nBars <> " commits" else "") ]
        , HH.span
            [ HP.style "font-size: 9px; color: #bbb;" ]
            [ HH.text (if hasSparkline then "green = additions, red = deletions, gray = rest of commit" else "") ]
        ]
    , HH.div
        [ HP.style "display: flex; align-items: center; gap: 12px;" ]
        ( (if hasBubblepack then [ renderModuleBubblepack state ] else [])
        <> (if hasSparkline then [ renderSparklineSvg state ] else [])
        )
    ]

-- | Inline SVG sparkline — pure Halogen, no Canvas FFI
renderSparklineSvg :: forall m. State -> H.ComponentHTML Action () m
renderSparklineSvg state =
  let nBars = Array.length state.sparklineBars
      -- viewBox width scales with bar count for adequate spacing
      vbWidth = max (Int.toNumber nBars) 200.0
      vbHeight = 72.0
      rects = Spark.toSvgRects { width: vbWidth, height: vbHeight } state.sparklineBars
  in HH.div [ HP.style "flex: 1; min-width: 0;" ]
       [ svgElem "svg"
           [ sa "viewBox" ("0 0 " <> show vbWidth <> " " <> show vbHeight)
           , sa "preserveAspectRatio" "none"
           , HP.style "width: 100%; height: 72px; display: block; border-radius: 3px; border: 1px solid #e0ddd5; background: #f5f2eb;"
           ]
           ( rects <#> \r ->
               svgElem "rect"
                 [ sa "x" (show r.x)
                 , sa "y" (show r.y)
                 , sa "width" (show r.width)
                 , sa "height" (show r.height)
                 , sa "fill" r.fill
                 ]
                 []
           )
       ]

-- =============================================================================
-- Module Bubblepack Glyph
-- =============================================================================

renderModuleBubblepack :: forall m. State -> H.ComponentHTML Action () m
renderModuleBubblepack state =
  let decls = state.lastInput.declarations
  in if Array.null decls then HH.text ""
     else
       let
         { declarations, packRadius } = packDeclarations decls state.lastInput.moduleName 200.0 200.0 Map.empty Map.empty
         pad = 2.0
         r = packRadius + pad
         viewBox = show (-r) <> " " <> show (-r) <> " " <> show (r * 2.0) <> " " <> show (r * 2.0)
       in
         svgElem "svg"
           [ sa "viewBox" viewBox
           , HP.style "width: 72px; height: 72px; flex-shrink: 0; overflow: visible; display: block;"
           ]
           (Array.concatMap renderDeclCircle declarations)
  where
  renderDeclCircle :: DeclarationCircle -> forall w i. Array (HH.HTML w i)
  renderDeclCircle decl =
    let hasChildren = not (Array.null decl.children)
    in
    [ svgElem "circle"
        [ sa "cx" (show decl.x)
        , sa "cy" (show decl.y)
        , sa "r" (show decl.r)
        , sa "fill" (kindColor decl.kind)
        , sa "fill-opacity" (if hasChildren then "0.3" else "0.85")
        , sa "stroke" (if hasChildren then kindColor decl.kind else "white")
        , sa "stroke-width" (if hasChildren then "1" else "0.5")
        ]
        [ svgElem "title" []
            [ HH.text $ decl.kind <> ": " <> decl.name ]
        ]
    ] <> (decl.children <#> \child -> renderChildCircle decl child)

  renderChildCircle :: DeclarationCircle -> ChildCircle -> forall w i. HH.HTML w i
  renderChildCircle parent child =
    svgElem "circle"
      [ sa "cx" (show (parent.x + child.x))
      , sa "cy" (show (parent.y + child.y))
      , sa "r" (show child.r)
      , sa "fill" (childKindColor parent.kind child.kind)
      , sa "fill-opacity" "0.85"
      , sa "stroke" "white"
      , sa "stroke-width" "0.3"
      ]
      [ svgElem "title" []
          [ HH.text $ child.kind <> ": " <> child.name ]
      ]

-- =============================================================================
-- Annotation header
-- =============================================================================

-- | A thread is a root annotation plus its chain of replies
type AnnotationThread =
  { root :: Loader.V2Annotation
  , replies :: Array Loader.V2Annotation
  }

-- | Build threads from an array of annotations.
-- | Roots have no supersedes; replies form chains via supersedes.
buildThreads :: Array Loader.V2Annotation -> Array AnnotationThread
buildThreads anns =
  let
    roots = Array.filter (\a -> a.supersedes == Nothing) anns
    -- Index replies by their supersedes target
    replyMap :: Map Int (Array Loader.V2Annotation)
    replyMap = Array.foldl (\acc a -> case a.supersedes of
      Just sid -> Map.insertWith (<>) sid [a] acc
      Nothing -> acc
    ) Map.empty anns
    -- Collect full chain from a root ID
    collectChain :: Int -> Array Loader.V2Annotation
    collectChain rootId =
      let direct = fromMaybe [] (Map.lookup rootId replyMap)
      in direct <> Array.concatMap (\r -> collectChain r.id) direct
  in roots <#> \root -> { root, replies: collectChain root.id }

renderAnnotationHeader :: forall m. State -> Array (H.ComponentHTML Action () m)
renderAnnotationHeader state
  | Array.null state.annotations = []
renderAnnotationHeader state =
  let
    anns = state.annotations
    cells = state.measuredCells
    threads = buildThreads anns
    -- Group threads by kind
    kindGroups :: Array { kind :: String, threads :: Array AnnotationThread }
    kindGroups =
      let grouped = Array.foldl (\acc t ->
            let k = t.root.kind
                existing = fromMaybe [] (Map.lookup k acc)
            in Map.insert k (Array.snoc existing t) acc
          ) Map.empty threads
      in Map.toUnfoldable grouped <#> \(Tuple k ts) -> { kind: k, threads: ts }
    sorted = kindGroups # Array.sortBy (comparing _.kind)
  in
  [ HH.div
      [ HP.style "margin-bottom: 0; display: grid; grid-template-columns: repeat(auto-fit, minmax(280px, 1fr)); gap: 0;" ]
      (Array.concatMap (\grp ->
        grp.threads <#> \thread ->
          let ann = thread.root
              borderColor = statusBorderColor ann.status
              isCollapsed = Set.member ann.id state.collapsedThreads
              hasReplies = not (Array.null thread.replies)
          in HH.div
            [ HP.style $ "padding: 10px 16px; border-right: 1px solid #e0e0e0; border-left: 3px solid " <> borderColor <> "; overflow-wrap: break-word;" ]
            ( [ HH.div
                  [ HP.style "display: flex; justify-content: space-between; align-items: center; margin-bottom: 6px;" ]
                  [ HH.span
                      [ HP.style "font-weight: 600; color: #999; text-transform: uppercase; font-size: 9px; letter-spacing: 1px;" ]
                      [ HH.text (ann.kind <> sourceTag ann.source) ]
                  , if hasReplies
                    then HH.span
                      [ HP.style "font-family: 'Fira Code', monospace; font-size: 9px; color: #999; cursor: pointer;"
                      , HE.onClick \_ -> ToggleThreadCollapse ann.id
                      ]
                      [ HH.text (if isCollapsed then "\x25b6 " <> show (Array.length thread.replies + 1) else "\x25bc thread") ]
                    else HH.text ""
                  ]
              ] <>
              -- Show root annotation content (always visible unless collapsed with replies)
              ( if isCollapsed && hasReplies
                then
                  -- When collapsed, show only the latest entry
                  let latest = fromMaybe ann (Array.last thread.replies)
                  in [ renderAnnotationContent 0 latest cells
                     , renderAnnotationFooter state latest
                     ]
                else
                  [ renderAnnotationContent 0 ann cells
                  , renderAnnotationFooter state ann
                  ] <>
                  -- Show replies
                  Array.concatMap (\reply ->
                    [ HH.div
                        [ HP.style "margin-left: 12px; padding-left: 8px; border-left: 2px solid #e0e0e0; opacity: 0.8; margin-top: 8px;" ]
                        [ HH.div
                            [ HP.style "font-size: 9px; color: #999; margin-bottom: 4px;" ]
                            [ HH.text (sourceTag reply.source <> " reply") ]
                        , renderAnnotationContent 0 reply cells
                        , renderAnnotationFooter state reply
                        ]
                    ]
                  ) thread.replies
              ) <>
              -- Reply input (if replying to this thread)
              ( case state.replyingTo of
                  Just rid | rid == ann.id || Array.any (\r -> r.id == rid) thread.replies ->
                    [ renderReplyInput state ]
                  _ -> []
              )
            )
      ) sorted)
  , HH.div [ HP.style "border-bottom: 2px solid #e0e0e0; margin: 8px 0 16px 0;" ] []
  ]

-- | Source label tag
sourceTag :: String -> String
sourceTag "ai" = " (ai)"
sourceTag "human" = " (human)"
sourceTag s = if s == "" then "" else " (" <> s <> ")"

-- | Render annotation text content (sentences with inline refs)
renderAnnotationContent :: forall m. Int -> Loader.V2Annotation -> Array MSM.MeasuredCell -> H.ComponentHTML Action () m
renderAnnotationContent annIdx ann cells =
  HH.ul
    [ HP.style "margin: 0; padding: 0 0 0 16px; list-style: disc; color: #444; font-size: 12px; line-height: 1.5;" ]
    (splitSentences ann.value <#> \sentence ->
      HH.li
        [ HP.style "margin-bottom: 3px;" ]
        (annotateText annIdx sentence cells)
    )

-- | Reply input textarea + buttons
renderReplyInput :: forall m. State -> H.ComponentHTML Action () m
renderReplyInput state =
  HH.div
    [ HP.style "margin-top: 8px; padding: 8px; background: #f9f9f9; border: 1px solid #e0e0e0; border-radius: 4px;" ]
    [ HH.textarea
        [ HP.style "width: 100%; min-height: 60px; font-family: 'Fira Code', monospace; font-size: 11px; border: 1px solid #ccc; border-radius: 3px; padding: 6px; box-sizing: border-box; resize: vertical;"
        , HP.value state.replyText
        , HP.placeholder "Your reply..."
        , HE.onValueInput UpdateReplyText
        ]
    , HH.div
        [ HP.style "margin-top: 6px; display: flex; gap: 8px;" ]
        [ HH.span
            [ HP.style "font-family: 'Fira Code', monospace; font-size: 9px; color: #fff; background: #2563eb; cursor: pointer; padding: 2px 8px; border-radius: 2px;"
            , HE.onClick \_ -> SubmitReply
            ]
            [ HH.text "Send" ]
        , HH.span
            [ HP.style "font-family: 'Fira Code', monospace; font-size: 9px; color: #999; cursor: pointer; padding: 2px 8px; border: 1px solid #ccc; border-radius: 2px;"
            , HE.onClick \_ -> CancelReply
            ]
            [ HH.text "Cancel" ]
        ]
    ]

-- | Border color based on annotation status
statusBorderColor :: String -> String
statusBorderColor = case _ of
  "confirmed" -> "#4caf50"
  "rejected"  -> "#e53935"
  "stale"     -> "#f57c00"
  _           -> "#bdbdbd"

-- | Status-dependent footer for an annotation card, with Reply button
renderAnnotationFooter :: forall m. State -> Loader.V2Annotation -> H.ComponentHTML Action () m
renderAnnotationFooter state ann =
  let
    replyBtn =
      HH.span
        [ HP.style "font-family: 'Fira Code', monospace; font-size: 9px; color: #666; cursor: pointer; padding: 1px 6px; border: 1px solid #ccc; border-radius: 2px;"
        , HE.onClick \_ -> StartReply ann.id
        ]
        [ HH.text "Reply" ]
    isReplying = state.replyingTo == Just ann.id
  in case ann.status of
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
        , if isReplying then HH.text "" else replyBtn
        ]
    "confirmed" ->
      HH.div
        [ HP.style "margin-top: 6px; display: flex; gap: 8px; align-items: center;" ]
        [ HH.span
            [ HP.style "font-family: 'Fira Code', monospace; font-size: 9px; color: #4caf50;" ]
            [ HH.text "\x2713 Confirmed" ]
        , if isReplying then HH.text "" else replyBtn
        ]
    "rejected" ->
      HH.div
        [ HP.style "margin-top: 6px; display: flex; gap: 8px; align-items: center;" ]
        [ HH.span
            [ HP.style "font-family: 'Fira Code', monospace; font-size: 9px; color: #e53935;" ]
            [ HH.text "\x2717 Disputed" ]
        , if isReplying then HH.text "" else replyBtn
        ]
    "stale" ->
      HH.div
        [ HP.style "margin-top: 6px; display: flex; gap: 8px; align-items: center;" ]
        [ HH.span
            [ HP.style "font-family: 'Fira Code', monospace; font-size: 9px; color: #f57c00;" ]
            [ HH.text "\x26a0 May be outdated" ]
        , if isReplying then HH.text "" else replyBtn
        ]
    _ ->
      HH.div
        [ HP.style "margin-top: 6px; display: flex; gap: 8px;" ]
        [ if isReplying then HH.text "" else replyBtn ]

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
    -- If heuristic chose a HATS-based view, trigger its imperative render
    afterState <- H.get
    case afterState.diagramMode of
      DeclStructureView -> renderDeclStructure
      ConcernClusterView -> renderConcernClusters
      _ -> pure unit

    -- Fetch commit sparkline data (non-blocking, renders after load)
    loadSparkline input.packageName input.moduleName

  Receive input -> do
    state <- H.get
    let changed = input.moduleName /= state.lastInput.moduleName
              || Array.length input.declarations /= Array.length state.lastInput.declarations
    let callsChanged = Map.size input.functionCalls /= Map.size state.lastInput.functionCalls
    H.modify_ _ { lastInput = input, annotations = input.annotations }
    when ((changed || callsChanged) && state.initialized) do
      H.modify_ _ { lanes = [], measuredCells = [], arcLayout = Nothing, layerLayout = Nothing
                   , declGraph = Nothing, declDecomp = Nothing
                   , subDeclAnalysis = Nothing, subDeclGraph = Nothing }
      renderSignatureMap input
      afterState' <- H.get
      case afterState'.diagramMode of
        DeclStructureView -> renderDeclStructure
        ConcernClusterView -> renderConcernClusters
        _ -> pure unit
    when (changed && state.initialized) do
      loadSparkline input.packageName input.moduleName

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

  LayerNodeHovered mName -> do
    H.modify_ _ { hoveredLayerNode = mName }

  SwitchDiagramMode mode -> do
    -- Clear HATS containers to prevent stale SVGs showing alongside new view
    liftEffect do
      clearContainer "#decl-structure-container"
      clearContainer "#concern-cluster-container"
    H.modify_ _ { diagramMode = mode, structureRendered = false }
    case mode of
      DeclStructureView -> renderDeclStructure
      ConcernClusterView -> renderConcernClusters
      _ -> pure unit

  ScrollToLanes -> do
    state <- H.get
    case Array.head state.lanes >>= (_.cells >>> Array.head) of
      Just firstCell ->
        liftEffect $ DOMHelpers.scrollElementIntoView ("sig-cell-" <> firstCell.name)
      Nothing -> pure unit

  OpenInEditor -> do
    state <- H.get
    log $ "[ModuleSignatureMapViz] Opening in VS Code: " <> state.lastInput.moduleName
    result <- liftAff $ Loader.fetchSourceLocation state.lastInput.moduleName
    case result of
      Right loc -> do
        log $ "[ModuleSignatureMapViz] Resolved path: " <> loc.filePath
        liftEffect $ openUri ("vscode://file/" <> loc.filePath)
      Left err ->
        log $ "[ModuleSignatureMapViz] Could not resolve path: " <> err

  ConfirmAnnotation annId -> do
    H.raise (AnnotationStatusChanged annId "confirmed")

  DisputeAnnotation annId -> do
    H.raise (AnnotationStatusChanged annId "rejected")

  StartReply annId -> do
    H.modify_ _ { replyingTo = Just annId, replyText = "" }

  CancelReply -> do
    H.modify_ _ { replyingTo = Nothing, replyText = "" }

  UpdateReplyText text -> do
    H.modify_ _ { replyText = text }

  SubmitReply -> do
    state <- H.get
    case state.replyingTo of
      Nothing -> pure unit
      Just targetId -> do
        let input = state.lastInput
            -- Find the annotation we're replying to, to inherit its kind
            targetAnn = Array.find (\a -> a.id == targetId) state.annotations
            kind = fromMaybe "summary" (targetAnn <#> _.kind)
        when (SCU.length state.replyText > 0) do
          H.modify_ _ { replyingTo = Nothing, replyText = "" }
          H.raise $ AnnotationReplyCreated
            { targetType: "module"
            , targetId: input.moduleName
            , kind
            , value: state.replyText
            , supersedes: targetId
            }

  ToggleThreadCollapse rootId -> do
    state <- H.get
    let newCollapsed = if Set.member rootId state.collapsedThreads
          then Set.delete rootId state.collapsedThreads
          else Set.insert rootId state.collapsedThreads
    H.modify_ _ { collapsedThreads = newCollapsed }

  CompareSnapshots ->
    H.raise CompareSnapshotsClicked

-- | Fetch numstat data for sparkline (pure SVG render happens via Halogen re-render)
loadSparkline :: forall m. MonadAff m => String -> String -> H.HalogenM State Action () Output m Unit
loadSparkline pkgName modName = do
  result <- liftAff $ Loader.fetchModuleNumstat 500 pkgName
  case result of
    Left err ->
      log $ "[Sparkline] Error fetching numstat: " <> err
    Right commits -> do
      let bars = Spark.prepareData modName commits
      log $ "[Sparkline] " <> modName <> ": " <> show (Array.length bars) <> " commits"
      H.modify_ _ { sparklineBars = bars }

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
  let layoutInput = { moduleName: input.moduleName
        , declarations: input.declarations
        , functionCalls: input.functionCalls
        , layoutWidth: 900.0
        }
  let arcLay = ArcDiagram.computeLayout layoutInput
  let mArcLayout = if Array.null arcLay.edges then Nothing else Just arcLay
  let layerLay = LayerDiagram.computeLayout layoutInput
  let mLayerLayout = if Array.null layerLay.nodes then Nothing else Just layerLay
  -- Pre-compute declaration call graph for structure views
  -- Include ALL internal names from call edges, not just exported declarations
  let allCalls = foldMap identity input.functionCalls
      internalCalls = Array.filter (\c -> not c.isCrossModule && c.calleeModule == input.moduleName && c.callerName /= c.calleeName) allCalls
      -- Start with exported declarations, then add any names from call graph edges
      exportedNames = Set.fromFoldable $ input.declarations <#> _.name
      callNames = foldl (\acc c -> Set.insert c.callerName (Set.insert c.calleeName acc)) Set.empty internalCalls
      declNames = Set.union exportedNames callNames
      edges = foldl (\acc call ->
        if Set.member call.callerName declNames && Set.member call.calleeName declNames
        then
          Map.alter (Just <<< Set.insert call.calleeName <<< fromMaybe Set.empty) call.callerName
            (Map.alter (Just <<< Set.insert call.callerName <<< fromMaybe Set.empty) call.calleeName acc)
        else acc
      ) Map.empty internalCalls
      declGraph = { nodes: Set.toUnfoldable declNames :: Array String, edges }
      declDecomp = Dec.analyzeGraph declGraph
  -- Eagerly fetch source and compute concern analysis (for color linkage across tabs)
  { mAnalysis, mSubDeclGraph } <- do
    result <- liftAff $ Loader.fetchModuleSource input.moduleName
    case result of
      Left _ -> pure { mAnalysis: Nothing, mSubDeclGraph: Nothing }
      Right src -> do
        let analysis = SDA.analyzeModuleSource src.source
        let { declarations: subDecls, internalCalls: subCalls } = SDA.branchesToDeclGraph analysis.allBranches
        let subNames = Set.fromFoldable $ subDecls <#> _.name
        let subEdges = foldl (\acc call ->
              if Set.member call.callerName subNames && Set.member call.calleeName subNames
              then
                Map.alter (Just <<< Set.insert call.calleeName <<< fromMaybe Set.empty) call.callerName
                  (Map.alter (Just <<< Set.insert call.callerName <<< fromMaybe Set.empty) call.calleeName acc)
              else acc
            ) Map.empty subCalls
        let subGraph = { nodes: Set.toUnfoldable subNames :: Array String, edges: subEdges }
        pure { mAnalysis: Just analysis, mSubDeclGraph: Just subGraph }
  -- Heuristic: choose default tab based on what's most informative
  let hasConcerns = case mAnalysis of
        Just a -> Array.length a.caseExpressions > 0
        Nothing -> false
  let { mode: defaultMode, reason } = chooseDiagramMode mLayerLayout declDecomp hasConcerns
  H.modify_ _ { lanes = newLanes, measuredCells = measured
               , arcLayout = mArcLayout, layerLayout = mLayerLayout
               , declGraph = Just declGraph, declDecomp = Just declDecomp
               , subDeclAnalysis = mAnalysis, subDeclGraph = mSubDeclGraph
               , diagramMode = defaultMode, diagramReason = reason }

-- =============================================================================
-- Diagram mode heuristic
-- =============================================================================

-- | Choose the most informative default diagram tab for a module.
chooseDiagramMode :: Maybe LayerDiagram.LayerLayout -> Dec.DecompInfo -> Boolean -> { mode :: DiagramMode, reason :: String }
chooseDiagramMode mLayerLayout declDecomp hasConcerns =
  let
    nBlocks = declDecomp.metrics.biconnectedComponentCount
    nAPs = declDecomp.metrics.articulationPointCount
    maxBlock = declDecomp.metrics.maxBlockSize
    nDecls = Map.size declDecomp.nodeBlock
    tree = declDecomp.metrics.treelikeness
  in
    -- Large dominant block with low treelikeness → tangled, show concerns (if available)
    if maxBlock > 10 && tree < 0.3 && hasConcerns then
      { mode: ConcernClusterView
      , reason: show maxBlock <> "-node tangled core — concern separation may help"
      }
    -- Multiple tightly-coupled clusters with articulation points → show structure
    else if nBlocks > 2 && nAPs > 0 then
      { mode: DeclStructureView
      , reason: show nBlocks <> " clusters connected by " <> show nAPs <> " hub declaration" <> (if nAPs > 1 then "s" else "")
      }
    -- Deep layer hierarchy → show layers
    else case mLayerLayout of
      Just l | l.maxLayer >= 3 ->
        { mode: LayerView
        , reason: show (l.maxLayer + 1) <> " call depth levels — layered internal architecture"
        }
      Just l | l.maxLayer > 0 ->
        { mode: LayerView
        , reason: show (Array.length l.nodes) <> " declarations across " <> show (l.maxLayer + 1) <> " layers"
        }
      _ ->
        { mode: LayerView
        , reason: show nDecls <> " declarations — flat structure"
        }

-- =============================================================================
-- Structure view rendering (HATS-based, rendered imperatively into containers)
-- =============================================================================

-- | Render the declaration structure (biconnected components) into a HATS container
renderDeclStructure :: forall m. MonadAff m => H.HalogenM State Action () Output m Unit
renderDeclStructure = do
  state <- H.get
  case state.declGraph, state.declDecomp of
    Just graph, Just info
      | Array.null graph.nodes -> liftEffect do
          clearContainer "#decl-structure-container"
          DOMHelpers.setInnerHTML "#decl-structure-container"
            "<div style=\"padding: 24px; color: #999; font-size: 12px; text-align: center;\">No internal call graph — declarations do not call each other</div>"
      | otherwise -> do
          let kindMap = foldl (\acc d -> Map.insert d.name d.kind acc) Map.empty state.lastInput.declarations
          liftEffect do
            clearContainer "#decl-structure-container"
            _ <- rerender "#decl-structure-container" (StructViz.callGraphTree graph info kindMap)
            pure unit
    _, _ -> liftEffect do
      DOMHelpers.setInnerHTML "#decl-structure-container"
        "<div style=\"padding: 24px; color: #999; font-size: 12px; text-align: center;\">No declaration data available</div>"

-- | Render the concern-clustered graph into a HATS container.
-- | Fetches module source if not yet analyzed.
renderConcernClusters :: forall m. MonadAff m => H.HalogenM State Action () Output m Unit
renderConcernClusters = do
  state <- H.get
  case state.subDeclAnalysis of
    Just analysis -> renderConcernGraph analysis
    Nothing -> do
      -- Fetch source and analyze
      result <- liftAff $ Loader.fetchModuleSource state.lastInput.moduleName
      case result of
        Left err ->
          log $ "[ConcernClusters] Failed to fetch source: " <> err
        Right src -> do
          let analysis = SDA.analyzeModuleSource src.source
          log $ "[ConcernClusters] " <> state.lastInput.moduleName <> ": "
              <> show (Array.length analysis.allBranches) <> " branches, "
              <> show (Array.length analysis.caseExpressions) <> " case expressions"
          -- Build sub-declaration graph
          let { declarations, internalCalls } = SDA.branchesToDeclGraph analysis.allBranches
          let declNames = Set.fromFoldable $ declarations <#> _.name
          let edges = foldl (\acc call ->
                if Set.member call.callerName declNames && Set.member call.calleeName declNames
                then
                  Map.alter (Just <<< Set.insert call.calleeName <<< fromMaybe Set.empty) call.callerName
                    (Map.alter (Just <<< Set.insert call.callerName <<< fromMaybe Set.empty) call.calleeName acc)
                else acc
              ) Map.empty internalCalls
          let graph = { nodes: Set.toUnfoldable declNames :: Array String, edges }
          H.modify_ _ { subDeclAnalysis = Just analysis, subDeclGraph = Just graph }
          renderConcernGraph analysis

renderConcernGraph :: forall m. MonadAff m => SDA.SubDeclAnalysis -> H.HalogenM State Action () Output m Unit
renderConcernGraph analysis = do
  state <- H.get
  if Array.null analysis.caseExpressions then
    liftEffect do
      clearContainer "#concern-cluster-container"
      DOMHelpers.setInnerHTML "#concern-cluster-container"
        "<div style=\"padding: 24px; color: #999; font-size: 12px; text-align: center;\">No case expressions found — concern clustering requires pattern-matching branches (e.g. handleAction)</div>"
  else case state.subDeclGraph of
    Just graph ->
      liftEffect do
        clearContainer "#concern-cluster-container"
        _ <- rerender "#concern-cluster-container"
               (StructViz.concernClusteredTree graph analysis.caseExpressions)
        pure unit
    Nothing -> pure unit

-- =============================================================================
-- Declaration click callback
-- =============================================================================

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
