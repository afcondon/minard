-- | Module Structure Visualization Component
-- |
-- | A Halogen component that renders a module's internal structure:
-- | layer diagrams, biconnected component decomposition, concern clustering,
-- | annotations and internal structure diagrams.
module CE2.Component.ModuleStructureViz
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
import Data.String as String
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
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS

import Type.Proxy (Proxy(..))

import CE2.Component.AnnotationGuide (renderAnnotationGuide)
import CE2.Component.DeclarationUsageGraph as DeclarationUsageGraph
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
import CE2.Viz.ModuleStructure as MSM
import CE2.Viz.SourceCode as SourceCode

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
  , moduleNameToId :: Map String Int
  , diagramsOnly :: Boolean  -- when true, render only the diagram section (no annotations, sparkline, source preview, embedded usage graph)
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

type ChildSlots =
  ( declarationUsageGraph :: DeclarationUsageGraph.Slot Unit
  )

_declarationUsageGraph :: Proxy "declarationUsageGraph"
_declarationUsageGraph = Proxy

data Query a = NoQuery a

data DiagramMode = LayerView | ArcView | DeclStructureView | ConcernClusterView

derive instance eqDiagramMode :: Eq DiagramMode

data FocusedSection = FocusAnnotations | FocusDiagrams

derive instance eqFocusedSection :: Eq FocusedSection

type State =
  { initialized :: Boolean
  , actionListener :: Maybe (HS.Listener Action)
  , lastInput :: Input
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
  , focusedSection :: Maybe FocusedSection
  , helpSection :: Maybe FocusedSection
  , sourcePreview :: Maybe { declarationName :: String }
  , cachedModuleSource :: Maybe String
  , focusedDeclaration :: Maybe String
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
  | OpenInEditor
  | ConfirmAnnotation Int
  | DisputeAnnotation Int
  | StartReply Int
  | CancelReply
  | UpdateReplyText String
  | SubmitReply
  | ToggleThreadCollapse Int
  | CompareSnapshots
  | FocusSection (Maybe FocusedSection)
  | ToggleHelp FocusedSection
  | DiagramNodeClicked String
  | ClosePreview
  | OpenPreviewInEditor
  | PreviewFullDetail
  | FocusDeclaration (Maybe String)
  | HandleUsageGraphOutput DeclarationUsageGraph.Output

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
  , focusedSection: Nothing
  , helpSection: Nothing
  , sourcePreview: Nothing
  , cachedModuleSource: Nothing
  , focusedDeclaration: Nothing
  }

-- =============================================================================
-- Render
-- =============================================================================

render :: forall m. MonadAff m => State -> H.ComponentHTML Action ChildSlots m
render state =
  HH.div
    [ HP.class_ (HH.ClassName "module-structure")
    , HP.style "overflow-y: auto; padding: 12px 16px; position: absolute; top: 0; left: 0; width: 100%; height: 100%; box-sizing: border-box;"
    ]
    [ renderSparklineRow state
    , case state.focusedSection of
        Just FocusAnnotations ->
          renderFocusableSection FocusAnnotations state $
            HH.div [] (renderAnnotationHeader state)
        Just FocusDiagrams ->
          renderFocusableSection FocusDiagrams state $
            renderDiagramSection state
        Nothing ->
          HH.div [ HP.style "display: flex; gap: 16px; min-height: 0;" ]
            [ HH.div [ HP.style "width: 33%; flex-shrink: 0; overflow-y: auto;" ]
                [ renderSourcePreview state
                , renderFocusableSection FocusAnnotations state $
                    HH.div [] (renderAnnotationHeader state)
                ]
            , HH.div [ HP.style "flex: 1; min-width: 0; overflow-y: auto;" ]
                [ renderFocusableSection FocusDiagrams state $
                    renderDiagramSection state
                ]
            ]
    -- Embedded declaration usage graph (unfolds when a declaration is focused)
    , renderEmbeddedUsageGraph state
    ]

-- | Embedded declaration usage graph — appears below diagrams when a declaration is focused
renderEmbeddedUsageGraph :: forall m. MonadAff m => State -> H.ComponentHTML Action ChildSlots m
renderEmbeddedUsageGraph state = case state.focusedDeclaration of
  Nothing -> HH.text ""
  Just declName ->
    let
      input = state.lastInput
      mDecl = Array.find (\d -> d.name == declName) input.declarations
      modId = Map.lookup input.moduleName input.moduleNameToId
      calls = fromMaybe [] (modId >>= \mid -> Map.lookup mid input.functionCalls)
    in
    HH.div
      [ HP.style "margin-top: 16px; border-top: 2px solid #c8c0a8; padding-top: 12px;"
      , HP.id "embedded-usage-graph"
      ]
      [ -- Header with close button
        HH.div
          [ HP.style "display: flex; align-items: center; justify-content: space-between; margin-bottom: 8px; padding: 0 4px;" ]
          [ HH.span
              [ HP.style "font-size: 12px; font-weight: 600; color: #555; font-family: 'Courier New', Courier, monospace;" ]
              [ HH.text $ "Dependencies: " <> declName ]
          , HH.span
              [ HP.style "font-size: 14px; color: #999; cursor: pointer; padding: 2px 6px;"
              , HE.onClick \_ -> FocusDeclaration Nothing
              ]
              [ HH.text "\x00D7" ]
          ]
      -- Usage graph
      , HH.div
          [ HP.style "height: 500px; border: 1px solid #e8e4d8; border-radius: 4px; overflow: hidden;" ]
          [ HH.slot _declarationUsageGraph unit DeclarationUsageGraph.component
              { packageName: input.packageName
              , moduleName: input.moduleName
              , declarationName: declName
              , focusTypeSignature: mDecl >>= _.typeSignature
              , declarations: input.declarations
              , moduleCalls: calls
              , allCalls: input.functionCalls
              , moduleNameToId: input.moduleNameToId
              }
              HandleUsageGraphOutput
          ]
      ]

-- | Wrapper that adds a focus/unfocus click target and help toggle to a section
renderFocusableSection :: forall m. FocusedSection -> State -> H.ComponentHTML Action ChildSlots m -> H.ComponentHTML Action ChildSlots m
renderFocusableSection section state content =
  let
    isFocused = state.focusedSection == Just section
    isHelpOpen = state.helpSection == Just section
    sectionLabel = case section of
      FocusAnnotations -> "Report"
      FocusDiagrams -> "Diagrams"
    headerStyle = "display: flex; align-items: center; justify-content: space-between; padding: 2px 0; margin-bottom: 4px;"
  in
    HH.div [ HP.style "margin-bottom: 12px;" ]
      [ HH.div
          [ HP.style headerStyle ]
          [ HH.span [ HP.style "font-size: 9px; color: #999; font-weight: 600; letter-spacing: 0.5px; text-transform: uppercase;" ]
              [ HH.text sectionLabel ]
          , HH.div [ HP.style "display: flex; align-items: center; gap: 6px;" ]
              [ HH.span
                  [ HP.style $ "cursor: pointer; display: flex; align-items: center; justify-content: center; width: 22px; height: 22px; border-radius: 50%; font-size: 13px; font-weight: 700; "
                      <> (if isHelpOpen then "background: #333; color: #fff;" else "background: #e8e5dd; color: #555;")
                  , HE.onClick \_ -> ToggleHelp section
                  , HP.title "What is this?"
                  ]
                  [ HH.text "?" ]
              , HH.span
                  [ HP.style "cursor: pointer; display: flex;"
                  , HE.onClick \_ -> FocusSection (Just section)
                  ]
                  [ if isFocused then collapseIcon else expandIcon ]
              ]
          ]
      , if isHelpOpen then renderSectionHelp section else HH.text ""
      , content
      ]

-- | Help content for each section
renderSectionHelp :: forall m. FocusedSection -> H.ComponentHTML Action ChildSlots m
renderSectionHelp = case _ of
  FocusAnnotations -> helpPanel
    [ "AI-generated report cards summarizing this module's architecture, quality issues, and role in the codebase."
    , "Each card can be Confirmed (agree), Disputed (disagree), or Replied to with additional context. This builds a conversation between you and the AI about the code's intent and health."
    , "Use this to capture decisions, flag technical debt, or record why something is the way it is."
    ]
  FocusDiagrams -> helpPanel
    [ "Three views of this module's internal structure, each revealing different refactoring opportunities:"
    , "Layers \x2014 Call hierarchy from top-level orchestrators down to leaf utilities. Red dashed lines are violations (upward calls). Hover any node to trace its dependencies."
    , "Declarations \x2014 Biconnected component decomposition. Circles in the same colored cluster are tightly coupled. Diamond nodes (\x25C7) are articulation points: the only connection between groups. Dashed lines are bridges: cut one and two groups separate. Hover to see what each function connects to."
    , "Concerns \x2014 Declarations grouped by shared sub-expressions (case branches, state fields). Each bubble is a potential standalone module. Cross-group edges show the coupling cost of extraction."
    ]
  where
  helpPanel items =
    HH.div [ HP.style "padding: 10px 12px; margin-bottom: 8px; background: #f5f2eb; border: 1px solid #d5d0c4; border-radius: 4px; font-size: 12px; color: #555; line-height: 1.6;" ]
      (items <#> \text -> HH.p [ HP.style "margin: 0 0 6px 0;" ] [ HH.text text ])

-- | Expand icon: four outward arrows (fullscreen)
expandIcon :: forall w i. HH.HTML w i
expandIcon =
  svgElem "svg"
    [ sa "viewBox" "0 0 16 16", HP.style "width: 14px; height: 14px;" ]
    [ -- Top-left arrow
      svgElem "path" [ sa "d" "M1 6V1h5M1 1l4.5 4.5", sa "stroke" "#222", sa "stroke-width" "1.8", sa "fill" "none", sa "stroke-linecap" "round" ] []
    -- Top-right arrow
    , svgElem "path" [ sa "d" "M15 6V1h-5M15 1l-4.5 4.5", sa "stroke" "#222", sa "stroke-width" "1.8", sa "fill" "none", sa "stroke-linecap" "round" ] []
    -- Bottom-left arrow
    , svgElem "path" [ sa "d" "M1 10v5h5M1 15l4.5-4.5", sa "stroke" "#222", sa "stroke-width" "1.8", sa "fill" "none", sa "stroke-linecap" "round" ] []
    -- Bottom-right arrow
    , svgElem "path" [ sa "d" "M15 10v5h-5M15 15l-4.5-4.5", sa "stroke" "#222", sa "stroke-width" "1.8", sa "fill" "none", sa "stroke-linecap" "round" ] []
    ]

-- | Collapse icon: four inward arrows (exit fullscreen)
collapseIcon :: forall w i. HH.HTML w i
collapseIcon =
  svgElem "svg"
    [ sa "viewBox" "0 0 16 16", HP.style "width: 14px; height: 14px;" ]
    [ svgElem "path" [ sa "d" "M5.5 1v4.5H1M5.5 5.5L1 1", sa "stroke" "#222", sa "stroke-width" "1.8", sa "fill" "none", sa "stroke-linecap" "round" ] []
    , svgElem "path" [ sa "d" "M10.5 1v4.5H15M10.5 5.5L15 1", sa "stroke" "#222", sa "stroke-width" "1.8", sa "fill" "none", sa "stroke-linecap" "round" ] []
    , svgElem "path" [ sa "d" "M5.5 15v-4.5H1M5.5 10.5L1 15", sa "stroke" "#222", sa "stroke-width" "1.8", sa "fill" "none", sa "stroke-linecap" "round" ] []
    , svgElem "path" [ sa "d" "M10.5 15v-4.5H15M10.5 10.5L15 15", sa "stroke" "#222", sa "stroke-width" "1.8", sa "fill" "none", sa "stroke-linecap" "round" ] []
    ]

-- =============================================================================
-- Diagram section: tabs + selected view
-- =============================================================================

renderDiagramSection :: forall m. State -> H.ComponentHTML Action ChildSlots m
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
    HH.div [ HP.style "margin: 8px 0 12px 0;" ]
      [ -- Tab bar
        HH.div [ HP.style "display: flex; align-items: baseline; gap: 0; margin-bottom: 0; border-bottom: 1px solid #ddd;" ]
          ( (if hasCalls then
              [ renderDiagramTab "Layers" LayerView state.diagramMode
              , renderDiagramTab "Declarations" DeclStructureView state.diagramMode
              , renderDiagramTab "Concerns" ConcernClusterView state.diagramMode
              ]
            else [])
          )
      -- Subtitle explaining the active diagram
      , HH.div [ HP.style "padding: 6px 0 4px; font-size: 10px; color: #888; line-height: 1.4;" ]
          [ HH.text $ case state.diagramMode of
              LayerView -> "Call hierarchy \x2014 top declarations call those below. Hover to trace dependencies. "
                <> (if state.diagramReason /= "" then state.diagramReason else "")
              DeclStructureView -> "Biconnected components of the internal call graph. Tightly coupled clusters share a color."
              ConcernClusterView -> "Declarations grouped by shared sub-expressions. Each group is a potential concern or responsibility."
              ArcView -> ""
          ]
      , -- Active diagram
        case state.diagramMode of
          LayerView -> renderLayerDiagram state
          ArcView -> renderLayerDiagram state
          DeclStructureView ->
            HH.div []
              [ HH.div [ HP.id "decl-structure-container", HP.style "min-height: 200px; background: #f0ede6; border: 1px solid #d5d0c4; border-radius: 4px;" ] []
              , renderBridgeAnalysis state
              ]
          ConcernClusterView ->
            HH.div [ HP.id "concern-cluster-container", HP.style "min-height: 200px; background: #f0ede6; border: 1px solid #d5d0c4; border-radius: 4px;" ] []
      , renderCtaBar declCount
      ]

renderDiagramTab :: forall m. String -> DiagramMode -> DiagramMode -> H.ComponentHTML Action ChildSlots m
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

renderArcDiagram :: forall m. State -> H.ComponentHTML Action ChildSlots m
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

renderLayerDiagram :: forall m. State -> H.ComponentHTML Action ChildSlots m
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
              , HP.style "display: block; border: 1px solid #d5d0c4; border-radius: 4px; background: #f0ede6;"
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
  Array.concatMap (\l ->
    let
      y = 30.0 + Int.toNumber (layout.maxLayer - l.layer) * 60.0
      isEven = l.layer `mod` 2 == 0
    in [ svgElem "rect"
           [ sa "x" "0", sa "y" (show y)
           , sa "width" (show layout.width), sa "height" "60"
           , sa "fill" (if isEven then "#eae7df" else "#f0ede6")
           , sa "stroke" "none"
           ] []
       , svgElem "text"
           [ sa "x" "4", sa "y" (show (y + 12.0))
           , sa "font-size" "8", sa "fill" "#b8b0a0"
           , sa "font-family" "system-ui, sans-serif"
           ] [ HH.text $ "L" <> show l.layer ]
       ]
  ) layout.layers

renderLayerEdge :: forall m w. State -> LayerDiagram.LayerLayout -> LayerDiagram.LayerEdge -> HH.HTML w m
renderLayerEdge state _layout edge =
  let
    isHoverActive = case state.hoveredLayerNode of
      Nothing -> false
      Just _ -> true
    isConnected = case state.hoveredLayerNode of
      Nothing -> true
      Just hovered -> edge.fromName == hovered || edge.toName == hovered
    opacity = if isConnected then (if isHoverActive then "0.7" else "0.2") else "0.04"
    width = if isConnected && isHoverActive then "1.5" else "0.8"
    isViolation = edge.crossesLayers > 1
    color = if isViolation then "#c05a4e" else "#94a3b8"
    -- Bezier for normal downward links, straight line for violations
    midY = (edge.fromY + edge.toY) / 2.0
    pathD = "M" <> show edge.fromX <> "," <> show edge.fromY
         <> " C" <> show edge.fromX <> "," <> show midY
         <> " " <> show edge.toX <> "," <> show midY
         <> " " <> show edge.toX <> "," <> show edge.toY
  in
    if isViolation then
      svgElem "line"
        [ sa "x1" (show edge.fromX), sa "y1" (show edge.fromY)
        , sa "x2" (show edge.toX), sa "y2" (show edge.toY)
        , sa "stroke" color
        , sa "stroke-width" width
        , sa "stroke-opacity" opacity
        , sa "stroke-dasharray" "4,2"
        , HP.style "transition: stroke-opacity 150ms ease, stroke-width 150ms ease;"
        ] []
    else
      svgElem "path"
        [ sa "d" pathD
        , sa "stroke" color
        , sa "stroke-width" width
        , sa "stroke-opacity" opacity
        , sa "fill" "none"
        , HP.style "transition: stroke-opacity 150ms ease, stroke-width 150ms ease;"
        ] []

renderLayerNode :: forall m. State -> LayerDiagram.LayerLayout -> LayerDiagram.LayerNode -> H.ComponentHTML Action ChildSlots m
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
    , HE.onClick \_ -> DiagramNodeClicked node.name
    ] []

renderLayerLabel :: forall m. State -> LayerDiagram.LayerLayout -> LayerDiagram.LayerNode -> H.ComponentHTML Action ChildSlots m
renderLayerLabel state _layout node =
  let
    isConnected = case state.hoveredLayerNode of
      Nothing -> true
      Just hovered -> hovered == node.name || layerNodeConnected hovered node.name state.layerLayout
    opacity = if isConnected then "1" else "0.15"
    -- Look up source line from declarations
    declLine = Array.findMap (\d ->
      if d.name == node.name then d.sourceSpan >>= \s -> Array.head s.start
      else Nothing
    ) state.lastInput.declarations
    lineTag = case declLine of
      Just l | l > 0 -> ":" <> show l
      _ -> ""
    baseName = if SCU.length node.name > 18 then SCU.take 17 node.name <> "\x2026" else node.name
    label = baseName <> lineTag
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

renderArcEdge :: forall m. State -> ArcDiagram.ArcLayout -> ArcDiagram.ArcEdge -> H.ComponentHTML Action ChildSlots m
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

renderArcNode :: forall m. State -> ArcDiagram.ArcLayout -> ArcDiagram.ArcNode -> H.ComponentHTML Action ChildSlots m
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

renderArcLabel :: forall m. State -> ArcDiagram.ArcLayout -> ArcDiagram.ArcNode -> H.ComponentHTML Action ChildSlots m
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
renderCtaBar :: forall m. Int -> H.ComponentHTML Action ChildSlots m
renderCtaBar declCount =
  HH.div
    [ HP.style "display: flex; justify-content: space-between; align-items: center; margin: 4px 0 0 0;" ]
    [ HH.span
        [ HP.style "font-family: 'Fira Code', monospace; font-size: 10px; color: #999;" ]
        [ HH.text (show declCount <> " declarations") ]
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
-- Source Preview Panel (below active diagram, above CTA bar)
-- =============================================================================

-- | Find the start line (1-indexed) for a declaration by name.
-- | First tries sourceSpan from declaration data, then falls back to searching source text.
findDeclStartLine :: String -> Array Loader.V2Declaration -> Maybe String -> Maybe Int
findDeclStartLine declName decls mSource =
  -- Try sourceSpan first
  case Array.findMap (\d ->
    if d.name == declName then d.sourceSpan >>= \s -> Array.head s.start
    else Nothing
  ) decls of
    Just l -> Just l
    -- Fall back to searching source text for "declName " at start of line
    Nothing -> case mSource of
      Nothing -> Nothing
      Just source ->
        let lines = String.split (String.Pattern "\n") source
        in map (_ + 1) $ Array.findIndex (\line ->
             SCU.take (SCU.length declName) line == declName
               && case SCU.charAt (SCU.length declName) line of
                    Just c -> c == ' ' || c == '\n'
                    Nothing -> true  -- name is entire line
           ) lines

-- | Find the end line (1-indexed) for a declaration by name
findDeclEndLine :: String -> Array Loader.V2Declaration -> Maybe Int
findDeclEndLine declName decls =
  Array.findMap (\d ->
    if d.name == declName then d.sourceSpan >>= \s -> Array.head s.end
    else Nothing
  ) decls

-- | Find the start line for a case branch or case expression function from sub-declaration analysis
findBranchStartLine :: String -> Maybe SDA.SubDeclAnalysis -> Maybe Int
findBranchStartLine name mAnalysis = case mAnalysis of
  Nothing -> Nothing
  Just analysis ->
    -- Try matching a branch name first
    case Array.findMap (\ce ->
      Array.findMap (\b -> if b.name == name then Just b.lineStart else Nothing) ce.branches
    ) analysis.caseExpressions of
      Just l -> Just l
      -- Try matching a case expression function name
      Nothing -> Array.findMap (\ce ->
        if ce.functionName == name then Just ce.lineStart else Nothing
      ) analysis.caseExpressions

-- | Find the kind for a declaration by name
findDeclKind :: String -> Array Loader.V2Declaration -> String
findDeclKind declName decls =
  fromMaybe "value" $ Array.findMap (\d ->
    if d.name == declName then Just d.kind
    else Nothing
  ) decls

renderSourcePreview :: forall m. State -> H.ComponentHTML Action ChildSlots m
renderSourcePreview state = case state.sourcePreview of
  Nothing -> HH.text ""
  Just sp ->
    let
      declName = sp.declarationName
      mStartLine = findDeclStartLine declName state.lastInput.declarations state.cachedModuleSource
      mEndLine = findDeclEndLine declName state.lastInput.declarations
      focusRange = case mStartLine, mEndLine of
        Just s, Just e -> Just { startLine: s, endLine: e }
        _, _ -> Nothing
      kind = findDeclKind declName state.lastInput.declarations
      headerStyle = "display: flex; align-items: center; justify-content: space-between; padding: 6px 10px; background: #e8e0cf; border-bottom: 1px solid #c5b99b; border-radius: 4px 4px 0 0;"
      bodyStyle = "max-height: 400px; overflow-y: auto; padding: 0; background: #faf8f3; border: 1px solid #d5d0c4; border-top: none; border-radius: 0 0 4px 4px; font-size: 12px;"
      linkStyle = "font-size: 10px; color: #2563eb; cursor: pointer; text-decoration: none; margin-left: 10px;"
      closeStyle = "font-size: 14px; color: #888; cursor: pointer; margin-left: 8px; line-height: 1;"
      lineTag = case mStartLine of
        Just l -> ":" <> show l
        Nothing -> ""
    in
      HH.div [ HP.style "margin: 8px 0;" ]
        [ HH.div [ HP.style headerStyle ]
            [ HH.div [ HP.style "display: flex; align-items: baseline; gap: 4px;" ]
                [ HH.span [ HP.style "font-weight: 600; font-size: 12px; color: #333; font-family: 'Fira Code', 'Courier New', monospace;" ]
                    [ HH.text (declName <> lineTag) ]
                , HH.span [ HP.style linkStyle, HE.onClick \_ -> OpenPreviewInEditor ] [ HH.text "Open in editor" ]
                , HH.span [ HP.style linkStyle, HE.onClick \_ -> PreviewFullDetail ] [ HH.text "Full detail" ]
                ]
            , HH.span [ HP.style closeStyle, HE.onClick \_ -> ClosePreview ] [ HH.text "\x00D7" ]
            ]
        , HH.div [ HP.id "source-preview-body", HP.style bodyStyle ]
            ( case state.cachedModuleSource of
                Just source ->
                  SourceCode.renderSource source [] focusRange kind
                    (\_ _ _ -> ClosePreview)  -- ident click is a no-op in preview context
                Nothing ->
                  [ HH.div [ HP.style "padding: 16px; color: #999; font-size: 12px; text-align: center;" ]
                      [ HH.text "Source not available" ]
                  ]
            )
        ]

-- =============================================================================
-- Bridge Analysis Panel (below Declarations diagram)
-- =============================================================================

renderBridgeAnalysis :: forall m. State -> H.ComponentHTML Action ChildSlots m
renderBridgeAnalysis state =
  case state.declDecomp, state.declGraph of
    Just info, Just graph ->
      let
        allCalls = foldMap identity state.lastInput.functionCalls
        bridgeList = Set.toUnfoldable info.bridgeSet :: Array (Tuple String String)
        apCount = Set.size info.aps
        bridgeCount = Array.length bridgeList
      in
        if bridgeCount == 0 && apCount == 0 then HH.text ""
        else
          HH.div [ HP.style "margin-top: 8px;" ]
            ( (if bridgeCount > 0
              then [ HH.div [ HP.style "font-size: 11px; font-weight: 600; color: #8b6914; margin-bottom: 6px;" ]
                       [ HH.text $ show bridgeCount <> " bridge" <> (if bridgeCount > 1 then "s" else "") <> " \x2014 cut points where this module could split" ]
                   ]
                <> (Array.take 8 bridgeList <#> \(Tuple from to) ->
                  renderBridgeCard from to info graph allCalls
                )
              else [])
            <> (if apCount > 0
              then
                let apList = Array.sort (Set.toUnfoldable info.aps :: Array String)
                    -- Only show APs that are in non-trivial blocks (genuinely connecting groups)
                    tangledAPs = Array.filter (\name ->
                      case Map.lookup name info.nodeBlock of
                        Just blockIdx ->
                          case Array.find (\b -> b.index == blockIdx) info.blocks of
                            Just block -> not block.isBridge && Set.size block.nodes > 2
                            Nothing -> false
                        Nothing -> false
                    ) apList
                in if Array.length tangledAPs == 0 then []
                else [ HH.div [ HP.style "font-size: 11px; font-weight: 600; color: #c62828; margin: 8px 0 4px;" ]
                         [ HH.text $ show (Array.length tangledAPs) <> " tangled hub" <> (if Array.length tangledAPs > 1 then "s" else "") <> " \x2014 embedded in cycles, harder to extract" ]
                     , HH.div [ HP.style "font-size: 11px; color: #555; line-height: 1.6; padding: 6px 8px; background: #f5f2eb; border-radius: 3px;" ]
                         (tangledAPs <#> \name ->
                           HH.div [ HP.style "padding: 1px 0;" ]
                             [ HH.span [ HP.style "font-weight: 500; color: #c62828;" ] [ HH.text name ]
                             , HH.span [ HP.style "color: #888;" ] [ HH.text $ " \x2014 removing this breaks cycles in its cluster" ]
                             ]
                         )
                     ]
              else [])
            )
    _, _ -> HH.text ""

renderBridgeCard :: forall m. String -> String -> Dec.DecompInfo -> Dec.SimpleGraph String -> Array Loader.V2FunctionCall -> H.ComponentHTML Action ChildSlots m
renderBridgeCard from to info graph _allCalls =
  let
    sideA = reachableWithout from to graph
    sideB = reachableWithout to from graph
    sideACount = Set.size sideA
    sideBCount = Set.size sideB

    fromIsAP = Set.member from info.aps
    toIsAP = Set.member to info.aps
  in
    HH.div [ HP.style "display: flex; align-items: baseline; gap: 6px; padding: 4px 10px; margin-bottom: 2px; background: #f5f2eb; border-radius: 4px; border-left: 3px solid #d4a017; font-size: 11px; line-height: 1.5;" ]
      [ HH.span [ HP.style "font-weight: 600; color: #2563eb; cursor: pointer;", HE.onClick \_ -> DiagramNodeClicked from ] [ HH.text from ]
      , HH.span [ HP.style "color: #999;" ] [ HH.text "\x2194" ]
      , HH.span [ HP.style "font-weight: 600; color: #2563eb; cursor: pointer;", HE.onClick \_ -> DiagramNodeClicked to ] [ HH.text to ]
      , HH.span [ HP.style "color: #888;" ]
          [ HH.text $ show sideACount <> " | " <> show sideBCount
              <> (if fromIsAP || toIsAP then " \x00B7 " <> (if fromIsAP then from else to) <> " is a hub" else "")
          ]
      ]

-- | Find all nodes reachable from `start` without crossing the edge to `excluded`
reachableWithout :: String -> String -> Dec.SimpleGraph String -> Set String
reachableWithout start excluded graph =
  go (Set.singleton start) [start]
  where
  go visited queue =
    case Array.uncons queue of
      Nothing -> visited
      Just { head, tail } ->
        let
          neighbors = fromMaybe Set.empty (Map.lookup head graph.edges)
          -- Exclude the specific bridge edge
          filtered = Set.delete excluded neighbors
          newNodes = Set.difference filtered visited
          newList = Set.toUnfoldable newNodes :: Array String
        in go (Set.union visited newNodes) (tail <> newList)

-- | Filter out compiler-generated declaration names (discard, bind, etc.)
-- | These are PureScript compiler artifacts from do-notation desugaring
-- | that add noise to structural analysis.
isCompilerGenerated :: String -> Boolean
isCompilerGenerated name =
  SCU.take 7 name == "discard"
  || SCU.take 4 name == "bind"
  || SCU.take 2 name == "$$"

-- =============================================================================
-- Commit Sparkline
-- =============================================================================

renderSparklineRow :: forall m. State -> H.ComponentHTML Action ChildSlots m
renderSparklineRow state
  | Array.null state.sparklineBars && Array.null state.lastInput.declarations = HH.div [] []  -- placeholder for stable child structure
renderSparklineRow state =
  let nBars = Array.length state.sparklineBars
      hasSparkline = nBars > 0
      hasBubblepack = not (Array.null state.lastInput.declarations)
  in HH.div
    [ HP.class_ (HH.ClassName "page-subnav")
    , HP.style "margin: -12px -16px 12px -16px; height: 52px;"
    ]
    ( (if hasBubblepack then [ renderModuleBubblepack state ] else [])
    <> (if hasSparkline then [ renderSparklineSvg state ] else [])
    <> [ HH.span
            [ HP.style "font-size: 9px; color: #665; font-weight: 500; white-space: nowrap;" ]
            [ HH.text (if hasSparkline then show nBars <> " commits" else "") ]
       ]
    )

-- | Inline SVG sparkline — pure Halogen, no Canvas FFI
renderSparklineSvg :: forall m. State -> H.ComponentHTML Action ChildSlots m
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
           , HP.style "width: 100%; height: 44px; display: block; border-radius: 3px; border: 1px solid #b8ad90; background: #e8e0cf;"
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

renderModuleBubblepack :: forall m. State -> H.ComponentHTML Action ChildSlots m
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
           , HP.style "width: 44px; height: 44px; flex-shrink: 0; overflow: visible; display: block;"
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

renderAnnotationHeader :: forall m. State -> Array (H.ComponentHTML Action ChildSlots m)
renderAnnotationHeader state
  | Array.null state.annotations =
      [ renderAnnotationGuide { compact: true } ]
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
renderAnnotationContent :: forall m. Int -> Loader.V2Annotation -> Array MSM.MeasuredCell -> H.ComponentHTML Action ChildSlots m
renderAnnotationContent annIdx ann cells =
  HH.ul
    [ HP.style "margin: 0; padding: 0 0 0 16px; list-style: disc; color: #444; font-size: 12px; line-height: 1.5;" ]
    (splitSentences ann.value <#> \sentence ->
      HH.li
        [ HP.style "margin-bottom: 3px;" ]
        (annotateText annIdx sentence cells)
    )

-- | Reply input textarea + buttons
renderReplyInput :: forall m. State -> H.ComponentHTML Action ChildSlots m
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
renderAnnotationFooter :: forall m. State -> Loader.V2Annotation -> H.ComponentHTML Action ChildSlots m
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
annotateText :: forall m. Int -> String -> Array MSM.MeasuredCell -> Array (H.ComponentHTML Action ChildSlots m)
annotateText annIdx text cells =
  let
    matches = findTextMatches text cells
    go :: Int -> Int -> Array TextMatch -> Array (H.ComponentHTML Action ChildSlots m)
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
renderInlineRef :: forall m. Int -> Int -> MSM.MeasuredCell -> H.ComponentHTML Action ChildSlots m
renderInlineRef _annIdx _refIdx cell =
  HH.span
    [ HP.style $ "padding: 1px 4px; border-radius: 3px;"
        <> " background: " <> MSM.kindBackground cell.kind <> ";"
        <> " border: 1px solid " <> MSM.kindBorder cell.kind <> ";"
        <> " cursor: pointer; font-family: 'Fira Code','SF Mono', monospace; font-size: 10px;"
    , HE.onClick \_ -> DiagramNodeClicked cell.name
    ]
    [ HH.text cell.name ]

-- =============================================================================
-- Action Handlers
-- =============================================================================

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action ChildSlots Output m Unit
handleAction = case _ of
  Initialize -> do
    state <- H.get
    let input = state.lastInput
    log $ "[ModuleStructureViz] Initializing: " <> input.moduleName
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
      H.modify_ _ { measuredCells = [], arcLayout = Nothing, layerLayout = Nothing
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
    log "[ModuleStructureViz] Finalizing"

  HandleDeclarationClick pkgName modName declName -> do
    log $ "[ModuleStructureViz] Declaration clicked: " <> declName
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

  OpenInEditor -> do
    state <- H.get
    log $ "[ModuleStructureViz] Opening in VS Code: " <> state.lastInput.moduleName
    result <- liftAff $ Loader.fetchSourceLocation state.lastInput.moduleName
    case result of
      Right loc -> do
        log $ "[ModuleStructureViz] Resolved path: " <> loc.filePath
        liftEffect $ openUri ("vscode://file/" <> loc.filePath)
      Left err ->
        log $ "[ModuleStructureViz] Could not resolve path: " <> err

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

  FocusSection mSection -> do
    state <- H.get
    let newFocus = case mSection of
          Just s | state.focusedSection == Just s -> Nothing
          _ -> mSection
    H.modify_ _ { focusedSection = newFocus }

  ToggleHelp section -> do
    state <- H.get
    let newHelp = if state.helpSection == Just section then Nothing else Just section
    H.modify_ _ { helpSection = newHelp }

  DiagramNodeClicked declName -> do
    log $ "[DiagramClick] " <> declName <> " → focusing dependency graph"
    handleAction (FocusDeclaration (Just declName))

  ClosePreview ->
    H.modify_ _ { sourcePreview = Nothing }

  OpenPreviewInEditor -> do
    log "[SourcePreview] OpenPreviewInEditor fired"
    state <- H.get
    case state.sourcePreview of
      Nothing -> log "[SourcePreview] No preview open, nothing to do"
      Just sp -> do
        log $ "[SourcePreview] Opening " <> sp.declarationName <> " in editor"
        result <- liftAff $ Loader.fetchSourceLocation state.lastInput.moduleName
        case result of
          Right loc -> do
            let startLine = case findDeclStartLine sp.declarationName state.lastInput.declarations state.cachedModuleSource of
                  Just l -> Just l
                  Nothing -> findBranchStartLine sp.declarationName state.subDeclAnalysis
            let lineArg = case startLine of
                  Just l -> ":" <> show l
                  Nothing -> ""
            let uri = "vscode://file/" <> loc.filePath <> lineArg
            log $ "[SourcePreview] Opening URI: " <> uri
            liftEffect $ openUri uri
          Left err ->
            log $ "[SourcePreview] Could not resolve path: " <> err

  PreviewFullDetail -> do
    state <- H.get
    case state.sourcePreview of
      Nothing -> pure unit
      Just sp -> do
        H.modify_ _ { sourcePreview = Nothing }
        H.raise (DeclarationClicked state.lastInput.packageName state.lastInput.moduleName sp.declarationName)

  FocusDeclaration mDecl -> do
    H.modify_ _ { focusedDeclaration = mDecl }
    case mDecl of
      Just _ -> liftEffect $ DOMHelpers.scrollElementIntoView "embedded-usage-graph"
      Nothing -> pure unit

  HandleUsageGraphOutput output -> case output of
    DeclarationUsageGraph.NodeClicked _pkgName modName declName -> do
      state <- H.get
      if modName == state.lastInput.moduleName
        then handleAction (FocusDeclaration (Just declName))
        else H.raise (DeclarationClicked state.lastInput.packageName modName declName)
    DeclarationUsageGraph.ModuleClicked modName -> do
      state <- H.get
      H.raise (DeclarationClicked state.lastInput.packageName modName "")
    DeclarationUsageGraph.OpenFocusInEditor -> do
      state <- H.get
      case state.focusedDeclaration of
        Nothing -> pure unit
        Just declName -> do
          result <- liftAff $ Loader.fetchSourceLocation state.lastInput.moduleName
          case result of
            Right loc -> do
              let startLine = findDeclStartLine declName state.lastInput.declarations state.cachedModuleSource
              let uri = "vscode://file/" <> loc.filePath <> case startLine of
                    Just l -> ":" <> show l
                    Nothing -> ""
              liftEffect $ openUri uri
            Left _ -> pure unit
    DeclarationUsageGraph.ViewModuleSignatures modName -> do
      state <- H.get
      H.raise (DeclarationClicked state.lastInput.packageName modName "")
    DeclarationUsageGraph.ViewPackage pkgName -> do
      H.raise (DeclarationClicked pkgName "" "")

-- | Fetch numstat data for sparkline (pure SVG render happens via Halogen re-render)
loadSparkline :: forall m. MonadAff m => String -> String -> H.HalogenM State Action ChildSlots Output m Unit
loadSparkline pkgName modName = do
  result <- liftAff $ Loader.fetchModuleNumstat 500 pkgName
  case result of
    Left err ->
      log $ "[Sparkline] Error fetching numstat: " <> err
    Right commits -> do
      let bars = Spark.prepareData modName commits
      log $ "[Sparkline] " <> modName <> ": " <> show (Array.length bars) <> " commits"
      H.modify_ _ { sparklineBars = bars }

-- | Prepare cells, compute arc layout, then update state.
renderSignatureMap :: forall m. MonadAff m => Input -> H.HalogenM State Action ChildSlots Output m Unit
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
      declNames = Set.filter (not <<< isCompilerGenerated) (Set.union exportedNames callNames)
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
  { mAnalysis, mSubDeclGraph, mSource } <- do
    result <- liftAff $ Loader.fetchModuleSource input.moduleName
    case result of
      Left _ -> pure { mAnalysis: Nothing, mSubDeclGraph: Nothing, mSource: Nothing }
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
        pure { mAnalysis: Just analysis, mSubDeclGraph: Just subGraph, mSource: Just src.source }
  -- Heuristic: choose default tab based on what's most informative
  let hasConcerns = case mAnalysis of
        Just a -> Array.length a.caseExpressions > 0
        Nothing -> false
  let { mode: defaultMode, reason } = chooseDiagramMode mLayerLayout declDecomp hasConcerns
  H.modify_ _ { measuredCells = measured
               , arcLayout = mArcLayout, layerLayout = mLayerLayout
               , declGraph = Just declGraph, declDecomp = Just declDecomp
               , subDeclAnalysis = mAnalysis, subDeclGraph = mSubDeclGraph
               , diagramMode = defaultMode, diagramReason = reason
               , cachedModuleSource = mSource }

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
renderDeclStructure :: forall m. MonadAff m => H.HalogenM State Action ChildSlots Output m Unit
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
          let mClickCb = state.actionListener <#> \listener name ->
                HS.notify listener (DiagramNodeClicked name)
          liftEffect do
            clearContainer "#decl-structure-container"
            _ <- rerender "#decl-structure-container" (StructViz.callGraphTree graph info kindMap mClickCb)
            pure unit
    _, _ -> liftEffect do
      DOMHelpers.setInnerHTML "#decl-structure-container"
        "<div style=\"padding: 24px; color: #999; font-size: 12px; text-align: center;\">No declaration data available</div>"

-- | Render the concern-clustered graph into a HATS container.
-- | Fetches module source if not yet analyzed.
renderConcernClusters :: forall m. MonadAff m => H.HalogenM State Action ChildSlots Output m Unit
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

renderConcernGraph :: forall m. MonadAff m => SDA.SubDeclAnalysis -> H.HalogenM State Action ChildSlots Output m Unit
renderConcernGraph analysis = do
  state <- H.get
  if Array.null analysis.caseExpressions then
    liftEffect do
      clearContainer "#concern-cluster-container"
      DOMHelpers.setInnerHTML "#concern-cluster-container"
        "<div style=\"padding: 24px; color: #999; font-size: 12px; text-align: center;\">No case expressions found — concern clustering requires pattern-matching branches (e.g. handleAction)</div>"
  else case state.subDeclGraph of
    Just graph -> do
      let mClickCb = state.actionListener <#> \listener name ->
            HS.notify listener (DiagramNodeClicked name)
      liftEffect do
        clearContainer "#concern-cluster-container"
        _ <- rerender "#concern-cluster-container"
               (StructViz.concernClusteredTree graph analysis.caseExpressions mClickCb)
        pure unit
    Nothing -> pure unit

-- =============================================================================
-- Declaration click callback
-- =============================================================================

-- | Create a declaration click callback that notifies the Halogen listener
makeDeclarationClickCallback :: Maybe (HS.Listener Action) -> String -> String -> String -> Effect Unit
makeDeclarationClickCallback mListener pkgName modName declName = case mListener of
  Just listener -> HS.notify listener (HandleDeclarationClick pkgName modName declName)
  Nothing -> log $ "[ModuleStructureViz] No listener for decl click: " <> pkgName <> "/" <> modName <> "/" <> declName

