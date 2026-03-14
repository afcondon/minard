-- | Compare Module Visualization
-- |
-- | Side-by-side 2×3 grid showing 3 diagram types (Layers,
-- | Declarations, Concerns) for two modules simultaneously. Designed for
-- | before/after refactoring comparison or comparing a module with its
-- | extracted counterpart.
module CE2.Component.CompareModuleViz
  ( component
  , Input
  , Output(..)
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
import Data.Set as Set
import Data.String as String
import Data.String.CodeUnits as SCU
import Data.Const (Const)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class.Console (log)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import CE2.Util.SVG (svgElem, sa)
import Halogen.HTML.Properties as HP

import CE2.Data.Decomposition as Dec
import CE2.Data.Loader as Loader
import CE2.Data.SubDeclarationAnalysis as SDA
import CE2.Component.ModuleAnatomyViz as StructViz
import CE2.Viz.DeclarationArcDiagram as ArcDiagram
import CE2.Viz.DeclarationLayerDiagram as LayerDiagram
import CE2.Viz.DOMHelpers as DOMHelpers

import Hylograph.HATS.InterpreterTick (clearContainer, rerender)

-- =============================================================================
-- Types
-- =============================================================================

type Input =
  { leftPackage :: String
  , leftModule :: String
  , rightPackage :: String
  , rightModule :: String
  , declarations :: Map Int (Array Loader.V2Declaration)
  , functionCalls :: Map Int (Array Loader.V2FunctionCall)
  , allModules :: Array Loader.V2ModuleListItem
  , beforeSnapshotId :: Maybe Int  -- When set, left column data is fetched from this snapshot
  }

data Output = GoBack

type Slot = H.Slot (Const Void) Output

-- | Per-column data (computed for each module)
type ColumnData =
  { moduleName :: String
  , packageName :: String
  , decls :: Array Loader.V2Declaration
  , calls :: Map Int (Array Loader.V2FunctionCall)
  , layerLayout :: Maybe LayerDiagram.LayerLayout
  , arcLayout :: Maybe ArcDiagram.ArcLayout
  , declGraph :: Maybe (Dec.SimpleGraph String)
  , declDecomp :: Maybe Dec.DecompInfo
  , subDeclAnalysis :: Maybe SDA.SubDeclAnalysis
  , subDeclGraph :: Maybe (Dec.SimpleGraph String)
  }

type State =
  { input :: Input
  , left :: Maybe ColumnData
  , right :: Maybe ColumnData
  }

data Action
  = Initialize
  | Receive Input

-- =============================================================================
-- Component
-- =============================================================================

component :: forall q m. MonadAff m => H.Component q Input Output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        , receive = Just <<< Receive
        }
    }

initialState :: Input -> State
initialState input =
  { input
  , left: Nothing
  , right: Nothing
  }

-- =============================================================================
-- Rendering
-- =============================================================================

render :: forall m. MonadAff m => State -> H.ComponentHTML Action () m
render state =
  let beforeLabel = case state.input.beforeSnapshotId of
        Just _ -> "BEFORE (snapshot)"
        Nothing -> "BEFORE"
      afterLabel = case state.input.beforeSnapshotId of
        Just _ -> "AFTER (current)"
        Nothing -> "AFTER"
  in HH.div
    [ HP.style "display: grid; grid-template-columns: 1fr 1fr; gap: 8px; padding: 8px; height: calc(100vh - 50px); overflow-y: auto;" ]
    [ -- Column headers
      columnHeader beforeLabel state.input.leftModule
    , columnHeader afterLabel state.input.rightModule
    -- Row 1: Layers
    , sectionLabel "Layers"
    , renderLayerPanel state.left
    , renderLayerPanel state.right
    -- Row 2: Declarations
    , sectionLabel "Declarations"
    , HH.div [ HP.id "compare-left-decl", HP.style panelStyle ] []
    , HH.div [ HP.id "compare-right-decl", HP.style panelStyle ] []
    -- Row 3: Concerns
    , sectionLabel "Concerns"
    , HH.div [ HP.id "compare-left-concern", HP.style panelStyle ] []
    , HH.div [ HP.id "compare-right-concern", HP.style panelStyle ] []
    ]

columnHeader :: forall w i. String -> String -> HH.HTML w i
columnHeader label moduleName =
  HH.div
    [ HP.style "padding: 8px 12px; background: #f5f5f5; border: 1px solid #e0e0e0; border-radius: 4px; font-size: 11px;" ]
    [ HH.span [ HP.style "font-weight: 700; color: #666; margin-right: 8px;" ] [ HH.text label ]
    , HH.span [ HP.style "font-weight: 600; color: #333;" ] [ HH.text (shortMod moduleName) ]
    ]

sectionLabel :: forall w i. String -> HH.HTML w i
sectionLabel label =
  HH.div
    [ HP.style "grid-column: 1 / -1; padding: 4px 12px; font-size: 10px; font-weight: 700; color: #888; text-transform: uppercase; letter-spacing: 0.5px; border-bottom: 1px solid #eee; margin-top: 4px;" ]
    [ HH.text label ]

panelStyle :: String
panelStyle = "border: 1px solid #e5e5e5; border-radius: 4px; background: #fafafa; min-height: 180px; overflow: hidden;"

-- =============================================================================
-- Layer Panel (Halogen SVG)
-- =============================================================================

renderLayerPanel :: forall w i. Maybe ColumnData -> HH.HTML w i
renderLayerPanel Nothing = loadingPanel
renderLayerPanel (Just col) = case col.layerLayout of
  Nothing -> emptyPanel "No internal call hierarchy"
  Just layout
    | Array.null layout.nodes -> emptyPanel "No internal call hierarchy"
    | otherwise ->
        HH.div [ HP.style panelStyle ]
          [ svgElem "svg"
              [ sa "viewBox" ("0 0 " <> show layout.width <> " " <> show layout.height)
              , sa "width" "100%"
              , sa "preserveAspectRatio" "xMidYMid meet"
              , HP.style "display: block;"
              ]
              ( renderLayerBands layout
              <> (layout.edges <#> renderLayerEdge)
              <> (layout.nodes <#> renderLayerNode col)
              <> (layout.nodes <#> renderLayerLabel col)
              )
          ]

renderLayerBands :: forall w i. LayerDiagram.LayerLayout -> Array (HH.HTML w i)
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

renderLayerEdge :: forall w i. LayerDiagram.LayerEdge -> HH.HTML w i
renderLayerEdge edge =
  let color = if edge.crossesLayers > 1 then "#c05a4e" else "#94a3b8"
  in svgElem "line"
    [ sa "x1" (show edge.fromX), sa "y1" (show edge.fromY)
    , sa "x2" (show edge.toX), sa "y2" (show edge.toY)
    , sa "stroke" color
    , sa "stroke-width" "0.8"
    , sa "stroke-opacity" "0.3"
    ] []

renderLayerNode :: forall w i. ColumnData -> LayerDiagram.LayerNode -> HH.HTML w i
renderLayerNode col node =
  let fillColor = case concernGroupForDecl node.name col.subDeclAnalysis of
        Just gi -> StructViz.blockColor gi
        Nothing -> layerKindColor node.kind node.effectful
  in svgElem "circle"
    [ sa "cx" (show node.x), sa "cy" (show node.y)
    , sa "r" (show node.r)
    , sa "fill" fillColor
    , sa "stroke" "#fff", sa "stroke-width" "0.8"
    ] []

renderLayerLabel :: forall w i. ColumnData -> LayerDiagram.LayerNode -> HH.HTML w i
renderLayerLabel col node =
  let
    label = if SCU.length node.name > 18 then SCU.take 17 node.name <> "\x2026" else node.name
    labelY = node.y + node.r + 12.0
    labelColor = case concernGroupForDecl node.name col.subDeclAnalysis of
      Just gi -> StructViz.blockColor gi
      Nothing -> if node.effectful then "#d97706" else "#2563eb"
  in svgElem "text"
    [ sa "x" (show node.x), sa "y" (show labelY)
    , sa "text-anchor" "start"
    , sa "font-size" "8px"
    , sa "font-family" "system-ui, sans-serif"
    , sa "fill" labelColor
    , sa "pointer-events" "none"
    , sa "transform" ("rotate(-45," <> show node.x <> "," <> show labelY <> ")")
    ]
    [ HH.text label ]

-- =============================================================================
-- Arc Panel (Halogen SVG)
-- =============================================================================

renderArcPanel :: forall w i. Maybe ColumnData -> HH.HTML w i
renderArcPanel Nothing = loadingPanel
renderArcPanel (Just col) = case col.arcLayout of
  Nothing -> emptyPanel "No intra-module function calls"
  Just layout
    | Array.null layout.edges -> emptyPanel "No intra-module function calls"
    | otherwise ->
        HH.div [ HP.style panelStyle ]
          [ svgElem "svg"
              [ sa "viewBox" ("0 0 " <> show layout.width <> " " <> show layout.height)
              , sa "width" "100%"
              , sa "preserveAspectRatio" "xMidYMid meet"
              , HP.style "display: block;"
              ]
              ( (layout.edges <#> renderArcEdge)
              <> (layout.nodes <#> renderArcNode layout)
              <> (layout.nodes <#> renderArcLabel layout)
              )
          ]

renderArcEdge :: forall w i. ArcDiagram.ArcEdge -> HH.HTML w i
renderArcEdge edge =
  let strokeW = Num.min 3.0 (0.75 + Int.toNumber edge.count * 0.5)
  in svgElem "path"
    [ sa "d" edge.pathD
    , sa "fill" "none"
    , sa "stroke" edge.color
    , sa "stroke-width" (show strokeW)
    , sa "opacity" "0.7"
    ] []

renderArcNode :: forall w i. ArcDiagram.ArcLayout -> ArcDiagram.ArcNode -> HH.HTML w i
renderArcNode layout node =
  svgElem "circle"
    [ sa "cx" (show node.x)
    , sa "cy" (show layout.baselineY)
    , sa "r" "4"
    , sa "fill" (ArcDiagram.heatColor node.heat)
    , sa "stroke" (ArcDiagram.heatColor (Num.min 1.0 (node.heat + 0.15)))
    , sa "stroke-width" "1.5"
    ] []

renderArcLabel :: forall w i. ArcDiagram.ArcLayout -> ArcDiagram.ArcNode -> HH.HTML w i
renderArcLabel layout node =
  let
    label = if SCU.length node.name > 16 then SCU.take 15 node.name <> "\x2026" else node.name
    labelY = layout.baselineY + 10.0
    labelColor = if node.effectful then "#d97706" else "#2563eb"
  in svgElem "text"
    [ sa "x" (show node.x), sa "y" (show labelY)
    , sa "text-anchor" "start"
    , sa "font-family" "'Fira Code', 'SF Mono', monospace"
    , sa "font-size" "8"
    , sa "fill" labelColor
    , sa "pointer-events" "none"
    , sa "transform" ("rotate(45 " <> show node.x <> " " <> show labelY <> ")")
    ]
    [ HH.text label ]

-- =============================================================================
-- Helpers
-- =============================================================================

loadingPanel :: forall w i. HH.HTML w i
loadingPanel = HH.div [ HP.style (panelStyle <> " display: flex; align-items: center; justify-content: center;") ]
  [ HH.text "Loading..." ]

emptyPanel :: forall w i. String -> HH.HTML w i
emptyPanel msg = HH.div [ HP.style (panelStyle <> " display: flex; align-items: center; justify-content: center; color: #999; font-size: 11px;") ]
  [ HH.text msg ]

shortMod :: String -> String
shortMod name = fromMaybe name $ Array.last $ String.split (String.Pattern ".") name

concernGroupForDecl :: String -> Maybe SDA.SubDeclAnalysis -> Maybe Int
concernGroupForDecl _name Nothing = Nothing
concernGroupForDecl name (Just analysis) =
  case Array.findIndex (\ce -> ce.functionName == name) analysis.caseExpressions of
    Just i -> Just i
    Nothing ->
      let
        groupRefs = Array.mapWithIndex (\i ce ->
          { group: i
          , refs: foldl (\acc br -> if Set.member name br.identifierRefs then acc + 1 else acc) 0 ce.branches
          }) analysis.caseExpressions
        best = foldl (\acc gr -> if gr.refs > acc.refs then gr else acc) { group: 0, refs: 0 } groupRefs
      in if best.refs > 0 then Just best.group else Nothing

layerKindColor :: String -> Boolean -> String
layerKindColor _ true = "#d97706"
layerKindColor _ false = "#2563eb"

-- =============================================================================
-- Data loading & computation
-- =============================================================================

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  Initialize -> loadBothColumns
  Receive input -> do
    old <- H.get
    when (input.leftModule /= old.input.leftModule || input.rightModule /= old.input.rightModule) do
      H.modify_ _ { input = input, left = Nothing, right = Nothing }
      loadBothColumns

loadBothColumns :: forall m. MonadAff m => H.HalogenM State Action () Output m Unit
loadBothColumns = do
  state <- H.get
  let input = state.input

  -- Left (before) column: fetch from snapshot API if beforeSnapshotId is set
  leftData <- case input.beforeSnapshotId of
    Just snapId -> do
      log $ "[CompareModuleViz] Loading BEFORE from snapshot " <> show snapId
      d <- computeColumnFromSnapshot snapId input.leftPackage input.leftModule
      log $ "[CompareModuleViz] BEFORE: " <> show (Array.length d.decls) <> " decls"
      pure d
    Nothing -> computeColumn input input.leftPackage input.leftModule
  H.modify_ _ { left = Just leftData }

  -- Right (after) column: always from current data
  rightData <- computeColumn input input.rightPackage input.rightModule
  log $ "[CompareModuleViz] AFTER: " <> show (Array.length rightData.decls) <> " decls"
  H.modify_ _ { right = Just rightData }

  renderHATSPanels

computeColumn :: forall m. MonadAff m => Input -> String -> String -> H.HalogenM State Action () Output m ColumnData
computeColumn input pkgName modName = do
  let mMod = Array.find (\m -> m.name == modName && m.package.name == pkgName) input.allModules
  let mModId = mMod <#> _.id
  let decls = case mModId of
        Just modId -> fromMaybe [] (Map.lookup modId input.declarations)
        Nothing -> []

  let layoutWidth = 440.0
  let layoutInput = { moduleName: modName, declarations: decls, functionCalls: input.functionCalls, layoutWidth }
  let layerLay = LayerDiagram.computeLayout layoutInput
  let mLayerLayout = if Array.null layerLay.nodes then Nothing else Just layerLay
  let arcLay = ArcDiagram.computeLayout layoutInput
  let mArcLayout = if Array.null arcLay.edges then Nothing else Just arcLay

  -- Declaration graph for decomposition
  let allCalls = foldMap identity input.functionCalls
  let exportedNames = Set.fromFoldable $ decls <#> _.name
  let internalCalls = Array.filter (\c ->
        not c.isCrossModule && c.calleeModule == modName && c.callerName /= c.calleeName
      ) allCalls
  let callNames = foldl (\acc c -> Set.insert c.callerName (Set.insert c.calleeName acc)) Set.empty internalCalls
  let declNames = Set.union exportedNames callNames
  let edges = foldl (\acc call ->
        if Set.member call.callerName declNames && Set.member call.calleeName declNames
        then Map.alter (Just <<< Set.insert call.calleeName <<< fromMaybe Set.empty) call.callerName
               (Map.alter (Just <<< Set.insert call.callerName <<< fromMaybe Set.empty) call.calleeName acc)
        else acc
      ) Map.empty internalCalls
  let graph = { nodes: Array.fromFoldable declNames, edges }
  let decomp = if Array.null graph.nodes then Nothing else Just (Dec.analyzeGraph graph)

  -- Concern analysis from source
  { mAnalysis, mSubDeclGraph } <- do
    result <- liftAff $ Loader.fetchModuleSource modName
    case result of
      Left _ -> pure { mAnalysis: Nothing, mSubDeclGraph: Nothing }
      Right src -> do
        let analysis = SDA.analyzeModuleSource src.source
        let { declarations: subDecls, internalCalls: subCalls } = SDA.branchesToDeclGraph analysis.allBranches
        let subNames = Set.fromFoldable $ subDecls <#> _.name
        let subEdges = foldl (\acc call ->
              if Set.member call.callerName subNames && Set.member call.calleeName subNames
              then Map.alter (Just <<< Set.insert call.calleeName <<< fromMaybe Set.empty) call.callerName
                     (Map.alter (Just <<< Set.insert call.callerName <<< fromMaybe Set.empty) call.calleeName acc)
              else acc
            ) Map.empty subCalls
        let subGraph = { nodes: Array.fromFoldable subNames :: Array String, edges: subEdges }
        pure { mAnalysis: Just analysis, mSubDeclGraph: Just subGraph }

  pure
    { moduleName: modName
    , packageName: pkgName
    , decls
    , calls: input.functionCalls
    , layerLayout: mLayerLayout
    , arcLayout: mArcLayout
    , declGraph: Just graph
    , declDecomp: decomp
    , subDeclAnalysis: mAnalysis
    , subDeclGraph: mSubDeclGraph
    }

-- | Fetch module data from a specific snapshot and compute column
computeColumnFromSnapshot :: forall m. MonadAff m => Int -> String -> String -> H.HalogenM State Action () Output m ColumnData
computeColumnFromSnapshot snapshotId pkgName modName = do
  -- Fetch modules from the before snapshot
  snapshotModulesResult <- liftAff $ Loader.fetchV2ModulesForSnapshot snapshotId
  let snapshotModules = case snapshotModulesResult of
        Right mods -> mods
        Left _ -> []

  -- Find the target module by name in the snapshot's module list
  let mMod = Array.find (\m -> m.name == modName) snapshotModules
  let mModId = mMod <#> _.id

  -- Fetch declarations for the before module
  decls <- case mModId of
    Just modId -> do
      result <- liftAff $ Loader.fetchV2ModuleDeclarations modId
      pure $ case result of
        Right ds -> ds
        Left _ -> []
    Nothing -> pure []

  -- Fetch function calls from the before snapshot
  snapshotCallsResult <- liftAff $ Loader.fetchV2AllCallsForSnapshot snapshotId
  let functionCalls = case snapshotCallsResult of
        Right allCalls -> Map.fromFoldable $ allCalls <#> \mc ->
          Tuple mc.moduleId (mc.calls <#> \c ->
            { callerName: c.callerName
            , calleeModule: c.calleeModule
            , calleeName: c.calleeName
            , isCrossModule: c.isCrossModule
            , callCount: c.callCount
            , sourceSpan: Nothing
            })
        Left _ -> Map.empty

  -- Compute layouts (same logic as computeColumn)
  let layoutWidth = 440.0
  let layoutInput = { moduleName: modName, declarations: decls, functionCalls, layoutWidth }
  let layerLay = LayerDiagram.computeLayout layoutInput
  let mLayerLayout = if Array.null layerLay.nodes then Nothing else Just layerLay
  let arcLay = ArcDiagram.computeLayout layoutInput
  let mArcLayout = if Array.null arcLay.edges then Nothing else Just arcLay

  -- Declaration graph for decomposition
  let allCalls = foldMap identity functionCalls
  let exportedNames = Set.fromFoldable $ decls <#> _.name
  let internalCalls = Array.filter (\c ->
        not c.isCrossModule && c.calleeModule == modName && c.callerName /= c.calleeName
      ) allCalls
  let callNames = foldl (\acc c -> Set.insert c.callerName (Set.insert c.calleeName acc)) Set.empty internalCalls
  let declNames = Set.union exportedNames callNames
  let edges = foldl (\acc call ->
        if Set.member call.callerName declNames && Set.member call.calleeName declNames
        then Map.alter (Just <<< Set.insert call.calleeName <<< fromMaybe Set.empty) call.callerName
               (Map.alter (Just <<< Set.insert call.callerName <<< fromMaybe Set.empty) call.calleeName acc)
        else acc
      ) Map.empty internalCalls
  let graph = { nodes: Array.fromFoldable declNames, edges }
  let decomp = if Array.null graph.nodes then Nothing else Just (Dec.analyzeGraph graph)

  -- Concern analysis from snapshot source (via worktree repo_path)
  { mAnalysis, mSubDeclGraph } <- do
    result <- liftAff $ Loader.fetchModuleSourceForSnapshot modName snapshotId
    case result of
      Left _ -> pure { mAnalysis: Nothing, mSubDeclGraph: Nothing }
      Right src -> do
        let analysis = SDA.analyzeModuleSource src.source
        let { declarations: subDecls, internalCalls: subCalls } = SDA.branchesToDeclGraph analysis.allBranches
        let subNames = Set.fromFoldable $ subDecls <#> _.name
        let subEdges = foldl (\acc call ->
              if Set.member call.callerName subNames && Set.member call.calleeName subNames
              then Map.alter (Just <<< Set.insert call.calleeName <<< fromMaybe Set.empty) call.callerName
                     (Map.alter (Just <<< Set.insert call.callerName <<< fromMaybe Set.empty) call.calleeName acc)
              else acc
            ) Map.empty subCalls
        let subGraph = { nodes: Array.fromFoldable subNames :: Array String, edges: subEdges }
        pure { mAnalysis: Just analysis, mSubDeclGraph: Just subGraph }

  pure
    { moduleName: modName
    , packageName: pkgName
    , decls
    , calls: functionCalls
    , layerLayout: mLayerLayout
    , arcLayout: mArcLayout
    , declGraph: Just graph
    , declDecomp: decomp
    , subDeclAnalysis: mAnalysis
    , subDeclGraph: mSubDeclGraph
    }

-- =============================================================================
-- HATS rendering (Declarations + Concerns)
-- =============================================================================

renderHATSPanels :: forall m. MonadAff m => H.HalogenM State Action () Output m Unit
renderHATSPanels = do
  state <- H.get
  renderDeclPanel "#compare-left-decl" state.left
  renderDeclPanel "#compare-right-decl" state.right
  renderConcernPanel "#compare-left-concern" state.left
  renderConcernPanel "#compare-right-concern" state.right

renderDeclPanel :: forall m. MonadAff m => String -> Maybe ColumnData -> H.HalogenM State Action () Output m Unit
renderDeclPanel _ Nothing = pure unit
renderDeclPanel cid (Just col) =
  case col.declGraph, col.declDecomp of
    Just graph, Just info
      | Array.null graph.nodes -> liftEffect do
          clearContainer cid
          DOMHelpers.setInnerHTML cid emptyDeclMsg
      | otherwise -> do
          let kindMap = foldl (\acc d -> Map.insert d.name d.kind acc) Map.empty col.decls
          liftEffect do
            clearContainer cid
            _ <- rerender cid (StructViz.callGraphTree graph info kindMap)
            pure unit
    _, _ -> liftEffect do
      clearContainer cid
      DOMHelpers.setInnerHTML cid emptyDeclMsg

renderConcernPanel :: forall m. MonadAff m => String -> Maybe ColumnData -> H.HalogenM State Action () Output m Unit
renderConcernPanel _ Nothing = pure unit
renderConcernPanel cid (Just col) =
  case col.subDeclAnalysis of
    Just analysis
      | Array.null analysis.caseExpressions -> liftEffect do
          clearContainer cid
          DOMHelpers.setInnerHTML cid emptyConcernMsg
      | otherwise ->
          case col.subDeclGraph of
            Just graph -> liftEffect do
              clearContainer cid
              _ <- rerender cid (StructViz.concernClusteredTree graph analysis.caseExpressions)
              pure unit
            Nothing -> liftEffect do
              clearContainer cid
              DOMHelpers.setInnerHTML cid emptyConcernMsg
    Nothing -> liftEffect do
      clearContainer cid
      DOMHelpers.setInnerHTML cid emptyConcernMsg

emptyDeclMsg :: String
emptyDeclMsg = "<div style=\"padding: 24px; color: #999; font-size: 11px; text-align: center;\">No internal call graph</div>"

emptyConcernMsg :: String
emptyConcernMsg = "<div style=\"padding: 24px; color: #999; font-size: 11px; text-align: center;\">No case expressions to analyze</div>"
