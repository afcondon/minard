-- | Structural Decomposition Visualization
-- |
-- | Shows biconnected component decomposition of the module dependency graph.
-- | Renders: metrics panel, annotated graph (nodes colored by block,
-- | articulation points as diamonds, bridges dashed), and before/after
-- | adjacency matrices.
module CE2.Component.StructuralDecompViz
  ( component
  , Input
  , Query
  , Slot
  ) where

import Prelude

import Data.Array as Array
import Data.Array (mapWithIndex, sortBy, (!!))
import Data.Foldable (foldl)
import Data.Int as Int
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number as Number
import Data.Set as Set
import Data.String as String
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import Hylograph.HATS (Tree, elem, staticStr, staticNum)
import Hylograph.HATS.InterpreterTick (clearContainer, rerender)
import Hylograph.Internal.Element.Types (ElementType(..))

import CE2.Data.Decomposition as Dec
import CE2.Data.LayerDiscovery as LD

-- =============================================================================
-- Types
-- =============================================================================

type Input =
  { allImports :: Array { moduleName :: String, imports :: Array String }
  , packages :: Array { name :: String, modules :: Array String, source :: String }
  }

data Query (a :: Type)

type Slot = H.Slot Query Void

-- | Which panel is active in the main area
data ViewPanel = DecompPanel | LayersPanel

derive instance Eq ViewPanel

type State =
  { input :: Input
  , decompInfo :: Maybe Dec.DecompInfo
  , graph :: Maybe (Dec.SimpleGraph String)
  , scopeFilter :: ScopeFilter
  , layerResult :: Maybe LD.LayerDiscoveryResult
  , viewPanel :: ViewPanel
  }

-- | Filter scope: which modules to include in decomposition
data ScopeFilter
  = AllModules
  | WorkspaceOnly  -- Only workspace package modules
  | SinglePackage String  -- Modules within one package

derive instance Eq ScopeFilter

data Action
  = Initialize
  | Receive Input
  | SetScope ScopeFilter
  | SetViewPanel ViewPanel

-- =============================================================================
-- Block colors (same palette as decomposition demo)
-- =============================================================================

blockColors :: Array String
blockColors =
  [ "#4e9a6d", "#c05a4e", "#5a8ec0", "#c09a4e", "#8e5ac0"
  , "#c04e8e", "#4ec0c0", "#a0a040", "#e07048", "#4888c0"
  , "#50a060", "#c06090", "#6080b0", "#b08040", "#7060a0"
  , "#40a0a0", "#a06050", "#5090a0", "#a08070", "#6090a0"
  ]

blockColor :: Int -> String
blockColor i = fromMaybe "#888" (blockColors !! (i `mod` Array.length blockColors))

-- =============================================================================
-- Component
-- =============================================================================

component :: forall o m. MonadAff m => H.Component Query Input o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval
        { initialize = Just Initialize
        , handleAction = handleAction
        , receive = Just <<< Receive
        }
    }

initialState :: Input -> State
initialState input =
  { input
  , decompInfo: Nothing
  , graph: Nothing
  , scopeFilter: WorkspaceOnly
  , layerResult: Nothing
  , viewPanel: DecompPanel
  }

-- =============================================================================
-- Render
-- =============================================================================

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  let
    wsPackages = Array.filter (\p -> p.source == "workspace") state.input.packages
    wsSorted = sortBy (\a b -> compare a.name b.name) wsPackages
  in
  HH.div [ HP.style "display: flex; flex-direction: column; height: 100%; gap: 12px; padding: 16px; font-family: system-ui, sans-serif;" ]
    [ -- Scope filter bar
      HH.div [ HP.style "display: flex; gap: 6px; align-items: center; flex-wrap: wrap;" ]
        ( [ HH.span [ HP.style "font-size: 13px; color: #666; margin-right: 4px;" ] [ HH.text "Scope:" ]
          , scopeButton state.scopeFilter WorkspaceOnly "Workspace"
          , scopeButton state.scopeFilter AllModules "All"
          , HH.span [ HP.style "width: 1px; height: 20px; background: #ddd; margin: 0 4px;" ] []
          ]
          <> (wsSorted <#> \pkg ->
            scopeButton state.scopeFilter (SinglePackage pkg.name) pkg.name
          )
        )
    -- Panel toggle
    , HH.div [ HP.style "display: flex; gap: 6px; align-items: center;" ]
        [ panelButton state.viewPanel DecompPanel "Decomposition"
        , panelButton state.viewPanel LayersPanel "Layer Discovery"
        ]
    , case state.decompInfo, state.graph of
        Just info, Just graph ->
          case state.viewPanel of
            DecompPanel -> renderDecompPanel info graph
            LayersPanel -> renderLayersPanel info graph state.layerResult
        _, _ ->
          HH.div [ HP.style "display: flex; align-items: center; justify-content: center; flex: 1; color: #888;" ]
            [ HH.text "Computing decomposition..." ]
    ]

panelButton :: forall m. ViewPanel -> ViewPanel -> String -> H.ComponentHTML Action () m
panelButton current target label =
  let active = current == target
      style = if active
        then "padding: 5px 16px; font-size: 13px; border: 1px solid #333; background: #333; color: #fff; border-radius: 3px; cursor: pointer; font-weight: 600;"
        else "padding: 5px 16px; font-size: 13px; border: 1px solid #ccc; background: #fff; color: #555; border-radius: 3px; cursor: pointer;"
  in HH.button [ HP.style style, HE.onClick \_ -> SetViewPanel target ] [ HH.text label ]

renderDecompPanel :: forall m. Dec.DecompInfo -> Dec.SimpleGraph String -> H.ComponentHTML Action () m
renderDecompPanel info graph =
  let nNodes = Array.length graph.nodes
  in
  HH.div [ HP.style "display: flex; gap: 16px; flex: 1; min-height: 0;" ]
    [ -- Main content
      HH.div [ HP.style "flex: 1; display: flex; flex-direction: column; gap: 16px; overflow-y: auto;" ]
        [ -- Annotated graph
          HH.div [ HP.style "border: 1px solid #ddd; border-radius: 4px; padding: 12px;" ]
            [ HH.h3 [ HP.style "margin: 0 0 8px; font-size: 14px; font-weight: 600; color: #333;" ]
                [ HH.text $ "Decomposition (" <> show nNodes <> " modules)" ]
            , HH.div [ HP.id "decomp-graph", HP.style "width: 100%; min-height: 320px;" ] []
            ]
        -- Before/after matrices
        , HH.div [ HP.style "display: flex; gap: 16px;" ]
            [ HH.div [ HP.style "flex: 1; border: 1px solid #ddd; border-radius: 4px; padding: 12px;" ]
                [ HH.h3 [ HP.style "margin: 0 0 4px; font-size: 14px; font-weight: 600; color: #333;" ]
                    [ HH.text "Raw Matrix" ]
                , HH.p [ HP.style "margin: 0 0 8px; font-size: 11px; color: #888;" ]
                    [ HH.text "Alphabetical order — structure hidden" ]
                , HH.div [ HP.id "decomp-matrix-raw", HP.style "width: 100%; min-height: 200px;" ] []
                ]
            , HH.div [ HP.style "flex: 1; border: 1px solid #ddd; border-radius: 4px; padding: 12px;" ]
                [ HH.h3 [ HP.style "margin: 0 0 4px; font-size: 14px; font-weight: 600; color: #333;" ]
                    [ HH.text "Block-Ordered Matrix" ]
                , HH.p [ HP.style "margin: 0 0 8px; font-size: 11px; color: #888;" ]
                    [ HH.text "Grouped by biconnected component — structure on diagonal" ]
                , HH.div [ HP.id "decomp-matrix-ordered", HP.style "width: 100%; min-height: 200px;" ] []
                ]
            ]
        ]
    -- Sidebar: metrics
    , HH.div [ HP.style "width: 240px; flex-shrink: 0;" ]
        [ renderMetrics info
        , renderLegend
        , renderBlockList info
        ]
    ]

-- =============================================================================
-- Layer Discovery Panel
-- =============================================================================

renderLayersPanel :: forall m. Dec.DecompInfo -> Dec.SimpleGraph String -> Maybe LD.LayerDiscoveryResult -> H.ComponentHTML Action () m
renderLayersPanel info _graph mResult =
  case mResult of
    Nothing ->
      HH.div [ HP.style "display: flex; align-items: center; justify-content: center; flex: 1; color: #888;" ]
        [ HH.text "Computing layer discovery..." ]
    Just result ->
      HH.div [ HP.style "display: flex; gap: 16px; flex: 1; min-height: 0;" ]
        [ -- Main content: layers + violations
          HH.div [ HP.style "flex: 1; display: flex; flex-direction: column; gap: 16px; overflow-y: auto;" ]
            [ -- Layer diagram (HATS rendered)
              HH.div [ HP.style "border: 1px solid #ddd; border-radius: 4px; padding: 12px;" ]
                [ HH.h3 [ HP.style "margin: 0 0 8px; font-size: 14px; font-weight: 600; color: #333;" ]
                    [ HH.text "Discovered Layers" ]
                , HH.p [ HP.style "margin: 0 0 8px; font-size: 11px; color: #888;" ]
                    [ HH.text "Layers derived from block-cut tree depth + namespace grouping" ]
                , HH.div [ HP.id "layer-diagram", HP.style "width: 100%; min-height: 200px;" ] []
                ]
            -- Layer detail cards
            , HH.div [ HP.style "display: flex; flex-direction: column; gap: 8px;" ]
                (result.layers <#> renderLayerCard)
            -- Violations
            , if Array.length result.violations > 0
              then renderViolationsList result.violations
              else HH.div [ HP.style "border: 1px solid #c8e6c9; border-radius: 4px; padding: 12px; background: #f1f8f1;" ]
                [ HH.text "No layer violations detected — clean layering!" ]
            -- YAML output
            , renderYamlOutput result.layers
            ]
        -- Sidebar: summary metrics
        , HH.div [ HP.style "width: 240px; flex-shrink: 0;" ]
            [ renderLayerMetrics info result
            ]
        ]

layerColors :: Array String
layerColors =
  [ "#1a73e8", "#e8710a", "#0d904f", "#c5221f", "#9334e6"
  , "#185abc", "#b06000", "#137333", "#a50e0e", "#7627bb"
  ]

layerColor :: Int -> String
layerColor i = fromMaybe "#666" (layerColors Array.!! (i `mod` Array.length layerColors))

renderLayerCard :: forall m. LD.DiscoveredLayer -> H.ComponentHTML Action () m
renderLayerCard layer =
  let
    moduleList = Array.sort (Set.toUnfoldable layer.modules :: Array String)
    shortNames = moduleList <#> \name ->
      fromMaybe name $ Array.last (String.split (String.Pattern ".") name)
  in
  HH.div [ HP.style $ "border: 1px solid #ddd; border-radius: 4px; padding: 12px; border-left: 4px solid " <> layerColor layer.order <> ";" ]
    [ HH.div [ HP.style "display: flex; justify-content: space-between; align-items: baseline; margin-bottom: 6px;" ]
        [ HH.span [ HP.style "font-size: 14px; font-weight: 600; color: #333;" ]
            [ HH.text $ layer.name ]
        , HH.span [ HP.style "font-size: 12px; color: #888;" ]
            [ HH.text $ "Layer " <> show layer.order <> " — " <> show (Set.size layer.modules) <> " modules" ]
        ]
    , HH.div [ HP.style "font-size: 11px; color: #666; line-height: 1.6;" ]
        [ HH.text $ String.joinWith ", " shortNames ]
    , HH.div [ HP.style "font-size: 10px; color: #999; margin-top: 4px; font-family: monospace;" ]
        [ HH.text $ "pattern: " <> layer.pattern ]
    ]

renderViolationsList :: forall m. Array LD.Violation -> H.ComponentHTML Action () m
renderViolationsList violations =
  HH.div [ HP.style "border: 1px solid #ffcdd2; border-radius: 4px; padding: 12px; background: #fff8f8;" ]
    ( [ HH.h3 [ HP.style "margin: 0 0 8px; font-size: 14px; font-weight: 600; color: #c62828;" ]
          [ HH.text $ show (Array.length violations) <> " Layer Violations" ]
      , HH.p [ HP.style "margin: 0 0 8px; font-size: 11px; color: #888;" ]
          [ HH.text "Edges going from a deeper layer to a shallower one (upward dependencies)" ]
      ]
      <> (Array.take 20 violations <#> \v ->
        let
          shortFrom = fromMaybe v.from $ Array.last (String.split (String.Pattern ".") v.from)
          shortTo = fromMaybe v.to $ Array.last (String.split (String.Pattern ".") v.to)
        in
        HH.div [ HP.style "display: flex; align-items: center; gap: 6px; padding: 2px 0; font-size: 12px;" ]
          [ HH.span [ HP.style $ "color: " <> layerColor v.fromLayer <> "; font-weight: 500;" ] [ HH.text shortFrom ]
          , HH.span [ HP.style "color: #c62828;" ] [ HH.text " → " ]
          , HH.span [ HP.style $ "color: " <> layerColor v.toLayer <> "; font-weight: 500;" ] [ HH.text shortTo ]
          , HH.span [ HP.style "color: #999; font-size: 10px;" ]
              [ HH.text $ "(L" <> show v.fromLayer <> " → L" <> show v.toLayer <> ")" ]
          ]
      )
      <> (if Array.length violations > 20
          then [ HH.div [ HP.style "font-size: 11px; color: #888; margin-top: 4px;" ]
                   [ HH.text $ "... and " <> show (Array.length violations - 20) <> " more" ] ]
          else [])
    )

renderYamlOutput :: forall m. Array LD.DiscoveredLayer -> H.ComponentHTML Action () m
renderYamlOutput layers =
  let yaml = LD.generateYaml layers
  in
  HH.div [ HP.style "border: 1px solid #ddd; border-radius: 4px; padding: 12px;" ]
    [ HH.h3 [ HP.style "margin: 0 0 8px; font-size: 14px; font-weight: 600; color: #333;" ]
        [ HH.text "Draft architecture.yml" ]
    , HH.p [ HP.style "margin: 0 0 8px; font-size: 11px; color: #888;" ]
        [ HH.text "Review and adjust before using" ]
    , HH.pre [ HP.style "margin: 0; padding: 12px; background: #f5f5f5; border-radius: 4px; font-size: 12px; line-height: 1.5; overflow-x: auto; white-space: pre-wrap;" ]
        [ HH.code [] [ HH.text yaml ] ]
    ]

renderLayerMetrics :: forall m. Dec.DecompInfo -> LD.LayerDiscoveryResult -> H.ComponentHTML Action () m
renderLayerMetrics info result =
  HH.div [ HP.style "border: 1px solid #ddd; border-radius: 4px; padding: 12px; margin-bottom: 12px;" ]
    [ HH.h3 [ HP.style "margin: 0 0 12px; font-size: 14px; font-weight: 600; color: #333;" ]
        [ HH.text "Layer Summary" ]
    , metricRow "Discovered layers" (show (Array.length result.layers))
    , metricRow "Total modules" (show (Array.length (Map.toUnfoldable result.moduleLayer :: Array (Tuple String Int))))
    , metricRow "Violations" (show (Array.length result.violations))
    , metricRow "Current treelikeness" (showPercent info.metrics.treelikeness)
    , metricRow "Without violations" (showPercent result.treelikenessWithout)
    , let improvement = (result.treelikenessWithout - info.metrics.treelikeness) * 100.0
          improvementStr = if improvement > 0.0
            then "+" <> showPercent (improvement / 100.0)
            else "no change"
      in metricRow "Improvement" improvementStr
    ]

scopeButton :: forall m. ScopeFilter -> ScopeFilter -> String -> H.ComponentHTML Action () m
scopeButton current target label =
  let active = current == target
      style = if active
        then "padding: 4px 12px; font-size: 12px; border: 1px solid #333; background: #333; color: #fff; border-radius: 3px; cursor: pointer;"
        else "padding: 4px 12px; font-size: 12px; border: 1px solid #ccc; background: #fff; color: #333; border-radius: 3px; cursor: pointer;"
  in HH.button [ HP.style style, HE.onClick \_ -> SetScope target ] [ HH.text label ]

renderMetrics :: forall m. Dec.DecompInfo -> H.ComponentHTML Action () m
renderMetrics info =
  let m = info.metrics
  in HH.div [ HP.style "border: 1px solid #ddd; border-radius: 4px; padding: 12px; margin-bottom: 12px;" ]
    [ HH.h3 [ HP.style "margin: 0 0 12px; font-size: 14px; font-weight: 600; color: #333;" ]
        [ HH.text "Metrics" ]
    , metricRow "Biconnected components" (show m.biconnectedComponentCount)
    , metricRow "Articulation points" (show m.articulationPointCount)
    , metricRow "Bridges" (show m.bridgeCount)
    , metricRow "Bipartite" (if m.isBipartite then "yes" else "no")
    , metricRow "Tree" (if m.isTree then "yes" else "no")
    , metricRow "Largest block" (show m.maxBlockSize <> " nodes")
    , metricRow "Treelikeness" (showPercent m.treelikeness)
    ]

metricRow :: forall m. String -> String -> H.ComponentHTML Action () m
metricRow label value =
  HH.div [ HP.style "display: flex; justify-content: space-between; padding: 3px 0; font-size: 13px;" ]
    [ HH.span [ HP.style "color: #666;" ] [ HH.text label ]
    , HH.span [ HP.style "font-weight: 600; color: #333;" ] [ HH.text value ]
    ]

showPercent :: Number -> String
showPercent n =
  let pct = n * 100.0
      rounded = Int.round (pct * 10.0)
  in show (Int.toNumber rounded / 10.0) <> "%"

renderLegend :: forall m. H.ComponentHTML Action () m
renderLegend =
  HH.div [ HP.style "border: 1px solid #ddd; border-radius: 4px; padding: 12px; margin-bottom: 12px;" ]
    [ HH.div [ HP.style "display: flex; align-items: center; gap: 6px; margin-bottom: 6px; font-size: 12px;" ]
        [ HH.span [ HP.style "display: inline-block; width: 12px; height: 12px; background: #fff; border: 2px solid #333; transform: rotate(45deg);" ] []
        , HH.text "Articulation point"
        ]
    , HH.div [ HP.style "display: flex; align-items: center; gap: 6px; font-size: 12px;" ]
        [ HH.span [ HP.style "display: inline-block; width: 20px; height: 0; border-top: 2px dashed #999;" ] []
        , HH.text "Bridge (dashed)"
        ]
    , HH.div [ HP.style "margin-top: 6px; font-size: 11px; color: #888;" ]
        [ HH.text "Colors = biconnected components" ]
    ]

renderBlockList :: forall m. Dec.DecompInfo -> H.ComponentHTML Action () m
renderBlockList info =
  let
    nonBridge = Array.filter (not <<< _.isBridge) info.blocks
    sorted = sortBy (\a b -> compare (Set.size b.nodes) (Set.size a.nodes)) nonBridge
    top10 = Array.take 10 sorted
  in
    HH.div [ HP.style "border: 1px solid #ddd; border-radius: 4px; padding: 12px;" ]
      ( [ HH.h3 [ HP.style "margin: 0 0 8px; font-size: 14px; font-weight: 600; color: #333;" ]
            [ HH.text "Largest Blocks" ]
        ]
        <> (top10 <#> \block ->
          let
            shape = Dec.classifyBlock info block
            shapeLabel = case shape of
              Dec.ShapeDense -> "dense"
              Dec.ShapeBipartite -> "bipartite"
              Dec.ShapeCycle -> "cycle"
              Dec.ShapeSparse -> "sparse"
              Dec.ShapeTree -> "tree"
            n = Set.size block.nodes
            e = Set.size block.edges
          in
            HH.div [ HP.style "display: flex; align-items: center; gap: 6px; padding: 3px 0; font-size: 12px;" ]
              [ HH.span [ HP.style $ "display: inline-block; width: 10px; height: 10px; border-radius: 2px; background: " <> blockColor block.index <> ";" ] []
              , HH.span [ HP.style "color: #333;" ] [ HH.text $ show n <> "n " <> show e <> "e" ]
              , HH.span [ HP.style "color: #888; font-style: italic;" ] [ HH.text shapeLabel ]
              ]
        )
      )

-- =============================================================================
-- Actions
-- =============================================================================

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> do
    computeAndRender

  Receive input -> do
    state <- H.get
    when (input.allImports /= state.input.allImports) do
      H.modify_ _ { input = input }
      computeAndRender

  SetScope scope -> do
    state <- H.get
    when (scope /= state.scopeFilter) do
      H.modify_ _ { scopeFilter = scope }
      computeAndRender

  SetViewPanel panel -> do
    state <- H.get
    when (panel /= state.viewPanel) do
      H.modify_ _ { viewPanel = panel }
      -- Re-render HATS for the active panel
      case panel of
        DecompPanel -> do
          case state.decompInfo, state.graph of
            Just info, Just graph -> liftEffect do
              clearContainer "#decomp-graph"
              _ <- rerender "#decomp-graph" (annotatedGraphTree graph info)
              clearContainer "#decomp-matrix-raw"
              _ <- rerender "#decomp-matrix-raw" (rawMatrixTree graph info)
              clearContainer "#decomp-matrix-ordered"
              _ <- rerender "#decomp-matrix-ordered" (blockMatrixTree graph info)
              pure unit
            _, _ -> pure unit
        LayersPanel -> do
          case state.layerResult, state.graph, state.decompInfo of
            Just result, Just graph, Just info -> liftEffect do
              clearContainer "#layer-diagram"
              _ <- rerender "#layer-diagram" (layerDiagramTree graph info result)
              pure unit
            _, _, _ -> pure unit

computeAndRender :: forall o m. MonadAff m => H.HalogenM State Action () o m Unit
computeAndRender = do
  state <- H.get
  let filtered = filterImports state.scopeFilter state.input
  let graph = Dec.importsToSimpleGraph filtered
  let info = Dec.analyzeGraph graph
  let layerResult = LD.discoverLayersFromImports graph info filtered

  log $ "[StructuralDecomp] " <> show (Array.length graph.nodes) <> " nodes, "
      <> show info.metrics.biconnectedComponentCount <> " blocks, "
      <> show info.metrics.articulationPointCount <> " APs, "
      <> show info.metrics.bridgeCount <> " bridges, "
      <> show (Array.length layerResult.layers) <> " layers, "
      <> show (Array.length layerResult.violations) <> " violations"

  H.modify_ _ { decompInfo = Just info, graph = Just graph, layerResult = Just layerResult }

  -- Render HATS visualizations for the active panel
  case state.viewPanel of
    DecompPanel -> liftEffect do
      clearContainer "#decomp-graph"
      _ <- rerender "#decomp-graph" (annotatedGraphTree graph info)
      clearContainer "#decomp-matrix-raw"
      _ <- rerender "#decomp-matrix-raw" (rawMatrixTree graph info)
      clearContainer "#decomp-matrix-ordered"
      _ <- rerender "#decomp-matrix-ordered" (blockMatrixTree graph info)
      pure unit
    LayersPanel -> liftEffect do
      clearContainer "#layer-diagram"
      _ <- rerender "#layer-diagram" (layerDiagramTree graph info layerResult)
      pure unit

-- =============================================================================
-- Scope filtering
-- =============================================================================

filterImports :: ScopeFilter -> Input -> Array { moduleName :: String, imports :: Array String }
filterImports AllModules input = input.allImports
filterImports WorkspaceOnly input =
  let
    wsModules = Set.fromFoldable $ Array.concat $
      Array.filter (\p -> p.source == "workspace") input.packages <#> _.modules
  in
    Array.filter (\mi -> Set.member mi.moduleName wsModules) input.allImports
      <#> \mi -> mi { imports = Array.filter (\imp -> Set.member imp wsModules) mi.imports }
filterImports (SinglePackage pkg) input =
  let
    pkgModules = Set.fromFoldable $ fromMaybe [] $
      Array.find (\p -> p.name == pkg) input.packages <#> _.modules
  in
    Array.filter (\mi -> Set.member mi.moduleName pkgModules) input.allImports
      <#> \mi -> mi { imports = Array.filter (\imp -> Set.member imp pkgModules) mi.imports }

-- =============================================================================
-- HATS Rendering: Annotated Graph
-- =============================================================================

annotatedGraphTree :: Dec.SimpleGraph String -> Dec.DecompInfo -> Tree
annotatedGraphTree graph info =
  let
    -- Get block-cut tree for layout
    bct = Dec.blockCutTree graph
    nBlocks = Array.length bct.blocks

    -- Build block-cut tree adjacency for BFS
    bctAdj = foldl (\acc edge ->
      Map.alter (Just <<< Set.insert edge.to <<< fromMaybe Set.empty) edge.from
        (Map.alter (Just <<< Set.insert edge.from <<< fromMaybe Set.empty) edge.to acc)
    ) Map.empty bct.tree

    -- Find largest block to use as root
    largestBlockIdx = fromMaybe 0 $ map _.index $
      Array.head $ sortBy (\a b -> compare (Set.size b.nodes) (Set.size a.nodes)) info.blocks

    -- BFS from largest block to assign depth/position in block-cut tree
    bfsOrder = bfs bctAdj largestBlockIdx
    maxDepth = max 1 (Array.length bfsOrder - 1)

    -- Layout dimensions
    width = 1000.0
    height = max 350.0 (Int.toNumber (maxDepth + 1) * 100.0 + 80.0)
    margin = 40.0

    -- Position blocks: each block gets a center point based on BFS depth
    -- Depth → y position, spread blocks horizontally within each depth layer
    blockPositions = assignBlockPositions bfsOrder bctAdj nBlocks bct.blocks width height margin

    -- Position nodes within each block in a circle around the block center
    nodePositions = foldl (\acc block ->
      let
        center = fromMaybe { x: width / 2.0, y: height / 2.0 } (Map.lookup block.index blockPositions)
        blockNodes = sortBy compare (Set.toUnfoldable block.nodes :: Array String)
        bn = Array.length blockNodes
        -- Radius proportional to sqrt of node count, but capped
        blockR = if bn <= 1 then 0.0
                 else min 80.0 (max 15.0 (Number.sqrt (Int.toNumber bn) * 10.0))
      in foldl (\a (Tuple i name) ->
        let
          angle = 2.0 * Number.pi * Int.toNumber i / Int.toNumber (max bn 1) - Number.pi / 2.0
          x = center.x + blockR * Number.cos angle
          y = center.y + blockR * Number.sin angle
        in Map.insert name { x, y } a
      ) acc (mapWithIndex Tuple blockNodes)
    ) Map.empty info.blocks

    -- Edges
    edgeElems = Array.concatMap (\name ->
      let targets = fromMaybe Set.empty (Map.lookup name graph.edges)
      in Array.mapMaybe (\tgt ->
        if name < tgt then
          case Map.lookup name nodePositions, Map.lookup tgt nodePositions of
            Just p1, Just p2 ->
              let
                isBridge = Set.member (Tuple name tgt) info.bridgeSet
                edgeColor = case Map.lookup (Tuple name tgt) info.edgeBlock of
                  Just bi -> blockColor bi
                  Nothing -> "#ccc"
              in Just $ elem Line
                [ staticNum "x1" p1.x, staticNum "y1" p1.y
                , staticNum "x2" p2.x, staticNum "y2" p2.y
                , staticStr "stroke" edgeColor
                , staticNum "stroke-width" (if isBridge then 2.0 else 0.6)
                , staticStr "stroke-dasharray" (if isBridge then "5,3" else "")
                , staticNum "stroke-opacity" (if isBridge then 0.7 else 0.25)
                ] []
            _, _ -> Nothing
        else Nothing
      ) (Set.toUnfoldable targets :: Array String)
    ) graph.nodes

    -- Block boundary circles (subtle background)
    blockBgs = Array.mapMaybe (\block ->
      if block.isBridge then Nothing
      else case Map.lookup block.index blockPositions of
        Nothing -> Nothing
        Just center ->
          let
            bn = Set.size block.nodes
            blockR = if bn <= 1 then 8.0
                     else min 80.0 (max 15.0 (Number.sqrt (Int.toNumber bn) * 10.0)) + 8.0
          in Just $ elem Circle
            [ staticNum "cx" center.x, staticNum "cy" center.y
            , staticNum "r" blockR
            , staticStr "fill" (blockColor block.index)
            , staticNum "fill-opacity" 0.06
            , staticStr "stroke" (blockColor block.index)
            , staticNum "stroke-opacity" 0.15
            , staticNum "stroke-width" 1.0
            ] []
    ) info.blocks

    -- Nodes
    nodeElems = Array.mapMaybe (\name ->
      case Map.lookup name nodePositions of
        Nothing -> Nothing
        Just pos ->
          let
            isAP = Set.member name info.aps
            bi = fromMaybe 0 (Map.lookup name info.nodeBlock)
            fill = blockColor bi
            r = if isAP then 6.0 else 4.0
            shortName = fromMaybe name $ Array.last (String.split (String.Pattern ".") name)
          in Just $
            if isAP then
              elem Group []
                [ elem Rect
                    [ staticNum "x" (pos.x - r), staticNum "y" (pos.y - r)
                    , staticNum "width" (r * 2.0), staticNum "height" (r * 2.0)
                    , staticStr "transform" $ "rotate(45," <> show pos.x <> "," <> show pos.y <> ")"
                    , staticStr "fill" "#fff", staticStr "stroke" "#333", staticNum "stroke-width" 1.5
                    ] []
                , elem Text
                    [ staticNum "x" pos.x, staticNum "y" (pos.y - r - 3.0)
                    , staticStr "text-anchor" "middle", staticStr "font-size" "7px"
                    , staticStr "fill" "#333", staticStr "font-family" "system-ui, sans-serif"
                    , staticStr "textContent" shortName
                    ] []
                ]
            else
              elem Group []
                [ elem Circle
                    [ staticNum "cx" pos.x, staticNum "cy" pos.y, staticNum "r" r
                    , staticStr "fill" fill, staticStr "stroke" "#fff", staticNum "stroke-width" 0.5
                    ] []
                , elem Text
                    [ staticNum "x" pos.x, staticNum "y" (pos.y - r - 2.0)
                    , staticStr "text-anchor" "middle", staticStr "font-size" "6px"
                    , staticStr "fill" "#555", staticStr "font-family" "system-ui, sans-serif"
                    , staticStr "textContent" shortName
                    ] []
                ]
    ) graph.nodes
  in
    elem SVG
      [ staticStr "viewBox" $ "0 0 " <> show width <> " " <> show height
      , staticStr "width" "100%"
      , staticStr "preserveAspectRatio" "xMidYMid meet"
      , staticStr "style" "background: #fafafa; border-radius: 4px;"
      ]
      (blockBgs <> edgeElems <> nodeElems)

-- | BFS traversal returning layers (array of arrays of node indices)
bfs :: Map.Map Int (Set.Set Int) -> Int -> Array (Array Int)
bfs adj root = go [root] (Set.singleton root) []
  where
  go queue visited layers =
    if Array.length queue == 0 then layers
    else
      let
        nextQueue = Array.concatMap (\n ->
          let nbrs = fromMaybe Set.empty (Map.lookup n adj)
          in Array.filter (\nb -> not (Set.member nb visited))
               (Set.toUnfoldable nbrs :: Array Int)
        ) queue
        nextVisited = Array.foldl (flip Set.insert) visited nextQueue
      in go nextQueue nextVisited (Array.snoc layers queue)

-- | Assign block center positions using BFS depth and horizontal spread
assignBlockPositions :: Array (Array Int) -> Map.Map Int (Set.Set Int) -> Int -> Array (Set.Set String)
  -> Number -> Number -> Number -> Map.Map Int { x :: Number, y :: Number }
assignBlockPositions bfsLayers _adj nBlocks _blocks w h margin =
  let
    nLayers = Array.length bfsLayers
    layerH = if nLayers <= 1 then 0.0
             else (h - margin * 2.0) / Int.toNumber (nLayers - 1)
  in
    Array.foldl (\acc (Tuple depth layer) ->
      let
        blockIndices = Array.filter (\idx -> idx < nBlocks) layer
        nInLayer = Array.length blockIndices
        spacing = if nInLayer <= 1 then 0.0
                  else (w - margin * 2.0) / Int.toNumber (nInLayer - 1)
        y = margin + Int.toNumber depth * layerH
      in Array.foldl (\a (Tuple i blockIdx) ->
        let x = if nInLayer <= 1 then w / 2.0
                else margin + Int.toNumber i * spacing
        in Map.insert blockIdx { x, y } a
      ) acc (mapWithIndex Tuple blockIndices)
    ) Map.empty (mapWithIndex Tuple bfsLayers)

-- =============================================================================
-- HATS Rendering: Layer Diagram
-- =============================================================================

layerDiagramTree :: Dec.SimpleGraph String -> Dec.DecompInfo -> LD.LayerDiscoveryResult -> Tree
layerDiagramTree _graph _info result =
  let
    nLayers = Array.length result.layers
    width = 900.0
    layerH = 60.0
    moduleR = 4.0
    margin = 30.0
    height = max 200.0 (Int.toNumber nLayers * layerH + margin * 2.0)

    -- Render each layer as a horizontal band with modules as dots
    layerElems = Array.concatMap (\layer ->
      let
        y = margin + Int.toNumber layer.order * layerH
        modules = Array.sort (Set.toUnfoldable layer.modules :: Array String)
        nMods = Array.length modules
        spacing = if nMods <= 1 then 0.0
                  else min 14.0 ((width - margin * 2.0 - 120.0) / Int.toNumber (nMods - 1))
        startX = margin + 120.0
        color = layerColor layer.order
      in
        -- Layer background band
        [ elem Rect
            [ staticNum "x" margin, staticNum "y" (y - layerH * 0.4)
            , staticNum "width" (width - margin * 2.0), staticNum "height" (layerH * 0.8)
            , staticStr "fill" color, staticNum "fill-opacity" 0.04
            , staticStr "stroke" color, staticNum "stroke-opacity" 0.15
            , staticNum "stroke-width" 1.0, staticNum "rx" 4.0
            ] []
        -- Layer label
        , elem Text
            [ staticNum "x" (margin + 8.0), staticNum "y" (y + 4.0)
            , staticStr "font-size" "12px", staticStr "font-weight" "600"
            , staticStr "fill" color, staticStr "font-family" "system-ui, sans-serif"
            , staticStr "textContent" layer.name
            ] []
        -- Module count
        , elem Text
            [ staticNum "x" (margin + 8.0), staticNum "y" (y + 16.0)
            , staticStr "font-size" "9px", staticStr "fill" "#999"
            , staticStr "font-family" "system-ui, sans-serif"
            , staticStr "textContent" (show nMods <> " modules")
            ] []
        ]
        -- Module dots
        <> mapWithIndex (\i name ->
          let
            x = startX + Int.toNumber i * spacing
            shortName = fromMaybe name $ Array.last (String.split (String.Pattern ".") name)
            isAP = Set.member name _info.aps
          in elem Group []
            [ elem Circle
                [ staticNum "cx" x, staticNum "cy" y, staticNum "r" (if isAP then 5.0 else moduleR)
                , staticStr "fill" (if isAP then "#fff" else color)
                , staticStr "stroke" (if isAP then "#333" else color)
                , staticNum "stroke-width" (if isAP then 1.5 else 0.5)
                , staticNum "fill-opacity" (if isAP then 1.0 else 0.7)
                ] []
            , elem Text
                [ staticNum "x" x, staticNum "y" (y - moduleR - 3.0)
                , staticStr "text-anchor" "middle", staticStr "font-size" "6px"
                , staticStr "fill" "#555", staticStr "font-family" "system-ui, sans-serif"
                , staticStr "textContent" shortName
                ] []
            ]
        ) modules
    ) result.layers

    -- Module position lookup for edge drawing
    modulePositions = foldl (\acc layer ->
      let
        modules = Array.sort (Set.toUnfoldable layer.modules :: Array String)
        nMods = Array.length modules
        spacing = if nMods <= 1 then 0.0
                  else min 14.0 ((width - margin * 2.0 - 120.0) / Int.toNumber (nMods - 1))
        startX = margin + 120.0
        y = margin + Int.toNumber layer.order * layerH
      in foldl (\a (Tuple i name) ->
        Map.insert name { x: startX + Int.toNumber i * spacing, y } a
      ) acc (mapWithIndex Tuple modules)
    ) Map.empty result.layers

    -- Draw violation edges in red
    violationEdges = Array.mapMaybe (\v ->
      case Map.lookup v.from modulePositions, Map.lookup v.to modulePositions of
        Just p1, Just p2 ->
          Just $ elem Line
            [ staticNum "x1" p1.x, staticNum "y1" p1.y
            , staticNum "x2" p2.x, staticNum "y2" p2.y
            , staticStr "stroke" "#c62828", staticNum "stroke-width" 1.0
            , staticNum "stroke-opacity" 0.5
            , staticStr "stroke-dasharray" "4,2"
            ] []
        _, _ -> Nothing
    ) result.violations

  in
    elem SVG
      [ staticStr "viewBox" $ "0 0 " <> show width <> " " <> show height
      , staticStr "width" "100%"
      , staticStr "preserveAspectRatio" "xMidYMid meet"
      , staticStr "style" "background: #fafafa; border-radius: 4px;"
      ]
      (violationEdges <> layerElems)

-- =============================================================================
-- HATS Rendering: Adjacency Matrices
-- =============================================================================

-- | Raw (alphabetical) matrix
rawMatrixTree :: Dec.SimpleGraph String -> Dec.DecompInfo -> Tree
rawMatrixTree graph info =
  let ordered = Array.sort graph.nodes
  in matrixTreeWith ordered graph info

-- | Block-ordered matrix
blockMatrixTree :: Dec.SimpleGraph String -> Dec.DecompInfo -> Tree
blockMatrixTree graph info =
  let
    ordered = sortBy (\a b ->
      let ba = fromMaybe 999 (Map.lookup a info.nodeBlock)
          bb = fromMaybe 999 (Map.lookup b info.nodeBlock)
      in case compare ba bb of
        EQ -> compare a b
        x -> x
    ) graph.nodes
  in matrixTreeWith ordered graph info

matrixTreeWith :: Array String -> Dec.SimpleGraph String -> Dec.DecompInfo -> Tree
matrixTreeWith ordered graph info =
  let
    n = Array.length ordered
    maxCells = 100  -- cap at 100x100

    -- If too many nodes, show message
    _ = unit
  in
    if n > maxCells then
      elem SVG
        [ staticStr "viewBox" "0 0 400 100"
        , staticStr "width" "100%"
        , staticStr "style" "background: #fafafa; border-radius: 4px;"
        ]
        [ elem Text
            [ staticNum "x" 200.0, staticNum "y" 50.0
            , staticStr "text-anchor" "middle"
            , staticStr "fill" "#888"
            , staticStr "font-size" "14px"
            , staticStr "font-family" "system-ui, sans-serif"
            , staticStr "textContent" $ show n <> " modules — matrix too large (max " <> show maxCells <> ")"
            ] []
        ]
    else
      let
        cellSize = max 3.0 (min 8.0 (400.0 / Int.toNumber n))
        labelW = 0.0  -- skip labels when dense
        gridSize = cellSize * Int.toNumber n
        viewW = gridSize + labelW + 10.0
        viewH = gridSize + 10.0

        -- Build cells
        cells = Array.concat $ mapWithIndex (\ri rowName ->
          mapWithIndex (\ci colName ->
            let
              hasEdge = case Map.lookup rowName graph.edges of
                Just targets -> Set.member colName targets
                Nothing -> false
              fill = if hasEdge then
                case Map.lookup (Tuple rowName colName) info.edgeBlock of
                  Just bi -> blockColor bi
                  Nothing -> "#4a9eff"
                else "transparent"
              opacity = if hasEdge then 0.85 else 0.0
            in elem Rect
              [ staticNum "x" (labelW + 5.0 + Int.toNumber ci * cellSize)
              , staticNum "y" (5.0 + Int.toNumber ri * cellSize)
              , staticNum "width" (cellSize - 0.5)
              , staticNum "height" (cellSize - 0.5)
              , staticStr "fill" fill
              , staticNum "fill-opacity" opacity
              ] []
          ) ordered
        ) ordered

        -- Block separator lines (only for block-ordered)
        separators = Array.mapMaybe (\i ->
          let
            name = fromMaybe "" (ordered !! i)
            prevName = if i > 0 then ordered !! (i - 1) else Nothing
            nameBlock = Map.lookup name info.nodeBlock
            prevBlock = prevName >>= \pn -> Map.lookup pn info.nodeBlock
          in
            if i > 0 && nameBlock /= prevBlock then
              let pos = 5.0 + Int.toNumber i * cellSize
              in Just $ elem Group []
                [ elem Line
                    [ staticNum "x1" (labelW + 5.0), staticNum "y1" pos
                    , staticNum "x2" (labelW + 5.0 + gridSize), staticNum "y2" pos
                    , staticStr "stroke" "#333", staticNum "stroke-width" 0.5
                    , staticNum "stroke-opacity" 0.4
                    ] []
                , elem Line
                    [ staticNum "x1" pos, staticNum "y1" 5.0
                    , staticNum "x2" pos, staticNum "y2" (5.0 + gridSize)
                    , staticStr "stroke" "#333", staticNum "stroke-width" 0.5
                    , staticNum "stroke-opacity" 0.4
                    ] []
                ]
            else Nothing
        ) (Array.range 0 (n - 1))
      in
        elem SVG
          [ staticStr "viewBox" $ "0 0 " <> show viewW <> " " <> show viewH
          , staticStr "width" "100%"
          , staticStr "preserveAspectRatio" "xMidYMid meet"
          , staticStr "style" "background: #fafafa; border-radius: 4px;"
          ]
          (cells <> separators)
