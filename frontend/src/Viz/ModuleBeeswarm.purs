-- | Module Beeswarm View (HATS Version)
-- |
-- | Visualizes modules as a horizontal beeswarm, clustered by package.
-- | - Circles represent modules, force-positioned along a horizontal axis
-- | - X position grouped by package (packages ordered by topo layer)
-- | - Y position determined by force collision (spreads out crowded packages)
-- | - Radius based on declaration count
-- | - Color by package source (workspace/registry/extra)
-- |
-- | Uses PSD3.Simulation for force simulation.
-- | Uses renderNodes: false pattern - simulation runs physics, HATS handles rendering.
module CE2.Viz.ModuleBeeswarm
  ( Config
  , ModuleNode
  , ModuleNodeRow
  , BeeswarmHandle
  , render
  , renderSinglePackage
  , SinglePackageConfig
  , setScope
  , updateColors
  , cleanup
  , prepareNodes
  ) where

import Prelude

import Data.Array as Array
import Data.Int (toNumber)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable as Nullable
import Data.Number (sqrt)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Ref as Ref

-- PSD3 HATS Imports
import Hylograph.HATS (Tree, elem, staticStr, staticNum, thunkedStr, thunkedNum, forEach)
import Hylograph.HATS.InterpreterTick (rerender, clearContainer)
import Hylograph.Internal.Selection.Types (ElementType(..))
import Hylograph.Simulation.HATS (tickUpdate)

-- Simulation imports
import Hylograph.Simulation
  ( runSimulation
  , Engine(..)
  , SimulationEvent(..)
  , SimulationHandle
  , subscribe
  , setup
  , collide
  , positionX
  , positionY
  , withStrength
  , withRadius
  , withX
  , withY
  , static
  , dynamic
  )
import Hylograph.ForceEngine.Simulation (SimulationNode)

import CE2.Containers as C
import CE2.Data.Loader (V2ModuleListItem, V2ModuleImports, V2Package)
import CE2.Types (ViewTheme, ColorMode)

-- =============================================================================
-- Types
-- =============================================================================

-- | Configuration for beeswarm rendering
type Config =
  { containerSelector :: String
  , width :: Number
  , height :: Number
  , packages :: Array V2Package  -- For computing package positions
  }

-- | Scope for filtering modules
data ModuleScope
  = AllModules
  | WorkspaceOnly      -- source == "workspace"
  | WorkspaceAndLocal  -- source == "workspace" || source == "extra"

derive instance eqModuleScope :: Eq ModuleScope

-- | Module as a simulation node
type ModuleNode = SimulationNode
  ( mod :: V2ModuleListItem
  , targetX :: Number      -- Target x position for forceX
  , r :: Number            -- Circle radius
  , color :: String        -- Fill color based on source
  )

-- | Row type for SimulationHandle
type ModuleNodeRow =
  ( mod :: V2ModuleListItem
  , targetX :: Number
  , r :: Number
  , color :: String
  )

-- | Handle for controlling the beeswarm after render
type BeeswarmHandle =
  { simHandle :: SimulationHandle ModuleNodeRow
  , config :: Config
  , stop :: Effect Unit
  }

-- =============================================================================
-- Public API
-- =============================================================================

-- | Render the module beeswarm view
render :: Config -> Array V2ModuleListItem -> Effect BeeswarmHandle
render config modules = do
  -- Clear existing content
  clearContainer config.containerSelector

  -- Prepare nodes with computed positions, radii, and colors
  let nodes = prepareNodes config modules

  log $ "[ModuleBeeswarm] Starting simulation with " <> show (Array.length nodes) <> " modules"

  -- Create SVG container using HATS
  renderSVGContainerHATS config (Array.length modules)

  -- Start simulation and get handle
  simHandle <- startSimulation config nodes

  pure { simHandle, config, stop: simHandle.stop }

-- | Update scope (filter visible nodes via GUP)
-- | After updating simulation data, does a full HATS rerender to handle enter/exit
setScope :: BeeswarmHandle -> Array V2ModuleListItem -> Effect Unit
setScope handle modules = do
  let nodes = prepareNodes handle.config modules
  log $ "[ModuleBeeswarm] setScope with " <> show (Array.length nodes) <> " modules"
  _ <- handle.simHandle.updateData nodes []
  -- Full HATS rerender to handle enter/exit (tickUpdate only does transforms)
  currentNodes <- handle.simHandle.getNodes
  renderNodesHATS currentNodes

-- | Update colors (after theme/colorMode change)
updateColors :: String -> ViewTheme -> ColorMode -> Effect Unit
updateColors containerSelector _theme _colorMode = do
  log $ "[ModuleBeeswarm] updateColors: " <> containerSelector
  -- For HATS, colors are re-rendered on each tick
  pure unit

-- | Clean up (stop simulation and clear)
cleanup :: String -> Effect Unit
cleanup selector = clearContainer selector

-- =============================================================================
-- Node Preparation
-- =============================================================================

-- | Prepare modules with target positions, radii, and colors
prepareNodes :: Config -> Array V2ModuleListItem -> Array ModuleNode
prepareNodes config modules =
  Array.mapWithIndex (prepareNode config packageXMap) modules
  where
  -- Build map of package name -> x position
  packageXMap = computePackagePositions config.packages config.width

-- | Prepare a single module as a simulation node
prepareNode :: Config -> Map String Number -> Int -> V2ModuleListItem -> ModuleNode
prepareNode config packageXMap idx modItem =
  let
    -- X position based on package (centered at origin)
    baseX = fromMaybe (config.width / 2.0) (Map.lookup modItem.package.name packageXMap)
    targetX = baseX - config.width / 2.0

    -- Radius based on declaration count (sqrt scale for area)
    declCount = toNumber modItem.declarationCount
    r = 3.0 + sqrt (max 1.0 declCount) * 1.5

    -- Color based on source
    color = getModuleColorDefault modItem.package.source

    -- Initial position: start at target x with deterministic y spread
    pseudoRandom = toNumber ((idx * 17 + 31) `mod` 100) / 100.0 - 0.5
    x = targetX
    y = pseudoRandom * config.height * 0.6
  in
    { id: modItem.id
    , x
    , y
    , vx: 0.0
    , vy: 0.0
    , fx: Nullable.null
    , fy: Nullable.null
    , mod: modItem
    , targetX
    , r
    , color
    }

-- | Compute x positions for packages (evenly distributed)
computePackagePositions :: Array V2Package -> Number -> Map String Number
computePackagePositions packages totalWidth =
  let
    padding = 60.0
    availableWidth = totalWidth - padding * 2.0
    pkgCount = Array.length packages

    -- Spread packages evenly across the width
    spacing = if pkgCount > 1 then availableWidth / toNumber (pkgCount - 1) else 0.0

    pkgPositions = Array.mapWithIndex (\idx pkg ->
      let x = padding + toNumber idx * spacing
      in Tuple pkg.name x
    ) packages
  in
    Map.fromFoldable pkgPositions

-- =============================================================================
-- Color Functions
-- =============================================================================

-- | Get fill color for a module based on source (default uniform style)
getModuleColorDefault :: String -> String
getModuleColorDefault source = case source of
  "workspace" -> "#4e79a7"  -- Blue
  "extra"     -> "#76b7b2"  -- Teal
  _           -> "#9c755f"  -- Brown (registry)

-- | Get stroke color based on source
getModuleStroke :: String -> String
getModuleStroke source = case source of
  "workspace" -> "#333"
  "extra"     -> "#666"
  _           -> "#999"

-- =============================================================================
-- HATS Rendering
-- =============================================================================

-- | Create the SVG container structure using HATS
renderSVGContainerHATS :: Config -> Int -> Effect Unit
renderSVGContainerHATS config moduleCount = do
  let
    -- ViewBox centered at origin
    vbX = -config.width / 2.0
    vbY = -config.height / 2.0
    viewBox = show vbX <> " " <> show vbY <> " " <> show config.width <> " " <> show config.height

    containerTree :: Tree
    containerTree =
      elem SVG
        [ staticStr "id" "module-beeswarm-svg"
        , staticStr "viewBox" viewBox
        , staticStr "width" "100%"
        , staticStr "height" "100%"
        , staticStr "class" "module-beeswarm"
        , staticStr "preserveAspectRatio" "xMidYMid meet"
        ]
        [ -- Title
          elem Text
            [ staticStr "x" "0"
            , staticNum "y" (-config.height / 2.0 + 25.0)
            , staticStr "text-anchor" "middle"
            , staticStr "font-size" "14"
            , staticStr "font-weight" "bold"
            , staticStr "fill" "#333"
            , staticStr "textContent" ("Modules (" <> show moduleCount <> " by package dependency depth)")
            ]
            []
        -- Left axis label
        , elem Text
            [ staticNum "x" (-config.width / 2.0 + 60.0)
            , staticNum "y" (config.height / 2.0 - 15.0)
            , staticStr "font-size" "11"
            , staticStr "fill" "#666"
            , staticStr "textContent" "← Leaf packages"
            ]
            []
        -- Right axis label
        , elem Text
            [ staticNum "x" (config.width / 2.0 - 60.0)
            , staticNum "y" (config.height / 2.0 - 15.0)
            , staticStr "text-anchor" "end"
            , staticStr "font-size" "11"
            , staticStr "fill" "#666"
            , staticStr "textContent" "Root packages →"
            ]
            []
        -- Nodes container
        , elem Group
            [ staticStr "id" C.moduleBeeswarmNodesId
            , staticStr "class" "modules"
            ]
            []
        ]
  _ <- rerender config.containerSelector containerTree
  pure unit

-- | Render all module nodes using HATS
renderNodesHATS :: Array ModuleNode -> Effect Unit
renderNodesHATS nodes = do
  let nodesTree = createModuleNodesTree nodes
  _ <- rerender C.moduleBeeswarmNodes nodesTree
  pure unit

-- | Create the nodes tree for HATS rendering
createModuleNodesTree :: Array ModuleNode -> Tree
createModuleNodesTree nodes =
  forEach "modules" Group nodes nodeKey moduleNodeHATS
  where
  nodeKey :: ModuleNode -> String
  nodeKey n = show n.id

-- | Module node template using HATS
moduleNodeHATS :: ModuleNode -> Tree
moduleNodeHATS node =
  elem Group
    [ staticStr "class" "module-group"
    , thunkedStr "transform" ("translate(" <> show node.x <> "," <> show node.y <> ")")
    , thunkedStr "data-id" (show node.id)
    , thunkedStr "data-name" node.mod.name
    , thunkedStr "data-package" node.mod.package.name
    ]
    [ elem Circle
        [ staticStr "class" "module-circle"
        , thunkedNum "r" node.r
        , thunkedStr "fill" node.color
        , thunkedStr "stroke" (getModuleStroke node.mod.package.source)
        , staticStr "stroke-width" "0.5"
        , staticStr "opacity" "0.9"
        ]
        []
    ]

-- =============================================================================
-- Force Simulation
-- =============================================================================

-- | Start the force simulation
startSimulation :: Config -> Array ModuleNode -> Effect (SimulationHandle ModuleNodeRow)
startSimulation _config nodes = do
  log $ "[ModuleBeeswarm] Creating simulation with " <> show (Array.length nodes) <> " nodes"

  { handle, events } <- runSimulation
    { engine: D3
    , setup: setup "module-beeswarm"
        [ -- forceX: attract to target x, strength 0.2
          positionX "x" # withX (dynamic _.targetX) # withStrength (static 0.2)
          -- forceY: center vertically, strength 0.02
        , positionY "y" # withY (static 0.0) # withStrength (static 0.02)
          -- collide: prevent overlap, strength 0.7
        , collide "collide" # withRadius (dynamic \n -> n.r + 1.5) # withStrength (static 0.7)
        ]
    , nodes: nodes
    , links: []
    , container: C.moduleBeeswarmNodes
    , alphaMin: 0.001
    }

  log "[ModuleBeeswarm] runSimulation returned, subscribing to events"

  -- Initial render with full HATS tree (creates elements)
  initialNodes <- handle.getNodes
  renderNodesHATS initialNodes

  -- Subscribe to events - tick uses fast path (transform-only)
  tickCountRef <- Ref.new 0
  _ <- subscribe events \event -> case event of
    Tick _ -> do
      -- Fast path: only update transforms (not full HATS rerender)
      currentNodes <- handle.getNodes
      tickUpdate C.moduleBeeswarmNodes currentNodes
      count <- Ref.read tickCountRef
      Ref.write (count + 1) tickCountRef
      when (count `mod` 100 == 0) do
        log $ "[ModuleBeeswarm] Tick " <> show count
    Completed -> log "[ModuleBeeswarm] Simulation completed"
    Started -> log "[ModuleBeeswarm] Simulation started"
    Stopped -> log "[ModuleBeeswarm] Simulation stopped"

  pure handle

-- =============================================================================
-- Single Package Module Beeswarm (Topo Ordering)
-- =============================================================================

-- | Configuration for single-package module beeswarm
type SinglePackageConfig =
  { containerSelector :: String
  , width :: Number
  , height :: Number
  , packageName :: String
  }

-- | Render a single package's modules in topo order based on intra-package imports
renderSinglePackage :: SinglePackageConfig -> Array V2ModuleListItem -> Array V2ModuleImports -> Effect BeeswarmHandle
renderSinglePackage config modules imports = do
  -- Clear existing content
  clearContainer config.containerSelector

  -- Compute topo layers for modules based on intra-package imports
  let topoLayers = computeModuleTopoLayers modules imports
      maxLayer = Array.foldl (\acc t -> max acc t.layer) 0
                   (Array.fromFoldable $ Map.values topoLayers)

  log $ "[ModuleBeeswarm] Single package: " <> config.packageName
      <> ", " <> show (Array.length modules) <> " modules"
      <> ", maxTopoLayer=" <> show maxLayer

  -- Prepare nodes with topo-based positions
  let nodes = prepareNodesWithTopo config topoLayers maxLayer modules

  -- Create SVG container using HATS
  renderSVGContainerSinglePkgHATS config (Array.length modules)

  -- Start simulation
  simHandle <- startSimulationSinglePkg config nodes

  pure
    { simHandle
    , config: { containerSelector: config.containerSelector
              , width: config.width
              , height: config.height
              , packages: []
              }
    , stop: simHandle.stop
    }

-- | Compute topo layers for modules within a package
computeModuleTopoLayers :: Array V2ModuleListItem -> Array V2ModuleImports -> Map Int { layer :: Int }
computeModuleTopoLayers modules imports =
  let
    -- Build module name -> id map for this package
    nameToId :: Map String Int
    nameToId = Map.fromFoldable $ modules <#> \m -> Tuple m.name m.id

    moduleIds :: Set Int
    moduleIds = Set.fromFoldable $ modules <#> _.id

    -- Set of module names in this package
    pkgModuleNames :: Set String
    pkgModuleNames = Set.fromFoldable $ modules <#> _.name

    -- Build intra-package import graph: moduleId -> Set of imported module IDs (within package)
    importGraph :: Map Int (Set Int)
    importGraph = Map.fromFoldable $ imports <#> \imp ->
      let
        intraPackageImports = Array.filter (\impName -> Set.member impName pkgModuleNames) imp.imports
        importedIds = Set.fromFoldable $ Array.mapMaybe (\n -> Map.lookup n nameToId) intraPackageImports
      in
        Tuple imp.moduleId importedIds

    -- Kahn's algorithm
    kahnLayers :: Map Int Int
    kahnLayers = go 0 Set.empty Map.empty (findLeaves importGraph moduleIds)
      where
      findLeaves :: Map Int (Set Int) -> Set Int -> Set Int
      findLeaves graph allIds =
        Set.filter (\modId ->
          case Map.lookup modId graph of
            Nothing -> true
            Just deps -> Set.isEmpty deps
        ) allIds

      go :: Int -> Set Int -> Map Int Int -> Set Int -> Map Int Int
      go layer processed result currentLayer
        | Set.isEmpty currentLayer =
            let unprocessed = Set.difference moduleIds processed
            in Array.foldl (\m modId -> Map.insert modId layer m) result
                 (Array.fromFoldable unprocessed)
        | otherwise =
            let
              newResult = Array.foldl (\m modId -> Map.insert modId layer m) result
                            (Array.fromFoldable currentLayer)
              newProcessed = Set.union processed currentLayer
              nextLayer = Set.filter (\modId ->
                not (Set.member modId newProcessed) &&
                case Map.lookup modId importGraph of
                  Nothing -> false
                  Just deps -> Set.isEmpty (Set.difference deps newProcessed)
              ) moduleIds
            in
              go (layer + 1) newProcessed newResult nextLayer
  in
    Map.fromFoldable $ (Map.toUnfoldable kahnLayers :: Array (Tuple Int Int)) <#>
      \(Tuple modId layer) -> Tuple modId { layer }

-- | Prepare modules with topo-based positions
prepareNodesWithTopo :: SinglePackageConfig -> Map Int { layer :: Int } -> Int -> Array V2ModuleListItem -> Array ModuleNode
prepareNodesWithTopo config topoLayers maxLayer modules =
  Array.mapWithIndex (prepareNodeWithTopo config topoLayers maxLayer layerPositions) modules
  where
  layerCounts = countModulesByLayer topoLayers maxLayer
  layerPositions = computeTopoLayerPositions layerCounts config.width

-- | Count modules per topo layer
countModulesByLayer :: Map Int { layer :: Int } -> Int -> Array Int
countModulesByLayer topoLayers maxLayer =
  Array.range 0 maxLayer <#> \layer ->
    Array.length $ Array.filter (\t -> t.layer == layer) (Array.fromFoldable $ Map.values topoLayers)

-- | Compute x positions for topo layers
computeTopoLayerPositions :: Array Int -> Number -> Array Number
computeTopoLayerPositions counts totalWidth =
  let
    padding = 100.0
    availableWidth = totalWidth - padding * 2.0
    minWidth = 40.0
    weights = counts <#> \c -> minWidth + sqrt (toNumber (max 1 c)) * 10.0
    totalWeight = Array.foldl (+) 0.0 weights
    scale = if totalWeight > 0.0 then availableWidth / totalWeight else 1.0
    normalized = weights <#> \w -> w * scale
  in
    scanl (+) padding normalized

-- | Prefix sum helper
scanl :: forall a b. (b -> a -> b) -> b -> Array a -> Array b
scanl f initial arr = Array.foldl go { acc: initial, result: [initial] } arr # _.result
  where
  go { acc, result } x =
    let newAcc = f acc x
    in { acc: newAcc, result: Array.snoc result newAcc }

-- | Prepare a single module with topo-based position
prepareNodeWithTopo :: SinglePackageConfig -> Map Int { layer :: Int } -> Int -> Array Number -> Int -> V2ModuleListItem -> ModuleNode
prepareNodeWithTopo config topoLayers maxLayer cumulativeX idx modItem =
  let
    layerInfo = fromMaybe { layer: 0 } $ Map.lookup modItem.id topoLayers
    topoLayer = layerInfo.layer

    targetX = (fromMaybe 0.0 $ Array.index cumulativeX topoLayer) - config.width / 2.0

    loc = toNumber $ fromMaybe 100 modItem.loc
    r = 8.0 + sqrt (max 10.0 loc) * 0.4

    color = topoLayerColorModule topoLayer maxLayer

    pseudoRandom = toNumber ((idx * 17 + 31) `mod` 100) / 100.0 - 0.5
    x = targetX
    y = pseudoRandom * config.height * 0.5
  in
    { id: modItem.id
    , x
    , y
    , vx: 0.0
    , vy: 0.0
    , fx: Nullable.null
    , fy: Nullable.null
    , mod: modItem
    , targetX
    , r
    , color
    }

-- | Color based on topo layer
topoLayerColorModule :: Int -> Int -> String
topoLayerColorModule layer maxLayer =
  let
    t = if maxLayer > 0 then toNumber layer / toNumber maxLayer else 0.0
    h = 45.0 + t * 180.0
    s = 60.0
    l = 55.0
  in
    "hsl(" <> show h <> ", " <> show s <> "%, " <> show l <> "%)"

-- | Create SVG container for single-package view using HATS
renderSVGContainerSinglePkgHATS :: SinglePackageConfig -> Int -> Effect Unit
renderSVGContainerSinglePkgHATS config moduleCount = do
  let
    vbX = -config.width / 2.0
    vbY = -config.height / 2.0
    viewBox = show vbX <> " " <> show vbY <> " " <> show config.width <> " " <> show config.height

    containerTree :: Tree
    containerTree =
      elem SVG
        [ staticStr "id" "module-beeswarm-svg"
        , staticStr "viewBox" viewBox
        , staticStr "width" "100%"
        , staticStr "height" "100%"
        , staticStr "class" "module-beeswarm single-package"
        , staticStr "preserveAspectRatio" "xMidYMid meet"
        ]
        [ -- Title
          elem Text
            [ staticStr "x" "0"
            , staticNum "y" (-config.height / 2.0 + 25.0)
            , staticStr "text-anchor" "middle"
            , staticStr "font-size" "14"
            , staticStr "font-weight" "bold"
            , staticStr "fill" "#333"
            , staticStr "textContent" (config.packageName <> " modules (" <> show moduleCount <> ") by import depth")
            ]
            []
        -- Subtitle
        , elem Text
            [ staticStr "x" "0"
            , staticNum "y" (-config.height / 2.0 + 42.0)
            , staticStr "text-anchor" "middle"
            , staticStr "font-size" "11"
            , staticStr "fill" "#666"
            , staticStr "textContent" "Leaves (no imports) ← → Roots (most deps)"
            ]
            []
        -- Left axis label
        , elem Text
            [ staticNum "x" (-config.width / 2.0 + 80.0)
            , staticNum "y" (config.height / 2.0 - 15.0)
            , staticStr "font-size" "11"
            , staticStr "fill" "#666"
            , staticStr "textContent" "← Leaves"
            ]
            []
        -- Right axis label
        , elem Text
            [ staticNum "x" (config.width / 2.0 - 80.0)
            , staticNum "y" (config.height / 2.0 - 15.0)
            , staticStr "text-anchor" "end"
            , staticStr "font-size" "11"
            , staticStr "fill" "#666"
            , staticStr "textContent" "Roots →"
            ]
            []
        -- Nodes container
        , elem Group
            [ staticStr "id" C.pkgModuleBeeswarmNodesId
            , staticStr "class" "modules"
            ]
            []
        ]
  _ <- rerender config.containerSelector containerTree
  pure unit

-- | Render all module nodes for single-package view using HATS
renderNodesSinglePkgHATS :: Array ModuleNode -> Effect Unit
renderNodesSinglePkgHATS nodes = do
  let nodesTree = createModuleNodesSinglePkgTree nodes
  _ <- rerender C.pkgModuleBeeswarmNodes nodesTree
  pure unit

-- | Create the nodes tree for single-package HATS rendering
createModuleNodesSinglePkgTree :: Array ModuleNode -> Tree
createModuleNodesSinglePkgTree nodes =
  forEach "modules" Group nodes nodeKey moduleNodeSinglePkgHATS
  where
  nodeKey :: ModuleNode -> String
  nodeKey n = show n.id

-- | Module node template for single-package view using HATS
moduleNodeSinglePkgHATS :: ModuleNode -> Tree
moduleNodeSinglePkgHATS node =
  elem Group
    [ staticStr "class" "module-group"
    , thunkedStr "transform" ("translate(" <> show node.x <> "," <> show node.y <> ")")
    , thunkedStr "data-id" (show node.id)
    , thunkedStr "data-name" node.mod.name
    ]
    [ elem Circle
        [ staticStr "class" "module-circle"
        , thunkedNum "r" node.r
        , thunkedStr "fill" node.color
        , staticStr "stroke" "#333"
        , staticStr "stroke-width" "1"
        , staticStr "opacity" "0.75"
        , staticStr "cursor" "pointer"
        ]
        []
    ]

-- | Start simulation for single-package view
startSimulationSinglePkg :: SinglePackageConfig -> Array ModuleNode -> Effect (SimulationHandle ModuleNodeRow)
startSimulationSinglePkg _config nodes = do
  log $ "[ModuleBeeswarm] Starting single-package simulation with " <> show (Array.length nodes) <> " nodes"

  { handle, events } <- runSimulation
    { engine: D3
    , setup: setup "module-beeswarm-single"
        [ positionX "x" # withX (dynamic _.targetX) # withStrength (static 0.35)
        , positionY "y" # withY (static 0.0) # withStrength (static 0.04)
        , collide "collide" # withRadius (dynamic \n -> n.r + 4.0) # withStrength (static 0.8)
        ]
    , nodes: nodes
    , links: []
    , container: C.pkgModuleBeeswarmNodes
    , alphaMin: 0.001
    }

  -- Initial render with full HATS tree (creates elements)
  initialNodes <- handle.getNodes
  renderNodesSinglePkgHATS initialNodes

  -- Subscribe to events - tick uses fast path (transform-only)
  tickCountRef <- Ref.new 0
  _ <- subscribe events \event -> case event of
    Tick _ -> do
      -- Fast path: only update transforms (not full HATS rerender)
      currentNodes <- handle.getNodes
      tickUpdate C.pkgModuleBeeswarmNodes currentNodes
      count <- Ref.read tickCountRef
      Ref.write (count + 1) tickCountRef
      when (count `mod` 100 == 0) do
        log $ "[ModuleBeeswarm/Single] Tick " <> show count
    Completed -> log "[ModuleBeeswarm/Single] Simulation completed"
    Started -> log "[ModuleBeeswarm/Single] Simulation started"
    Stopped -> log "[ModuleBeeswarm/Single] Simulation stopped"

  pure handle
