-- | Bubble Pack Beeswarm - Packages as circle-packed bubbles in beeswarm layout (HATS Version)
-- |
-- | Powers of Ten intermediate view: packages rendered as larger circles
-- | with their modules packed inside. Beeswarm physics positions the packages.
-- |
-- | Uses PSD3.Simulation for force simulation.
-- | Uses psd3-layout's packSiblingsMap for circle packing.
-- | Uses renderNodes: false pattern - simulation runs physics, HATS handles rendering.
module CE2.Viz.BubblePackBeeswarm
  ( render
  , renderWithPositions
  , setScope
  , updateColors
  , Config
  , Callbacks
  , BubblePackHandle
  , PackedPackageNode
  , PackedPackageNodeRow
  , PackedModuleCircle
  , InitialPosition
  ) where

import Prelude

import Data.Array as Array
import Data.Int (toNumber)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Data.Nullable as Nullable
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class.Console (log)

-- PSD3 HATS Imports
import Hylograph.HATS (Tree, elem, staticStr, staticNum, thunkedStr, thunkedNum, forEach, withBehaviors, onMouseEnter, onMouseLeave, onClick, onClickWithModifier, onCoordinatedHighlight)
import Hylograph.HATS.InterpreterTick (rerender, clearContainer)
import Hylograph.Internal.Element.Types (ElementType(..))
import Hylograph.Internal.Behavior.Types (HighlightClass(..))
import Data.Number (sqrt)
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
  , manyBody
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

-- Circle packing from psd3-layout
import DataViz.Layout.Hierarchy.Pack (packSiblingsMap, Circle)

import CE2.Containers as C
import CE2.Types (SimNode, NodeType(..), Package, ViewTheme, ColorMode)

-- =============================================================================
-- Types
-- =============================================================================

type Config =
  { containerSelector :: String
  , width :: Number
  , height :: Number
  , packages :: Array Package  -- Package metadata for grouping
  , maxTopoLayer :: Int        -- For topo-based X positioning
  , moduleImports :: Map String (Array String)    -- Module name -> modules it imports
  , moduleImportedBy :: Map String (Array String) -- Module name -> modules that import it
  , appPackages :: Set String  -- Package names that have a bundleModule (apps)
  }

-- | Callbacks for user interactions
type Callbacks =
  { onPackageClick :: String -> Effect Unit  -- Plain circle click: package name → drill in
  , onPackageModifierClick :: String -> Effect Unit  -- Modifier+circle click: package name → focal filter
  , onPackageLabelClick :: String -> Effect Unit  -- Label click: package name → treemap view
  , onPackageHover :: Maybe String -> Effect Unit  -- Package name or Nothing
  , onModuleClick :: String -> String -> Effect Unit  -- Package name, Module name
  , onModuleHover :: String -> Maybe String -> Effect Unit  -- Package name, Maybe module name
  }

-- | A module circle packed within a package
type PackedModuleCircle =
  { name :: String
  , x :: Number      -- Position relative to package center
  , y :: Number
  , r :: Number
  , packageName :: String        -- Parent package (for callbacks)
  , packageColor :: String       -- Inherit package color
  , imports :: Array String      -- Module names this imports (within same package)
  , importedBy :: Array String   -- Module names that import this (within same package)
  }

-- | Package node for force simulation with packed modules inside
type PackedPackageNode = SimulationNode
  ( name :: String
  , moduleCount :: Int
  , color :: String
  , r :: Number                         -- Enclosing radius for collision
  , modules :: Array PackedModuleCircle -- Pre-packed module positions
  , targetX :: Number                   -- Target X for topo-based positioning
  , topoLayer :: Int                    -- Topo layer for coloring
  , source :: String                    -- "registry" | "workspace" | "extra"
  , isApp :: Boolean                    -- true if has bundleModule
  )

-- | Handle for interacting with the visualization
type BubblePackHandle =
  { simHandle :: SimulationHandle (PackedPackageNodeRow)
  , config :: Config
  , stop :: Effect Unit
  }

-- | Initial position for hero transition (from beeswarm)
type InitialPosition = { name :: String, x :: Number, y :: Number, r :: Number }

-- Row type for SimulationHandle
type PackedPackageNodeRow =
  ( name :: String
  , moduleCount :: Int
  , color :: String
  , r :: Number
  , modules :: Array PackedModuleCircle
  , targetX :: Number
  , topoLayer :: Int
  , source :: String
  , isApp :: Boolean
  )

-- =============================================================================
-- Public API
-- =============================================================================

-- | Render the bubble pack beeswarm view
render :: Config -> Callbacks -> Array SimNode -> Effect BubblePackHandle
render config callbacks nodes = do
  log $ "[BubblePackBeeswarm] Rendering with " <> show (Array.length nodes) <> " nodes"

  -- Clear existing content
  clearContainer config.containerSelector

  -- Separate packages and modules
  let packageNodes = Array.filter (\n -> n.nodeType == PackageNode) nodes
      moduleNodes = Array.filter (\n -> n.nodeType == ModuleNode) nodes

  log $ "[BubblePackBeeswarm] Packages: " <> show (Array.length packageNodes)
      <> ", Modules: " <> show (Array.length moduleNodes)

  -- Group modules by package and prepare packed nodes
  let modulesByPackage = groupModulesByPackage moduleNodes
      packedNodes = preparePackedNodes config packageNodes modulesByPackage

  -- Log layer distribution
  let layers = map _.topoLayer packageNodes
      minL = Array.foldl min 999 layers
      maxL = Array.foldl max 0 layers
  log $ "[BubblePackBeeswarm] Prepared " <> show (Array.length packedNodes) <> " bubble packs"
      <> ", layers " <> show minL <> "-" <> show maxL

  -- Render SVG container using HATS
  renderSVGContainerHATS config

  -- Start force simulation
  { handle, events } <- runSimulation
    { engine: D3
    , setup: setup "bubblepack"
        [ manyBody "charge" # withStrength (static (-100.0))
        , collide "collision" # withRadius (dynamic \n -> n.r + 10.0)
        , positionX "forceX" # withX (dynamic _.targetX) # withStrength (static 0.6)
        , positionY "forceY" # withY (static 0.0) # withStrength (static 0.1)
        ]
    , nodes: packedNodes
    , links: []
    , container: C.bubblePackBeeswarmNodes
    , alphaMin: 0.001
    }

  -- Initial render with full HATS tree (creates elements with behaviors)
  initialNodes <- handle.getNodes
  renderNodesHATS callbacks initialNodes

  -- Subscribe to simulation events - tick uses fast path (transform-only)
  _ <- subscribe events \event -> case event of
    Tick _ -> do
      -- Fast path: only update transforms (not full HATS rerender)
      currentNodes <- handle.getNodes
      tickUpdate C.bubblePackBeeswarmNodes currentNodes
    Completed -> do
      log "[BubblePackBeeswarm] Simulation converged"
      finalNodes <- handle.getNodes
      let tree = createPackageNodesTreeWithLinks callbacks finalNodes
      _ <- rerender C.bubblePackBeeswarmNodes tree
      pure unit
    Started -> log "[BubblePackBeeswarm] Simulation started"
    Stopped -> pure unit

  log "[BubblePackBeeswarm] Simulation running"

  pure
    { simHandle: handle
    , config
    , stop: handle.stop
    }

-- | Render with initial positions (for Beeswarm → SolarSwarm transition)
renderWithPositions :: Config -> Callbacks -> Array SimNode -> Array InitialPosition -> Effect BubblePackHandle
renderWithPositions config callbacks nodes positions = do
  log $ "[BubblePackBeeswarm] Rendering with " <> show (Array.length nodes) <> " nodes"
      <> ", " <> show (Array.length positions) <> " initial positions"

  -- Clear existing content
  clearContainer config.containerSelector

  -- Separate packages and modules
  let packageNodes = Array.filter (\n -> n.nodeType == PackageNode) nodes
      moduleNodes = Array.filter (\n -> n.nodeType == ModuleNode) nodes

  -- Build position lookup map
  let positionMap = Map.fromFoldable $ map (\p -> Tuple p.name { x: p.x, y: p.y, r: p.r }) positions

  -- Group modules by package and prepare packed nodes at initial positions
  let modulesByPackage = groupModulesByPackage moduleNodes
      packedNodes = preparePackedNodesAtPositions config packageNodes modulesByPackage positionMap

  log $ "[BubblePackBeeswarm] Prepared " <> show (Array.length packedNodes) <> " bubble packs at initial positions"

  -- Render SVG container using HATS
  renderSVGContainerHATS config

  -- Start force simulation
  { handle, events } <- runSimulation
    { engine: D3
    , setup: setup "bubblepack"
        [ manyBody "charge" # withStrength (static (-100.0))
        , collide "collision" # withRadius (dynamic \n -> n.r + 10.0)
        , positionX "forceX" # withX (dynamic _.targetX) # withStrength (static 0.6)
        , positionY "forceY" # withY (static 0.0) # withStrength (static 0.1)
        ]
    , nodes: packedNodes
    , links: []
    , container: C.bubblePackBeeswarmNodes
    , alphaMin: 0.001
    }

  -- Initial render with full HATS tree (creates elements with behaviors)
  initialNodes <- handle.getNodes
  renderNodesHATS callbacks initialNodes

  -- Subscribe to simulation events - tick uses fast path
  _ <- subscribe events \event -> case event of
    Tick _ -> do
      currentNodes <- handle.getNodes
      tickUpdate C.bubblePackBeeswarmNodes currentNodes
    Completed -> do
      log "[BubblePackBeeswarm] Simulation converged"
      finalNodes <- handle.getNodes
      let tree = createPackageNodesTreeWithLinks callbacks finalNodes
      _ <- rerender C.bubblePackBeeswarmNodes tree
      pure unit
    Started -> log "[BubblePackBeeswarm] Simulation started"
    Stopped -> pure unit

  pure
    { simHandle: handle
    , config
    , stop: handle.stop
    }

-- | Update visible packages (GUP - enter/exit animations)
-- | After updating simulation data, does a full HATS rerender to handle enter/exit
-- | (The fast tickUpdate path only handles position changes, not element changes)
setScope :: BubblePackHandle -> Callbacks -> Array SimNode -> Effect Unit
setScope handle callbacks nodes = do
  log $ "[BubblePackBeeswarm] setScope called with " <> show (Array.length nodes) <> " nodes"

  let packageNodes = Array.filter (\n -> n.nodeType == PackageNode) nodes
      moduleNodes = Array.filter (\n -> n.nodeType == ModuleNode) nodes
      modulesByPackage = groupModulesByPackage moduleNodes
      packedNodes = preparePackedNodes handle.config packageNodes modulesByPackage

  result <- handle.simHandle.updateData packedNodes []

  log $ "[BubblePackBeeswarm] GUP: " <>
    show (Array.length result.nodes.entered) <> " entered, " <>
    show (Array.length result.nodes.updated) <> " updated, " <>
    show (Array.length result.nodes.exited) <> " exited"

  -- Full HATS rerender to handle enter/exit (tickUpdate only does transforms)
  currentNodes <- handle.simHandle.getNodes
  renderNodesHATS callbacks currentNodes

-- | Update colors in place (re-renders with new colors)
updateColors :: String -> ViewTheme -> ColorMode -> Effect Unit
updateColors containerSelector _theme _colorMode = do
  log $ "[BubblePackBeeswarm] updateColors: " <> containerSelector
  -- For HATS, colors are re-rendered on each tick
  pure unit

-- =============================================================================
-- Node Preparation
-- =============================================================================

-- | Group modules by their package name
groupModulesByPackage :: Array SimNode -> Map.Map String (Array SimNode)
groupModulesByPackage modules =
  Array.foldl addModule Map.empty modules
  where
  addModule acc mod =
    let existing = fromMaybe [] $ Map.lookup mod.package acc
    in Map.insert mod.package (Array.snoc existing mod) acc

-- | Prepare packages with circle-packed modules
preparePackedNodes
  :: Config
  -> Array SimNode
  -> Map.Map String (Array SimNode)
  -> Array PackedPackageNode
preparePackedNodes config packageNodes modulesByPackage =
  Array.mapWithIndex preparePkg packageNodes
  where
  distinctLayers = Array.nub $ Array.sort $ map _.topoLayer packageNodes
  numLayers = max 1 (Array.length distinctLayers - 1)
  layerRanks = Map.fromFoldable $ Array.mapWithIndex (\idx layer -> Tuple layer idx) distinctLayers

  padding = 150.0
  usableWidth = config.width - 2.0 * padding

  calcTargetX :: Int -> Number
  calcTargetX layer =
    let rank = fromMaybe 0 $ Map.lookup layer layerRanks
        normalizedRank = toNumber rank / toNumber numLayers
    in padding + normalizedRank * usableWidth - config.width / 2.0

  preparePkg idx pkg =
    let
      modules = fromMaybe [] $ Map.lookup pkg.name modulesByPackage
      moduleCount = Array.length modules

      moduleCircles :: Array Circle
      moduleCircles = modules # map \m ->
        { x: 0.0
        , y: 0.0
        , r: max 3.0 (m.r * 0.5)
        }

      packed = packSiblingsMap moduleCircles
      color = packageColor idx

      toPackedModule :: SimNode -> Circle -> PackedModuleCircle
      toPackedModule m c =
        { name: m.name
        , x: c.x
        , y: c.y
        , r: c.r
        , packageName: pkg.name
        , packageColor: color
        , imports: fromMaybe [] $ Map.lookup m.name config.moduleImports
        , importedBy: fromMaybe [] $ Map.lookup m.name config.moduleImportedBy
        }

      packedModules = Array.zipWith toPackedModule modules packed.circles
      r = max 30.0 (packed.radius + 8.0)
      targetX = calcTargetX pkg.topoLayer
      pseudoRandom = toNumber ((idx * 17 + 31) `mod` 100) / 100.0 - 0.5
      x = targetX
      y = pseudoRandom * config.height * 0.6
    in
      { id: pkg.id
      , x
      , y
      , vx: 0.0
      , vy: 0.0
      , fx: Nullable.null
      , fy: Nullable.null
      , name: pkg.name
      , moduleCount
      , color
      , r
      , modules: packedModules
      , targetX
      , topoLayer: pkg.topoLayer
      , source: pkg.source
      , isApp: Set.member pkg.name config.appPackages
      }

-- | Prepare packages at initial positions (for hero transition)
preparePackedNodesAtPositions
  :: Config
  -> Array SimNode
  -> Map String (Array SimNode)
  -> Map String { x :: Number, y :: Number, r :: Number }
  -> Array PackedPackageNode
preparePackedNodesAtPositions config packageNodes modulesByPackage positionMap =
  Array.mapWithIndex preparePkg packageNodes
  where
  distinctLayers = Array.nub $ Array.sort $ map _.topoLayer packageNodes
  numLayers = max 1 (Array.length distinctLayers - 1)
  layerRanks = Map.fromFoldable $ Array.mapWithIndex (\idx layer -> Tuple layer idx) distinctLayers

  padding = 150.0
  usableWidth = config.width - 2.0 * padding

  calcTargetX :: Int -> Number
  calcTargetX layer =
    let rank = fromMaybe 0 $ Map.lookup layer layerRanks
        normalizedRank = toNumber rank / toNumber numLayers
    in padding + normalizedRank * usableWidth - config.width / 2.0

  preparePkg idx pkg =
    let
      modules = fromMaybe [] $ Map.lookup pkg.name modulesByPackage
      moduleCount = Array.length modules

      moduleCircles :: Array Circle
      moduleCircles = modules # map \m ->
        { x: 0.0
        , y: 0.0
        , r: max 3.0 (m.r * 0.5)
        }

      packed = packSiblingsMap moduleCircles
      color = packageColor idx

      toPackedMod :: SimNode -> Circle -> PackedModuleCircle
      toPackedMod m c =
        { name: m.name
        , x: c.x
        , y: c.y
        , r: c.r
        , packageName: pkg.name
        , packageColor: color
        , imports: fromMaybe [] $ Map.lookup m.name config.moduleImports
        , importedBy: fromMaybe [] $ Map.lookup m.name config.moduleImportedBy
        }

      packedModules = Array.zipWith toPackedMod modules packed.circles
      r = max 30.0 (packed.radius + 8.0)
      targetX = calcTargetX pkg.topoLayer

      x = case Map.lookup pkg.name positionMap of
        Just pos -> pos.x
        Nothing -> targetX

      y = case Map.lookup pkg.name positionMap of
        Just pos -> pos.y
        Nothing ->
          let pseudoRandom = toNumber ((idx * 17 + 31) `mod` 100) / 100.0 - 0.5
          in pseudoRandom * config.height * 0.6
    in
      { id: pkg.id
      , x
      , y
      , vx: 0.0
      , vy: 0.0
      , fx: Nullable.null
      , fy: Nullable.null
      , name: pkg.name
      , moduleCount
      , color
      , r
      , modules: packedModules
      , targetX
      , topoLayer: pkg.topoLayer
      , source: pkg.source
      , isApp: false
      }

-- =============================================================================
-- HATS Rendering
-- =============================================================================

-- | Render the SVG container structure using HATS
renderSVGContainerHATS :: Config -> Effect Unit
renderSVGContainerHATS config = do
  let
    vbMinX = -config.width / 2.0
    vbMinY = -config.height / 2.0
    viewBox = show vbMinX <> " " <> show vbMinY <> " " <>
              show config.width <> " " <> show config.height

    containerTree :: Tree
    containerTree =
      elem SVG
        [ staticStr "id" "bubblepack-svg"
        , staticStr "viewBox" viewBox
        , staticStr "width" "100%"
        , staticStr "height" "100%"
        , staticStr "style" "background: transparent;"
        ]
        [ -- Background rect for click handling
          elem Rect
            [ staticStr "class" "background"
            , staticNum "x" vbMinX
            , staticNum "y" vbMinY
            , staticNum "width" config.width
            , staticNum "height" config.height
            , staticStr "fill" "transparent"
            ]
            []
        , -- Nodes group for simulation
          elem Group
            [ staticStr "id" C.bubblePackBeeswarmNodesId
            , staticStr "class" "bubblepack-nodes"
            ]
            []
        ]
  _ <- rerender config.containerSelector containerTree
  pure unit

-- | Render all package nodes using HATS
renderNodesHATS :: Callbacks -> Array PackedPackageNode -> Effect Unit
renderNodesHATS callbacks nodes = do
  let nodesTree = createPackageNodesTree callbacks nodes
  _ <- rerender C.bubblePackBeeswarmNodes nodesTree
  pure unit

-- | Create the nodes tree for HATS rendering
-- | Wrapped in a Group to match createPackageNodesTreeWithLinks structure
-- | so HATS can reconcile when links are added after simulation converges
createPackageNodesTree :: Callbacks -> Array PackedPackageNode -> Tree
createPackageNodesTree callbacks nodes =
  elem Group []
    [ forEach "packages" Group nodes nodeKey (packageNodeHATS callbacks)
    ]
  where
  nodeKey :: PackedPackageNode -> String
  nodeKey n = n.name

-- | Package node template using HATS
packageNodeHATS :: Callbacks -> PackedPackageNode -> Tree
packageNodeHATS callbacks node =
  -- Hover behaviors on the Group (affects all children)
  withBehaviors
    [ onMouseEnter (callbacks.onPackageHover (Just node.name))
    , onMouseLeave (callbacks.onPackageHover Nothing)
    ]
  $ elem Group
      [ thunkedStr "transform" ("translate(" <> show node.x <> "," <> show node.y <> ")")
      , staticStr "class" "package-bubble"
      , thunkedStr "data-id" (show node.id)  -- Required for tickUpdate
      , thunkedStr "data-name" node.name
      , staticStr "cursor" "pointer"
      ]
      ( [ -- Package enclosing shape: rounded rect for apps, circle for others
          -- Plain click → drill into package, modifier+click → focal filter
          withBehaviors [ onClickWithModifier
                            (callbacks.onPackageClick node.name)
                            (callbacks.onPackageModifierClick node.name)
                        ]
          $ if node.isApp
            then elem Rect
              [ staticStr "class" "package-circle app-rect"
              , thunkedNum "x" (-(node.r * 0.9))
              , thunkedNum "y" (-(node.r * 0.9))
              , thunkedNum "width" (node.r * 1.8)
              , thunkedNum "height" (node.r * 1.8)
              , staticStr "rx" "8"
              , staticStr "ry" "8"
              , thunkedStr "fill" node.color
              , staticStr "fill-opacity" "0.15"
              , thunkedStr "stroke" node.color
              , staticStr "stroke-width" "2"
              ]
              []
            else elem Circle
              [ staticStr "class" "package-circle"
              , staticStr "cx" "0"
              , staticStr "cy" "0"
              , thunkedNum "r" node.r
              , thunkedStr "fill" node.color
              , staticStr "fill-opacity" "0.15"
              , thunkedStr "stroke" node.color
              , staticStr "stroke-width" "2"
              ]
              []
        ]
        -- Source indicator for registry/extra packages
        <> let letter = if node.source == "registry" then "p"
                        else if node.source == "extra" then "e"
                        else ""
               fontSize = max 8.0 (node.r * 0.25)
           in if letter /= "" && node.r > 20.0
              then [ elem Text
                       [ staticStr "x" "0"
                       , thunkedNum "y" (-(node.r - fontSize))
                       , staticStr "text-anchor" "middle"
                       , staticStr "dominant-baseline" "central"
                       , thunkedStr "font-size" (show fontSize)
                       , staticStr "fill" "rgba(0,0,0,0.3)"
                       , staticStr "font-family" "'Courier New', Courier, monospace"
                       , staticStr "pointer-events" "none"
                       , thunkedStr "textContent" letter
                       ]
                       []
                   ]
              else []
        -- Module circles inside
        <> map (moduleCircleHATS callbacks node) node.modules
        <>
        [ -- Label background rect
          let charWidth = 7.8
              labelW = toNumber (String.length node.name) * charWidth + 8.0
              labelH = 18.0
              labelY = node.r + 15.0
          in elem Rect
            [ staticStr "class" "label-bg"
            , thunkedNum "x" (-(labelW / 2.0))
            , thunkedNum "y" (labelY - labelH + 4.0)
            , thunkedNum "width" labelW
            , thunkedNum "height" labelH
            , staticStr "rx" "3"
            , staticStr "fill" "#F5F0E6"
            , staticStr "fill-opacity" "0.85"
            ]
            []
        , -- Package label below (click → treemap view)
          withBehaviors [ onClick (callbacks.onPackageLabelClick node.name) ]
          $ elem Text
            [ staticStr "class" "package-label"
            , thunkedNum "y" (node.r + 15.0)
            , staticStr "text-anchor" "middle"
            , staticStr "fill" "#666"
            , staticStr "font-size" "13px"
            , staticStr "font-weight" "500"
            , staticStr "font-family" "system-ui, sans-serif"
            , staticStr "cursor" "pointer"  -- Indicate clickable
            , thunkedStr "textContent" node.name
            ]
            []
        ]
      )

-- | Module circle using HATS
moduleCircleHATS :: Callbacks -> PackedPackageNode -> PackedModuleCircle -> Tree
moduleCircleHATS callbacks node mod =
  withBehaviors
    [ onClick (callbacks.onModuleClick node.name mod.name)
    , onMouseEnter (callbacks.onModuleHover node.name (Just mod.name))
    , onMouseLeave (callbacks.onModuleHover node.name Nothing)
    , onCoordinatedHighlight
        { identify: mod.name
        , classify: \hoveredId ->
            if mod.name == hoveredId then Primary
            else if Array.elem hoveredId mod.imports then Related
            else if Array.elem hoveredId mod.importedBy then Related
            else Dimmed
        , group: Just "modules"
        }
    ]
  $ elem Group
      [ staticStr "class" "module-circle-group"
      , staticStr "cursor" "pointer"
      , thunkedStr "data-module" mod.name
      ]
      [ elem Circle
          [ staticStr "class" "module-circle"
          , thunkedNum "cx" mod.x
          , thunkedNum "cy" mod.y
          , thunkedNum "r" mod.r
          , thunkedStr "fill" mod.packageColor
          , staticStr "fill-opacity" "0.7"
          , staticStr "stroke" "white"
          , staticStr "stroke-width" "0.5"
          , staticStr "pointer-events" "all"
          ]
          []
      ]

-- =============================================================================
-- Color Functions
-- =============================================================================

-- | Categorical color palette for packages
packageColor :: Int -> String
packageColor idx =
  fromMaybe "#888" $ Array.index colors (idx `mod` Array.length colors)
  where
  colors =
    [ "#4e79a7"  -- Blue
    , "#f28e2b"  -- Orange
    , "#e15759"  -- Red
    , "#76b7b2"  -- Teal
    , "#59a14f"  -- Green
    , "#edc948"  -- Yellow
    , "#b07aa1"  -- Purple
    , "#ff9da7"  -- Pink
    , "#9c755f"  -- Brown
    , "#bab0ac"  -- Gray
    ]

-- =============================================================================
-- Cross-Package Dependency Links
-- =============================================================================

-- | Compute cross-package module import links
-- | Only includes imports where the target module is in a DIFFERENT package
computeCrossPackageLinks :: Array PackedPackageNode -> Array { fromModule :: String, toModule :: String }
computeCrossPackageLinks nodes =
  let
    -- Build a set of all module names per package
    moduleToPackage :: Map String String
    moduleToPackage = Map.fromFoldable $ nodes >>= \n ->
      n.modules <#> \m -> Tuple m.name n.name
  in
    nodes >>= \n ->
      n.modules >>= \m ->
        m.imports # Array.mapMaybe \imp ->
          case Map.lookup imp moduleToPackage of
            Just pkg | pkg /= n.name -> Just { fromModule: m.name, toModule: imp }
            _ -> Nothing

-- | Build absolute position map for all modules
-- | Module positions are relative to package center, so we add package (x, y)
buildModulePositionMap :: Array PackedPackageNode -> Map String { x :: Number, y :: Number }
buildModulePositionMap nodes =
  Map.fromFoldable $ nodes >>= \n ->
    n.modules <#> \m ->
      Tuple m.name { x: n.x + m.x, y: n.y + m.y }

-- | Render all cross-package module dependency links as curved paths
renderModuleDependencyLinks
  :: Map String { x :: Number, y :: Number }
  -> Array { fromModule :: String, toModule :: String }
  -> Tree
renderModuleDependencyLinks posMap links =
  let paths = links # Array.mapMaybe \link -> do
        fromPos <- Map.lookup link.fromModule posMap
        toPos <- Map.lookup link.toModule posMap
        pure $ moduleLinkPath fromPos toPos link.fromModule link.toModule
  in elem Group
    [ staticStr "class" "dependency-links"
    , staticStr "pointer-events" "none"
    , thunkedStr "data-link-count" (show (Array.length paths))
    ]
    paths

-- | Render a curved link between two module positions
moduleLinkPath
  :: { x :: Number, y :: Number }
  -> { x :: Number, y :: Number }
  -> String -> String -> Tree
moduleLinkPath from to fromName toName =
  let
    dx = to.x - from.x
    dy = to.y - from.y
    dist = sqrt (dx * dx + dy * dy)

    curvature = min 0.3 (dist / 500.0) * 40.0
    perpX = if dist > 0.0 then -dy / dist * curvature else 0.0
    perpY = if dist > 0.0 then dx / dist * curvature else 0.0
    midX = (from.x + to.x) / 2.0
    midY = (from.y + to.y) / 2.0
    cx = midX + perpX
    cy = midY + perpY

    pathD = "M " <> show from.x <> " " <> show from.y
         <> " Q " <> show cx <> " " <> show cy
         <> " " <> show to.x <> " " <> show to.y

    linkId = fromName <> "→" <> toName
  in
    withBehaviors
      [ onCoordinatedHighlight
          { identify: linkId
          , classify: \hoveredId ->
              if hoveredId == fromName || hoveredId == toName then Related
              else Dimmed
          , group: Just "modules"  -- Same group as module circles
          }
      ]
    $ elem Path
        [ thunkedStr "d" pathD
        , staticStr "fill" "none"
        , staticStr "stroke" "#f59e0b"
        , staticStr "stroke-width" "0.75"
        , staticStr "stroke-opacity" "0"  -- Invisible until hover (matches treemap pattern)
        , staticStr "class" "dependency-link"
        ]
        []

-- | Create package nodes tree WITH cross-package dependency links
-- | Used after simulation converges (Completed event) when positions are final
createPackageNodesTreeWithLinks :: Callbacks -> Array PackedPackageNode -> Tree
createPackageNodesTreeWithLinks callbacks nodes =
  let posMap = buildModulePositionMap nodes
      links = computeCrossPackageLinks nodes
  in elem Group []
       [ forEach "packages" Group nodes nodeKey (packageNodeHATS callbacks)
       , renderModuleDependencyLinks posMap links
       ]
  where
  nodeKey :: PackedPackageNode -> String
  nodeKey n = n.name
