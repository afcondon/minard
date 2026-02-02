-- | Force-Directed Graph Visualization for Site Explorer
-- |
-- | Shows routes as nodes with links representing navigation paths.
-- | Clustering: Unreachable/Archived nodes cluster left, Reachable/Extra cluster right.
-- | Node colors indicate status: reachable (gold), unreachable (burgundy),
-- | extra (coral), archived (grey).
module SiteExplorer.ForceGraph
  ( initForceGraph
  , ForceGraphHandle
  ) where

import Prelude

import Data.Array as Array
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable (null)
import Data.String as String
import Data.Traversable (traverse)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Random (random)
import Effect.Ref as Ref
import Hylograph.ForceEngine.Core as Core
import Hylograph.ForceEngine.Simulation as Sim
import Hylograph.ForceEngine.Simulation (SimulationNode)
import Hylograph.ForceEngine.Types (ForceSpec(..), defaultCollide, defaultLink, defaultManyBody)
import Hylograph.HATS (Tree, elem, forEach, staticStr, staticNum, thunkedNum, thunkedStr, onClick, withBehaviors)
import Hylograph.HATS.InterpreterTick (rerender)
import Hylograph.Internal.Selection.Types (ElementType(..))
import SiteExplorer.Types (RouteInfo, RouteStatus(..))

-- =============================================================================
-- Types
-- =============================================================================

-- | Configuration for the force graph
type ForceConfig =
  { width :: Number
  , height :: Number
  , nodeRadius :: Number
  , linkDistance :: Number
  , reachableCenterX :: Number
  , orphanCenterX :: Number
  , centerY :: Number
  }

defaultConfig :: ForceConfig
defaultConfig =
  { width: 800.0
  , height: 600.0
  , nodeRadius: 10.0
  , linkDistance: 60.0
  , reachableCenterX: 800.0 * (2.0 / 3.0)  -- 2/3 for reachable (right)
  , orphanCenterX: 800.0 / 3.0              -- 1/3 for unreachable/archived (left)
  , centerY: 300.0
  }

-- | Colors - Rose Adler Art Deco palette
colors ::
  { reachable :: String
  , unreachable :: String
  , extra :: String
  , archived :: String
  , link :: String
  , linkHighlight :: String
  , text :: String
  , bg :: String
  , clusterLabel :: String
  }
colors =
  { reachable: "#c9a227"    -- Gold
  , unreachable: "#722f37"  -- Burgundy
  , extra: "#d4a574"        -- Blush/coral
  , archived: "#8a7f72"     -- Taupe
  , link: "#a89f94"         -- Warm grey
  , linkHighlight: "#c9a227" -- Gold for highlighted links
  , text: "#1a1a2e"         -- Navy
  , bg: "#faf6ed"           -- Cream
  , clusterLabel: "#8a7f72" -- Taupe for cluster labels
  }

-- | Simulation node for a route
-- | Uses gridX/gridY for ForceXGrid/ForceYGrid targeting
type RouteNode = SimulationNode
  ( name :: String
  , urlPath :: String
  , status :: RouteStatus
  , depth :: Int
  , gridX :: Number  -- Target X for ForceXGrid
  , gridY :: Number  -- Target Y for ForceYGrid
  )

-- | Link between routes
type RouteLink =
  { source :: Int
  , target :: Int
  }

-- | Handle returned from initialization
type ForceGraphHandle =
  { stop :: Effect Unit
  , onNodeClick :: (String -> Effect Unit) -> Effect Unit
  }

-- =============================================================================
-- HATS Trees
-- =============================================================================

-- | Container SVG structure with cluster labels
-- | Uses viewBox for coordinate system, 100% width/height to fill container
containerTree :: ForceConfig -> Tree
containerTree config =
  elem SVG
    [ staticStr "width" "100%"
    , staticStr "height" "100%"
    , staticStr "viewBox" ("0 0 " <> show config.width <> " " <> show config.height)
    , staticStr "preserveAspectRatio" "xMidYMid meet"
    , staticStr "class" "force-graph-svg"
    ]
    [ -- Cluster labels
      elem Group [ staticStr "class" "cluster-labels" ]
        [ elem Text
            [ staticNum "x" config.orphanCenterX
            , staticNum "y" 30.0
            , staticStr "text-anchor" "middle"
            , staticStr "class" "cluster-label"
            , staticStr "textContent" "UNREACHABLE"
            ] []
        , elem Text
            [ staticNum "x" config.reachableCenterX
            , staticNum "y" 30.0
            , staticStr "text-anchor" "middle"
            , staticStr "class" "cluster-label"
            , staticStr "textContent" "REACHABLE"
            ] []
        ]
    , elem Group [ staticStr "class" "links", staticStr "id" "force-graph-links" ] []
    , elem Group [ staticStr "class" "nodes", staticStr "id" "force-graph-nodes" ] []
    , elem Group [ staticStr "class" "labels", staticStr "id" "force-graph-labels" ] []
    ]

-- | Links tree with arrow markers
linksTree :: Array RouteNode -> Array RouteLink -> Tree
linksTree nodes links =
  let
    nodeMap = Map.fromFoldable $ map (\n -> n.id /\ n) nodes
    getNode id = Map.lookup id nodeMap
  in
  forEach "links" Line links (\l -> show l.source <> "-" <> show l.target) \link ->
    case getNode link.source, getNode link.target of
      Just src, Just tgt ->
        elem Line
          [ thunkedNum "x1" src.x
          , thunkedNum "y1" src.y
          , thunkedNum "x2" tgt.x
          , thunkedNum "y2" tgt.y
          , staticStr "class" "link"
          , staticStr "stroke" colors.link
          , staticNum "stroke-width" 2.0
          ] []
      _, _ ->
        elem Line [] []

-- | Nodes tree with .node class for CSS styling and click handler
nodesTree :: ForceConfig -> (String -> Effect Unit) -> Array RouteNode -> Tree
nodesTree config onNodeClick nodes =
  forEach "nodes" Circle nodes (\n -> show n.id) \node ->
    let
      -- Node radius based on depth (like original)
      radius = case node.depth of
        0 -> 16.0
        1 -> 12.0
        _ -> config.nodeRadius
    in
    withBehaviors [ onClick (onNodeClick node.name) ] $
      elem Circle
        [ thunkedNum "cx" node.x
        , thunkedNum "cy" node.y
        , staticNum "r" radius
        , thunkedStr "fill" (statusColor node.status)
        , staticStr "class" "node"  -- For CSS drop-shadow
        , staticStr "data-route" node.name
        , staticStr "cursor" "pointer"
        ] []

-- | Labels tree (route names)
labelsTree :: Array RouteNode -> Tree
labelsTree nodes =
  forEach "labels" Text nodes (\n -> show n.id) \node ->
    let
      -- Label offset based on depth (like original)
      dy = case node.depth of
        0 -> -20.0
        _ -> -14.0
    in
    elem Text
      [ thunkedNum "x" node.x
      , thunkedNum "y" (node.y + dy)
      , staticStr "text-anchor" "middle"
      , staticStr "class" "label"
      , thunkedStr "textContent" (shortName node.name)
      ] []

-- | Get color for route status
statusColor :: RouteStatus -> String
statusColor = case _ of
  Reachable -> colors.reachable
  Unreachable -> colors.unreachable
  Extra -> colors.extra
  Archived -> colors.archived

-- | Shorten route name for label (like original: max 20 chars, truncate to 18 + "...")
shortName :: String -> String
shortName name =
  let len = String.length name
  in if len > 20
    then String.take 18 name <> "..."
    else name

-- | Check if a status should cluster on the left (orphan cluster)
isOrphan :: RouteStatus -> Boolean
isOrphan = case _ of
  Unreachable -> true
  Archived -> true
  _ -> false

-- =============================================================================
-- Initialization
-- =============================================================================

-- | Initialize the force graph visualization
initForceGraph :: String -> Array RouteInfo -> Effect ForceGraphHandle
initForceGraph selector routes = do
  let config = defaultConfig

  -- Convert routes to simulation nodes with cluster targets
  nodes <- createRouteNodes config routes

  -- Build links from foundFrom relationships
  let links = buildLinks routes

  -- Click callback ref (initialized with no-op, set via onNodeClick)
  clickCallbackRef <- Ref.new (\_ -> pure unit :: Effect Unit)

  -- State ref for tick updates (includes callback ref for access during tick)
  stateRef <- Ref.new { nodes, links, config, clickCallback: clickCallbackRef }

  -- Create simulation
  sim <- Sim.create Sim.defaultConfig
  Sim.setNodes nodes sim

  -- Set links and add link force
  when (Array.length links > 0) do
    Sim.setLinks links sim
    Sim.addForce (Link "link" defaultLink
      { distance = config.linkDistance
      , strength = 1.0
      , iterations = 1
      }) sim

  -- Add clustering forces using ForceXGrid/ForceYGrid
  -- These pull each node toward its individual gridX/gridY target
  let forceXHandle = Core.createForceXGrid 0.08  -- strength
  let forceYHandle = Core.createForceYGrid 0.05  -- strength
  _ <- Core.initializeForce forceXHandle nodes
  _ <- Core.initializeForce forceYHandle nodes
  Sim.addForceHandle "clusterX" forceXHandle sim
  Sim.addForceHandle "clusterY" forceYHandle sim

  -- Collision prevents overlap
  Sim.addForce (Collide "collide" defaultCollide
    { radius = config.nodeRadius + 5.0
    , strength = 0.7
    , iterations = 2
    }) sim

  -- Many-body repulsion - stronger for reachable, weaker for orphans
  Sim.addForce (ManyBody "charge" defaultManyBody { strength = -100.0 }) sim

  -- Render initial DOM structure using HATS
  _ <- rerender selector (containerTree config)

  -- Tick handler - update positions using HATS
  Sim.onTick (tick stateRef) sim
  Sim.start sim

  pure
    { stop: Sim.stop sim
    , onNodeClick: \callback -> Ref.write callback clickCallbackRef
    }

-- | Tick handler - renders with current positions using HATS
tick :: Ref.Ref { nodes :: Array RouteNode, links :: Array RouteLink, config :: ForceConfig, clickCallback :: Ref.Ref (String -> Effect Unit) } -> Effect Unit
tick stateRef = do
  state <- Ref.read stateRef
  callback <- Ref.read state.clickCallback
  _ <- rerender "#force-graph-links" (linksTree state.nodes state.links)
  _ <- rerender "#force-graph-nodes" (nodesTree state.config callback state.nodes)
  _ <- rerender "#force-graph-labels" (labelsTree state.nodes)
  pure unit

-- | Create route nodes from RouteInfo data
createRouteNodes :: ForceConfig -> Array RouteInfo -> Effect (Array RouteNode)
createRouteNodes config routes = do
  let jitterRange = 50.0

  traverse (\{ route, idx } -> do
    dx <- (\r -> (r - 0.5) * jitterRange) <$> random
    dy <- (\r -> (r - 0.5) * jitterRange) <$> random

    -- Calculate initial position based on cluster
    let targetX = if isOrphan route.status
          then config.orphanCenterX
          else config.reachableCenterX

    pure
      { id: idx
      , x: targetX + dx
      , y: config.centerY + dy
      , vx: 0.0
      , vy: 0.0
      , fx: null
      , fy: null
      , name: route.name
      , urlPath: route.urlPath
      , status: route.status
      , depth: fromMaybe 99 route.depth
      , gridX: targetX        -- Target X for ForceXGrid
      , gridY: config.centerY -- Target Y for ForceYGrid
      }
  ) (Array.mapWithIndex (\idx route -> { idx, route }) routes)

-- | Build links from foundFrom relationships
buildLinks :: Array RouteInfo -> Array RouteLink
buildLinks routes =
  let
    pathToIdx = Map.fromFoldable $
      Array.mapWithIndex (\idx r -> r.urlPath /\ idx) routes
  in
  Array.catMaybes $ map (\{ route, idx } ->
    case route.foundFrom of
      Nothing -> Nothing
      Just fromPath ->
        case Map.lookup fromPath pathToIdx of
          Just sourceIdx -> Just { source: sourceIdx, target: idx }
          Nothing -> Nothing
  ) (Array.mapWithIndex (\idx route -> { idx, route }) routes)
