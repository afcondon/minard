-- | Dependency Neighborhood - Force-directed overlay showing callers/callees
-- |
-- | Renders a force-directed graph centered on a focused declaration,
-- | showing its immediate dependency neighborhood: which declarations
-- | it calls (callees) and which call it (callers), including cross-module
-- | relationships. Designed as an overlay on top of a dimmed declaration treemap.
module CE2.Viz.DependencyNeighborhood
  ( Config
  , NeighborhoodHandle
  , render
  , cleanup
  ) where

import Prelude

import Data.Array as Array
import Data.Int (toNumber) as Data.Int
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Nullable as Nullable
import Data.Number (sqrt, min, cos, sin) as Data.Number
import Data.String as String
import Effect (Effect)
import Effect.Console (log)

-- HATS
import Hylograph.HATS (Tree, elem, staticStr, thunkedStr, thunkedNum, forEach, withBehaviors, onClick)
import Hylograph.HATS.InterpreterTick (rerender, clearContainer)
import Hylograph.Internal.Selection.Types (ElementType(..))
-- Simulation
import Hylograph.Simulation
  ( runSimulation
  , Engine(..)
  , SimulationEvent(..)
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

import CE2.Data.Loader (V2FunctionCall)
import CE2.Viz.ModuleTreemapEnriched (kindColor)

-- =============================================================================
-- Types
-- =============================================================================

-- | Direction of dependency relationship
data Direction = Self | Caller | Callee

derive instance eqDirection :: Eq Direction

-- | Configuration for the neighborhood overlay
type Config =
  { containerSelector :: String
  , width :: Number
  , height :: Number
  , packageName :: String
  , moduleName :: String
  , declarationName :: String
  , declarationKind :: String
  , onDeclarationClick :: Maybe (String -> String -> String -> Effect Unit)
  }

-- | Handle for cleanup
type NeighborhoodHandle = { stop :: Effect Unit }

-- | A neighbor in the dependency graph
type Neighbor =
  { name :: String
  , moduleName :: String
  , direction :: Direction
  , kind :: String  -- declaration kind if known, "" if cross-module
  }

-- | Simulation node for the neighborhood graph
type NeighborNodeRow =
  ( name :: String
  , moduleName :: String
  , direction :: Direction
  , displayLabel :: String
  , r :: Number
  , color :: String
  , targetX :: Number
  , targetY :: Number
  )

type NeighborNode = SimulationNode NeighborNodeRow

-- =============================================================================
-- Public API
-- =============================================================================

-- | Render the dependency neighborhood overlay
render :: Config -> Map Int (Array V2FunctionCall) -> Effect NeighborhoodHandle
render config callsMap = do
  let neighbors = extractNeighbors config callsMap
  log $ "[DependencyNeighborhood] " <> config.declarationName
      <> ": " <> show (Array.length neighbors) <> " neighbors"

  if Array.null neighbors then do
    -- No neighbors: render a simple "no dependencies" label
    renderEmptyState config
    pure { stop: pure unit }
  else do
    -- Prepare simulation nodes
    let nodes = prepareNodes config neighbors
    -- Start simulation and render
    startSimulation config nodes

-- | Clean up the neighborhood overlay
cleanup :: String -> Effect Unit
cleanup selector = clearContainer selector

-- =============================================================================
-- Data Extraction
-- =============================================================================

-- | Extract neighbors from the function calls map
extractNeighbors :: Config -> Map Int (Array V2FunctionCall) -> Array Neighbor
extractNeighbors config callsMap =
  let
    allCalls :: Array V2FunctionCall
    allCalls = Array.concatMap identity (Array.fromFoldable (Map.values callsMap))

    -- Outgoing: this declaration calls others
    callees :: Array Neighbor
    callees = allCalls
      # Array.filter (\c -> c.callerName == config.declarationName)
      -- Don't include self-calls
      # Array.filter (\c -> not (c.calleeName == config.declarationName && c.calleeModule == config.moduleName))
      <#> \c ->
        { name: c.calleeName
        , moduleName: c.calleeModule
        , direction: Callee
        , kind: ""
        }

    -- Incoming: others call this declaration
    callers :: Array Neighbor
    callers = allCalls
      # Array.filter (\c -> c.calleeName == config.declarationName && c.calleeModule == config.moduleName)
      -- Don't include self-calls
      # Array.filter (\c -> not (c.callerName == config.declarationName))
      <#> \c ->
        { name: c.callerName
        , moduleName: config.moduleName  -- callerName is always from the same module in our data
        , direction: Caller
        , kind: ""
        }

    -- Deduplicate by (name, moduleName)
    dedup :: Array Neighbor -> Array Neighbor
    dedup = Array.nubBy (\a b -> compare (key a) (key b))
      where key n = n.moduleName <> "." <> n.name
  in
    dedup (callees <> callers)

-- =============================================================================
-- Node Preparation
-- =============================================================================

-- | Prepare simulation nodes: center node (self) + neighbor nodes
prepareNodes :: Config -> Array Neighbor -> Array NeighborNode
prepareNodes config neighbors =
  let
    centerX = config.width / 2.0
    centerY = config.height / 2.0

    -- Center node (the focused declaration)
    selfNode :: NeighborNode
    selfNode =
      { id: 0
      , x: centerX
      , y: centerY
      , vx: 0.0
      , vy: 0.0
      , fx: Nullable.notNull centerX
      , fy: Nullable.notNull centerY
      , name: config.declarationName
      , moduleName: config.moduleName
      , direction: Self
      , displayLabel: config.declarationName
      , r: 20.0
      , color: kindColor config.declarationKind
      , targetX: centerX
      , targetY: centerY
      }

    -- Neighbor nodes arranged in initial arc
    neighborNodes :: Array NeighborNode
    neighborNodes = Array.mapWithIndex (\idx neighbor ->
      let
        -- Spread initial positions in a circle around center
        n = Array.length neighbors
        angle = 2.0 * 3.14159265 * (Data.Int.toNumber idx) / (Data.Int.toNumber (max 1 n))
        initDist = 80.0
        isCrossModule = neighbor.moduleName /= config.moduleName
        label = if isCrossModule
                then shortModName neighbor.moduleName <> "." <> neighbor.name
                else neighbor.name
        nodeColor = case neighbor.direction of
          Caller -> "#0ea5e9"  -- teal/sky
          Callee -> "#f59e0b"  -- amber
          Self -> kindColor config.declarationKind
      in
        { id: idx + 1
        , x: centerX + initDist * Data.Number.cos angle
        , y: centerY + initDist * Data.Number.sin angle
        , vx: 0.0
        , vy: 0.0
        , fx: Nullable.null
        , fy: Nullable.null
        , name: neighbor.name
        , moduleName: neighbor.moduleName
        , direction: neighbor.direction
        , displayLabel: label
        , r: 12.0
        , color: nodeColor
        , targetX: centerX
        , targetY: centerY
        }
    ) neighbors
  in
    Array.cons selfNode neighborNodes

-- | Shorten a module name (take last segment)
shortModName :: String -> String
shortModName fullName =
  case Array.last (String.split (String.Pattern ".") fullName) of
    Just s -> s
    Nothing -> fullName

-- =============================================================================
-- Simulation
-- =============================================================================

-- | Start the force simulation and render
startSimulation :: Config -> Array NeighborNode -> Effect NeighborhoodHandle
startSimulation config nodes = do
  let centerX = config.width / 2.0
      centerY = config.height / 2.0
      nodesGroupId = config.containerSelector <> " > svg > #neighborhood-nodes"

  -- Render SVG container
  let containerTree = elem SVG
        [ staticStr "viewBox" $ "0 0 " <> show config.width <> " " <> show config.height
        , staticStr "width" "100%"
        , staticStr "height" "100%"
        , staticStr "preserveAspectRatio" "xMidYMid meet"
        , staticStr "style" "display: block;"
        ]
        [ elem Group [ staticStr "id" "neighborhood-links" ] []
        , elem Group [ staticStr "id" "neighborhood-nodes" ] []
        ]
  _ <- rerender config.containerSelector containerTree

  -- Start simulation
  { handle, events } <- runSimulation
    { engine: D3
    , setup: setup "neighborhood"
        [ manyBody "charge" # withStrength (static (-80.0))
        , collide "collision" # withRadius (dynamic \n -> n.r + 4.0) # withStrength (static 0.8)
        , positionX "cx" # withX (static centerX) # withStrength (static 0.05)
        , positionY "cy" # withY (static centerY) # withStrength (static 0.05)
        ]
    , nodes: nodes
    , links: []
    , container: nodesGroupId
    , alphaMin: 0.01
    }

  -- Initial render
  initialNodes <- handle.getNodes
  renderNodesHATS config initialNodes

  -- Subscribe to events
  _ <- subscribe events \event -> case event of
    Tick _ -> do
      currentNodes <- handle.getNodes
      -- Full rerender on tick (needed for links which connect to moving nodes)
      renderFullHATS config currentNodes
    Completed -> do
      log "[DependencyNeighborhood] Simulation completed"
      finalNodes <- handle.getNodes
      renderFullHATS config finalNodes
    Started -> log "[DependencyNeighborhood] Simulation started"
    Stopped -> pure unit

  pure { stop: handle.stop }

-- =============================================================================
-- HATS Rendering
-- =============================================================================

-- | Render nodes using HATS (initial render)
renderNodesHATS :: Config -> Array NeighborNode -> Effect Unit
renderNodesHATS config nodes = do
  let tree = buildNodesTree config nodes
  _ <- rerender (config.containerSelector <> " > svg > #neighborhood-nodes") tree
  pure unit

-- | Full render including links (called on tick)
renderFullHATS :: Config -> Array NeighborNode -> Effect Unit
renderFullHATS config nodes = do
  -- Find the center node
  let mCenter = Array.find (\n -> n.direction == Self) nodes
  case mCenter of
    Nothing -> pure unit
    Just center -> do
      let neighbors = Array.filter (\n -> n.direction /= Self) nodes
      -- Render links
      let linkTree = buildLinksTree center neighbors
      _ <- rerender (config.containerSelector <> " > svg > #neighborhood-links") linkTree
      -- Render nodes
      let nodeTree = buildNodesTree config nodes
      _ <- rerender (config.containerSelector <> " > svg > #neighborhood-nodes") nodeTree
      pure unit

-- | Build the nodes HATS tree
buildNodesTree :: Config -> Array NeighborNode -> Tree
buildNodesTree config nodes =
  forEach "nodes" Group nodes (\n -> n.moduleName <> "." <> n.name) (neighborNodeElem config)

-- | Render a single neighbor node
neighborNodeElem :: Config -> NeighborNode -> Tree
neighborNodeElem config node =
  let
    isSelf = node.direction == Self
    isCrossModule = node.moduleName /= config.moduleName
    clickBehavior = case config.onDeclarationClick of
      Nothing -> []
      Just handler ->
        if isSelf then []
        else [ onClick (handler config.packageName node.moduleName node.name) ]
    strokeColor = if isSelf then "#fff" else "rgba(255,255,255,0.6)"
    strokeWidth = if isSelf then "2.5" else "1.5"
    fontWeight = if isSelf then "700" else "500"
    fontSize = if isSelf then "11" else "9"
    labelY = node.r + 14.0
  in
    withBehaviors clickBehavior
    $ elem Group
        [ thunkedStr "transform" ("translate(" <> show node.x <> "," <> show node.y <> ")")
        , staticStr "class" "neighborhood-node"
        , staticStr "cursor" (if isSelf then "default" else "pointer")
        ]
        [ -- Node circle
          elem Circle
            [ staticStr "cx" "0"
            , staticStr "cy" "0"
            , thunkedNum "r" node.r
            , thunkedStr "fill" node.color
            , staticStr "fill-opacity" "0.85"
            , staticStr "stroke" strokeColor
            , staticStr "stroke-width" strokeWidth
            ]
            []
        -- Primary label (name)
        , elem Text
            [ staticStr "x" "0"
            , thunkedNum "y" labelY
            , staticStr "text-anchor" "middle"
            , staticStr "font-size" fontSize
            , staticStr "fill" "rgba(255, 255, 255, 0.9)"
            , staticStr "font-family" "system-ui, sans-serif"
            , staticStr "font-weight" fontWeight
            , staticStr "pointer-events" "none"
            , thunkedStr "textContent" node.displayLabel
            ]
            []
        -- Cross-module annotation
        , if isCrossModule && not isSelf
          then elem Text
            [ staticStr "x" "0"
            , thunkedNum "y" (labelY + 11.0)
            , staticStr "text-anchor" "middle"
            , staticStr "font-size" "7"
            , staticStr "fill" "rgba(255, 255, 255, 0.5)"
            , staticStr "font-family" "system-ui, sans-serif"
            , staticStr "font-style" "italic"
            , staticStr "pointer-events" "none"
            , thunkedStr "textContent" (shortModName node.moduleName)
            ]
            []
          else elem Group [] []
        ]

-- | Build the links HATS tree
buildLinksTree :: NeighborNode -> Array NeighborNode -> Tree
buildLinksTree center neighbors =
  forEach "links" Group neighbors
    (\n -> n.moduleName <> "." <> n.name)
    (neighborLinkElem center)

-- | Render a link from center to a neighbor
neighborLinkElem :: NeighborNode -> NeighborNode -> Tree
neighborLinkElem center neighbor =
  let
    linkColor = case neighbor.direction of
      Caller -> "#0ea5e9"  -- sky
      Callee -> "#f59e0b"  -- amber
      Self -> "#888"

    -- Curved path
    dx = neighbor.x - center.x
    dy = neighbor.y - center.y
    dist = Data.Number.sqrt (dx * dx + dy * dy)
    curvature = Data.Number.min 0.3 (dist / 400.0) * 25.0
    perpX = if dist > 0.0 then -dy / dist * curvature else 0.0
    perpY = if dist > 0.0 then dx / dist * curvature else 0.0
    mx = (center.x + neighbor.x) / 2.0 + perpX
    my = (center.y + neighbor.y) / 2.0 + perpY

    pathD = "M " <> show center.x <> " " <> show center.y
         <> " Q " <> show mx <> " " <> show my
         <> " " <> show neighbor.x <> " " <> show neighbor.y
  in
    elem Path
      [ thunkedStr "d" pathD
      , staticStr "fill" "none"
      , staticStr "stroke" linkColor
      , staticStr "stroke-width" "1.5"
      , staticStr "stroke-opacity" "0.5"
      , staticStr "class" "neighborhood-link"
      , staticStr "pointer-events" "none"
      ]
      []

-- =============================================================================
-- Empty State
-- =============================================================================

-- | Render a simple message when there are no dependencies
renderEmptyState :: Config -> Effect Unit
renderEmptyState config = do
  let tree = elem SVG
        [ staticStr "viewBox" $ "0 0 " <> show config.width <> " " <> show config.height
        , staticStr "width" "100%"
        , staticStr "height" "100%"
        , staticStr "preserveAspectRatio" "xMidYMid meet"
        , staticStr "style" "display: block;"
        ]
        [ -- Center node only
          elem Group
            [ thunkedStr "transform" ("translate(" <> show (config.width / 2.0) <> "," <> show (config.height / 2.0) <> ")")
            ]
            [ elem Circle
                [ staticStr "cx" "0"
                , staticStr "cy" "0"
                , staticStr "r" "20"
                , thunkedStr "fill" (kindColor config.declarationKind)
                , staticStr "fill-opacity" "0.85"
                , staticStr "stroke" "#fff"
                , staticStr "stroke-width" "2.5"
                ]
                []
            , elem Text
                [ staticStr "x" "0"
                , staticStr "y" "34"
                , staticStr "text-anchor" "middle"
                , staticStr "font-size" "11"
                , staticStr "fill" "rgba(255, 255, 255, 0.9)"
                , staticStr "font-family" "system-ui, sans-serif"
                , staticStr "font-weight" "700"
                , staticStr "pointer-events" "none"
                , thunkedStr "textContent" config.declarationName
                ]
                []
            , elem Text
                [ staticStr "x" "0"
                , staticStr "y" "50"
                , staticStr "text-anchor" "middle"
                , staticStr "font-size" "8"
                , staticStr "fill" "rgba(255, 255, 255, 0.4)"
                , staticStr "font-family" "system-ui, sans-serif"
                , staticStr "font-style" "italic"
                , staticStr "pointer-events" "none"
                , staticStr "textContent" "no dependencies found"
                ]
                []
            ]
        ]
  _ <- rerender config.containerSelector tree
  pure unit
