-- | Package Set Beeswarm View (HATS Version)
-- |
-- | Visualizes the full PureScript package registry as a horizontal beeswarm.
-- | - Circles represent packages, force-positioned along a horizontal axis
-- | - X position attracted to topo layer (non-uniform spacing by layer population)
-- | - Y position determined by force collision (spreads out crowded layers)
-- | - Colors mapped to publish date (warm amber → cool teal)
-- | - Thin black outlines, transparent to layer over other views
-- |
-- | Uses PSD3.Simulation for force simulation.
-- | Uses renderNodes: false pattern - simulation runs physics, HATS handles rendering.
module CE2.Viz.PackageSetBeeswarm
  ( Config
  , PackageNode
  , PackageNodeRow
  , DateRange
  , BeeswarmHandle
  , InitialPosition
  , render
  , renderWithPositions
  , setScope
  , updateColors
  , cleanup
  , prepareNodes
  , computeDateRange
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl)
import Data.Int (toNumber, fromString)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable as Nullable
import Data.Number (sqrt)
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Ref as Ref

-- PSD3 HATS Imports
import Hylograph.HATS (Tree, elem, staticStr, staticNum, thunkedStr, thunkedNum, forEach, withBehaviors, onCoordinatedHighlight, onClick)
import Hylograph.HATS.InterpreterTick (rerender, clearContainer)
import Hylograph.Internal.Selection.Types (ElementType(..))
import Hylograph.Internal.Behavior.Types (HighlightClass(..))
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
import Hylograph.ForceEngine.Setup (withAlphaDecay)

import CE2.Data.Loader (PackageSetPackage)
import CE2.Types (ViewTheme, ColorMode(..))

-- =============================================================================
-- Types
-- =============================================================================

-- | Configuration for beeswarm rendering
type Config =
  { containerSelector :: String
  , width :: Number
  , height :: Number
  , projectPackages :: Set String  -- Packages our project uses
  , maxTopoLayer :: Int
  , colorMode :: ColorMode  -- How to color the packages
  , onPackageClick :: Maybe (String -> Effect Unit)  -- packageName -> Effect
  , enableHighlighting :: Boolean  -- Enable coordinated highlighting on hover
  }

-- | Package as a simulation node
type PackageNode = SimulationNode
  ( pkg :: PackageSetPackage
  , targetX :: Number      -- Target x position for forceX
  , r :: Number            -- Circle radius
  , isUsed :: Boolean      -- Is this one of our project's packages?
  , color :: String        -- Fill color based on publish date
  , dependsOn :: Array String      -- Package names this package depends on
  , dependedOnBy :: Array String   -- Package names that depend on this package
  )

-- | Date range for color computation
type DateRange =
  { minTime :: Number
  , maxTime :: Number
  }

-- | Row type for our PackageNode (for SimulationHandle type parameter)
type PackageNodeRow =
  ( pkg :: PackageSetPackage
  , targetX :: Number
  , r :: Number
  , isUsed :: Boolean
  , color :: String
  , dependsOn :: Array String
  , dependedOnBy :: Array String
  )

-- | Handle for controlling the beeswarm after render
type BeeswarmHandle =
  { simHandle :: SimulationHandle PackageNodeRow
  , config :: Config
  , dateRange :: DateRange
  , stop :: Effect Unit
  }

-- | Initial position for hero transition (from treemap)
type InitialPosition = { name :: String, x :: Number, y :: Number, r :: Number }

-- =============================================================================
-- Public API
-- =============================================================================

-- | Render the beeswarm view
render :: Config -> Array PackageSetPackage -> Effect BeeswarmHandle
render config packages = do
  -- Clear existing content
  clearContainer config.containerSelector

  -- Compute date range for color scaling
  let dateRange = computeDateRange packages

  -- Prepare nodes with computed positions, radii, and colors
  let nodes = prepareNodes config dateRange packages

  log $ "[PackageSetBeeswarm] Starting simulation with " <> show (Array.length nodes) <> " packages"

  -- Create SVG container using HATS
  renderSVGContainerHATS config

  -- Start simulation and get handle
  simHandle <- startSimulation config nodes

  pure { simHandle, config, dateRange, stop: simHandle.stop }

-- | Render beeswarm with initial positions (for hero transition from treemap)
renderWithPositions :: Config -> Array PackageSetPackage -> Array InitialPosition -> Effect BeeswarmHandle
renderWithPositions config packages positions = do
  -- Clear existing content
  clearContainer config.containerSelector

  -- Compute date range for color scaling
  let dateRange = computeDateRange packages

  -- Build position lookup map
  let positionMap = Map.fromFoldable $ positions <#> \p -> Tuple p.name { x: p.x, y: p.y, r: p.r }

  -- Prepare nodes at initial positions
  let nodes = prepareNodesAtPositions config dateRange positionMap packages

  log $ "[PackageSetBeeswarm] Starting hero transition with " <> show (Array.length nodes) <> " packages"
      <> ", " <> show (Map.size positionMap) <> " initial positions"

  -- Create SVG container using HATS
  renderSVGContainerHATS config

  -- Start simulation (will settle into beeswarm pattern)
  simHandle <- startSimulation config nodes

  pure { simHandle, config, dateRange, stop: simHandle.stop }

-- | Clean up (stop simulation and clear)
cleanup :: String -> Effect Unit
cleanup selector = clearContainer selector

-- | Update circle colors in place
updateColors :: String -> ViewTheme -> ColorMode -> Effect Unit
updateColors containerSelector _theme _colorMode = do
  log $ "[PackageSetBeeswarm] updateColors: " <> containerSelector
  -- For HATS, colors are re-rendered on each tick
  pure unit

-- | Update the beeswarm scope by adding/removing packages
-- | Update the beeswarm scope by adding/removing packages
-- | After updating simulation data, does a full HATS rerender to handle enter/exit
-- | (The fast tickUpdate path only handles position changes, not element changes)
setScope :: BeeswarmHandle -> Array PackageSetPackage -> Effect Unit
setScope handle packages = do
  log $ "[PackageSetBeeswarm] setScope START: " <> show (Array.length packages) <> " packages"

  let nodes = prepareNodes handle.config handle.dateRange packages
  log $ "[PackageSetBeeswarm] setScope: prepared " <> show (Array.length nodes) <> " nodes"

  result <- handle.simHandle.updateData nodes []

  log $ "[PackageSetBeeswarm] setScope END: " <>
    show (Array.length result.nodes.entered) <> " entered, " <>
    show (Array.length result.nodes.updated) <> " updated, " <>
    show (Array.length result.nodes.exited) <> " exited"

  -- Full HATS rerender to handle enter/exit (tickUpdate only does transforms)
  currentNodes <- handle.simHandle.getNodes
  renderNodesHATS handle.config currentNodes

-- =============================================================================
-- Node Preparation
-- =============================================================================

-- | Prepare packages with target positions, radii, and colors
prepareNodes :: Config -> DateRange -> Array PackageSetPackage -> Array PackageNode
prepareNodes config dateRange packages =
  Array.mapWithIndex (prepareNode config dateRange layerInfo dependedOnByMap) packages
  where
  layerCounts = countByLayer packages config.maxTopoLayer
  layerInfo = computeLayerPositions layerCounts config.width
  dependedOnByMap = buildDependedOnByMap packages

-- | Prepare nodes at specific initial positions (for hero transition)
prepareNodesAtPositions
  :: Config
  -> DateRange
  -> Map String { x :: Number, y :: Number, r :: Number }
  -> Array PackageSetPackage
  -> Array PackageNode
prepareNodesAtPositions config dateRange positionMap packages =
  Array.mapWithIndex (prepareNodeAtPosition config dateRange layerInfo positionMap dependedOnByMap) packages
  where
  layerCounts = countByLayer packages config.maxTopoLayer
  layerInfo = computeLayerPositions layerCounts config.width
  dependedOnByMap = buildDependedOnByMap packages

-- | Prepare a single node, using initial position from map if available
prepareNodeAtPosition
  :: Config
  -> DateRange
  -> Array Number
  -> Map String { x :: Number, y :: Number, r :: Number }
  -> Map String (Array String)
  -> Int
  -> PackageSetPackage
  -> PackageNode
prepareNodeAtPosition config dateRange cumulativeX positionMap dependedOnByMap idx pkg =
  let
    padding = 60.0
    targetX = padding + getLayerX pkg.topoLayer cumulativeX - config.width / 2.0

    kloc = toNumber pkg.totalLoc / 1000.0
    r = 4.0 + sqrt (max 0.1 kloc) * 3.0

    isUsed = Set.member pkg.name config.projectPackages

    -- Color based on colorMode
    color = getNodeColor config.colorMode pkg dateRange config.maxTopoLayer

    { x, y } = case Map.lookup pkg.name positionMap of
      Just pos ->
        { x: pos.x - config.width / 2.0
        , y: pos.y - config.height / 2.0
        }
      Nothing ->
        let pseudoRandom = toNumber ((idx * 17 + 31) `mod` 100) / 100.0 - 0.5
        in { x: targetX, y: pseudoRandom * config.height * 0.6 }

    -- Dependency relationships
    dependsOn = pkg.depends
    dependedOnBy = fromMaybe [] $ Map.lookup pkg.name dependedOnByMap
  in
    { id: pkg.id
    , x
    , y
    , vx: 0.0
    , vy: 0.0
    , fx: Nullable.null
    , fy: Nullable.null
    , pkg
    , targetX
    , r
    , isUsed
    , color
    , dependsOn
    , dependedOnBy
    }

-- | Prepare a single package as a simulation node
prepareNode :: Config -> DateRange -> Array Number -> Map String (Array String) -> Int -> PackageSetPackage -> PackageNode
prepareNode config dateRange cumulativeX dependedOnByMap idx pkg =
  let
    padding = 60.0
    targetX = padding + getLayerX pkg.topoLayer cumulativeX - config.width / 2.0

    kloc = toNumber pkg.totalLoc / 1000.0
    r = 4.0 + sqrt (max 0.1 kloc) * 3.0

    isUsed = Set.member pkg.name config.projectPackages

    -- Color based on colorMode
    color = getNodeColor config.colorMode pkg dateRange config.maxTopoLayer

    pseudoRandom = toNumber ((idx * 17 + 31) `mod` 100) / 100.0 - 0.5
    x = targetX
    y = pseudoRandom * config.height * 0.6

    -- Dependency relationships
    dependsOn = pkg.depends
    dependedOnBy = fromMaybe [] $ Map.lookup pkg.name dependedOnByMap
  in
    { id: pkg.id
    , x
    , y
    , vx: 0.0
    , vy: 0.0
    , fx: Nullable.null
    , fy: Nullable.null
    , pkg
    , targetX
    , r
    , isUsed
    , color
    , dependsOn
    , dependedOnBy
    }

-- | Count packages per layer
countByLayer :: Array PackageSetPackage -> Int -> Array Int
countByLayer packages maxLayer =
  Array.range 0 maxLayer <#> \layer ->
    Array.length $ Array.filter (\p -> p.topoLayer == layer) packages

-- | Compute cumulative x positions for non-uniform spacing
computeLayerPositions :: Array Int -> Number -> Array Number
computeLayerPositions counts totalWidth =
  let
    padding = 120.0
    availableWidth = totalWidth - padding
    minWidth = 15.0
    weights = counts <#> \c -> minWidth + sqrt (toNumber (max 1 c)) * 5.0
    totalWeight = Array.foldl (+) 0.0 weights
    scale = availableWidth / totalWeight
    normalized = weights <#> \w -> w * scale
  in
    scanl (+) 0.0 normalized

-- | Get x position for a layer
getLayerX :: Int -> Array Number -> Number
getLayerX layer cumulative =
  fromMaybe 0.0 $ Array.index cumulative layer

-- | Prefix sum (scan left)
scanl :: forall a b. (b -> a -> b) -> b -> Array a -> Array b
scanl f initial arr = Array.foldl go { acc: initial, result: [initial] } arr # _.result
  where
  go { acc, result } x =
    let newAcc = f acc x
    in { acc: newAcc, result: Array.snoc result newAcc }

-- | Build reverse dependency map: package name -> packages that depend on it
buildDependedOnByMap :: Array PackageSetPackage -> Map String (Array String)
buildDependedOnByMap packages =
  let
    -- For each package, create pairs (dependency, packageName) for each of its dependencies
    pairs :: Array (Tuple String String)
    pairs = packages # Array.concatMap \pkg ->
      pkg.depends <#> \dep -> Tuple dep pkg.name
  in
    -- Group by dependency name
    foldl (\acc (Tuple dep dependedOnBy) ->
      Map.alter (Just <<< Array.cons dependedOnBy <<< fromMaybe []) dep acc
    ) Map.empty pairs

-- =============================================================================
-- Date-based Coloring
-- =============================================================================

-- | Compute date range from packages
computeDateRange :: Array PackageSetPackage -> DateRange
computeDateRange packages =
  let
    timestamps = Array.catMaybes $ packages <#> \p ->
      p.publishedAt >>= parseTimestamp

    minTime = fromMaybe defaultMinTime $ Array.head $ Array.sort timestamps
    maxTime = fromMaybe defaultMaxTime $ Array.last $ Array.sort timestamps
  in
    { minTime, maxTime }
  where
  defaultMinTime = 1661990400000.0  -- 2022-09-01
  defaultMaxTime = 1737590400000.0  -- 2025-01-23

-- | Parse ISO date string to timestamp (milliseconds)
parseTimestamp :: String -> Maybe Number
parseTimestamp str =
  if String.length str >= 10
    then Just $ parseISODate (String.take 10 str)
    else Nothing

-- | Parse YYYY-MM-DD to approximate timestamp
parseISODate :: String -> Number
parseISODate str =
  let
    year = fromMaybe 2024 $ fromString (String.take 4 str)
    month = fromMaybe 1 $ fromString (String.take 2 (String.drop 5 str))
    day = fromMaybe 1 $ fromString (String.take 2 (String.drop 8 str))
    days = (year - 1970) * 365 + (month - 1) * 30 + day
  in
    toNumber days * 86400000.0

-- | Get node color based on ColorMode
getNodeColor :: ColorMode -> PackageSetPackage -> DateRange -> Int -> String
getNodeColor colorMode pkg dateRange maxTopoLayer = case colorMode of
  DefaultUniform -> "#4e79a7"  -- Default blue
  ProjectScope -> "#4e79a7"    -- Blue (highlight handled elsewhere)
  FullRegistryTopo -> getTopoColor pkg.topoLayer maxTopoLayer
  ProjectScopeTopo -> getTopoColor pkg.topoLayer maxTopoLayer
  PublishDate -> getDateColor pkg.publishedAt dateRange

-- | Get color based on topo layer (green-to-blue gradient)
getTopoColor :: Int -> Int -> String
getTopoColor layer maxLayer =
  let
    t = if maxLayer > 0 then toNumber layer / toNumber maxLayer else 0.0
    -- Green (120) for leaves → Blue (240) for roots
    h = 120.0 + t * 120.0
    s = 65.0
    l = 50.0
  in
    "hsl(" <> show h <> ", " <> show s <> "%, " <> show l <> "%)"

-- | Get color based on publish date
getDateColor :: Maybe String -> DateRange -> String
getDateColor mPublishedAt dateRange =
  case mPublishedAt >>= parseTimestamp of
    Nothing -> "#999"
    Just timestamp ->
      let
        range = dateRange.maxTime - dateRange.minTime
        t = if range > 0.0
              then (timestamp - dateRange.minTime) / range
              else 0.5
        t' = max 0.0 (min 1.0 t)
        h = 35.0 + t' * 140.0
        s = 70.0 - t' * 10.0
        l = 65.0 - t' * 10.0
      in
        "hsl(" <> show h <> ", " <> show s <> "%, " <> show l <> "%)"

-- =============================================================================
-- HATS Rendering
-- =============================================================================

-- | Create the SVG container structure using HATS
renderSVGContainerHATS :: Config -> Effect Unit
renderSVGContainerHATS config = do
  let
    vbX = -config.width / 2.0
    vbY = -config.height / 2.0
    viewBox = show vbX <> " " <> show vbY <> " " <> show config.width <> " " <> show config.height

    containerTree :: Tree
    containerTree =
      elem SVG
        [ staticStr "id" "package-set-beeswarm-svg"
        , staticStr "viewBox" viewBox
        , staticStr "width" "100%"
        , staticStr "height" "100%"
        , staticStr "class" "package-set-beeswarm"
        , staticStr "preserveAspectRatio" "xMidYMid meet"
        ]
        [ -- Left axis label
          elem Text
            [ staticNum "x" (-config.width / 2.0 + 60.0)
            , staticNum "y" (config.height / 2.0 - 15.0)
            , staticStr "font-size" "11"
            , staticStr "fill" "#666"
            , staticStr "textContent" "← Leaves (no deps)"
            ]
            []
        -- Right axis label
        , elem Text
            [ staticNum "x" (config.width / 2.0 - 60.0)
            , staticNum "y" (config.height / 2.0 - 15.0)
            , staticStr "text-anchor" "end"
            , staticStr "font-size" "11"
            , staticStr "fill" "#666"
            , staticStr "textContent" "Roots (most deps) →"
            ]
            []
        -- Nodes container
        , elem Group
            [ staticStr "id" "beeswarm-nodes"
            , staticStr "class" "packages"
            ]
            []
        ]
  _ <- rerender config.containerSelector containerTree
  pure unit

-- | Render all package nodes using HATS
renderNodesHATS :: Config -> Array PackageNode -> Effect Unit
renderNodesHATS config nodes = do
  let nodesTree = createPackageNodesTree config nodes
  _ <- rerender "#beeswarm-nodes" nodesTree
  pure unit

-- | Create the nodes tree for HATS rendering
createPackageNodesTree :: Config -> Array PackageNode -> Tree
createPackageNodesTree config nodes =
  forEach "packages" Group nodes nodeKey (packageNodeHATS config)
  where
  nodeKey :: PackageNode -> String
  nodeKey n = show n.id

-- | Package node template using HATS
packageNodeHATS :: Config -> PackageNode -> Tree
packageNodeHATS config node =
  let
    -- Build behaviors based on config
    behaviors =
      (if config.enableHighlighting
        then [ onCoordinatedHighlight
                 { identify: node.pkg.name
                 , classify: \hoveredPkg ->
                     if node.pkg.name == hoveredPkg then Primary
                     -- If hoveredPkg depends on me, I'm upstream (green)
                     else if Array.elem hoveredPkg node.dependedOnBy then Upstream
                     -- If I depend on hoveredPkg, I'm downstream (blue)
                     else if Array.elem hoveredPkg node.dependsOn then Downstream
                     else Dimmed
                 , group: Nothing  -- Global coordination
                 }
             ]
        else [])
      <> case config.onPackageClick of
           Just clickHandler -> [ onClick (clickHandler node.pkg.name) ]
           Nothing -> []
  in
    withBehaviors behaviors $
      elem Group
        [ thunkedStr "transform" ("translate(" <> show node.x <> "," <> show node.y <> ")")
        , thunkedStr "class" ("package-group" <> if node.isUsed then " project-package" else "")
        , thunkedStr "data-id" (show node.id)  -- Required for tickUpdate
        , thunkedStr "data-name" node.pkg.name
        , thunkedStr "data-layer" (show node.pkg.topoLayer)
        ]
        [ -- Package circle
          elem Circle
            [ staticStr "cx" "0"
            , staticStr "cy" "0"
            , thunkedNum "r" node.r
            , thunkedStr "fill" node.color
            , staticStr "stroke" "#333"
            , thunkedStr "stroke-width" (if node.isUsed then "1.5" else "0.5")
            , staticStr "opacity" "1.0"
            , staticStr "cursor" "pointer"
            , staticStr "class" "package-circle"  -- For CSS transitions
            ]
            []
        ]

-- =============================================================================
-- Force Simulation
-- =============================================================================

-- | Start the force simulation
startSimulation :: Config -> Array PackageNode -> Effect (SimulationHandle PackageNodeRow)
startSimulation config nodes = do
  log $ "[PackageSetBeeswarm] Creating simulation with " <> show (Array.length nodes) <> " nodes"
  log $ "[PackageSetBeeswarm] Container: " <> "#beeswarm-nodes"
  log $ "[PackageSetBeeswarm] Forces: positionX(0.5), positionY(0.03), collide(0.7)"

  { handle, events } <- runSimulation
    { engine: D3
    , setup: setup "package-set-beeswarm"
        [ positionX "x" # withX (dynamic _.targetX) # withStrength (static 0.5)
        , positionY "y" # withY (static 0.0) # withStrength (static 0.03)
        , collide "collide" # withRadius (dynamic \n -> n.r + 2.0) # withStrength (static 0.7)
        ]
        # withAlphaDecay 0.005
    , nodes: nodes
    , links: []
    , container: "#beeswarm-nodes"
    , alphaMin: 0.0001
    }

  log "[PackageSetBeeswarm] runSimulation returned, subscribing to events"

  -- Initial render with full HATS tree (creates elements)
  initialNodes <- handle.getNodes
  renderNodesHATS config initialNodes

  -- Subscribe to events - tick uses fast path (transform-only)
  tickCountRef <- Ref.new 0
  _ <- subscribe events \event -> case event of
    Tick _ -> do
      -- Fast path: only update transforms (not full HATS rerender)
      currentNodes <- handle.getNodes
      tickUpdate "#beeswarm-nodes" currentNodes
      count <- Ref.read tickCountRef
      Ref.write (count + 1) tickCountRef
      when (count `mod` 50 == 0) do
        log $ "[PackageSetBeeswarm] Tick " <> show count
    Completed -> log "[PackageSetBeeswarm] Simulation completed"
    Started -> log "[PackageSetBeeswarm] Simulation started"
    Stopped -> log "[PackageSetBeeswarm] Simulation stopped"

  pure handle
