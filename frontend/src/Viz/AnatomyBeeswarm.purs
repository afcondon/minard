-- | Anatomy Beeswarm Visualization
-- |
-- | Force-directed beeswarm showing project anatomy:
-- | - Workspace packages (gold): source == "workspace"
-- | - Direct dependencies (blue): union of workspace depends, minus workspace names
-- | - Transitive dependencies (gray): everything else
-- |
-- | Positioned by topoLayer (leaves left, roots right), sized by LOC.
-- | Uses hylograph-simulation + HATS (same pattern as PackageSetBeeswarm).
module CE2.Viz.AnatomyBeeswarm
  ( Config
  , BeeswarmHandle
  , AnatomyNodeRow
  , PackageCategory(..)
  , render
  , cleanup
  , classify
  , computeDirectDepNames
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable as Nullable
import Data.Number (sqrt)
import Data.Set (Set)
import Data.Set as Set
import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Ref as Ref

-- HATS Imports
import Hylograph.HATS (Tree, elem, staticStr, staticNum, thunkedStr, thunkedNum, forEach, withBehaviors, onClick)
import Hylograph.HATS.InterpreterTick (rerender, clearContainer)
import Hylograph.Internal.Element.Types (ElementType(..))
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

-- =============================================================================
-- Types
-- =============================================================================

-- | Package categories for anatomy coloring
data PackageCategory = Workspace | DirectDep | Transitive

derive instance eqPackageCategory :: Eq PackageCategory

-- | Configuration for anatomy beeswarm
type Config =
  { containerSelector :: String
  , width :: Number
  , height :: Number
  , maxTopoLayer :: Int
  , onClick :: Maybe (String -> Effect Unit)
  }

-- | Node in the anatomy simulation
type AnatomyNode = SimulationNode
  ( pkg :: PackageSetPackage
  , targetX :: Number
  , r :: Number
  , category :: PackageCategory
  , color :: String
  , strokeColor :: String
  )

type AnatomyNodeRow =
  ( pkg :: PackageSetPackage
  , targetX :: Number
  , r :: Number
  , category :: PackageCategory
  , color :: String
  , strokeColor :: String
  )

-- | Handle for controlling the beeswarm
type BeeswarmHandle =
  { simHandle :: SimulationHandle AnatomyNodeRow
  , stop :: Effect Unit
  }

-- =============================================================================
-- Classification
-- =============================================================================

-- | Compute the set of direct dependency names from workspace packages
computeDirectDepNames :: Array PackageSetPackage -> Set String
computeDirectDepNames packages =
  let
    wsNames = Set.fromFoldable $ Array.mapMaybe
      (\p -> if p.source == "workspace" then Just p.name else Nothing) packages
    allWsDeps = foldl (\acc p ->
      if p.source == "workspace"
        then Set.union acc (Set.fromFoldable p.depends)
        else acc
      ) Set.empty packages
  in
    Set.difference allWsDeps wsNames

-- | Classify a package into workspace, direct dep, or transitive
classify :: Set String -> Set String -> PackageSetPackage -> PackageCategory
classify wsNames directDepNames pkg
  | Set.member pkg.name wsNames = Workspace
  | Set.member pkg.name directDepNames = DirectDep
  | otherwise = Transitive

-- =============================================================================
-- Coloring
-- =============================================================================

-- | Get color for a package based on its category and topoLayer
anatomyColor :: PackageCategory -> Int -> Int -> String
anatomyColor cat layer maxLayer =
  case cat of
    Workspace ->
      -- Gold/amber, slightly varied by topoLayer
      let l = 55.0 + toNumber layer / toNumber (max 1 maxLayer) * 10.0
      in "hsl(40, 85%, " <> show l <> "%)"
    DirectDep ->
      -- Saturated blue, varied by topoLayer
      let l = 45.0 + toNumber layer / toNumber (max 1 maxLayer) * 10.0
      in "hsl(210, 65%, " <> show l <> "%)"
    Transitive ->
      -- Desaturated blue-gray, lighter with higher topoLayer distance
      let t = toNumber layer / toNumber (max 1 maxLayer)
          s = 15.0
          l = 55.0 + t * 20.0
      in "hsl(210, " <> show s <> "%, " <> show l <> "%)"

-- | Get stroke color for a category
anatomyStroke :: PackageCategory -> String
anatomyStroke = case _ of
  Workspace -> "hsl(40, 85%, 40%)"
  DirectDep -> "hsl(210, 65%, 35%)"
  Transitive -> "hsl(210, 10%, 50%)"

-- =============================================================================
-- Public API
-- =============================================================================

-- | Render the anatomy beeswarm
render :: Config -> Array PackageSetPackage -> Effect BeeswarmHandle
render config packages = do
  clearContainer config.containerSelector

  let wsNames = Set.fromFoldable $ Array.mapMaybe
        (\p -> if p.source == "workspace" then Just p.name else Nothing) packages
      directDepNames = computeDirectDepNames packages
      nodes = prepareNodes config wsNames directDepNames packages

  log $ "[AnatomyBeeswarm] Starting with " <> show (Array.length nodes) <> " packages"

  renderSVGContainer config
  simHandle <- startSimulation config nodes

  pure { simHandle, stop: simHandle.stop }

-- | Clean up
cleanup :: String -> Effect Unit
cleanup = clearContainer

-- =============================================================================
-- Node Preparation
-- =============================================================================

prepareNodes :: Config -> Set String -> Set String -> Array PackageSetPackage -> Array AnatomyNode
prepareNodes config wsNames directDepNames packages =
  Array.mapWithIndex (prepareNode config wsNames directDepNames layerInfo) packages
  where
  layerCounts = countByLayer packages config.maxTopoLayer
  layerInfo = computeLayerPositions layerCounts config.width

prepareNode :: Config -> Set String -> Set String -> Array Number -> Int -> PackageSetPackage -> AnatomyNode
prepareNode config wsNames directDepNames cumulativeX idx pkg =
  let
    padding = 60.0
    layerX = getLayerX pkg.topoLayer cumulativeX
    -- Flip: high topoLayer (apps/workspace) on LEFT, low topoLayer (leaves) on RIGHT
    targetX = config.width / 2.0 - padding - layerX

    kloc = toNumber pkg.totalLoc / 1000.0
    r = 8.0 + sqrt (max 0.1 kloc) * 5.0

    cat = classify wsNames directDepNames pkg
    color = anatomyColor cat pkg.topoLayer config.maxTopoLayer
    strokeColor = anatomyStroke cat

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
    , pkg
    , targetX
    , r
    , category: cat
    , color
    , strokeColor
    }

-- =============================================================================
-- Layer positioning (same logic as PackageSetBeeswarm)
-- =============================================================================

countByLayer :: Array PackageSetPackage -> Int -> Array Int
countByLayer packages maxLayer =
  Array.range 0 maxLayer <#> \layer ->
    Array.length $ Array.filter (\p -> p.topoLayer == layer) packages

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

getLayerX :: Int -> Array Number -> Number
getLayerX layer cumulative =
  fromMaybe 0.0 $ Array.index cumulative layer

scanl :: forall a b. (b -> a -> b) -> b -> Array a -> Array b
scanl f initial arr = Array.foldl go { acc: initial, result: [initial] } arr # _.result
  where
  go { acc, result } x' =
    let newAcc = f acc x'
    in { acc: newAcc, result: Array.snoc result newAcc }

-- =============================================================================
-- SVG Container + HATS Rendering
-- =============================================================================

renderSVGContainer :: Config -> Effect Unit
renderSVGContainer config = do
  let
    vbX = -config.width / 2.0
    vbY = -config.height / 2.0
    viewBox = show vbX <> " " <> show vbY <> " " <> show config.width <> " " <> show config.height

    containerTree :: Tree
    containerTree =
      elem SVG
        [ staticStr "id" "anatomy-beeswarm-svg"
        , staticStr "viewBox" viewBox
        , staticStr "width" "100%"
        , staticStr "height" "100%"
        , staticStr "class" "anatomy-beeswarm"
        , staticStr "preserveAspectRatio" "xMidYMid meet"
        ]
        [ -- Left axis label (apps/roots on left)
          elem Text
            [ staticNum "x" (-config.width / 2.0 + 60.0)
            , staticNum "y" (config.height / 2.0 - 15.0)
            , staticStr "font-size" "11"
            , staticStr "fill" "#999"
            , staticStr "textContent" "\x2190 Your code (most deps)"
            ]
            []
        -- Right axis label (leaves on right)
        , elem Text
            [ staticNum "x" (config.width / 2.0 - 60.0)
            , staticNum "y" (config.height / 2.0 - 15.0)
            , staticStr "text-anchor" "end"
            , staticStr "font-size" "11"
            , staticStr "fill" "#999"
            , staticStr "textContent" "Leaves (no deps) \x2192"
            ]
            []
        -- Legend
        , renderLegend config
        -- Nodes container
        , elem Group
            [ staticStr "id" "anatomy-beeswarm-nodes"
            , staticStr "class" "packages"
            ]
            []
        ]
  _ <- rerender config.containerSelector containerTree
  pure unit

-- | Inline SVG legend
renderLegend :: Config -> Tree
renderLegend config =
  let
    lx = -config.width / 2.0 + 20.0
    ly = -config.height / 2.0 + 20.0
  in
    elem Group
      [ staticStr "class" "anatomy-legend"
      , staticStr "transform" ("translate(" <> show lx <> "," <> show ly <> ")")
      ]
      [ legendItem 0.0 "hsl(40, 85%, 60%)" "Your code"
      , legendItem 24.0 "hsl(210, 65%, 50%)" "Direct deps"
      , legendItem 48.0 "hsl(210, 15%, 65%)" "Transitive deps"
      ]

legendItem :: Number -> String -> String -> Tree
legendItem yOff color label =
  elem Group
    [ staticStr "transform" ("translate(0," <> show yOff <> ")")
    ]
    [ elem Circle
        [ staticStr "cx" "6"
        , staticStr "cy" "6"
        , staticStr "r" "5"
        , staticStr "fill" color
        , staticStr "stroke" "#555"
        , staticStr "stroke-width" "0.5"
        ]
        []
    , elem Text
        [ staticStr "x" "16"
        , staticStr "y" "10"
        , staticStr "font-size" "11"
        , staticStr "fill" "#666"
        , staticStr "font-family" "'Courier New', monospace"
        , staticStr "textContent" label
        ]
        []
    ]

-- | Render nodes using HATS
renderNodesHATS :: Config -> Array AnatomyNode -> Effect Unit
renderNodesHATS config nodes = do
  let nodesTree = createNodesTree config nodes
  _ <- rerender "#anatomy-beeswarm-nodes" nodesTree
  pure unit

createNodesTree :: Config -> Array AnatomyNode -> Tree
createNodesTree config nodes =
  forEach "anatomy-packages" Group nodes nodeKey (nodeHATS config)
  where
  nodeKey :: AnatomyNode -> String
  nodeKey n = show n.id

-- | Single package node template
nodeHATS :: Config -> AnatomyNode -> Tree
nodeHATS config node =
  let
    clickBehaviors = case config.onClick of
      Just handler -> [ onClick (handler node.pkg.name) ]
      Nothing -> []

    isApp = node.pkg.bundleModule /= Nothing
  in
    elem Group
      [ thunkedStr "transform" ("translate(" <> show node.x <> "," <> show node.y <> ")")
      , thunkedStr "class" "anatomy-package-group"
      , thunkedStr "data-id" (show node.id)
      , thunkedStr "data-name" node.pkg.name
      ]
      (
        -- Workspace apps: rounded rect; others: circle
        if node.category == Workspace && isApp
        then
          [ withBehaviors clickBehaviors $
              elem Rect
                [ thunkedNum "x" (-(node.r * 0.8))
                , thunkedNum "y" (-(node.r * 0.6))
                , thunkedNum "width" (node.r * 1.6)
                , thunkedNum "height" (node.r * 1.2)
                , staticStr "rx" "4"
                , staticStr "ry" "4"
                , thunkedStr "fill" node.color
                , thunkedStr "stroke" node.strokeColor
                , thunkedNum "stroke-width" 1.5
                , staticStr "cursor" "pointer"
                , staticStr "class" "anatomy-circle app-rect"
                ]
                []
          ]
        else
          [ withBehaviors clickBehaviors $
              elem Circle
                [ staticStr "cx" "0"
                , staticStr "cy" "0"
                , thunkedNum "r" node.r
                , thunkedStr "fill" node.color
                , thunkedStr "stroke" node.strokeColor
                , thunkedNum "stroke-width" (if node.category == Workspace then 1.5 else 0.5)
                , staticStr "cursor" "pointer"
                , staticStr "class" "anatomy-circle"
                ]
                []
          ]
      <>
      [ -- Package name label
        withBehaviors clickBehaviors $
          elem Text
            [ staticStr "x" "0"
            , thunkedStr "y" (show (node.r + 12.0))
            , staticStr "text-anchor" "middle"
            , staticStr "font-size" "11"
            , staticStr "font-family" "'Courier New', monospace"
            , staticStr "fill" "#555"
            , staticStr "class" "anatomy-label"
            , staticStr "cursor" "pointer"
            , thunkedStr "textContent" node.pkg.name
            ]
            []
      ]
      )

-- =============================================================================
-- Force Simulation
-- =============================================================================

startSimulation :: Config -> Array AnatomyNode -> Effect (SimulationHandle AnatomyNodeRow)
startSimulation config nodes = do
  log $ "[AnatomyBeeswarm] Creating simulation with " <> show (Array.length nodes) <> " nodes"

  { handle, events } <- runSimulation
    { engine: D3
    , setup: setup "anatomy-beeswarm"
        [ positionX "x" # withX (dynamic _.targetX) # withStrength (static 0.5)
        , positionY "y" # withY (static 0.0) # withStrength (static 0.03)
        , collide "collide" # withRadius (dynamic \n -> n.r + 2.0) # withStrength (static 0.7)
        ]
        # withAlphaDecay 0.005
    , nodes: nodes
    , links: []
    , container: "#anatomy-beeswarm-nodes"
    , alphaMin: 0.0001
    }

  -- Initial render
  initialNodes <- handle.getNodes
  renderNodesHATS config initialNodes

  -- Subscribe to tick events for position updates
  tickCountRef <- Ref.new 0
  _ <- subscribe events \event -> case event of
    Tick _ -> do
      currentNodes <- handle.getNodes
      tickUpdate "#anatomy-beeswarm-nodes" currentNodes
      count <- Ref.read tickCountRef
      Ref.write (count + 1) tickCountRef
      when (count `mod` 50 == 0) do
        log $ "[AnatomyBeeswarm] Tick " <> show count
    Completed -> log "[AnatomyBeeswarm] Simulation completed"
    Started -> log "[AnatomyBeeswarm] Simulation started"
    Stopped -> log "[AnatomyBeeswarm] Simulation stopped"

  pure handle
