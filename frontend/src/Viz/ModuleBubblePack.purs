-- | Module Bubble Pack - Modules as circle-packed bubbles with declaration categories
-- |
-- | Similar to package BubblePackBeeswarm but at module level:
-- | - Parent circles = modules (sized by total declaration count)
-- | - Child circles = declaration categories (value, data, newtype, type_class, etc.)
-- |
-- | Uses PSD3.Simulation for force simulation.
-- | Uses psd3-layout's packSiblingsMap for circle packing.
-- |
-- | Uses renderNodes: false pattern - simulation runs the physics,
-- | HATS handles all rendering on each tick.
module CE2.Viz.ModuleBubblePack
  ( render
  , setScope
  , updateColors
  , Config
  , BubblePackHandle
  , PackedModuleNode
  , PackedModuleNodeRow
  , DeclarationCircle
  ) where

import Prelude

import Data.Array as Array
import Data.Int (toNumber)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable as Nullable
import Data.Number (sqrt)
import Data.String.CodeUnits as SCU
import Data.String.Pattern (Pattern(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class.Console (log)
import Foreign.Object (Object)
import Foreign.Object as Object

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
import CE2.Data.Loader (V2ModuleListItem, V2Package, V2ModuleDeclarationStats)
import CE2.Types (ViewTheme, ColorMode)

-- =============================================================================
-- Types
-- =============================================================================

type Config =
  { containerSelector :: String
  , width :: Number
  , height :: Number
  , packages :: Array V2Package  -- For package-based positioning
  }

-- | A declaration category circle packed within a module
type DeclarationCircle =
  { kind :: String  -- "value", "data", "newtype", "type_class", "type_synonym", "foreign"
  , count :: Int
  , x :: Number     -- Position relative to module center
  , y :: Number
  , r :: Number
  }

-- | Module node for force simulation with packed declaration categories inside
type PackedModuleNode = SimulationNode
  ( name :: String
  , packageName :: String
  , packageSource :: String
  , declarationCount :: Int
  , color :: String
  , r :: Number                         -- Enclosing radius for collision
  , categories :: Array DeclarationCircle -- Pre-packed declaration categories
  )

-- | Handle for interacting with the visualization
type BubblePackHandle =
  { simHandle :: SimulationHandle PackedModuleNodeRow
  , config :: Config
  , stop :: Effect Unit
  }

-- Row type for SimulationHandle
type PackedModuleNodeRow =
  ( name :: String
  , packageName :: String
  , packageSource :: String
  , declarationCount :: Int
  , color :: String
  , r :: Number
  , categories :: Array DeclarationCircle
  )

-- =============================================================================
-- Public API
-- =============================================================================

-- | Render the module bubble pack view
-- | Takes modules with their declaration stats
render :: Config -> Array V2ModuleListItem -> Map Int V2ModuleDeclarationStats -> Effect BubblePackHandle
render config modules statsMap = do
  log $ "[ModuleBubblePack] Rendering with " <> show (Array.length modules) <> " modules"

  -- Clear existing content
  clearContainer config.containerSelector

  -- Prepare packed nodes
  let packedNodes = preparePackedNodes config modules statsMap

  log $ "[ModuleBubblePack] Prepared " <> show (Array.length packedNodes) <> " bubble packs"

  -- Render SVG container using HATS
  renderSVGContainerHATS config

  -- Start force simulation
  { handle, events } <- runSimulation
    { engine: D3
    , setup: setup "module-bubblepack"
        [ manyBody "charge" # withStrength (static (-80.0))
        , collide "collision" # withRadius (dynamic \n -> n.r + 8.0)
        , positionX "forceX" # withX (static 0.0) # withStrength (static 0.05)
        , positionY "forceY" # withY (static 0.0) # withStrength (static 0.08)
        ]
    , nodes: packedNodes
    , links: []
    , container: C.moduleBubblePackNodes
    , alphaMin: 0.001
    }

  -- Initial render with full HATS tree (creates elements)
  initialNodes <- handle.getNodes
  renderNodesHATS config initialNodes

  -- Subscribe to simulation events - tick uses fast path (transform-only)
  _ <- subscribe events \event -> case event of
    Tick _ -> do
      -- Fast path: only update transforms (not full HATS rerender)
      currentNodes <- handle.getNodes
      tickUpdate C.moduleBubblePackNodes currentNodes
    Completed -> log "[ModuleBubblePack] Simulation converged"
    Started -> log "[ModuleBubblePack] Simulation started"
    Stopped -> pure unit

  log "[ModuleBubblePack] Simulation running"

  pure
    { simHandle: handle
    , config
    , stop: handle.stop
    }

-- | Update visible modules (GUP - enter/exit animations)
-- | After updating simulation data, does a full HATS rerender to handle enter/exit
setScope :: BubblePackHandle -> Array V2ModuleListItem -> Map Int V2ModuleDeclarationStats -> Effect Unit
setScope handle modules statsMap = do
  log $ "[ModuleBubblePack] setScope called with " <> show (Array.length modules) <> " modules"

  let packedNodes = preparePackedNodes handle.config modules statsMap

  -- Update simulation data (triggers GUP)
  result <- handle.simHandle.updateData packedNodes []

  log $ "[ModuleBubblePack] GUP: " <>
    show (Array.length result.nodes.entered) <> " entered, " <>
    show (Array.length result.nodes.updated) <> " updated, " <>
    show (Array.length result.nodes.exited) <> " exited"

  -- Full HATS rerender to handle enter/exit (tickUpdate only does transforms)
  currentNodes <- handle.simHandle.getNodes
  renderNodesHATS handle.config currentNodes

-- | Update colors in place (re-renders with new colors)
updateColors :: String -> ViewTheme -> ColorMode -> Effect Unit
updateColors containerSelector _theme _colorMode = do
  log $ "[ModuleBubblePack] updateColors: " <> containerSelector
  -- For HATS, color updates happen via re-render on next tick
  -- Colors are embedded in the tree so they'll be picked up automatically
  pure unit

-- =============================================================================
-- Node Preparation
-- =============================================================================

-- | Prepare modules with circle-packed declaration categories
preparePackedNodes
  :: Config
  -> Array V2ModuleListItem
  -> Map Int V2ModuleDeclarationStats
  -> Array PackedModuleNode
preparePackedNodes config modules statsMap =
  Array.mapWithIndex prepareModule modules
  where
  -- Build package x-position map for clustering
  packageXMap = computePackagePositions config.packages config.width

  prepareModule idx modItem =
    let
      -- Get declaration stats for this module
      stats = Map.lookup modItem.id statsMap
      declarationCount = modItem.declarationCount

      -- Create category circles from stats
      categoryCircles = case stats of
        Nothing -> []
        Just s -> kindsToCategoryCircles s.kinds

      -- Pack the categories
      packed = packSiblingsMap (categoryCircles <#> \c -> { x: 0.0, y: 0.0, r: c.r })

      -- Convert to DeclarationCircle with kind/count
      packedCategories = Array.zipWith attachPosition categoryCircles packed.circles

      -- Enclosing radius with padding
      baseR = if declarationCount > 0
              then 12.0 + sqrt (toNumber declarationCount) * 2.0
              else 15.0
      r = max baseR (packed.radius + 6.0)

      -- Initial position based on package
      baseX = fromMaybe (config.width / 2.0) (Map.lookup modItem.package.name packageXMap)
      pseudoRandom = toNumber ((idx * 17 + 31) `mod` 100) / 100.0 - 0.5
      x = baseX - config.width / 2.0
      y = pseudoRandom * config.height * 0.6

      -- Color based on source
      color = getModuleColor modItem.package.source
    in
      { id: modItem.id
      , x
      , y
      , vx: 0.0
      , vy: 0.0
      , fx: Nullable.null
      , fy: Nullable.null
      , name: modItem.name
      , packageName: modItem.package.name
      , packageSource: modItem.package.source
      , declarationCount
      , color
      , r
      , categories: packedCategories
      }

  attachPosition :: DeclarationCircle -> Circle -> DeclarationCircle
  attachPosition cat c = cat { x = c.x, y = c.y }

-- | Convert kind counts to declaration circles
kindsToCategoryCircles :: Object Int -> Array DeclarationCircle
kindsToCategoryCircles kinds =
  Array.filter (\c -> c.count > 0) allCategories
  where
  getCount k = fromMaybe 0 $ Object.lookup k kinds

  -- Create circles for each known kind
  allCategories =
    [ mkCategory "value" (getCount "value")
    , mkCategory "data" (getCount "data")
    , mkCategory "newtype" (getCount "newtype")
    , mkCategory "type_class" (getCount "type_class")
    , mkCategory "type_synonym" (getCount "type_synonym")
    , mkCategory "foreign" (getCount "foreign")
    , mkCategory "alias" (getCount "alias")
    ]

  mkCategory kind count =
    { kind
    , count
    , x: 0.0
    , y: 0.0
    , r: sqrt (toNumber count + 1.0) * 3.0  -- Sqrt scale for area
    }

-- | Compute x positions for packages (evenly distributed)
computePackagePositions :: Array V2Package -> Number -> Map String Number
computePackagePositions packages totalWidth =
  let
    padding = 60.0
    availableWidth = totalWidth - padding * 2.0
    pkgCount = Array.length packages
    spacing = if pkgCount > 1 then availableWidth / toNumber (pkgCount - 1) else 0.0
  in
    Map.fromFoldable $ Array.mapWithIndex
      (\idx pkg -> pkg.name /\ (padding + toNumber idx * spacing))
      packages

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
        [ staticStr "id" "module-bubblepack-svg"
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
            [ staticStr "id" C.moduleBubblePackNodesId
            , staticStr "class" "module-bubblepack-nodes"
            ]
            []
        ]
  _ <- rerender config.containerSelector containerTree
  pure unit

-- | Render all module nodes using HATS
renderNodesHATS :: Config -> Array PackedModuleNode -> Effect Unit
renderNodesHATS _config nodes = do
  let nodesTree = createModuleNodesTree nodes
  _ <- rerender C.moduleBubblePackNodes nodesTree
  pure unit

-- | Create the nodes tree for HATS rendering
createModuleNodesTree :: Array PackedModuleNode -> Tree
createModuleNodesTree nodes =
  forEach "modules" Group nodes nodeKey moduleNodeHATS
  where
  nodeKey :: PackedModuleNode -> String
  nodeKey n = show n.id

-- | Module node template using HATS
moduleNodeHATS :: PackedModuleNode -> Tree
moduleNodeHATS node =
  elem Group
    [ thunkedStr "transform" ("translate(" <> show node.x <> "," <> show node.y <> ")")
    , staticStr "class" "module-bubble"
    , thunkedStr "data-id" (show node.id)  -- Required for tickUpdate
    , thunkedStr "data-name" node.name
    , thunkedStr "data-package" node.packageName
    ]
    ( [ -- Module enclosing circle (translucent)
        elem Circle
          [ staticStr "class" "module-circle"
          , staticStr "cx" "0"
          , staticStr "cy" "0"
          , thunkedNum "r" node.r
          , thunkedStr "fill" node.color
          , staticStr "fill-opacity" "0.15"
          , thunkedStr "stroke" node.color
          , staticStr "stroke-width" "1.5"
          ]
          []
      ]
      -- Category circles inside
      <> map toCategoryCircleHATS node.categories
      <>
      [ -- Module name label below
        elem Text
          [ staticStr "class" "module-label"
          , thunkedNum "y" (node.r + 12.0)
          , staticStr "text-anchor" "middle"
          , staticStr "fill" "#666"
          , staticStr "font-size" "9px"
          , staticStr "font-weight" "500"
          , staticStr "font-family" "system-ui, sans-serif"
          , thunkedStr "textContent" (shortenModuleName node.name)
          ]
          []
      , -- Declaration count above
        elem Text
          [ staticStr "class" "decl-count"
          , thunkedNum "y" (-(node.r) - 4.0)
          , staticStr "text-anchor" "middle"
          , staticStr "fill" "#888"
          , staticStr "font-size" "8px"
          , staticStr "font-family" "system-ui, sans-serif"
          , thunkedStr "textContent" (if node.declarationCount > 0
              then show node.declarationCount <> " decls"
              else "")
          ]
          []
      ]
    )

-- | Category circle using HATS
toCategoryCircleHATS :: DeclarationCircle -> Tree
toCategoryCircleHATS cat =
  elem Circle
    [ staticStr "class" "category-circle"
    , thunkedNum "cx" cat.x
    , thunkedNum "cy" cat.y
    , thunkedNum "r" cat.r
    , thunkedStr "fill" (kindColor cat.kind)
    , staticStr "fill-opacity" "0.8"
    , staticStr "stroke" "white"
    , staticStr "stroke-width" "0.5"
    , thunkedStr "data-kind" cat.kind
    ]
    []

-- | Shorten module name for display (last segment only)
shortenModuleName :: String -> String
shortenModuleName name =
  -- Just take the last segment after the last dot
  case SCU.lastIndexOf (Pattern ".") name of
    Nothing -> name
    Just idx -> SCU.drop (idx + 1) name

-- =============================================================================
-- Color Functions
-- =============================================================================

-- | Color for declaration kind
kindColor :: String -> String
kindColor = case _ of
  "value"        -> "#4e79a7"  -- Blue - functions/values
  "data"         -> "#59a14f"  -- Green - data types
  "newtype"      -> "#76b7b2"  -- Teal - newtypes
  "type_class"   -> "#f28e2b"  -- Orange - type classes
  "type_synonym" -> "#edc948"  -- Yellow - type synonyms
  "foreign"      -> "#e15759"  -- Red - FFI
  "alias"        -> "#b07aa1"  -- Purple - aliases
  _              -> "#bab0ac"  -- Gray - unknown

-- | Base color for module based on source
getModuleColor :: String -> String
getModuleColor = case _ of
  "workspace" -> "#4e79a7"  -- Blue
  "extra"     -> "#76b7b2"  -- Teal
  _           -> "#9c755f"  -- Brown (registry)
