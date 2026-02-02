-- | Polyglot Sunburst Visualization
-- |
-- | Visualizes the polyglot PureScript ecosystem as a sunburst diagram:
-- | - Center: Root (all PureScript projects)
-- | - Inner ring: Backends (JavaScript, Erlang, Python, Lua)
-- | - Middle ring: Projects
-- | - Outer ring: Packages
-- |
-- | Uses HATS for declarative rendering and hylograph-layout's partition layout.
module CE2.Viz.PolyglotSunburst
  ( Config
  , render
  , cleanup
  ) where

import Prelude

import Data.Array as Array
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Number (cos, sin, pi)
import Data.String as String
import Effect (Effect)

-- HATS imports
import Hylograph.HATS (Tree, elem, staticStr, thunkedStr)
import Hylograph.HATS.InterpreterTick (rerender, clearContainer)
import Hylograph.Internal.Selection.Types (ElementType(..))

-- Layout imports
import DataViz.Layout.Hierarchy.Partition
  ( PartitionNode(..)
  , PartitionConfig
  , defaultPartitionConfig
  , partition
  , HierarchyData(..)
  , hierarchy
  , sunburstArcPath
  , flattenPartition
  )

-- Local imports
import CE2.Data.Loader (PolyglotSummary, PolyglotBackend, PolyglotProject, PolyglotPackage)
import CE2.Types (ViewTheme(..), themeColors)

-- =============================================================================
-- Types
-- =============================================================================

-- | Configuration for sunburst rendering
type Config =
  { containerSelector :: String
  , width :: Number
  , height :: Number
  , theme :: ViewTheme
  }

-- | Unified node type for the hierarchy (sum type for different levels)
data SunburstNodeData
  = RootData { name :: String }
  | BackendData
      { name :: String
      , displayName :: String
      , totalLoc :: Int
      , packageCount :: Int
      }
  | ProjectData
      { id :: Int
      , name :: String
      , backend :: String
      , packageCount :: Int
      }
  | PackageData
      { id :: Int
      , name :: String
      , version :: String
      , source :: String
      , totalLoc :: Int
      , moduleCount :: Int
      }

-- | Flattened node for rendering (after partition layout)
type FlatNode =
  { data_ :: SunburstNodeData
  , depth :: Int
  , x0 :: Number  -- Angular start (0-1)
  , x1 :: Number  -- Angular end (0-1)
  , y0 :: Number  -- Radial start (0-1)
  , y1 :: Number  -- Radial end (0-1)
  , value :: Number
  }

-- =============================================================================
-- Public API
-- =============================================================================

-- | Render the sunburst visualization
render :: Config -> PolyglotSummary -> Effect Unit
render config summary = do
  -- Clear existing content
  clearContainer config.containerSelector

  -- Convert to hierarchy, compute layout, flatten, and render
  let hierarchyData = summaryToHierarchy summary
      partitioned = computeLayout config hierarchyData
      flattened = flattenToRenderNodes partitioned
      svgTree = buildSVGTree config flattened

  _ <- rerender config.containerSelector svgTree
  pure unit

-- | Clean up the visualization
cleanup :: String -> Effect Unit
cleanup selector = clearContainer selector

-- =============================================================================
-- Data Conversion
-- =============================================================================

-- | Convert PolyglotSummary to HierarchyData for partition layout
summaryToHierarchy :: PolyglotSummary -> HierarchyData SunburstNodeData
summaryToHierarchy summary =
  HierarchyData
    { data_: RootData { name: summary.name }
    , value: Nothing  -- Will be computed from children
    , children: Just $ map backendToHierarchy summary.children
    }

-- | Convert backend to hierarchy node
backendToHierarchy :: PolyglotBackend -> HierarchyData SunburstNodeData
backendToHierarchy backend =
  HierarchyData
    { data_: BackendData
        { name: backend.name
        , displayName: backend.displayName
        , totalLoc: backend.totalLoc
        , packageCount: backend.packageCount
        }
    , value: Nothing
    , children: Just $ map projectToHierarchy backend.children
    }

-- | Convert project to hierarchy node
projectToHierarchy :: PolyglotProject -> HierarchyData SunburstNodeData
projectToHierarchy project =
  HierarchyData
    { data_: ProjectData
        { id: project.id
        , name: project.name
        , backend: project.backend
        , packageCount: project.packageCount
        }
    , value: Nothing
    , children: Just $ map packageToHierarchy project.children
    }

-- | Convert package to hierarchy node (leaf)
packageToHierarchy :: PolyglotPackage -> HierarchyData SunburstNodeData
packageToHierarchy pkg =
  HierarchyData
    { data_: PackageData
        { id: pkg.id
        , name: pkg.name
        , version: pkg.version
        , source: pkg.source
        , totalLoc: pkg.totalLoc
        , moduleCount: pkg.moduleCount
        }
    , value: Just $ toNumber $ max 100 pkg.totalLoc  -- Minimum size for visibility
    , children: Nothing
    }

-- =============================================================================
-- Layout Computation
-- =============================================================================

-- | Compute partition layout
computeLayout :: Config -> HierarchyData SunburstNodeData -> PartitionNode SunburstNodeData
computeLayout _config hierarchyData =
  let
    -- Convert to PartitionNode
    root = hierarchy hierarchyData

    -- Configure partition for sunburst
    -- Use normalized coordinates (0-1) for both x and y
    -- x = angle (0-1 = 0-2π), y = radius (0-1)
    partitionConfig :: PartitionConfig SunburstNodeData
    partitionConfig = defaultPartitionConfig
      { size = { width: 1.0, height: 1.0 }
      , padding = 0.002  -- Small padding between arcs
      }
  in
    partition partitionConfig root

-- | Flatten partition tree to array of render nodes
flattenToRenderNodes :: PartitionNode SunburstNodeData -> Array FlatNode
flattenToRenderNodes root =
  flattenPartition root <#> \(PartNode n) ->
    { data_: n.data_
    , depth: n.depth
    , x0: n.x0
    , x1: n.x1
    , y0: n.y0
    , y1: n.y1
    , value: n.value
    }

-- =============================================================================
-- SVG Rendering
-- =============================================================================

-- | Build the complete SVG tree
buildSVGTree :: Config -> Array FlatNode -> Tree
buildSVGTree config nodes =
  let
    colors = themeColors config.theme
    radius = min config.width config.height / 2.0 - 20.0  -- Leave margin
    cx = config.width / 2.0
    cy = config.height / 2.0
  in
  elem SVG
    [ staticStr "id" "polyglot-sunburst-svg"
    , staticStr "viewBox" $ "0 0 " <> show config.width <> " " <> show config.height
    , staticStr "width" "100%"
    , staticStr "height" "100%"
    , staticStr "style" $ "background: " <> colors.background <> "; display: block;"
    , staticStr "preserveAspectRatio" "xMidYMid meet"
    ]
    [ -- Transform group to center the sunburst
      elem Group
        [ staticStr "transform" $ "translate(" <> show cx <> "," <> show cy <> ")" ]
        [ -- Render all arcs
          elem Group
            [ staticStr "class" "sunburst-arcs" ]
            (Array.mapMaybe (renderArc config radius) nodes)
        , -- Render labels for larger segments
          elem Group
            [ staticStr "class" "sunburst-labels" ]
            (Array.mapMaybe (renderLabel config radius) nodes)
        ]
    ]

-- | Render a single arc segment
renderArc :: Config -> Number -> FlatNode -> Maybe Tree
renderArc config radius node =
  -- Skip root (depth 0) - it's the center
  if node.depth == 0 then Nothing
  else
    let
      pathD = sunburstArcPath node.x0 node.y0 node.x1 node.y1 radius
      fillColor = nodeColor config.theme node
      name = nodeName node.data_
    in
    Just $ elem Path
      [ staticStr "d" pathD
      , staticStr "fill" fillColor
      , staticStr "stroke" "rgba(255, 255, 255, 0.3)"
      , staticStr "stroke-width" "0.5"
      , staticStr "class" $ "sunburst-arc depth-" <> show node.depth
      , staticStr "data-name" name
      ]
      []

-- | Render a label for a segment (only for segments large enough)
renderLabel :: Config -> Number -> FlatNode -> Maybe Tree
renderLabel config radius node =
  -- Only render labels for depth 1 (backends) and 2 (projects) with enough space
  if node.depth == 0 || node.depth > 2 then Nothing
  else
    let
      arcSpan = node.x1 - node.x0
      -- Only label if arc is at least 5% of the circle
      shouldLabel = arcSpan > 0.05
    in
    if not shouldLabel then Nothing
    else
      let
        colors = themeColors config.theme
        -- Position label at middle of arc, middle of radial extent
        midAngle = (node.x0 + node.x1) / 2.0 * 2.0 * pi - (pi / 2.0)
        midRadius = ((node.y0 + node.y1) / 2.0) * radius
        lx = cos midAngle * midRadius
        ly = sin midAngle * midRadius

        -- Rotate text to follow arc
        textAngle = midAngle * 180.0 / pi
        -- Flip text if on left side of circle
        adjustedAngle = if midAngle > pi / 2.0 && midAngle < pi * 1.5
                        then textAngle + 180.0
                        else textAngle

        name = nodeName node.data_
        displayName = case node.data_ of
          BackendData b -> b.displayName
          _ -> name

        fontSize = if node.depth == 1 then "12" else "9"
      in
      Just $ elem Text
        [ thunkedStr "x" (show lx)
        , thunkedStr "y" (show ly)
        , staticStr "text-anchor" "middle"
        , staticStr "dominant-baseline" "middle"
        , staticStr "font-size" fontSize
        , staticStr "fill" colors.text
        , staticStr "font-family" "'Courier New', Courier, monospace"
        , staticStr "transform" $ "rotate(" <> show adjustedAngle <> "," <> show lx <> "," <> show ly <> ")"
        , staticStr "textContent" (truncateName 15 displayName)
        ]
        []

-- =============================================================================
-- Helpers
-- =============================================================================

-- | Get node name for any node type
nodeName :: SunburstNodeData -> String
nodeName = case _ of
  RootData r -> r.name
  BackendData b -> b.name
  ProjectData p -> p.name
  PackageData pkg -> pkg.name

-- | Get fill color based on node depth and type
nodeColor :: ViewTheme -> FlatNode -> String
nodeColor theme node = case node.depth of
  1 -> -- Backend level: distinct colors per backend
    case node.data_ of
      BackendData b -> backendColor b.name
      _ -> "gray"
  2 -> -- Project level: slightly lighter version of backend color
    case node.data_ of
      ProjectData p -> projectColor p.backend
      _ -> "gray"
  3 -> -- Package level: based on source (workspace vs registry)
    case node.data_ of
      PackageData pkg -> packageColor theme pkg.source
      _ -> "gray"
  _ -> "gray"

-- | Color for backend ring
backendColor :: String -> String
backendColor = case _ of
  "js"     -> "hsl(210, 70%, 45%)"   -- Blue for JavaScript
  "erlang" -> "hsl(340, 70%, 50%)"   -- Red/pink for Erlang
  "python" -> "hsl(55, 70%, 45%)"    -- Yellow for Python
  "lua"    -> "hsl(280, 60%, 50%)"   -- Purple for Lua
  _        -> "hsl(0, 0%, 50%)"      -- Gray fallback

-- | Color for project ring (lighter than backend)
projectColor :: String -> String
projectColor = case _ of
  "js"     -> "hsl(210, 60%, 55%)"
  "erlang" -> "hsl(340, 60%, 60%)"
  "python" -> "hsl(55, 60%, 55%)"
  "lua"    -> "hsl(280, 50%, 60%)"
  _        -> "hsl(0, 0%, 60%)"

-- | Color for package ring (based on source type)
packageColor :: ViewTheme -> String -> String
packageColor theme source = case source of
  "workspace" -> case theme of
    BlueprintTheme -> "rgba(255, 255, 255, 0.6)"
    _ -> "rgba(60, 60, 60, 0.7)"
  "registry"  -> case theme of
    BlueprintTheme -> "rgba(100, 150, 200, 0.5)"
    _ -> "rgba(100, 100, 100, 0.5)"
  "extra"     -> case theme of
    BlueprintTheme -> "rgba(150, 200, 150, 0.5)"
    _ -> "rgba(80, 120, 80, 0.5)"
  _           -> "rgba(128, 128, 128, 0.5)"

-- | Truncate a name with ellipsis
truncateName :: Int -> String -> String
truncateName maxLen name =
  if String.length name > maxLen
    then String.take maxLen name <> "..."
    else name
