-- | Package Set Treemap View (HATS Version)
-- |
-- | Visualizes the full PureScript package registry as a treemap.
-- | - Rectangles represent packages, area proportional to module count
-- | - Blueprint blue palette (#4a9eff)
-- | - White circle at center of each rect
-- | - No modules shown at this level
module CE2.Viz.PackageSetTreemap
  ( Config
  , PackageRenderData(..)
  , render
  , renderWithHighlighting  -- Render with hover highlighting enabled
  , updateColors
  , cleanup
  , computeCellPositions  -- Compute circle positions for transitions (pure)
  , getCellPositions      -- DEPRECATED: use computeCellPositions instead
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl)
import Data.Int (toNumber, floor)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (sqrt)
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Data.Tuple (Tuple(..))
import Effect (Effect)

-- PSD3 HATS Imports
import Hylograph.HATS (Tree, elem, staticStr, thunkedStr, thunkedNum, forEach, withBehaviors, onCoordinatedHighlight)
import Hylograph.HATS.InterpreterTick (rerender, clearContainer)
import Hylograph.Internal.Selection.Types (ElementType(..))
import Hylograph.Internal.Behavior.Types (HighlightClass(..))

-- Layout
import DataViz.Layout.Hierarchy.Types (ValuedNode(..))
import DataViz.Layout.Hierarchy.Treemap (TreemapNode(..), treemap, defaultTreemapConfig, squarify, phi)

import CE2.Data.Loader (PackageSetPackage)
import CE2.Types (CellContents(..), ViewTheme(..), themeColors, ColorMode)

-- =============================================================================
-- Types
-- =============================================================================

-- | Configuration for treemap rendering
type Config =
  { containerSelector :: String
  , width :: Number
  , height :: Number
  , projectPackages :: Set String    -- Direct project dependencies
  , transitivePackages :: Set String -- Transitive dependencies
  , theme :: ViewTheme               -- Visual theme (Blueprint/Paperwhite/Beige)
  , cellContents :: CellContents     -- What to render in cells (Circle/Text/Empty)
  }

-- | Package with computed treemap position
type PositionedPackage =
  { pkg :: PackageSetPackage
  , x :: Number
  , y :: Number
  , width :: Number
  , height :: Number
  }

-- | Data bound to each package element (used for data-driven rendering)
-- | This is what gets stored in __data__ on each package group
newtype PackageRenderData = PackageRenderData
  { name :: String
  , topoLayer :: Int
  , inProject :: Boolean      -- Direct dependency
  , isTransitive :: Boolean   -- Transitive dependency
  -- Layout info
  , x :: Number
  , y :: Number
  , width :: Number
  , height :: Number
  -- Computed values for rendering
  , cx :: Number              -- Center x
  , cy :: Number              -- Center y
  , circleR :: Number         -- Circle radius
  -- Dependency info for hover highlighting
  , dependsOn :: Array String   -- Packages this package depends on
  , dependedBy :: Array String  -- Packages that depend on this package
  }

derive instance eqPackageRenderData :: Eq PackageRenderData
derive instance ordPackageRenderData :: Ord PackageRenderData

-- =============================================================================
-- Public API
-- =============================================================================

-- | Render the treemap view (without hover highlighting)
render :: Config -> Array PackageSetPackage -> Effect Unit
render config packages = do
  -- Clear existing content
  clearContainer config.containerSelector

  -- Compute treemap layout and render (no dependency data)
  let positioned = computeTreemapPositions config packages
      packageData = positioned <#> toPackageRenderDataWithoutDeps config
  renderTreemapSVGImpl config packageData false

-- | Render the treemap view with hover highlighting enabled
-- | When hovering a package, highlights its dependencies and dependents
renderWithHighlighting :: Config -> Array PackageSetPackage -> Effect Unit
renderWithHighlighting config packages = do
  -- Clear existing content
  clearContainer config.containerSelector

  -- Build dependency maps
  let pkgNames = Set.fromFoldable $ map _.name packages
      dependsOnMap = buildDependsOnMap packages pkgNames
      dependedByMap = buildDependedByMap packages pkgNames

  -- Compute treemap layout and render with dependency data
  let positioned = computeTreemapPositions config packages
      packageData = positioned <#> toPackageRenderDataWithDeps config dependsOnMap dependedByMap
  renderTreemapSVGImpl config packageData true

-- | Clean up (no simulation to stop, just clear)
cleanup :: String -> Effect Unit
cleanup selector = clearContainer selector

-- | Update colors on existing treemap
-- |
-- | In HATS, we re-render the tree with new colors. This is more declarative
-- | than the old D3-style select-and-modify pattern, and HATS handles
-- | efficient updates internally.
-- |
-- | Note: This function requires the package data to be passed again.
-- | Callers should use render/renderWithHighlighting instead if they have
-- | access to the package data.
updateColors :: String -> ViewTheme -> ColorMode -> Effect Unit
updateColors _containerSelector _theme _colorMode = do
  -- In HATS, color updates happen by re-rendering the tree.
  -- This function is kept for API compatibility but callers should
  -- use render/renderWithHighlighting with updated config instead.
  pure unit

-- ColorMode and color helpers imported from CE2.Types

-- =============================================================================
-- Layout Computation
-- =============================================================================

-- | Compute treemap positions for all packages
-- | Groups by topo layer, sorts by name within each layer
computeTreemapPositions :: Config -> Array PackageSetPackage -> Array PositionedPackage
computeTreemapPositions config packages =
  let
    -- Group packages by topo layer
    maxLayer = Array.foldl (\m pkg -> max m pkg.topoLayer) 0 packages

    -- Create layer groups (depth=1), each containing packages sorted by name (depth=2)
    layerGroups :: Array (ValuedNode PackageSetPackage)
    layerGroups = Array.range 0 maxLayer <#> \layer ->
      let
        -- Packages in this layer, sorted by name
        layerPkgs = packages
          # Array.filter (\pkg -> pkg.topoLayer == layer)
          # Array.sortBy (\a b -> compare a.name b.name)

        -- Create leaf nodes for packages
        pkgLeaves :: Array (ValuedNode PackageSetPackage)
        pkgLeaves = layerPkgs <#> \pkg ->
          VNode
            { data_: pkg
            , depth: 2
            , height: 0
            , value: toNumber (max 1 pkg.totalLoc)
            , children: []
            , parent: Nothing
            }

        layerValue = Array.foldl (\acc (VNode n) -> acc + n.value) 0.0 pkgLeaves

        -- Layer group node (uses dummy package data)
        layerData :: PackageSetPackage
        layerData = { name: "layer-" <> show layer, id: -layer, version: "", topoLayer: layer, depends: [], description: Nothing, license: Nothing, repositoryOwner: Nothing, repositoryName: Nothing, publishedAt: Nothing, releaseNumber: 0, moduleCount: 0, totalLoc: 0 }
      in
        VNode
          { data_: layerData
          , depth: 1
          , height: 1
          , value: layerValue
          , children: pkgLeaves
          , parent: Nothing
          }

    -- Filter out empty layers
    nonEmptyLayers = Array.filter (\(VNode n) -> not (Array.null n.children)) layerGroups

    -- Create root node containing layer groups
    rootData :: PackageSetPackage
    rootData = { name: "root", id: 0, version: "", topoLayer: -1, depends: [], description: Nothing, license: Nothing, repositoryOwner: Nothing, repositoryName: Nothing, publishedAt: Nothing, releaseNumber: 0, moduleCount: 0, totalLoc: 0 }

    root :: ValuedNode PackageSetPackage
    root = VNode
      { data_: rootData
      , depth: 0
      , height: 2
      , value: Array.foldl (\acc (VNode n) -> acc + n.value) 0.0 nonEmptyLayers
      , children: nonEmptyLayers
      , parent: Nothing
      }

    -- Configure and run treemap
    treemapConfig = defaultTreemapConfig
      { size = { width: config.width, height: config.height }
      , tile = squarify phi
      , paddingInner = 2.0
      , paddingOuter = 2.0
      }

    -- Compute positions
    positioned = treemap treemapConfig root

    -- Extract positioned packages from nested structure (skip root and layer groups)
    TNode rootNode = positioned
    allPackages = rootNode.children >>= \(TNode layerNode) -> layerNode.children
  in
    allPackages <#> toPositionedPackage

-- | Convert TreemapNode to PositionedPackage
toPositionedPackage :: TreemapNode PackageSetPackage -> PositionedPackage
toPositionedPackage (TNode n) =
  { pkg: n.data_
  , x: n.x0
  , y: n.y0
  , width: n.x1 - n.x0
  , height: n.y1 - n.y0
  }

-- =============================================================================
-- Dependency Map Building (for hover highlighting)
-- =============================================================================

-- | Build a map from package name to the packages it depends on
buildDependsOnMap :: Array PackageSetPackage -> Set String -> Map String (Array String)
buildDependsOnMap packages pkgNames =
  Map.fromFoldable $ packages <#> \pkg ->
    Tuple pkg.name (Array.filter (\dep -> Set.member dep pkgNames) pkg.depends)

-- | Build a reverse map: package name to packages that depend on it
buildDependedByMap :: Array PackageSetPackage -> Set String -> Map String (Array String)
buildDependedByMap packages pkgNames =
  let
    -- For each package's dependencies, record the reverse relationship
    pairs :: Array (Tuple String String)
    pairs = packages >>= \pkg ->
      pkg.depends
        # Array.filter (\dep -> Set.member dep pkgNames)
        <#> \dep -> Tuple dep pkg.name  -- dep is depended on by pkg
  in foldl (\acc (Tuple depended depender) ->
      Map.alter (Just <<< Array.cons depender <<< fromMaybe []) depended acc
    ) Map.empty pairs

-- =============================================================================
-- Rendering (HATS)
-- =============================================================================

-- | Render the treemap SVG (implementation)
renderTreemapSVGImpl :: Config -> Array PackageRenderData -> Boolean -> Effect Unit
renderTreemapSVGImpl config packageData enableHighlighting = do
  let svgTree = buildSVGTree config packageData enableHighlighting
  _ <- rerender config.containerSelector svgTree
  pure unit

-- | Convert PositionedPackage to PackageRenderData (without dependency data)
toPackageRenderDataWithoutDeps :: Config -> PositionedPackage -> PackageRenderData
toPackageRenderDataWithoutDeps config pp =
  let
    cx = pp.x + pp.width / 2.0
    cy = pp.y + pp.height / 2.0
    maxR = (min pp.width pp.height) / 2.0 - 2.0
    kloc = toNumber pp.pkg.totalLoc / 1000.0
    circleR = min maxR (3.0 + sqrt kloc * 3.0)
  in PackageRenderData
    { name: pp.pkg.name
    , topoLayer: pp.pkg.topoLayer
    , inProject: Set.member pp.pkg.name config.projectPackages
    , isTransitive: Set.member pp.pkg.name config.transitivePackages
    , x: pp.x
    , y: pp.y
    , width: pp.width
    , height: pp.height
    , cx
    , cy
    , circleR
    , dependsOn: []
    , dependedBy: []
    }

-- | Convert PositionedPackage to PackageRenderData (with dependency data for highlighting)
toPackageRenderDataWithDeps :: Config -> Map String (Array String) -> Map String (Array String) -> PositionedPackage -> PackageRenderData
toPackageRenderDataWithDeps config dependsOnMap dependedByMap pp =
  let
    cx = pp.x + pp.width / 2.0
    cy = pp.y + pp.height / 2.0
    maxR = (min pp.width pp.height) / 2.0 - 2.0
    kloc = toNumber pp.pkg.totalLoc / 1000.0
    circleR = min maxR (3.0 + sqrt kloc * 3.0)
  in PackageRenderData
    { name: pp.pkg.name
    , topoLayer: pp.pkg.topoLayer
    , inProject: Set.member pp.pkg.name config.projectPackages
    , isTransitive: Set.member pp.pkg.name config.transitivePackages
    , x: pp.x
    , y: pp.y
    , width: pp.width
    , height: pp.height
    , cx
    , cy
    , circleR
    , dependsOn: fromMaybe [] $ Map.lookup pp.pkg.name dependsOnMap
    , dependedBy: fromMaybe [] $ Map.lookup pp.pkg.name dependedByMap
    }

-- | Build the SVG tree structure using forEach
buildSVGTree :: Config -> Array PackageRenderData -> Boolean -> Tree
buildSVGTree config packageData enableHighlighting =
  let colors = themeColors config.theme
  in
  elem SVG
    [ staticStr "id" "galaxy-treemap-svg"
    , staticStr "viewBox" $ "0 0 " <> show config.width <> " " <> show config.height
    , staticStr "width" "100%"
    , staticStr "height" "100%"
    , staticStr "style" $ "background: " <> colors.background <> "; display: block;"
    , staticStr "preserveAspectRatio" "xMidYMid meet"
    ]
    [ -- Package rectangles group with data join
      elem Group
        [ staticStr "class" "treemap-packages" ]
        [ -- forEach: creates one group per package
          if enableHighlighting
            then forEach "packages" Group packageData (\(PackageRenderData d) -> d.name)
                   (packageGroupTreeWithHighlighting config)
            else forEach "packages" Group packageData (\(PackageRenderData d) -> d.name)
                   (packageGroupTree config)
        ]
    ]

-- | Create tree for a single package group (called from forEach template)
packageGroupTree :: Config -> PackageRenderData -> Tree
packageGroupTree config (PackageRenderData d) =
  let
    colors = themeColors config.theme
    -- Use background color for rects - strokes create the grid pattern
    rectFill = colors.background
    -- Stronger stroke for blueprint grid effect
    strokeColor = case config.theme of
      BlueprintTheme -> "rgba(255, 255, 255, 0.6)"  -- Brighter white for blueprint
      _ -> colors.stroke
  in
  elem Group
    [ staticStr "class" $ "treemap-package" <> if d.inProject then " project-package" else ""
    , staticStr "data-name" d.name
    , staticStr "data-layer" (show d.topoLayer)
    ]
    [ -- Rectangle with data-dependent attributes
      elem Rect
        [ thunkedNum "x" d.x
        , thunkedNum "y" d.y
        , thunkedNum "width" d.width
        , thunkedNum "height" d.height
        , staticStr "fill" rectFill
        , staticStr "stroke" strokeColor
        , staticStr "stroke-width" "1"
        , staticStr "class" "package-rect"
        ]
        []
    , -- Cell contents based on config
      renderCellContent config colors (PackageRenderData d)
    ]

-- | Create tree for a single package group with coordinated highlighting behavior
packageGroupTreeWithHighlighting :: Config -> PackageRenderData -> Tree
packageGroupTreeWithHighlighting config (PackageRenderData d) =
  let
    colors = themeColors config.theme
    -- Use background color for rects - strokes create the grid pattern
    rectFill = colors.background
    -- Stronger stroke for blueprint grid effect
    strokeColor = case config.theme of
      BlueprintTheme -> "rgba(255, 255, 255, 0.6)"  -- Brighter white for blueprint
      _ -> colors.stroke
  in
  withBehaviors
    [ onCoordinatedHighlight
        { identify: d.name
        , classify: \hoveredName ->
            if d.name == hoveredName then Primary
            else if Array.elem hoveredName d.dependsOn then Related    -- I depend on the hovered package
            else if Array.elem hoveredName d.dependedBy then Related   -- The hovered package depends on me
            else Dimmed
        , group: Nothing  -- Global coordination
        }
    ]
  $ elem Group
      [ staticStr "class" $ "treemap-package" <> if d.inProject then " project-package" else ""
      , staticStr "data-name" d.name
      , staticStr "data-layer" (show d.topoLayer)
      ]
      [ -- Rectangle with data-dependent attributes
        elem Rect
          [ thunkedNum "x" d.x
          , thunkedNum "y" d.y
          , thunkedNum "width" d.width
          , thunkedNum "height" d.height
          , staticStr "fill" rectFill
          , staticStr "stroke" strokeColor
          , staticStr "stroke-width" "1"
          , staticStr "class" "package-rect"
          ]
          []
      , -- Cell contents based on config
        renderCellContent config colors (PackageRenderData d)
      ]

-- | Render cell content based on cellContents config
renderCellContent :: Config -> { background :: String, stroke :: String, text :: String, textMuted :: String } -> PackageRenderData -> Tree
renderCellContent config colors (PackageRenderData d) = case config.cellContents of
  CellEmpty ->
    elem Group [] []

  CellText ->
    if d.width > 30.0 && d.height > 20.0
    then elem Text
      [ thunkedNum "x" d.cx
      , thunkedNum "y" d.cy
      , staticStr "text-anchor" "middle"
      , staticStr "dominant-baseline" "middle"
      , thunkedStr "font-size" (if d.width > 80.0 then "9" else "7")
      , staticStr "fill" colors.text
      , staticStr "font-family" "'Courier New', Courier, monospace"
      , thunkedStr "textContent" (truncateName (floor (d.width / 6.0)) d.name)
      ]
      []
    else elem Group [] []

  CellCircle ->
    if d.width > 8.0 && d.height > 8.0
    then elem Group []
        [ elem Circle
            [ thunkedNum "cx" d.cx
            , thunkedNum "cy" d.cy
            , thunkedNum "r" d.circleR
            , staticStr "fill" colors.stroke  -- Initial fill
            , staticStr "stroke" colors.text
            , staticStr "stroke-width" "0.5"
            ]
            []
        , -- Label below circle
          if d.width > 50.0 && d.height > 30.0
          then elem Text
            [ thunkedNum "x" d.cx
            , thunkedNum "y" (d.cy + d.circleR + 12.0)
            , staticStr "text-anchor" "middle"
            , thunkedStr "font-size" (if d.width > 80.0 then "9" else "7")
            , staticStr "fill" colors.textMuted
            , staticStr "font-family" "'Courier New', Courier, monospace"
            , thunkedStr "textContent" (truncateName (floor (d.width / 6.0)) d.name)
            ]
            []
          else elem Group [] []
        ]
    else elem Group [] []

  -- ModuleCircles and BubblePack fall back to Circle for package set view
  _ ->
    if d.width > 8.0 && d.height > 8.0
    then elem Group []
        [ elem Circle
            [ thunkedNum "cx" d.cx
            , thunkedNum "cy" d.cy
            , thunkedNum "r" d.circleR
            , staticStr "fill" colors.stroke
            , staticStr "stroke" colors.text
            , staticStr "stroke-width" "0.5"
            ]
            []
        , if d.width > 50.0 && d.height > 30.0
          then elem Text
            [ thunkedNum "x" d.cx
            , thunkedNum "y" (d.cy + d.circleR + 12.0)
            , staticStr "text-anchor" "middle"
            , thunkedStr "font-size" (if d.width > 80.0 then "9" else "7")
            , staticStr "fill" colors.textMuted
            , staticStr "font-family" "'Courier New', Courier, monospace"
            , thunkedStr "textContent" (truncateName (floor (d.width / 6.0)) d.name)
            ]
            []
          else elem Group [] []
        ]
    else elem Group [] []

-- | Truncate name to fit
truncateName :: Int -> String -> String
truncateName maxLen name =
  if String.length name > maxLen
    then String.take maxLen name <> "…"
    else name

-- =============================================================================
-- Position Export (for transitions)
-- =============================================================================

-- | Compute circle center positions for treemap cells (for Treemap -> Beeswarm transition)
-- | Returns array of { name, x, y, r } for each package circle
-- | This is a pure function - no DOM reading needed since positions are computed from data
computeCellPositions :: Config -> Array PackageSetPackage -> Array { name :: String, x :: Number, y :: Number, r :: Number }
computeCellPositions config packages =
  let
    positioned = computeTreemapPositions config packages
  in positioned <#> \pp ->
    let
      cx = pp.x + pp.width / 2.0
      cy = pp.y + pp.height / 2.0
      maxR = (min pp.width pp.height) / 2.0 - 2.0
      kloc = toNumber pp.pkg.totalLoc / 1000.0
      circleR = min maxR (3.0 + sqrt kloc * 3.0)
    in { name: pp.pkg.name, x: cx, y: cy, r: circleR }

-- | DEPRECATED: Use computeCellPositions instead
-- | This function is kept for backwards compatibility but throws an error
getCellPositions :: String -> Effect (Array { name :: String, x :: Number, y :: Number, r :: Number })
getCellPositions _selector = do
  -- Log deprecation warning and return empty - callers should use computeCellPositions
  pure []
