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
import Hylograph.HATS (Tree, elem, staticStr, thunkedStr, thunkedNum, forEach, withBehaviors, onCoordinatedHighlight, onClick)
import Hylograph.HATS.InterpreterTick (rerender, clearContainer)
import Hylograph.Internal.Element.Types (ElementType(..))
import Hylograph.Internal.Behavior.Types (HighlightClass(..))  -- Primary, Related, Upstream, Downstream, Dimmed, Neutral

-- Layout
import DataViz.Layout.Hierarchy.Types (ValuedNode(..))
import DataViz.Layout.Hierarchy.Treemap (TreemapNode(..), treemap, defaultTreemapConfig, squarify, phi)

import CE2.Data.Loader (PackageSetPackage)
import CE2.Types (CellContents(..), ColorMode, ViewTheme, isDarkTheme, themeColors)

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
  , onRectClick :: Maybe (String -> Effect Unit)    -- Click handler for rect → package treemap
  , onCircleClick :: Maybe (String -> Effect Unit)  -- Click handler for circle → neighborhood/SolarSwarm
  , infraLayerThreshold :: Int       -- Hide deps to packages with topoLayer < threshold (0 = show all, 2 = hide layers 0-1)
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
  -- Source info for visual indicators
  , source :: String            -- "registry" | "workspace" | "extra"
  , isApp :: Boolean            -- true if has bundleModule
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

  -- Build dependency maps (filtered by infraLayerThreshold)
  let pkgNames = Set.fromFoldable $ map _.name packages
      dependsOnMap = buildDependsOnMap packages pkgNames config.infraLayerThreshold
      dependedByMap = buildDependedByMap packages pkgNames config.infraLayerThreshold

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
        -- For unparsed packages (registry), use default size so they're visible
        -- Workspace packages use actual LOC; registry gets 500 LOC equivalent
        defaultLocForUnparsed = 500.0
        pkgLeaves :: Array (ValuedNode PackageSetPackage)
        pkgLeaves = layerPkgs <#> \pkg ->
          let pkgValue = if pkg.totalLoc > 0
                         then toNumber pkg.totalLoc
                         else defaultLocForUnparsed
          in VNode
            { data_: pkg
            , depth: 2
            , height: 0
            , value: pkgValue
            , children: []
            , parent: Nothing
            }

        layerValue = Array.foldl (\acc (VNode n) -> acc + n.value) 0.0 pkgLeaves

        -- Layer group node (uses dummy package data)
        layerData :: PackageSetPackage
        layerData = { name: "layer-" <> show layer, id: -layer, version: "", topoLayer: layer, depends: [], description: Nothing, license: Nothing, repositoryOwner: Nothing, repositoryName: Nothing, publishedAt: Nothing, releaseNumber: 0, moduleCount: 0, totalLoc: 0, source: "registry", bundleModule: Nothing }
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
    rootData = { name: "root", id: 0, version: "", topoLayer: -1, depends: [], description: Nothing, license: Nothing, repositoryOwner: Nothing, repositoryName: Nothing, publishedAt: Nothing, releaseNumber: 0, moduleCount: 0, totalLoc: 0, source: "registry", bundleModule: Nothing }

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
-- | When infraThreshold > 0, dependencies on packages with topoLayer < threshold are excluded
buildDependsOnMap :: Array PackageSetPackage -> Set String -> Int -> Map String (Array String)
buildDependsOnMap packages pkgNames infraThreshold =
  let topoMap = Map.fromFoldable $ packages <#> \p -> Tuple p.name p.topoLayer
  in Map.fromFoldable $ packages <#> \pkg ->
    Tuple pkg.name (Array.filter (\dep ->
      Set.member dep pkgNames
      && (fromMaybe 0 (Map.lookup dep topoMap)) >= infraThreshold
    ) pkg.depends)

-- | Build a reverse map: package name to packages that depend on it
-- | When infraThreshold > 0, links to packages with topoLayer < threshold are excluded
buildDependedByMap :: Array PackageSetPackage -> Set String -> Int -> Map String (Array String)
buildDependedByMap packages pkgNames infraThreshold =
  let
    topoMap = Map.fromFoldable $ packages <#> \p -> Tuple p.name p.topoLayer
    -- For each package's dependencies, record the reverse relationship
    -- Filter out deps to low-layer packages
    pairs :: Array (Tuple String String)
    pairs = packages >>= \pkg ->
      pkg.depends
        # Array.filter (\dep ->
            Set.member dep pkgNames
            && (fromMaybe 0 (Map.lookup dep topoMap)) >= infraThreshold)
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
    circleR = min maxR (7.0 + sqrt kloc * 5.0)
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
    , source: pp.pkg.source
    , isApp: pp.pkg.bundleModule /= Nothing
    }

-- | Convert PositionedPackage to PackageRenderData (with dependency data for highlighting)
toPackageRenderDataWithDeps :: Config -> Map String (Array String) -> Map String (Array String) -> PositionedPackage -> PackageRenderData
toPackageRenderDataWithDeps config dependsOnMap dependedByMap pp =
  let
    cx = pp.x + pp.width / 2.0
    cy = pp.y + pp.height / 2.0
    maxR = (min pp.width pp.height) / 2.0 - 2.0
    kloc = toNumber pp.pkg.totalLoc / 1000.0
    circleR = min maxR (7.0 + sqrt kloc * 5.0)
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
    , source: pp.pkg.source
    , isApp: pp.pkg.bundleModule /= Nothing
    }

-- | Build a position map from package name to center coordinates
buildPackagePositionMap :: Array PackageRenderData -> Map String { x :: Number, y :: Number }
buildPackagePositionMap pkgs =
  Map.fromFoldable $ pkgs <#> \(PackageRenderData d) ->
    Tuple d.name { x: d.cx, y: d.cy }

-- | Render all package dependency links as curved paths
renderPackageDependencyLinks :: Map String { x :: Number, y :: Number } -> Array PackageRenderData -> Tree
renderPackageDependencyLinks posMap pkgs =
  let links = Array.concatMap (packageOutgoingLinks posMap) pkgs
  in elem Group
    [ staticStr "class" "dependency-links"
    , staticStr "pointer-events" "none"
    , thunkedStr "data-link-count" (show (Array.length links))
    ]
    links

-- | Generate outgoing links from a single package to its dependencies
packageOutgoingLinks :: Map String { x :: Number, y :: Number } -> PackageRenderData -> Array Tree
packageOutgoingLinks posMap (PackageRenderData d) =
  d.dependsOn # Array.mapMaybe \depName ->
    Map.lookup depName posMap <#> \toPos ->
      packageLinkPath { x: d.cx, y: d.cy } toPos d.name depName

-- | Render a curved link between two package centers
-- | Uses a quadratic Bezier curve, participates in CoordinatedHighlight
packageLinkPath :: { x :: Number, y :: Number } -> { x :: Number, y :: Number } -> String -> String -> Tree
packageLinkPath from to fromName toName =
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
              -- fromName depends on toName, so:
              -- hovering fromName → link shows its upstream dep → Upstream (green)
              -- hovering toName → link shows its downstream dependent → Downstream (orange)
              if hoveredId == fromName then Upstream
              else if hoveredId == toName then Downstream
              else Dimmed
          , group: Nothing  -- Global coordination (same as package cells)
          }
      ]
    $ elem Path
        [ thunkedStr "d" pathD
        , staticStr "fill" "none"
        , staticStr "stroke" "#888"
        , staticStr "stroke-width" "0.75"
        , staticStr "stroke-opacity" "0"  -- Invisible until hover
        , staticStr "class" "dependency-link"
        ]
        []

-- | Build the SVG tree structure using forEach
buildSVGTree :: Config -> Array PackageRenderData -> Boolean -> Tree
buildSVGTree config packageData enableHighlighting =
  let colors = themeColors config.theme
      posMap = buildPackagePositionMap packageData
  in
  elem SVG
    [ staticStr "id" "galaxy-treemap-svg"
    , staticStr "viewBox" $ "0 0 " <> show config.width <> " " <> show config.height
    , staticStr "width" "100%"
    , staticStr "height" "100%"
    , staticStr "style" "background: transparent; display: block;"
    , staticStr "preserveAspectRatio" "xMidYMid meet"
    , thunkedStr "data-infra-threshold" (show config.infraLayerThreshold)
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
    , -- Dependency link layer (on top of packages, pointer-events: none)
      if enableHighlighting
        then renderPackageDependencyLinks posMap packageData
        else elem Group [] []
    ]

-- | Create tree for a single package group (called from forEach template)
packageGroupTree :: Config -> PackageRenderData -> Tree
packageGroupTree config (PackageRenderData d) =
  let
    colors = themeColors config.theme
    -- Use background color for rects - strokes create the grid pattern
    rectFill = colors.background
    -- Stronger stroke for blueprint grid effect
    strokeColor = if isDarkTheme config.theme
      then "rgba(255, 255, 255, 0.6)"
      else colors.stroke

    -- Rectangle element - may have click handler for navigating to package treemap
    rectElem = elem Rect
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

    rectWithClick = case config.onRectClick of
      Just handler -> withBehaviors [ onClick (handler d.name) ] rectElem
      Nothing -> rectElem
  in
  elem Group
    [ staticStr "class" $ "treemap-package" <> if d.inProject then " project-package" else ""
    , staticStr "data-name" d.name
    , staticStr "data-layer" (show d.topoLayer)
    ]
    [ rectWithClick
    , -- Cell contents (circle, text) - circle may have its own click handler
      renderCellContent config colors (PackageRenderData d)
    ]

-- | Create tree for a single package group with coordinated highlighting behavior
-- | Highlight triggers only from circle/label hover, NOT rect background
packageGroupTreeWithHighlighting :: Config -> PackageRenderData -> Tree
packageGroupTreeWithHighlighting config (PackageRenderData d) =
  let
    colors = themeColors config.theme
    -- Use background color for rects - strokes create the grid pattern
    rectFill = colors.background
    -- Stronger stroke for blueprint grid effect
    strokeColor = if isDarkTheme config.theme
      then "rgba(255, 255, 255, 0.6)"
      else colors.stroke

    -- Highlighting behavior — attached to cell content, not the group
    -- so hovering the rect background doesn't trigger highlighting
    highlightBehavior = onCoordinatedHighlight
        { identify: d.name
        , classify: \hoveredName ->
            if d.name == hoveredName then Primary
            -- I depend on the hovered package → hovered is upstream of me → I highlight as Downstream
            else if Array.elem hoveredName d.dependsOn then Downstream
            -- The hovered package depends on me → I am upstream of hovered → I highlight as Upstream
            else if Array.elem hoveredName d.dependedBy then Upstream
            else Dimmed
        , group: Nothing  -- Global coordination
        }

    -- Rectangle element - may have click handler for navigating to package treemap
    rectElem = elem Rect
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

    rectWithClick = case config.onRectClick of
      Just handler -> withBehaviors [ onClick (handler d.name) ] rectElem
      Nothing -> rectElem

    -- Cell content with highlight behavior — only this area triggers hover
    cellContent = renderCellContent config colors (PackageRenderData d)
    highlightedCellContent = withBehaviors [ highlightBehavior ] cellContent
  in
  elem Group
      [ staticStr "class" $ "treemap-package" <> if d.inProject then " project-package" else ""
      , staticStr "data-name" d.name
      , staticStr "data-layer" (show d.topoLayer)
      ]
      [ rectWithClick
      , highlightedCellContent
      ]

-- | Render cell content based on cellContents config
-- | Circles may have click handlers for navigating to SolarSwarm/neighborhood view
renderCellContent :: Config -> { background :: String, stroke :: String, text :: String, textMuted :: String } -> PackageRenderData -> Tree
renderCellContent config colors (PackageRenderData d) =
  let
    -- Circle element with optional click handler
    makeCircle =
      let circleElem = elem Circle
            [ thunkedNum "cx" d.cx
            , thunkedNum "cy" d.cy
            , thunkedNum "r" d.circleR
            , staticStr "fill" colors.stroke  -- Initial fill
            , staticStr "stroke" colors.text
            , staticStr "stroke-width" "0.5"
            , staticStr "class" "package-circle"
            ]
            []
      in case config.onCircleClick of
        Just handler -> withBehaviors [ onClick (handler d.name) ] circleElem
        Nothing -> circleElem

    -- Rounded rectangle for app packages (has bundleModule)
    makeAppRect =
      let rectW = d.circleR * 1.6
          rectH = d.circleR * 1.2
          rectElem = elem Rect
            [ thunkedNum "x" (d.cx - rectW / 2.0)
            , thunkedNum "y" (d.cy - rectH / 2.0)
            , thunkedNum "width" rectW
            , thunkedNum "height" rectH
            , staticStr "rx" "4"
            , staticStr "ry" "4"
            , staticStr "fill" colors.stroke
            , staticStr "stroke" colors.text
            , staticStr "stroke-width" "0.5"
            , staticStr "class" "package-circle app-rect"
            ]
            []
      in case config.onCircleClick of
        Just handler -> withBehaviors [ onClick (handler d.name) ] rectElem
        Nothing -> rectElem

    -- Source indicator letter inside circle
    makeSourceIndicator =
      let letter = if d.source == "registry" then "p"
                   else if d.source == "extra" then "e"
                   else ""  -- workspace: no indicator
          fontSize = max 6.0 (d.circleR * 0.6)
      in if letter /= "" && d.circleR > 5.0
         then elem Text
           [ thunkedNum "x" d.cx
           , thunkedNum "y" d.cy
           , staticStr "text-anchor" "middle"
           , staticStr "dominant-baseline" "central"
           , thunkedStr "font-size" (show fontSize)
           , staticStr "fill" colors.textMuted
           , staticStr "font-family" "'Courier New', Courier, monospace"
           , staticStr "pointer-events" "none"
           , thunkedStr "textContent" letter
           ]
           []
         else elem Group [] []

    -- Label below circle
    makeLabel =
      if d.width > 50.0 && d.height > 30.0
      then elem Text
        [ thunkedNum "x" d.cx
        , thunkedNum "y" (d.cy + d.circleR + 12.0)
        , staticStr "text-anchor" "middle"
        , thunkedStr "font-size" (if d.width > 80.0 then "12" else "9")
        , staticStr "fill" colors.textMuted
        , staticStr "font-family" "'Courier New', Courier, monospace"
        , thunkedStr "textContent" (truncateName (floor (d.width / 6.0)) d.name)
        ]
        []
      else elem Group [] []
  in
  case config.cellContents of
    CellEmpty ->
      elem Group [] []

    CellText ->
      if d.width > 30.0 && d.height > 20.0
      then elem Text
        [ thunkedNum "x" d.cx
        , thunkedNum "y" d.cy
        , staticStr "text-anchor" "middle"
        , staticStr "dominant-baseline" "middle"
        , thunkedStr "font-size" (if d.width > 80.0 then "12" else "9")
        , staticStr "fill" colors.text
        , staticStr "font-family" "'Courier New', Courier, monospace"
        , thunkedStr "textContent" (truncateName (floor (d.width / 6.0)) d.name)
        ]
        []
      else elem Group [] []

    CellCircle ->
      if d.width > 8.0 && d.height > 8.0
      then
        if d.isApp
        then elem Group [] [ makeAppRect, makeLabel ]
        else elem Group [] [ makeCircle, makeSourceIndicator, makeLabel ]
      else elem Group [] []

    -- ModuleCircles and BubblePack fall back to Circle for package set view
    _ ->
      if d.width > 8.0 && d.height > 8.0
      then
        if d.isApp
        then elem Group [] [ makeAppRect, makeLabel ]
        else elem Group [] [ makeCircle, makeSourceIndicator, makeLabel ]
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
      circleR = min maxR (7.0 + sqrt kloc * 5.0)
    in { name: pp.pkg.name, x: cx, y: cy, r: circleR }

-- | DEPRECATED: Use computeCellPositions instead
-- | This function is kept for backwards compatibility but throws an error
getCellPositions :: String -> Effect (Array { name :: String, x :: Number, y :: Number, r :: Number })
getCellPositions _selector = do
  -- Log deprecation warning and return empty - callers should use computeCellPositions
  pure []
