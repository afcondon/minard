-- | Namespace Tree Visualization
-- |
-- | Renders the module namespace hierarchy as a radial tidy tree
-- | (Reingold-Tilford layout in polar coordinates). Root at center,
-- | leaves at perimeter. Zoomable via mouse wheel / pinch.
-- |
-- | Supports package-based filtering: only namespaces contributed by
-- | selected packages are shown. Nodes colored by dominant package category.
-- |
-- | Pipeline: flat array → filter → Data.Tree → tree layout → radial projection → HATS SVG
module CE2.Viz.NamespaceTree
  ( Config
  , render
  ) where

import Prelude

import Control.Comonad.Cofree (head, tail)
import Data.Array as Array
import Data.Foldable (foldl, maximum)
import Data.Int (toNumber)
import Data.List (List(..), fromFoldable)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (sqrt, pi, cos, sin)
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Data.Tuple (Tuple(..))
import Data.Tree (Tree, mkTree) as DT
import Effect (Effect)

-- HATS imports
import Hylograph.HATS (Tree, elem, staticStr, thunkedStr, onZoom, withBehaviors, onClick) as HATS
import Hylograph.HATS.InterpreterTick (rerender)
import Hylograph.Internal.Behavior.Types (ZoomConfig(..), ScaleExtent(..))
import Hylograph.Internal.Element.Types (ElementType(..))

-- Layout imports
import DataViz.Layout.Hierarchy.Tree (tree, defaultTreeConfig)
import DataViz.Layout.Hierarchy.Link (linkBezierRadial)

-- Local imports
import Effect.Class.Console (log)
import CE2.Data.Loader (V2NamespaceTreeNode)
import CE2.Data.PackageCategory (PackageCategory, categoryColor)
import CE2.Types (ViewTheme)

-- =============================================================================
-- Types
-- =============================================================================

type Config =
  { containerSelector :: String
  , theme :: ViewTheme
  , selectedPackages :: Set String
  , nsPkgMap :: Map.Map Int (Array { packageName :: String, moduleCount :: Int })
  , packageCategories :: Map.Map String PackageCategory
  , onNodeClick :: Maybe (String -> Effect Unit)  -- dominant packageName
  }

-- | Node data carried through layout
-- | After layout: x = angle (degrees), y = radius (pixels)
type NodeData =
  { x :: Number
  , y :: Number
  , depth :: Int
  , segment :: String
  , path :: String
  , moduleCount :: Int
  , totalLoc :: Int
  , isLeaf :: Boolean
  , dbId :: Int
  , isSynthetic :: Boolean
  , dominantColor :: String
  , isPathOnly :: Boolean
  , dominantPackage :: Maybe String
  }

-- =============================================================================
-- Public API
-- =============================================================================

render :: Config -> Array V2NamespaceTreeNode -> Effect Unit
render config nodes = do
  let
    -- Filter nodes by selected packages
    filteredNodes = filterByPackages config nodes
    mForest = buildForest config filteredNodes
  log $ "[NamespaceTree] render: input=" <> show (Array.length nodes)
    <> " filtered=" <> show (Array.length filteredNodes)
    <> " selectedPkgs=" <> show (Set.size config.selectedPackages)
    <> " nsPkgMapSize=" <> show (Map.size config.nsPkgMap)
  case mForest of
    Nothing -> pure unit
    Just forest -> do
      -- Always wrap under a virtual root labeled with the selected packages
      let rootLabel = case Set.size config.selectedPackages of
            0 -> "(all packages)"
            n | n <= 3 -> String.joinWith ", " (Set.toUnfoldable config.selectedPackages :: Array String)
            n -> show n <> " packages"
          singleTree = DT.mkTree
                   { x: 0.0, y: 0.0, depth: 0
                   , segment: rootLabel, path: ""
                   , moduleCount: 0, totalLoc: 0
                   , isLeaf: false, dbId: -1, isSynthetic: true
                   , dominantColor: pathOnlyColor
                   , isPathOnly: true
                   , dominantPackage: Nothing
                   }
                   forest
          result = layoutTree singleTree
          hatsTree = renderRadialTree config result
      void $ rerender config.containerSelector hatsTree

-- =============================================================================
-- Filtering
-- =============================================================================

-- | Path-only color for ancestor nodes with no direct modules
pathOnlyColor :: String
pathOnlyColor = "hsl(220, 10%, 80%)"

-- | Filter namespace nodes to only those contributed by selected packages.
-- | Also includes all ancestors (so "Data" appears if "Data.Array" is active).
filterByPackages :: Config -> Array V2NamespaceTreeNode -> Array V2NamespaceTreeNode
filterByPackages config nodes
  | Set.isEmpty config.selectedPackages = nodes  -- No filter if nothing selected
  | Map.isEmpty config.nsPkgMap = nodes           -- No mapping data yet → show all
  | otherwise =
    let
      -- Find namespace IDs where any contributing package is selected
      directlyIncluded :: Set Int
      directlyIncluded =
        let entries :: Array (Tuple Int (Array { packageName :: String, moduleCount :: Int }))
            entries = Map.toUnfoldable config.nsPkgMap
        in foldl (\acc (Tuple nsId pkgs) ->
          let hasSelectedPkg = Array.any (\p -> Set.member p.packageName config.selectedPackages) pkgs
          in if hasSelectedPkg then Set.insert nsId acc else acc
          ) Set.empty entries

      -- Build parentId lookup for walking up ancestors
      parentOf :: Map.Map Int (Maybe Int)
      parentOf = Map.fromFoldable $ nodes <#> \n ->
        Tuple n.id n.parentId

      -- Walk up parentId chains to include all ancestors
      addAncestors :: Set Int -> Set Int
      addAncestors included =
        foldl walkUp included (Set.toUnfoldable included :: Array Int)
        where
        walkUp acc nodeId =
          case Map.lookup nodeId parentOf of
            Just (Just pid) ->
              if Set.member pid acc
              then acc
              else walkUp (Set.insert pid acc) pid
            _ -> acc

      allIncluded = addAncestors directlyIncluded
    in
      Array.filter (\n -> Set.member n.id allIncluded) nodes

-- | Determine the dominant color and package for a namespace node
dominantColorForNode :: Config -> V2NamespaceTreeNode -> { color :: String, isPathOnly :: Boolean, dominantPkg :: Maybe String }
dominantColorForNode config node =
  case Map.lookup node.id config.nsPkgMap of
    Nothing -> { color: pathOnlyColor, isPathOnly: true, dominantPkg: Nothing }
    Just pkgs ->
      -- Filter to only selected packages
      let selectedPkgs = Array.filter (\p -> Set.member p.packageName config.selectedPackages) pkgs
      in case Array.head (Array.sortWith (\p -> negate p.moduleCount) selectedPkgs) of
        Nothing -> { color: pathOnlyColor, isPathOnly: true, dominantPkg: Nothing }
        Just dominant ->
          case Map.lookup dominant.packageName config.packageCategories of
            Just cat -> { color: categoryColor cat, isPathOnly: false, dominantPkg: Just dominant.packageName }
            Nothing -> { color: pathOnlyColor, isPathOnly: false, dominantPkg: Just dominant.packageName }

-- =============================================================================
-- Step 1: Build rose tree from flat array
-- =============================================================================

buildForest :: Config -> Array V2NamespaceTreeNode -> Maybe (List (DT.Tree NodeData))
buildForest config nodes =
  let
    -- Group nodes by parentId
    childrenOf :: Map.Map (Maybe Int) (Array V2NamespaceTreeNode)
    childrenOf = foldl addChild Map.empty nodes
      where
        addChild acc node =
          let key = node.parentId
              existing = fromMaybe [] (Map.lookup key acc)
          in Map.insert key (Array.snoc existing node) acc

    -- Find roots (parentId == Nothing), sorted alphabetically to avoid crossings
    roots = Array.sortWith _.segment $ fromMaybe [] (Map.lookup Nothing childrenOf)

    -- Recursively build tree
    buildNode :: V2NamespaceTreeNode -> DT.Tree NodeData
    buildNode node =
      let children = Array.sortWith _.segment $ fromMaybe [] (Map.lookup (Just node.id) childrenOf)
          childTrees = map buildNode children
          hasChildren = Array.length children > 0
          { color, isPathOnly, dominantPkg } = dominantColorForNode config node
          -- If a node has both children AND modules, add a synthetic leaf
          -- so the modules are represented in the layout
          syntheticLeaf =
            if hasChildren && node.moduleCount > 0
            then [ DT.mkTree
                    { x: 0.0, y: 0.0, depth: 0
                    , segment: "(" <> show node.moduleCount <> " modules)"
                    , path: node.path
                    , moduleCount: node.moduleCount
                    , totalLoc: node.totalLoc
                    , isLeaf: true
                    , dbId: node.id
                    , isSynthetic: true
                    , dominantColor: color
                    , isPathOnly: isPathOnly
                    , dominantPackage: dominantPkg
                    }
                    Nil
                 ]
            else []
          allChildren = childTrees <> syntheticLeaf
      in DT.mkTree
          { x: 0.0, y: 0.0, depth: 0
          , segment: node.segment
          , path: node.path
          , moduleCount: node.moduleCount
          , totalLoc: node.totalLoc
          , isLeaf: node.isLeaf
          , dbId: node.id
          , isSynthetic: false
          , dominantColor: color
          , isPathOnly: isPathOnly
          , dominantPackage: dominantPkg
          }
          (fromFoldable allChildren)
  in
    if Array.length roots == 0
    then Nothing
    else Just (fromFoldable (map buildNode roots))

-- =============================================================================
-- Step 2: Layout
-- =============================================================================

-- | Count leaves in a tree
countLeaves :: forall a. DT.Tree a -> Int
countLeaves t =
  let children = tail t
  in case children of
    Nil -> 1
    _   -> foldl (\acc child -> acc + countLeaves child) 0 children

-- | Find max depth in a tree
maxDepth :: DT.Tree NodeData -> Int
maxDepth t =
  let children = tail t
  in case children of
    Nil -> 0
    _   -> 1 + (fromMaybe 0 $ maximum (map maxDepth children))

type LayoutResult =
  { positioned :: DT.Tree NodeData
  , outerRadius :: Number
  }

layoutTree :: DT.Tree NodeData -> LayoutResult
layoutTree inputTree =
  let
    depth = maxDepth inputTree

    -- Radial sizing: 80px per depth level, minimum 300px
    outerRadius = max 300.0 (toNumber (depth + 1) * 80.0)

    -- Tree layout with angular extent in degrees and radial extent in pixels
    -- width → spread axis → angle (0–360 degrees)
    -- height → depth axis → radius (0–outerRadius pixels)
    config = defaultTreeConfig
      { size = { width: 360.0, height: outerRadius }
      , minSeparation = 1.0
      }

    positioned = tree config inputTree
  in
    { positioned, outerRadius }

-- =============================================================================
-- Step 3: Render radial tree with HATS
-- =============================================================================

-- | Flatten a positioned tree into an array of (node, parentNode) pairs
flattenTree :: DT.Tree NodeData -> Array { node :: NodeData, parent :: Maybe NodeData }
flattenTree t = flattenWith Nothing t
  where
    flattenWith :: Maybe NodeData -> DT.Tree NodeData -> Array { node :: NodeData, parent :: Maybe NodeData }
    flattenWith mParent tree' =
      let nd = head tree'
          self = [{ node: nd, parent: mParent }]
          children = tail tree'
          childResults = foldl (\acc child -> acc <> flattenWith (Just nd) child) [] children
      in self <> childResults

-- | Convert polar (angle degrees, radius) to Cartesian, centered at origin
-- | Uses D3 convention: 0° at top (12 o'clock), clockwise
toCartesian :: NodeData -> { cx :: Number, cy :: Number }
toCartesian nd =
  let angleRad = (nd.x - 90.0) * pi / 180.0
  in { cx: nd.y * cos angleRad, cy: nd.y * sin angleRad }

-- | Node circle radius based on module count
nodeRadius :: NodeData -> Number
nodeRadius nd
  | nd.isSynthetic = 2.0
  | otherwise = clamp 2.5 10.0 (2.5 + sqrt (toNumber nd.moduleCount) * 1.5)

renderRadialTree :: Config -> LayoutResult -> HATS.Tree
renderRadialTree config result =
  let
    r = result.outerRadius
    -- SVG size: diameter + padding for labels
    padding = 200.0
    svgSize = (r + padding) * 2.0
    center = svgSize / 2.0

    -- Flatten tree
    allEntries = flattenTree result.positioned

    -- Render elements
    links = Array.mapMaybe renderLink allEntries
    nodes = map renderNode allEntries
    labels = map renderLabel allEntries

    zoomConfig = ZoomConfig
      { scaleExtent: ScaleExtent 0.3 10.0
      , targetSelector: ".zoom-group"
      }
  in
    HATS.withBehaviors [ HATS.onZoom zoomConfig ] $
      HATS.elem SVG
        [ HATS.thunkedStr "viewBox" $ "0 0 " <> show svgSize <> " " <> show svgSize
        , HATS.staticStr "width" "100%"
        , HATS.staticStr "height" "100%"
        , HATS.staticStr "class" "namespace-tree-svg"
        , HATS.staticStr "style" "font-family: 'Fira Code', 'JetBrains Mono', monospace;"
        ]
        [ -- Zoom target group (transformed by zoom behavior)
          HATS.elem Group
            [ HATS.staticStr "class" "zoom-group" ]
            [ -- Center translation (all radial coords are relative to origin)
              HATS.elem Group
                [ HATS.thunkedStr "transform" $ "translate(" <> show center <> "," <> show center <> ")" ]
                [ -- Links layer
                  HATS.elem Group [ HATS.staticStr "class" "links" ] links
                , -- Nodes layer
                  HATS.elem Group [ HATS.staticStr "class" "nodes" ] nodes
                , -- Labels layer
                  HATS.elem Group [ HATS.staticStr "class" "labels" ] labels
                ]
            ]
        ]

  where
    -- Render a radial bezier link
    -- linkBezierRadial takes (angle_degrees, radius) pairs directly
    renderLink :: { node :: NodeData, parent :: Maybe NodeData } -> Maybe HATS.Tree
    renderLink { node: nd, parent: mParent } =
      case mParent of
        Nothing -> Nothing
        Just p ->
          let d = linkBezierRadial p.x p.y nd.x nd.y
          in Just $ HATS.elem Path
              [ HATS.thunkedStr "d" d
              , HATS.staticStr "fill" "none"
              , HATS.staticStr "stroke" "hsl(220, 10%, 80%)"
              , HATS.staticStr "stroke-width" "1"
              ]
              []

    -- Render a node circle at Cartesian position, colored by dominant package
    renderNode :: { node :: NodeData, parent :: Maybe NodeData } -> HATS.Tree
    renderNode { node: nd } =
      let
        { cx, cy } = toCartesian nd
        radius = nodeRadius nd
        opacity = if nd.isPathOnly then 0.3
                  else if nd.moduleCount > 0
                       then clamp 0.5 1.0 (0.5 + toNumber nd.moduleCount / 50.0)
                       else 0.35
        circleElem = HATS.elem Circle
          [ HATS.thunkedStr "cx" (show cx)
          , HATS.thunkedStr "cy" (show cy)
          , HATS.thunkedStr "r" (show radius)
          , HATS.thunkedStr "fill" nd.dominantColor
          , HATS.thunkedStr "opacity" (show opacity)
          , HATS.staticStr "cursor" (case nd.dominantPackage of
              Just _ -> "pointer"
              Nothing -> "default")
          ]
          []
      in case config.onNodeClick, nd.dominantPackage of
        Just handler, Just pkg -> HATS.withBehaviors [ HATS.onClick (handler pkg) ] circleElem
        _, _ -> circleElem

    -- Render a text label with radial orientation
    renderLabel :: { node :: NodeData, parent :: Maybe NodeData } -> HATS.Tree
    renderLabel { node: nd } =
      let
        { cx, cy } = toCartesian nd
        radius = nodeRadius nd

        -- Virtual root at center: render horizontally, not radially
        isRoot = nd.depth == 0 && nd.isSynthetic

        -- Label text
        isLeafNode = nd.isLeaf || nd.isSynthetic
        label = if nd.isSynthetic then nd.segment
                else if isLeafNode && nd.moduleCount > 0
                then nd.segment <> " (" <> show nd.moduleCount <> ")"
                else nd.segment

        -- Font styling
        fontSize = if isRoot then "10"
                   else if nd.isSynthetic then "7"
                   else "9"
        fill = if isRoot then "hsl(0, 0%, 40%)"
               else if nd.isPathOnly then "hsl(0, 0%, 60%)"
               else if nd.isSynthetic then "hsl(0, 0%, 60%)"
               else "hsl(0, 0%, 25%)"

        -- Radial label orientation
        -- Right side (angle < 180°): text reads left-to-right, anchor start
        -- Left side (angle ≥ 180°): flip 180° for readability, anchor end
        isRightSide = nd.x < 180.0
        textRotation = if isRightSide then nd.x - 90.0 else nd.x + 90.0
        anchor = if isRoot then "middle"
                 else if isRightSide then "start" else "end"
        dx = if isRoot then 0.0
             else if isRightSide then radius + 4.0 else negate (radius + 4.0)
      in
        if label == "" then
          HATS.elem Group [] []
        else if isRoot then
          -- Root label: horizontal, centered at origin
          HATS.elem Group
            [ HATS.thunkedStr "transform" $
                "translate(" <> show cx <> "," <> show cy <> ")"
            ]
            [ HATS.elem Text
                [ HATS.staticStr "dy" "0.31em"
                , HATS.staticStr "text-anchor" "middle"
                , HATS.staticStr "font-size" fontSize
                , HATS.staticStr "fill" fill
                , HATS.staticStr "font-weight" "600"
                , HATS.thunkedStr "textContent" label
                ]
                []
            ]
        else
          HATS.elem Group
            [ HATS.thunkedStr "transform" $
                "translate(" <> show cx <> "," <> show cy <> ") rotate(" <> show textRotation <> ")"
            ]
            [ HATS.elem Text
                [ HATS.thunkedStr "dx" (show dx)
                , HATS.staticStr "dy" "0.31em"
                , HATS.staticStr "text-anchor" anchor
                , HATS.staticStr "font-size" fontSize
                , HATS.staticStr "fill" fill
                , HATS.thunkedStr "textContent" label
                ]
                []
            ]
