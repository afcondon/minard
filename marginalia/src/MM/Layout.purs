-- | Pure layout: turn a decoded TreeData into a flat list of positioned cells.
-- |
-- | Two rectangle tiers via the Hylograph squarify treemap -- domains (depth 1) and
-- | umbrellas (depth 2, treated as leaves so the rectangles stop there). Inside each
-- | umbrella we run a TWO-LEVEL hierarchical circle pack (Hylograph's recursive `pack`):
-- | the umbrella's direct children become outer bubbles, and any child that is itself a
-- | container shows its leaves as inner bubbles. Most umbrellas are flat (their children
-- | are leaves) and read as a single ring of dots; the deep ones reveal their structure.
module MM.Layout
  ( Cell
  , Bubble
  , layoutTree
  ) where

import Prelude

import Data.Array (concatMap, length, null)
import Data.Maybe (Maybe(..))
import DataViz.Layout.Hierarchy.Pack (HierarchyData(..), PackNode(..), defaultPackConfig, hierarchy, pack)
import DataViz.Layout.Hierarchy.Treemap (TreemapNode(..), defaultTreemapConfig, phi, squarify, treemap)
import DataViz.Layout.Hierarchy.Types (ValuedNode(..))
import MM.Types (TreeData(..), TreeNode(..))

-- | A positioned rectangle. depth 1 = domain region, depth 2 = umbrella (carries bubbles).
type Cell =
  { id :: Int
  , name :: String
  , domain :: Maybe String
  , status :: Maybe String
  , tags :: Array String
  , tagline :: Maybe String
  , description :: Maybe String
  , cover :: Maybe String
  , repoUrl :: Maybe String
  , count :: Int -- leaf-descendant count (the "N sub-projects" figure)
  , x :: Number
  , y :: Number
  , w :: Number
  , h :: Number
  , depth :: Int
  , leaf :: Boolean
  , bubbles :: Array Bubble
  }

-- | A packed project, in absolute SVG coordinates. depth 1 = direct child of the umbrella,
-- | depth 2 = a leaf inside a child-container. leaf = has no inner bubbles.
type Bubble =
  { id :: Int
  , name :: String
  , domain :: Maybe String
  , status :: Maybe String
  , tags :: Array String
  , description :: Maybe String
  , cover :: Maybe String
  , repoUrl :: Maybe String
  , depth :: Int
  , leaf :: Boolean
  , x :: Number
  , y :: Number
  , r :: Number
  }

-- Treemap (rectangles) -----------------------------------------------------

toValued :: Int -> Int -> TreeNode -> ValuedNode TreeNode
toValued rectDepth depth tn@(TreeNode n) =
  VNode
    { data_: tn
    , depth
    , height: 0
    , parent: Nothing
    , value: n.value
    , children: if depth >= rectDepth then [] else map (toValued rectDepth (depth + 1)) n.children
    }

leafDescendants :: TreeNode -> Array TreeNode
leafDescendants tn@(TreeNode n) =
  if null n.children then [ tn ] else concatMap leafDescendants n.children

-- Mondrian line-width for the editorial (one-tier) view; zero for the dense atlas view.
lineWidth :: Int -> Number
lineWidth d = if d == 1 then 16.0 else 0.0

layoutTree :: Number -> Number -> TreeData -> Array Cell
layoutTree w h (TreeData td) =
  let
    rectDepth = td.rectDepth
    pad = if rectDepth == 1 then { inner: 0.0, outer: 0.0 } else { inner: 8.0, outer: 4.0 }
    -- Editorial gridlines: lay out in a region shrunk by one line-width and inset each cell
    -- by the same in mkCell, so the inner lines (two cells meeting) equal the outer frame.
    line = lineWidth rectDepth
    root = toValued rectDepth 0 td.nodes
    cfg = defaultTreemapConfig
      { size = { width: w - line, height: h - line }
      , paddingInner = pad.inner
      , paddingOuter = pad.outer
      , tile = squarify phi
      }
  in
    walk rectDepth (treemap cfg root)

walk :: Int -> TreemapNode TreeNode -> Array Cell
walk rectDepth (TNode n) =
  let
    self = if n.depth == 0 then [] else [ mkCell rectDepth (TNode n) ]
  in
    self <> concatMap (walk rectDepth) n.children

mkCell :: Int -> TreemapNode TreeNode -> Cell
mkCell rectDepth (TNode n) =
  let
    TreeNode d = n.data_
    -- inset each editorial cell by one line-width; combined with the shrunk layout region
    -- (see layoutTree) the black backdrop reads as equal De Stijl lines everywhere.
    line = lineWidth rectDepth
    x = n.x0 + line
    y = n.y0 + line
    w = (n.x1 - n.x0) - line
    h = (n.y1 - n.y0) - line
    isLeaf = n.depth >= rectDepth
  in
    { id: d.id
    , name: d.name
    , domain: d.domain
    , status: d.status
    , tags: d.tags
    , tagline: d.tagline
    , description: d.description
    , cover: d.cover
    , repoUrl: d.repoUrl
    , count: if isLeaf then length (leafDescendants n.data_) else 0
    , x
    , y
    , w
    , h
    , depth: n.depth
    , leaf: isLeaf
    , bubbles: if isLeaf then packBubbles x y w h n.data_ else []
    }

-- Two-level circle pack (bubbles) ------------------------------------------

-- An umbrella's direct children become outer bubbles; a child that is itself a container
-- shows its leaves (flattened beyond two levels) as inner bubbles. A childless umbrella
-- shows itself as a single bubble.
umbrellaHier :: TreeNode -> HierarchyData TreeNode
umbrellaHier tn@(TreeNode n) =
  let kids = if null n.children then [ tn ] else n.children
  in HierarchyData { data_: tn, value: Nothing, children: Just (map childHier kids) }

childHier :: TreeNode -> HierarchyData TreeNode
childHier c@(TreeNode cn) =
  if null cn.children then leafHier c
  else HierarchyData { data_: c, value: Nothing, children: Just (map leafHier (leafDescendants c)) }

leafHier :: TreeNode -> HierarchyData TreeNode
leafHier l = HierarchyData { data_: l, value: Just 1.0, children: Nothing }

packBubbles :: Number -> Number -> Number -> Number -> TreeNode -> Array Bubble
packBubbles x0 y0 w h tn =
  let
    bw = w * 0.84
    bh = h * 0.84
    cfg = defaultPackConfig { size = { width: bw, height: bh }, padding = 2.0 }
    packed = pack cfg (hierarchy (umbrellaHier tn))
    ox = x0 + (w - bw) / 2.0
    oy = y0 + (h - bh) / 2.0
  in
    flattenPack ox oy packed

flattenPack :: Number -> Number -> PackNode TreeNode -> Array Bubble
flattenPack ox oy (PackNode n) =
  let
    self = if n.depth == 0 then [] else [ mkBubble ox oy (PackNode n) ]
  in
    self <> concatMap (flattenPack ox oy) n.children

mkBubble :: Number -> Number -> PackNode TreeNode -> Bubble
mkBubble ox oy (PackNode n) =
  let
    TreeNode d = n.data_
  in
    { id: d.id
    , name: d.name
    , domain: d.domain
    , status: d.status
    , tags: d.tags
    , description: d.description
    , cover: d.cover
    , repoUrl: d.repoUrl
    , depth: n.depth
    , leaf: null n.children
    , x: ox + n.x
    , y: oy + n.y
    , r: n.r
    }
