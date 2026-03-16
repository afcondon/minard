-- | System Sankey — Architecture diagram for the Minard system
-- |
-- | A HATS-based Sankey diagram showing the flow of data through
-- | Minard's pipeline: sources → loader → DB → API → view families → pages.
-- | Module Structure is the convergence point where Maps and Reports merge.
module CE2.Viz.SystemSankey
  ( renderSystemSankey
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe)
import DataViz.Layout.Sankey.Compute as Sankey
import DataViz.Layout.Sankey.Path as SankeyPath
import DataViz.Layout.Sankey.Types (LinkCSVRow)
import Effect (Effect)
import Hylograph.HATS (Tree, elem, forEach, staticStr, thunkedStr, withBehaviors, onCoordinatedHighlight, HighlightClass(..)) as HATS
import Hylograph.HATS.Friendly as F
import Hylograph.HATS.InterpreterTick as HATSInterp
import Hylograph.Internal.Element.Types (ElementType(..))

-- =============================================================================
-- Static DAG Data
-- =============================================================================

-- | Col 1: Sources
-- | Col 2: Rust Loader
-- | Col 3: DuckDB
-- | Col 4: API Server
-- | Col 5: View families (Maps, Reports, Anatomy, Git)
-- | Col 6: Individual pages (with Module Structure shared by Maps + Reports)
systemLinks :: Array LinkCSVRow
systemLinks =
  -- === Sources → Loader ===
  [ { s: ".purs source",    t: "Rust Loader",      v: 50.0 }
  , { s: "spago config",    t: "Rust Loader",      v: 12.0 }
  , { s: "git repo",        t: "Rust Loader",      v: 15.0 }
  , { s: "registry repos",  t: "Rust Loader",      v: 10.0 }

  -- === Loader → DB ===
  , { s: "Rust Loader",     t: "DuckDB",           v: 87.0 }

  -- === AI contributes to Reports (annotations surface there) ===
  , { s: "AI (Claude)",     t: "Reports",          v: 12.0 }

  -- === DB → API Server ===
  , { s: "DuckDB",          t: "API Server",       v: 95.0 }

  -- === API Server queries git directly ===
  , { s: "git repo",        t: "API Server",       v: 8.0 }

  -- === API → View families ===
  , { s: "API Server",      t: "Maps",             v: 28.0 }
  , { s: "API Server",      t: "Reports",          v: 22.0 }
  , { s: "API Server",      t: "Anatomy",          v: 20.0 }
  , { s: "API Server",      t: "Git",              v: 22.0 }

  -- === Human → Reports ===
  , { s: "Human",           t: "Reports",          v: 5.0 }

  -- === Maps → pages ===
  , { s: "Maps",            t: "Galaxy",            v: 10.0 }
  , { s: "Maps",            t: "Pkg Treemap",       v: 10.0 }
  , { s: "Maps",            t: "Module Structure",  v: 8.0 }

  -- === Reports → pages ===
  , { s: "Reports",         t: "Pkg Report",        v: 14.0 }
  , { s: "Reports",         t: "Annotations",       v: 8.0 }
  , { s: "Reports",         t: "Module Structure",  v: 5.0 }  -- merges with Maps

  -- === Anatomy → pages ===
  , { s: "Anatomy",         t: "Project Anatomy",   v: 8.0 }
  , { s: "Anatomy",         t: "Decomposition",     v: 10.0 }

  -- === Git → pages ===
  , { s: "Git",             t: "Git Overview",      v: 8.0 }
  , { s: "Git",             t: "Commit Grid",       v: 8.0 }
  , { s: "Git",             t: "Signatures",        v: 6.0 }
  ]

-- | Edge labels for key connections
type EdgeLabel = { source :: String, target :: String, label :: String }

edgeLabels :: Array EdgeLabel
edgeLabels =
  [ { source: "DuckDB",      target: "API Server",  label: "SQL" }
  , { source: "git repo",    target: "API Server",  label: "blame, log" }
  , { source: "AI (Claude)", target: "Reports",     label: "annotations" }
  , { source: "Human",       target: "Reports",     label: "review" }
  ]

-- =============================================================================
-- Node Colors
-- =============================================================================

nodeColor :: String -> String
nodeColor = case _ of
  -- Sources
  ".purs source"    -> "#8B6914"
  "spago config"    -> "#A67C2E"
  "git repo"        -> "#7A8B6E"
  "registry repos"  -> "#6A8A5A"
  "AI (Claude)"     -> "#7B68AD"
  "Human"           -> "#C07040"
  -- Infrastructure
  "Rust Loader"     -> "#6B8FA3"
  "DuckDB"          -> "#4A7A8A"
  "API Server"      -> "#5A7A9A"
  -- View families
  "Maps"            -> "#2563EB"
  "Reports"         -> "#D97706"
  "Anatomy"         -> "#059669"
  "Git"             -> "#7C3AED"
  -- Maps pages
  "Galaxy"          -> "#60A5FA"
  "Pkg Treemap"     -> "#93C5FD"
  -- Reports pages
  "Pkg Report"      -> "#FBBF24"
  "Annotations"     -> "#F59E0B"
  -- Shared page
  "Module Structure" -> "#6366F1"
  -- Anatomy pages
  "Project Anatomy"  -> "#34D399"
  "Decomposition"    -> "#6EE7B7"
  -- Git pages
  "Git Overview"     -> "#A78BFA"
  "Commit Grid"      -> "#C4B5FD"
  "Signatures"       -> "#DDD6FE"
  _                  -> "#999999"

-- =============================================================================
-- Rendering
-- =============================================================================

renderSystemSankey :: String -> Number -> Number -> Effect Unit
renderSystemSankey selector width height = do
  let
    layoutResult = Sankey.computeLayout systemLinks width height
    nodeFlats = layoutResult.nodes <#> \n ->
      { name: n.name, x0: n.x0, y0: n.y0, x1: n.x1, y1: n.y1 }
    linkFlats = layoutResult.links <#> \link ->
      let
        srcNode = SankeyPath.findNode layoutResult.nodes link.sourceIndex
        tgtNode = SankeyPath.findNode layoutResult.nodes link.targetIndex
      in
        { pathD: SankeyPath.generateLinkPath layoutResult.nodes link
        , sourceName: fromMaybe "" $ map _.name srcNode
        , targetName: fromMaybe "" $ map _.name tgtNode
        , sourceX1: fromMaybe 0.0 $ map _.x1 srcNode
        , targetX0: fromMaybe 0.0 $ map _.x0 tgtNode
        , midX: (fromMaybe 0.0 (map _.x1 srcNode) + fromMaybe 0.0 (map _.x0 tgtNode)) / 2.0
        , midY: (link.y0 + link.y1) / 2.0
        }
    -- Filter links that have labels
    labeledLinks = Array.mapMaybe (\lf ->
      Array.find (\el -> el.source == lf.sourceName && el.target == lf.targetName) edgeLabels
        <#> \el -> { label: el.label, midX: lf.midX, midY: lf.midY, sourceName: lf.sourceName }
    ) linkFlats
    tree = buildSankey width height nodeFlats linkFlats labeledLinks
  _ <- HATSInterp.rerender selector tree
  pure unit

type NodeFlat = { name :: String, x0 :: Number, y0 :: Number, x1 :: Number, y1 :: Number }
type LinkFlat =
  { pathD :: String, sourceName :: String, targetName :: String
  , sourceX1 :: Number, targetX0 :: Number
  , midX :: Number, midY :: Number
  }
type LabeledLink = { label :: String, midX :: Number, midY :: Number, sourceName :: String }

buildSankey :: Number -> Number -> Array NodeFlat -> Array LinkFlat -> Array LabeledLink -> HATS.Tree
buildSankey width height nodes links labeledLinks =
  HATS.elem SVG
    [ F.viewBox 0.0 0.0 (width + 20.0) (height + 20.0)
    , F.preserveAspectRatio "xMidYMid meet"
    ]
    [ -- Gradient definitions
      HATS.elem Defs []
        (Array.mapWithIndex (\i link ->
          let
            gradId = "grad-" <> show i
            srcColor = nodeColor link.sourceName
            tgtColor = nodeColor link.targetName
          in HATS.elem LinearGradient
               [ HATS.staticStr "id" gradId
               , HATS.staticStr "gradientUnits" "userSpaceOnUse"
               , HATS.staticStr "x1" (show link.sourceX1)
               , HATS.staticStr "x2" (show link.targetX0)
               ]
               [ HATS.elem Stop
                   [ HATS.staticStr "offset" "0%"
                   , HATS.staticStr "stop-color" srcColor
                   , HATS.staticStr "stop-opacity" "0.35"
                   ] []
               , HATS.elem Stop
                   [ HATS.staticStr "offset" "100%"
                   , HATS.staticStr "stop-color" tgtColor
                   , HATS.staticStr "stop-opacity" "0.35"
                   ] []
               ]
        ) links)
    , HATS.elem Group [ F.transform "translate(10,10)" ]
        [ linksLayer <> nodesLayer <> nodeLabelsLayer <> edgeLabelsLayer ]
    ]
  where
  -- Links with gradient fills
  linksLayer = HATS.forEach "links" Path links (\l -> l.sourceName <> "→" <> l.targetName) \link ->
    let
      linkId = link.sourceName <> "→" <> link.targetName
      linkIdx = fromMaybe 0 $ Array.findIndex (\l -> (l.sourceName <> "→" <> l.targetName) == linkId) links
      gradRef = "url(#grad-" <> show linkIdx <> ")"
    in HATS.withBehaviors
         [ HATS.onCoordinatedHighlight
             { identify: linkId
             , classify: \hoveredId ->
                 if hoveredId == linkId then HATS.Primary
                 else if hoveredId == link.sourceName || hoveredId == link.targetName then HATS.Related
                 else HATS.Dimmed
             , group: Nothing
             }
         ] $
         HATS.elem Path
           [ F.d link.pathD
           , F.attr "fill" gradRef
           , F.stroke "none"
           , F.attr "class" "sankey-link"
           ] []

  -- Nodes
  nodesLayer = HATS.forEach "nodes" Rect nodes _.name \node ->
    let
      w = node.x1 - node.x0
      nodeH = node.y1 - node.y0
      color = nodeColor node.name
    in HATS.withBehaviors
         [ HATS.onCoordinatedHighlight
             { identify: node.name
             , classify: \hoveredId ->
                 if hoveredId == node.name then HATS.Primary
                 else HATS.Dimmed
             , group: Nothing
             }
         ] $
         HATS.elem Rect
           [ F.x node.x0, F.y node.y0
           , F.width w, F.height nodeH
           , F.fill color
           , F.attr "rx" "2"
           , F.attr "class" "sankey-node"
           , F.style "cursor: pointer"
           ] []

  -- Node labels
  nodeLabelsLayer = HATS.forEach "labels" Text nodes _.name \node ->
    let
      nodeH = node.y1 - node.y0
      isRightSide = node.x0 > width * 0.6
      labelX = if isRightSide then node.x0 - 4.0 else node.x1 + 4.0
      labelY = node.y0 + nodeH / 2.0
      anchor = if isRightSide then "end" else "start"
      -- Smaller font for leaf pages
      isLeafPage = node.x0 > width * 0.7
      fontSize = if isLeafPage then "9" else "10"
    in HATS.withBehaviors
         [ HATS.onCoordinatedHighlight
             { identify: node.name
             , classify: \hoveredId ->
                 if hoveredId == node.name then HATS.Primary
                 else HATS.Dimmed
             , group: Nothing
             }
         ] $
         HATS.elem Text
           [ HATS.thunkedStr "x" (show labelX)
           , HATS.thunkedStr "y" (show labelY)
           , HATS.staticStr "text-anchor" anchor
           , HATS.staticStr "dominant-baseline" "middle"
           , HATS.staticStr "font-size" fontSize
           , HATS.staticStr "font-weight" "500"
           , HATS.staticStr "fill" "#333"
           , HATS.staticStr "font-family" "-apple-system, 'Helvetica Neue', sans-serif"
           , HATS.thunkedStr "textContent" node.name
           ] []

  -- Edge labels (small italic text at midpoint of labeled links)
  edgeLabelsLayer = HATS.forEach "edge-labels" Text labeledLinks (\l -> l.sourceName <> "-label") \ll ->
    HATS.elem Text
      [ HATS.thunkedStr "x" (show ll.midX)
      , HATS.thunkedStr "y" (show (ll.midY - 3.0))
      , HATS.staticStr "text-anchor" "middle"
      , HATS.staticStr "dominant-baseline" "middle"
      , HATS.staticStr "font-size" "8"
      , HATS.staticStr "font-style" "italic"
      , HATS.staticStr "font-weight" "400"
      , HATS.staticStr "fill" "#888"
      , HATS.staticStr "font-family" "-apple-system, 'Helvetica Neue', sans-serif"
      , HATS.thunkedStr "textContent" ll.label
      ] []
