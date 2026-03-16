-- | System Sankey — Architecture diagram for the Minard system
-- |
-- | A HATS-based Sankey diagram showing the flow of data through
-- | Minard's pipeline: sources → loader → DB → API → views → editor.
-- | Includes AI and human interaction paths (one-directional for DAG).
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
-- Static DAG Data — 5 columns
-- =============================================================================

-- | Col 1: Sources → Col 2: Infrastructure → Col 3: API → Col 4: Views → Col 5: Editor
-- | Plus AI and Human as cross-cutting participants
systemLinks :: Array LinkCSVRow
systemLinks =
  -- Col 1 → Col 2: Sources feed the loader
  [ { s: ".purs source",   t: "Rust Loader",  v: 50.0 }
  , { s: "spago config",   t: "Rust Loader",  v: 12.0 }
  , { s: "git history",    t: "Rust Loader",  v: 15.0 }

  -- Col 2 → Col 3: Loader populates DB, API serves it
  , { s: "Rust Loader",    t: "DuckDB",       v: 77.0 }
  , { s: "DuckDB",         t: "API Server",   v: 85.0 }

  -- AI writes annotations into DB
  , { s: "AI (Claude)",    t: "DuckDB",       v: 12.0 }

  -- Col 3 → Col 4: API feeds all four view families
  , { s: "API Server",     t: "Maps",         v: 25.0 }
  , { s: "API Server",     t: "Reports",      v: 22.0 }
  , { s: "API Server",     t: "Anatomy",      v: 18.0 }
  , { s: "API Server",     t: "Git",          v: 18.0 }

  -- Human reviews in Reports
  , { s: "Human",          t: "Reports",      v: 5.0 }

  -- Col 4 → Col 5: All views connect to VS Code
  , { s: "Maps",           t: "VS Code",      v: 25.0 }
  , { s: "Reports",        t: "VS Code",      v: 27.0 }
  , { s: "Anatomy",        t: "VS Code",      v: 18.0 }
  , { s: "Git",            t: "VS Code",      v: 18.0 }
  ]

-- =============================================================================
-- Node Colors
-- =============================================================================

nodeColor :: String -> String
nodeColor = case _ of
  ".purs source"  -> "#8B6914"
  "spago config"  -> "#A67C2E"
  "git history"   -> "#7A8B6E"
  "AI (Claude)"   -> "#7B68AD"
  "Human"         -> "#C07040"
  "Rust Loader"   -> "#6B8FA3"
  "DuckDB"        -> "#4A7A8A"
  "API Server"    -> "#5A7A9A"
  "Maps"          -> "#2563EB"
  "Reports"       -> "#D97706"
  "Anatomy"       -> "#059669"
  "Git"           -> "#7C3AED"
  "VS Code"       -> "#333333"
  _               -> "#999999"

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
        , sourceX1: fromMaybe 0.0 $ map _.x1 srcNode  -- right edge of source node
        , targetX0: fromMaybe 0.0 $ map _.x0 tgtNode  -- left edge of target node
        }
    tree = buildSankey width height nodeFlats linkFlats
  _ <- HATSInterp.rerender selector tree
  pure unit

type NodeFlat = { name :: String, x0 :: Number, y0 :: Number, x1 :: Number, y1 :: Number }
type LinkFlat = { pathD :: String, sourceName :: String, targetName :: String, sourceX1 :: Number, targetX0 :: Number }

buildSankey :: Number -> Number -> Array NodeFlat -> Array LinkFlat -> HATS.Tree
buildSankey width height nodes links =
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
        [ linksLayer <> nodesLayer <> labelsLayer ]
    ]
  where
  -- Links with gradient fills
  linksLayer = HATS.forEach "links" Path links (\l -> l.sourceName <> "→" <> l.targetName) \link ->
    let
      linkId = link.sourceName <> "→" <> link.targetName
      -- Find index of this link for gradient reference
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

  -- Labels (using thunkedStr for textContent)
  labelsLayer = HATS.forEach "labels" Text nodes _.name \node ->
    let
      nodeH = node.y1 - node.y0
      isRightSide = node.x0 > width * 0.65
      labelX = if isRightSide then node.x0 - 4.0 else node.x1 + 4.0
      labelY = node.y0 + nodeH / 2.0
      anchor = if isRightSide then "end" else "start"
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
           , HATS.staticStr "font-size" "10"
           , HATS.staticStr "font-weight" "500"
           , HATS.staticStr "fill" "#333"
           , HATS.staticStr "font-family" "-apple-system, 'Helvetica Neue', sans-serif"
           , HATS.thunkedStr "textContent" node.name
           ] []
