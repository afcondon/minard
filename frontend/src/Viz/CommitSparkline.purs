-- | Commit Sparkline (pure SVG data)
-- |
-- | Produces SVG rect specs for a per-commit sparkline visualization.
-- | Diverging bar chart around a central axis:
-- |   - Above center: additions (gray = total commit, green = this module)
-- |   - Below center: deletions (gray = total commit, red = this module)
-- | Heights are log-scaled to handle the wide range of commit sizes.
-- | Bars space out to fill available width.
module CE2.Viz.CommitSparkline
  ( SparklineBar
  , SparklineRect
  , prepareData
  , toSvgRects
  , renderSparkline
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl)
import Data.Int (toNumber)
import Data.Maybe (fromMaybe)
import Data.Number (log) as Num
import Foreign.Object as Object
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import CE2.Data.Loader as Loader
import CE2.Util.SVG (svgElem, sa)

-- =============================================================================
-- Types
-- =============================================================================

-- | Pre-computed bar data for one commit (ready for rendering)
type SparklineBar =
  { totalAdded :: Int     -- total lines added across all files in commit
  , totalDeleted :: Int   -- total lines deleted across all files in commit
  , moduleAdded :: Int    -- lines added in this module
  , moduleDeleted :: Int  -- lines deleted from this module
  , commitHash :: String
  , message :: String
  }

-- | A positioned, colored rectangle for SVG rendering
type SparklineRect =
  { x :: Number
  , y :: Number
  , width :: Number
  , height :: Number
  , fill :: String
  }

-- =============================================================================
-- Data preparation (pure)
-- =============================================================================

-- | Extract sparkline bars for a specific module from numstat commits.
-- | Returns bars in chronological order (oldest first = leftmost).
prepareData :: String -> Array Loader.NumstatCommit -> Array SparklineBar
prepareData moduleName commits =
  Array.reverse commits <#> \c ->
    let modStats = Object.lookup moduleName c.modules
    in { totalAdded: c.totalAdded
       , totalDeleted: c.totalDeleted
       , moduleAdded: fromMaybe 0 (map _.added modStats)
       , moduleDeleted: fromMaybe 0 (map _.deleted modStats)
       , commitHash: c.shortHash
       , message: c.message
       }

-- =============================================================================
-- SVG rect generation (pure)
-- =============================================================================

-- | Convert sparkline bars to SVG rects (pure, no Effect).
-- | Diverging layout: additions above center, deletions below.
-- | viewBox dimensions should be the target size; bars space out to fill it.
toSvgRects :: { width :: Number, height :: Number } -> Array SparklineBar -> Array SparklineRect
toSvgRects dims bars =
  let nBars = Array.length bars
      n = toNumber nBars
      pitch = if nBars > 0 then dims.width / n else 1.0
      barW = max 0.5 (pitch * 0.6)
      maxVal = foldl (\acc b -> max acc (max b.totalAdded b.totalDeleted)) 1 bars
      logMax = logScale (toNumber maxVal)
      halfH = dims.height / 2.0
      centerY = halfH
  in
    -- Center axis line
    [{ x: 0.0, y: centerY - 0.25, width: dims.width, height: 0.5, fill: "#e0e0e0" }]
    <> Array.concatMap (\{ idx, bar } ->
      let x = toNumber idx * pitch + (pitch - barW) / 2.0
      in
        -- Gray additions (above center)
        (if bar.totalAdded > 0
          then let barH = halfH * logScale (toNumber bar.totalAdded) / logMax
               in [{ x, y: centerY - barH, width: barW, height: barH, fill: "#d4d4d4" }]
          else [])
        -- Gray deletions (below center)
        <> (if bar.totalDeleted > 0
          then let barH = halfH * logScale (toNumber bar.totalDeleted) / logMax
               in [{ x, y: centerY, width: barW, height: barH, fill: "#d4d4d4" }]
          else [])
        -- Green module additions (above center)
        <> (if bar.moduleAdded > 0
          then let barH = halfH * logScale (toNumber bar.moduleAdded) / logMax
               in [{ x, y: centerY - barH, width: barW, height: barH, fill: "#22c55e" }]
          else [])
        -- Red module deletions (below center)
        <> (if bar.moduleDeleted > 0
          then let barH = halfH * logScale (toNumber bar.moduleDeleted) / logMax
               in [{ x, y: centerY, width: barW, height: barH, fill: "#ef4444" }]
          else [])
    ) (Array.mapWithIndex (\i b -> { idx: i, bar: b }) bars)

-- =============================================================================
-- Helpers
-- =============================================================================

-- | Log scale: log(1 + n) to compress wide ranges while keeping 0 as 0
logScale :: Number -> Number
logScale n = Num.log (1.0 + n)

-- =============================================================================
-- HTML Rendering (pure Halogen, no Canvas FFI)
-- =============================================================================

-- | Render a sparkline as an inline SVG element.
-- | Shows a diverging bar chart: additions above center (green), deletions below (red).
renderSparkline :: forall w i. Array SparklineBar -> HH.HTML w i
renderSparkline bars =
  let nBars = Array.length bars
      vbWidth = max (toNumber nBars) 200.0
      vbHeight = 72.0
      rects = toSvgRects { width: vbWidth, height: vbHeight } bars
  in
  if nBars == 0
    then HH.text ""
    else HH.div [ HP.style "flex: 1; min-width: 0;" ]
      [ svgElem "svg"
          [ sa "viewBox" ("0 0 " <> show vbWidth <> " " <> show vbHeight)
          , sa "preserveAspectRatio" "none"
          , HP.style "width: 100%; height: 44px; display: block; border-radius: 3px; border: 1px solid #b8ad90; background: #e8e0cf;"
          ]
          ( rects <#> \r ->
              svgElem "rect"
                [ sa "x" (show r.x)
                , sa "y" (show r.y)
                , sa "width" (show r.width)
                , sa "height" (show r.height)
                , sa "fill" r.fill
                ]
                []
          )
      ]
