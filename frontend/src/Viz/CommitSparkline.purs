-- | Canvas 2D Commit Sparkline
-- |
-- | Renders a compact per-commit sparkline for a single module.
-- | Diverging bar chart around a central axis:
-- |   - Above center: additions (gray = total commit, green = this module)
-- |   - Below center: deletions (gray = total commit, red = this module)
-- | Heights are log-scaled to handle the wide range of commit sizes.
-- | Bars space out to fill available width.
module CE2.Viz.CommitSparkline
  ( Context2D
  , getContext2D
  , getElementWidth
  , setCanvasDimensions
  , render
  , SparklineBar
  , SparklineRect
  , prepareData
  , toSvgRects
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (log) as Num
import Data.Traversable (for_)
import Effect (Effect)
import Foreign.Object as Object
import Web.HTML.HTMLCanvasElement (HTMLCanvasElement)
import Web.HTML.HTMLElement (HTMLElement)
import CE2.Data.Loader as Loader

-- =============================================================================
-- Canvas FFI (minimal)
-- =============================================================================

foreign import data Context2D :: Type

foreign import getContext2D :: HTMLCanvasElement -> Effect Context2D
foreign import setFillStyle :: Context2D -> String -> Effect Unit
foreign import fillRect :: Context2D -> Number -> Number -> Number -> Number -> Effect Unit
foreign import getElementWidth :: HTMLElement -> Effect Number
foreign import setCanvasDimensions :: HTMLCanvasElement -> Number -> Number -> Effect Unit

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
-- Rendering (diverging bar chart, fills available width)
-- =============================================================================

-- | Render the sparkline onto a canvas.
-- | Width and height are the canvas pixel dimensions.
-- | Bars space out evenly to fill the width; each bar is 60% of its slot.
render :: Context2D -> { width :: Number, height :: Number } -> Array SparklineBar -> Effect Unit
render ctx dims bars = do
  let nBars = Array.length bars
  when (nBars == 0) $ pure unit
  when (nBars > 0) do
    -- Clear
    setFillStyle ctx "#f5f2eb"
    fillRect ctx 0.0 0.0 dims.width dims.height

    -- Compute spacing
    let n = toNumber nBars
        pitch = dims.width / n        -- space per bar slot
        barW = max 1.0 (pitch * 0.6)  -- bar width = 60% of slot, min 1px

    -- Find max for scaling (either side)
    let maxVal = foldl (\acc b -> max acc (max b.totalAdded b.totalDeleted)) 1 bars
        logMax = logScale (toNumber maxVal)
        halfH = dims.height / 2.0
        centerY = halfH

    -- Center axis line
    setFillStyle ctx "#e0e0e0"
    fillRect ctx 0.0 (centerY - 0.5) dims.width 1.0

    -- Draw each bar
    for_ (Array.mapWithIndex (\i b -> { idx: i, bar: b }) bars) \{ idx, bar } -> do
      let x = toNumber idx * pitch + (pitch - barW) / 2.0  -- centered in slot

      -- Gray additions (above center)
      when (bar.totalAdded > 0) do
        let barH = halfH * logScale (toNumber bar.totalAdded) / logMax
        setFillStyle ctx "#d4d4d4"
        fillRect ctx x (centerY - barH) barW barH

      -- Gray deletions (below center)
      when (bar.totalDeleted > 0) do
        let barH = halfH * logScale (toNumber bar.totalDeleted) / logMax
        setFillStyle ctx "#d4d4d4"
        fillRect ctx x centerY barW barH

      -- Green module additions (above center, overlays gray)
      when (bar.moduleAdded > 0) do
        let barH = halfH * logScale (toNumber bar.moduleAdded) / logMax
        setFillStyle ctx "#22c55e"
        fillRect ctx x (centerY - barH) barW barH

      -- Red module deletions (below center, overlays gray)
      when (bar.moduleDeleted > 0) do
        let barH = halfH * logScale (toNumber bar.moduleDeleted) / logMax
        setFillStyle ctx "#ef4444"
        fillRect ctx x centerY barW barH

-- =============================================================================
-- SVG rect generation (pure, for inline SVG rendering)
-- =============================================================================

-- | A positioned, colored rectangle for SVG rendering
type SparklineRect =
  { x :: Number
  , y :: Number
  , width :: Number
  , height :: Number
  , fill :: String
  }

-- | Convert sparkline bars to SVG rects (pure, no Effect).
-- | Diverging layout: additions above center, deletions below.
-- | viewBoxWidth should be the target width; bars space out to fill it.
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
