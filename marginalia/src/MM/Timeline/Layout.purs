-- | Pure layout for the timeline: umbrellas as swimlane bands, threads as bars.
-- |
-- | Each umbrella is a band of rows. Its threads are packed into rows first-fit in
-- | order of start date, where a thread occupies its bar PLUS its label (labels sit
-- | after the bar, or before it when they would run off the right edge). So a band
-- | is as tall as its busiest stretch needs and no taller, and every bar keeps a
-- | readable name.
-- |
-- | Geometry comes from Hylograph: `DataViz.Layout.Pattern.swimlane` places every bar
-- | (one global lane per row, header rows included), and a `Hylograph.Scale.Pure`
-- | linear scale maps day numbers onto the swimlane's 0..1 start/end.
module MM.Timeline.Layout
  ( Config
  , defaultConfig
  , LabelSide(..)
  , Bar
  , Band
  , MonthTick
  , Chart
  , FateCounts
  , layoutTimeline
  , countFates
  ) where

import Prelude

import Data.Array (filter, findIndex, foldl, length, snoc, sortBy, updateAt, zip, (..))
import Data.Array as Array
import Data.Date (Date, Month(..), exactDate, month, year)
import Data.Enum (fromEnum, toEnum)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.String (length) as Str
import Data.Tuple (Tuple(..))
import DataViz.Layout.Pattern (swimlane)
import DataViz.Layout.Pattern.Types (Rect, padding)
import Hylograph.Scale.Pure (ContinuousScale, applyScale, domain, linear, range)
import MM.Timeline.Types (Fate(..), Thread(..), ThreadName(..), Timeline(..), Umbrella, UmbrellaId, dateFromDayNumber, dayNumber)

type Config =
  { width :: Number -- viewBox width of the chart
  , padLeft :: Number
  , padRight :: Number
  , rowHeight :: Number
  , headerRows :: Int -- rows a band's header takes
  , gapRows :: Int -- empty rows after each band
  , charWidth :: Number -- estimated label advance per character
  , labelGap :: Number -- bar end (or end mark) to label
  , rowGap :: Number -- minimum space between neighbours in a row
  , minBar :: Number -- a one-day thread is still visible
  }

defaultConfig :: Config
defaultConfig =
  { width: 1400.0
  , padLeft: 14.0
  , padRight: 14.0
  , rowHeight: 19.0
  , headerRows: 2
  , gapRows: 1
  , charWidth: 5.9
  , labelGap: 9.0
  , rowGap: 10.0
  , minBar: 3.0
  }

-- | Where a bar's label goes: after it, before it, or (for a bar that spans nearly
-- | the whole period, so neither side has room) inside it, at its start.
data LabelSide = LabelAfter | LabelBefore | LabelInside

derive instance Eq LabelSide

-- | A positioned thread. `rect` is its whole row slot from the swimlane; the bar is
-- | drawn inside it (`x0`..`x1`), with active-day ticks at `ticks`.
type Bar =
  { thread :: Thread
  , rect :: Rect
  , x0 :: Number
  , x1 :: Number
  , ticks :: Array Number
  , label :: String
  , labelSide :: LabelSide
  }

type FateCounts = { total :: Int, halfDone :: Int, abandoned :: Int, parked :: Int }

type Band =
  { umbrella :: Umbrella
  , header :: Rect -- the band's header slot (full width)
  , body :: Rect -- the band's rows (full width), for the lane tint
  , bars :: Array Bar
  , counts :: FateCounts -- over the threads drawn
  , countsAll :: FateCounts -- over every thread in the umbrella
  }

type MonthTick = { x :: Number, label :: String, yearStart :: Boolean }

type Chart =
  { width :: Number
  , height :: Number
  , bands :: Array Band
  , months :: Array MonthTick
  , plotLeft :: Number
  , plotRight :: Number
  }

countFates :: Array Thread -> FateCounts
countFates = foldl step { total: 0, halfDone: 0, abandoned: 0, parked: 0 }
  where
  step c (Thread t) = case t.fate of
    HalfDone -> c { total = c.total + 1, halfDone = c.halfDone + 1 }
    Abandoned -> c { total = c.total + 1, abandoned = c.abandoned + 1 }
    Parked -> c { total = c.total + 1, parked = c.parked + 1 }
    _ -> c { total = c.total + 1 }

-- | Lay out the umbrellas in `shown` (in the timeline's own umbrella order), drawing
-- | only the threads that pass `keep`.
layoutTimeline :: Config -> (Thread -> Boolean) -> (UmbrellaId -> Boolean) -> Timeline -> Chart
layoutTimeline cfg keep shown (Timeline tl) =
  let
    d0 = dayNumber tl.period.from
    d1 = dayNumber tl.period.to + 1
    plotLeft = cfg.padLeft
    plotRight = cfg.width - cfg.padRight
    plotW = plotRight - plotLeft
    frac :: ContinuousScale
    frac = linear # domain [ toNumber d0, toNumber d1 ] # range [ 0.0, 1.0 ]
    xOfDay :: Number -> Number
    xOfDay n = plotLeft + applyScale frac n * plotW
    toFrac :: Number -> Number
    toFrac x = (x - plotLeft) / plotW

    umbrellas = filter (\u -> shown u.id) tl.umbrellas
    threadsOf u = filter (\(Thread t) -> t.umbrella == u.id) tl.threads

    -- Pack one umbrella's kept threads into rows.
    packBand :: Umbrella -> { umbrella :: Umbrella, rows :: Int, items :: Array Packed, all :: Array Thread }
    packBand u =
      let
        all = threadsOf u
        kept = sortBy byStart (filter keep all)
        placed = foldl (place cfg plotRight xOfDay) { rowEnds: [], items: [] } kept
      in
        { umbrella: u, rows: length placed.rowEnds, items: placed.items, all }

    packed = map packBand umbrellas

    -- Assign global lanes: header rows, then the band's rows, then a gap.
    laneOffsets = Array.scanl (\acc b -> acc + cfg.headerRows + max 1 b.rows + cfg.gapRows) 0 packed
    starts = Array.cons 0 laneOffsets
    totalLanes = fromMaybe 0 (Array.last starts)
    height = toNumber totalLanes * cfg.rowHeight
    vp = { width: cfg.width, height, padding: padding { top: 0.0, right: cfg.padRight, bottom: 0.0, left: cfg.padLeft } }
    lanes = swimlane totalLanes 0.0 vp

    mkBand (Tuple b start) =
      let
        headerRect = fullWidth (fromMaybe zeroRect (Array.head (lanes [ { lane: start, start: 0.0, end: 1.0 } ])))
        bodyTop = start + cfg.headerRows
        bodyRects = lanes [ { lane: bodyTop, start: 0.0, end: 1.0 } ]
        bodyRect = case Array.head bodyRects of
          Just r -> fullWidth r { height = toNumber (max 1 b.rows) * cfg.rowHeight }
          Nothing -> zeroRect
        rects = lanes (map (\p -> { lane: bodyTop + p.row, start: toFrac p.x0, end: toFrac p.x1 }) b.items)
        bars = map (\(Tuple p r) -> mkBar xOfDay p r) (zip b.items rects)
      in
        { umbrella: b.umbrella
        , header: headerRect { height = toNumber cfg.headerRows * cfg.rowHeight }
        , body: bodyRect
        , bars
        , counts: countFates (map _.thread b.items)
        , countsAll: countFates b.all
        }

    fullWidth r = r { x = 0.0, width = cfg.width }
  in
    { width: cfg.width
    , height
    , bands: map mkBand (zip packed starts)
    , months: monthTicks xOfDay d0 d1
    , plotLeft
    , plotRight
    }

zeroRect :: Rect
zeroRect = { x: 0.0, y: 0.0, width: 0.0, height: 0.0 }

byStart :: Thread -> Thread -> Ordering
byStart (Thread a) (Thread b) = compare a.first b.first <> compare b.last a.last

-- Packing ---------------------------------------------------------------------

type Packed =
  { thread :: Thread
  , row :: Int
  , x0 :: Number
  , x1 :: Number
  , label :: String
  , labelSide :: LabelSide
  }

-- Room the end mark needs (arrowhead for active, pause bars for parked).
endMark :: Fate -> Number
endMark = case _ of
  Active -> 7.0
  Parked -> 6.0
  _ -> 0.0

labelOf :: Thread -> String
labelOf (Thread t) = unwrap t.name <> case t.successor of
  Just (ThreadName s) -> "  \x2192 " <> s
  Nothing -> ""

place
  :: Config
  -> Number
  -> (Number -> Number)
  -> { rowEnds :: Array Number, items :: Array Packed }
  -> Thread
  -> { rowEnds :: Array Number, items :: Array Packed }
place cfg plotRight xOfDay acc th@(Thread t) =
  let
    x0 = xOfDay (toNumber (dayNumber t.first))
    x1 = max (x0 + cfg.minBar) (xOfDay (toNumber (dayNumber t.last + 1)))
    label = labelOf th
    labelW = toNumber (Str.length label) * cfg.charWidth
    afterEnd = x1 + endMark t.fate + cfg.labelGap + labelW
    side
      | afterEnd <= plotRight + cfg.padLeft = LabelAfter
      | x0 - cfg.labelGap - labelW >= 0.0 = LabelBefore
      | otherwise = LabelInside
    extent = case side of
      LabelAfter -> { lo: x0, hi: afterEnd }
      LabelBefore -> { lo: max 0.0 (x0 - cfg.labelGap - labelW), hi: x1 + endMark t.fate }
      LabelInside -> { lo: x0, hi: x1 + endMark t.fate }
    fits end = end + cfg.rowGap <= extent.lo
    row = fromMaybe (length acc.rowEnds) (findIndex fits acc.rowEnds)
    rowEnds =
      if row < length acc.rowEnds then fromMaybe acc.rowEnds (updateAt row extent.hi acc.rowEnds)
      else snoc acc.rowEnds extent.hi
  in
    { rowEnds
    , items: snoc acc.items { thread: th, row, x0, x1, label, labelSide: side }
    }

mkBar :: (Number -> Number) -> Packed -> Rect -> Bar
mkBar xOfDay p r =
  let
    Thread t = p.thread
    tickX d = xOfDay (toNumber (dayNumber d) + 0.5)
  in
    { thread: p.thread
    , rect: r
    , x0: r.x
    , x1: r.x + max r.width 0.0
    , ticks: map tickX t.days
    , label: p.label
    , labelSide: p.labelSide
    }

-- Month axis ------------------------------------------------------------------

monthTicks :: (Number -> Number) -> Int -> Int -> Array MonthTick
monthTicks xOfDay d0 d1 =
  case dateFromDayNumber d0 of
    Nothing -> []
    Just start ->
      let
        y0 = fromEnum (year start)
        m0 = fromEnum (month start)
        -- months from the start month to (at most) 15 months on; the period is 13
        candidates = map (\i -> { y: y0 + (m0 - 1 + i) / 12, m: (m0 - 1 + i) `mod` 12 + 1 }) (0 .. 15)
        firstOf c = do
          yy <- toEnum c.y
          mm <- toEnum c.m
          dd <- toEnum 1
          exactDate yy mm dd
        ticks = Array.mapMaybe
          ( \c -> case firstOf c of
              Just d | dayNumber d >= d0 - 31 && dayNumber d < d1 ->
                let yearStart = c.m == 1 || (c.y == y0 && c.m == m0)
                in Just { x: xOfDay (toNumber (max d0 (dayNumber d))), label: monthLabel yearStart c.y d, yearStart }
              _ -> Nothing
          )
          candidates
      in
        filter (\tk -> tk.x >= 0.0) ticks

monthLabel :: Boolean -> Int -> Date -> String
monthLabel withYear y d =
  let
    name = case month d of
      January -> "Jan"
      February -> "Feb"
      March -> "Mar"
      April -> "Apr"
      May -> "May"
      June -> "Jun"
      July -> "Jul"
      August -> "Aug"
      September -> "Sep"
      October -> "Oct"
      November -> "Nov"
      December -> "Dec"
  in
    name <> if withYear then " " <> show y else ""
