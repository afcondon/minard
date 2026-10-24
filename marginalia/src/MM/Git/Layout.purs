-- | Pure layout for the Git view: a commit calendar in the manner of GitHub's, one
-- | column per week (Monday at the top), with three weekly rows hung under the same
-- | columns so a week reads straight down:
-- |
-- | * lines of code, added above a baseline (stacked by umbrella, in the map's colours)
-- |   and deleted below it, on one scale;
-- | * net lines of code, cumulative, as a filled line;
-- | * lines of prose (Markdown) added, as small bars.
-- |
-- | A calendar cell's shade is the day's commits on one of five steps, cut at the
-- | quartiles of the days that have any, as GitHub does, so a few very busy days
-- | don't wash the rest out. Selecting an umbrella redraws everything from that
-- | umbrella's counts alone.
module MM.Git.Layout
  ( Shading(..)
  , Cell
  , Stack
  , Week
  , MonthMark
  , Strip
  , GitChart
  , chartWidth
  , layoutGit
  , countsFor
  ) where

import Prelude

import Data.Array (filter, find, foldl, length, mapMaybe, range, snoc, sort, (!!))
import Data.Date (Date, Month(..), day, month)
import Data.Enum (fromEnum)
import Data.Foldable (maximum)
import Data.Int (floor, toNumber)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Tuple (Tuple(..))
import Hylograph.Scale.Pure (applyScale, domain, linear, range) as Scale
import MM.Git.Types (Counts, DayActivity, GitActivity(..), addCounts, zeroCounts)
import MM.Marks (monthShort)
import MM.Timeline.Types (Umbrella, UmbrellaId, dateFromDayNumber, dayNumber)

-- | What a calendar cell's colour says: only how busy the day was, or also which
-- | umbrella most of its commits went to.
data Shading = ByActivity | ByUmbrella

derive instance Eq Shading

type Cell =
  { date :: Date
  , x :: Number
  , y :: Number
  , size :: Number
  , level :: Int -- 0 (no commits) .. 4
  , counts :: Counts
  , lead :: Maybe UmbrellaId -- the umbrella with most of the day's commits
  }

-- | One umbrella's share of a week's added lines: a segment of the stacked bar.
type Stack = { umbrella :: Maybe UmbrellaId, y0 :: Number, y1 :: Number }

type Week =
  { start :: Date
  , x :: Number
  , width :: Number
  , counts :: Counts
  , added :: Array Stack
  , deletedTo :: Number -- the deleted bar runs from the code baseline down to here
  , proseTop :: Number
  , netY :: Number -- the cumulative net line's height at this week's centre
  }

type MonthMark = { x :: Number, label :: String, yearStart :: Boolean }

-- | A labelled band of the chart, top and bottom in SVG units.
type Strip = { top :: Number, bottom :: Number, label :: String, peak :: Int }

type GitChart =
  { width :: Number
  , height :: Number
  , gutter :: Number
  , step :: Number
  , calendarTop :: Number
  , cells :: Array Cell
  , weeks :: Array Week
  , months :: Array MonthMark
  , thresholds :: Array Int -- lower bounds of levels 1..4
  , code :: Strip
  , codeBaseline :: Number
  , net :: Strip
  , netFinal :: Int
  , prose :: Strip
  }

chartWidth :: Number
chartWidth = 1080.0

-- | A day's counts for the current selection: everything, or one umbrella's share.
countsFor :: Maybe UmbrellaId -> DayActivity -> Counts
countsFor sel d = case sel of
  Nothing -> d.total
  Just u -> maybe0 (find (\x -> x.umbrella == Just u) d.byUmbrella)
  where
  maybe0 = case _ of
    Just x -> x.counts
    Nothing -> zeroCounts

-- The Monday on or before a day, as a day number (1970-01-01 was a Thursday).
mondayOf :: Int -> Int
mondayOf n = n - ((n + 3) `mod` 7)

layoutGit :: Array Umbrella -> Maybe UmbrellaId -> GitActivity -> GitChart
layoutGit umbrellas selected (GitActivity g) =
  let
    gutter = 34.0
    from = dayNumber g.period.from
    to = dayNumber g.period.to
    firstMonday = mondayOf from
    nWeeks = (mondayOf to - firstMonday) / 7 + 1
    step = min 18.0 (toNumber (floor ((chartWidth - gutter - 4.0) / toNumber nWeeks)))
    gap = 3.0
    size = step - gap
    calendarTop = 18.0
    calendarBottom = calendarTop + 7.0 * step

    byDay = Map.fromFoldable (map (\d -> Tuple (dayNumber d.date) d) g.days)
    countsOn n = fromMaybe zeroCounts (countsFor selected <$> Map.lookup n byDay)

    -- the umbrella with most of a day's commits (whole selection when one is chosen)
    leadOn n = case selected of
      Just u -> if (countsOn n).commits > 0 then Just u else Nothing
      Nothing -> Map.lookup n byDay >>= \d ->
        foldl (\best x -> if maybe (-1) _.counts.commits best < x.counts.commits then Just x else best) Nothing d.byUmbrella >>= _.umbrella

    busy = sort (filter (_ > 0) (map (\n -> (countsOn n).commits) (range from to)))
    quartile q = fromMaybe 1 (busy !! floor (q * toNumber (length busy - 1)))
    thresholds = dedupeUp [ 1, max 2 (quartile 0.25), max 3 (quartile 0.5), max 4 (quartile 0.75) ]
    levelOf c = if c <= 0 then 0 else length (filter (\t -> c >= t) thresholds)

    colX w = gutter + toNumber w * step

    cells = mapMaybe
      ( \n -> do
          date <- dateFromDayNumber n
          let
            w = (mondayOf n - firstMonday) / 7
            row = n - mondayOf n
            c = countsOn n
          pure { date, x: colX w, y: calendarTop + toNumber row * step, size, level: levelOf c.commits, counts: c, lead: leadOn n }
      )
      (range from to)

    weekCounts w =
      foldl addCounts zeroCounts
        (map countsOn (filter (\n -> n >= from && n <= to) (range (firstMonday + 7 * w) (firstMonday + 7 * w + 6))))
    weekUmbrella w u =
      foldl addCounts zeroCounts
        ( mapMaybe (\n -> Map.lookup n byDay >>= \d -> _.counts <$> find (\x -> x.umbrella == u) d.byUmbrella)
            (range (firstMonday + 7 * w) (firstMonday + 7 * w + 6))
        )
    weekIdx = range 0 (nWeeks - 1)
    allWeeks = map weekCounts weekIdx

    -- Rows ---------------------------------------------------------------------
    peakAdded = fromMaybe 1 (maximum (map _.codeAdded allWeeks))
    peakDeleted = fromMaybe 0 (maximum (map _.codeDeleted allWeeks))
    codeUp = 96.0
    codeTop = calendarBottom + 40.0
    codeBaseline = codeTop + codeUp
    perLine = codeUp / toNumber (max 1 peakAdded)
    codeBottom = codeBaseline + perLine * toNumber peakDeleted
    codeY v = codeBaseline - perLine * toNumber v

    nets = scanSum (map (\c -> c.codeAdded - c.codeDeleted) allWeeks)
    netFinal = fromMaybe 0 (nets !! (nWeeks - 1))
    netPeak = fromMaybe 1 (maximum nets)
    netTop = codeBottom + 40.0
    netBottom = netTop + 64.0
    netScale v = Scale.applyScale (Scale.linear # Scale.domain [ 0.0, toNumber (max 1 netPeak) ] # Scale.range [ netBottom, netTop ]) (toNumber v)

    peakProse = fromMaybe 1 (maximum (map _.proseAdded allWeeks))
    proseTop' = netBottom + 40.0
    proseBottom = proseTop' + 40.0
    proseY v = proseBottom - (proseBottom - proseTop') * toNumber v / toNumber (max 1 peakProse)

    -- stack order: the map's umbrella order, then lines outside any umbrella
    order = case selected of
      Just u -> [ Just u ]
      Nothing -> map (Just <<< _.id) umbrellas `snoc` Nothing
    stacksFor w =
      let
        parts = map (\u -> { umbrella: u, v: (if selected == Nothing then weekUmbrella w u else weekCounts w).codeAdded }) order
      in
        (foldl (\acc p -> { at: acc.at + p.v, out: acc.out `snoc` { umbrella: p.umbrella, y0: codeY acc.at, y1: codeY (acc.at + p.v) } }) { at: 0, out: [] } (filter (\p -> p.v > 0) parts)).out

    weeks = mapMaybe
      ( \w -> do
          start <- dateFromDayNumber (firstMonday + 7 * w)
          counts <- allWeeks !! w
          net <- nets !! w
          pure
            { start
            , x: colX w
            , width: size
            , counts
            , added: stacksFor w
            , deletedTo: codeBaseline + perLine * toNumber counts.codeDeleted
            , proseTop: proseY counts.proseAdded
            , netY: netScale net
            }
      )
      weekIdx

    months = mapMaybe
      ( \w -> do
          monday <- dateFromDayNumber (firstMonday + 7 * w)
          sunday <- dateFromDayNumber (firstMonday + 7 * w + 6)
          -- a column carries a month's label if the month's first day falls in it
          let starts = fromEnum (day sunday) <= 7 && month sunday /= month monday || fromEnum (day monday) == 1
          if w == 0 || starts then
            let m = if fromEnum (day monday) == 1 || w == 0 then month monday else month sunday
            in Just { x: colX w, label: monthShort m, yearStart: m == January }
          else Nothing
      )
      weekIdx
  in
    { width: chartWidth
    , height: proseBottom + 8.0
    , gutter
    , step
    , calendarTop
    , cells
    , weeks
    , months
    , thresholds
    , code: { top: codeTop, bottom: codeBottom, label: "LINES OF CODE PER WEEK, ADDED AND DELETED", peak: peakAdded }
    , codeBaseline
    , net: { top: netTop, bottom: netBottom, label: "NET LINES OF CODE, CUMULATIVE", peak: netPeak }
    , netFinal
    , prose: { top: proseTop', bottom: proseBottom, label: "LINES OF PROSE (MARKDOWN) PER WEEK", peak: peakProse }
    }

-- | Running totals.
scanSum :: Array Int -> Array Int
scanSum = _.out <<< foldl (\acc v -> { at: acc.at + v, out: acc.out `snoc` (acc.at + v) }) { at: 0, out: [] }

-- | Thresholds must rise strictly, or two levels would never be drawn apart.
dedupeUp :: Array Int -> Array Int
dedupeUp = _.out <<< foldl (\acc t -> let t' = max t (acc.last + 1) in { last: t', out: acc.out `snoc` t' }) { last: 0, out: [] }
