-- | Pure layout for the Line view: one vertical line of time, oldest at the top, drawn
-- | as a ribbon cable of umbrella strands, with annotations beside it.
-- |
-- | Oldest at the top so the page reads as a story in reading order: a thread's
-- | annotation sits below the threads that came before it, a half-done thread "goes
-- | quiet" further down the page than it started, and the shifts of attention (the
-- | April turn from Hylograph to music) read downwards, the way the swimlane Timeline
-- | reads left to right.
-- |
-- | * Strands: one per umbrella, side by side in the map's order. Each strand's width
-- |   follows how many of that umbrella's threads were worked on that week (sqrt, on
-- |   one scale shared by all strands, so widths compare across strands); a quiet
-- |   week shrinks to a hairline, so the strand never disappears.
-- | * Annotations: one per chosen thread, at its telling moment (see `momentOf`),
-- |   placed without overlap by `MM.Line.Dodge`, tied to the strand by a leader.
-- | * Two modes: Wide puts the ribbon in the middle with annotations on both sides;
-- |   Narrow (phones) puts it at the left edge with annotations to its right.
module MM.Line.Layout
  ( Moment(..)
  , momentLabel
  , Annotation
  , Mode(..)
  , Side(..)
  , Strand
  , Card
  , MonthMark
  , Geometry
  , minActiveDays
  , chooseAnnotations
  , cardHeight
  , layoutLine
  ) where

import Prelude

import Data.Array (catMaybes, filter, findIndex, foldl, length, mapWithIndex, range, snoc, sortBy, zipWith)
import Data.Array as Array
import Data.Date (Date, Month(..), exactDate, month, year)
import Data.Enum (fromEnum, toEnum)
import Data.Foldable (maximum)
import Data.Int (ceil, floor, toNumber)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Number (sqrt)
import Data.Set as Set
import Data.Tuple (Tuple(..), fst, snd)
import Hylograph.Scale.Pure (ContinuousScale, applyScale, domain, linear, range) as Scale
import MM.Line.Dodge (dodge)
import MM.Marks (fmtDate)
import MM.Timeline.Types (Fate(..), Thread(..), Timeline(..), Umbrella, UmbrellaId, dateFromDayNumber, dayNumber)

-- | The moment of a thread an annotation marks.
data Moment = Started | ShippedAt | SupersededAt | WentQuiet | ParkedAt

derive instance Eq Moment

momentLabel :: Moment -> String
momentLabel = case _ of
  Started -> "started"
  ShippedAt -> "shipped"
  SupersededAt -> "superseded"
  WentQuiet -> "went quiet"
  ParkedAt -> "parked"

-- | The telling moment: unfinished threads where they went quiet (the point of the
-- | page); shipped, superseded and parked threads where they ended that way; threads
-- | still going, or of unknown fate, where they began.
momentOf :: Thread -> { moment :: Moment, date :: Date }
momentOf (Thread t) = case t.fate of
  HalfDone -> { moment: WentQuiet, date: t.last }
  Abandoned -> { moment: WentQuiet, date: t.last }
  Shipped -> { moment: ShippedAt, date: t.last }
  Superseded -> { moment: SupersededAt, date: t.last }
  Parked -> { moment: ParkedAt, date: t.last }
  Active -> { moment: Started, date: t.first }
  Unknown -> { moment: Started, date: t.first }

type Annotation = { thread :: Thread, moment :: Moment, date :: Date, umbrella :: UmbrellaId }

-- | The density rule. Every half-done and abandoned thread is annotated; any other
-- | thread is annotated if it had at least this many active days, or if Andrew wrote
-- | it a note or gave it an image.
minActiveDays :: Int
minActiveDays = 8

chooseAnnotations :: Array Thread -> Array Annotation
chooseAnnotations = map annotate <<< filter chosen
  where
  chosen (Thread t) =
    t.fate == HalfDone || t.fate == Abandoned
      || length t.days >= minActiveDays
      || isJust t.note
      || isJust t.image
  annotate th@(Thread t) =
    let m = momentOf th
    in { thread: th, moment: m.moment, date: m.date, umbrella: t.umbrella }

-- | Cards have a fixed height that depends only on their content, so layout never
-- | needs to measure the DOM. Must agree with `.ln-card` in styles.css.
cardHeight :: Annotation -> Number
cardHeight a =
  let Thread t = a.thread
  in 80.0 + (if isJust t.note then 38.0 else 0.0)

data Mode = Wide | Narrow

derive instance Eq Mode

data Side = LeftSide | RightSide

derive instance Eq Side

type Strand =
  { umbrella :: Umbrella
  , index :: Int
  , cx :: Number
  , outline :: String -- SVG path of the strand's varying-width body
  , halfMax :: Number
  }

type Card =
  { annotation :: Annotation
  , side :: Side
  , x :: Number
  , y :: Number
  , w :: Number
  , h :: Number
  , anchorX :: Number -- the strand, at the moment's date
  , anchorY :: Number
  , leader :: String -- SVG path from anchor to card
  , spanTop :: Number -- the thread's first..last on the strand, for highlighting
  , spanBottom :: Number
  , dayYs :: Array Number -- the thread's active days, on the strand
  }

type MonthMark = { y :: Number, label :: String, yearStart :: Boolean }

type Geometry =
  { mode :: Mode
  , width :: Number
  , height :: Number
  , top :: Number
  , timeBottom :: Number
  , ribbonLeft :: Number
  , ribbonRight :: Number
  , strands :: Array Strand
  , months :: Array MonthMark
  , cards :: Array Card
  , caption :: Maybe { y :: Number, text :: String } -- for a long empty stretch at the top
  }

type Dims =
  { slot :: Number -- width allotted to each strand
  , maxW :: Number -- strand width in its busiest week
  , minW :: Number -- strand width in a quiet week
  , gutter :: Number -- month labels
  , reach :: Number -- ribbon edge to card edge
  , cardMax :: Number
  , gap :: Number
  }

dimsFor :: Mode -> Dims
dimsFor = case _ of
  Wide -> { slot: 26.0, maxW: 22.0, minW: 1.6, gutter: 50.0, reach: 34.0, cardMax: 360.0, gap: 10.0 }
  Narrow -> { slot: 14.0, maxW: 12.0, minW: 1.2, gutter: 48.0, reach: 18.0, cardMax: 520.0, gap: 8.0 }

headerHeight :: Number
headerHeight = 34.0

-- | `width` is the pixel width available; `keep` picks the umbrellas whose cards show
-- | (all strands are always drawn, so the ribbon keeps its shape when filtered).
layoutLine :: Number -> (UmbrellaId -> Boolean) -> Timeline -> Geometry
layoutLine width keep (Timeline tl) =
  let
    mode = if width < 760.0 then Narrow else Wide
    dm = dimsFor mode
    nStrands = length tl.umbrellas
    ribbonW = toNumber nStrands * dm.slot
    ribbonLeft = case mode of
      Wide -> (width - ribbonW) / 2.0
      Narrow -> dm.gutter
    ribbonRight = ribbonLeft + ribbonW
    d0 = dayNumber tl.period.from
    d1 = dayNumber tl.period.to + 1
    nDays = toNumber (d1 - d0)

    annotations = sortBy byDate (filter (\a -> keep a.umbrella) (chooseAnnotations tl.threads))
    totalCardH = foldl (\acc a -> acc + cardHeight a + dm.gap) 0.0 annotations
    sides = case mode of
      Wide -> 2.0
      Narrow -> 1.0
    -- One continuous scale; its density is set so the cards roughly fit their months.
    pxPerDay = clampN (if mode == Wide then 5.0 else 7.0) 40.0 (totalCardH / sides / nDays * 1.1)
    top = headerHeight
    timeBottom = top + nDays * pxPerDay
    yScale :: Scale.ContinuousScale
    yScale = Scale.linear # Scale.domain [ toNumber d0, toNumber d1 ] # Scale.range [ top, timeBottom ]
    yOfDay :: Number -> Number
    yOfDay = Scale.applyScale yScale
    yOfDate d = yOfDay (toNumber (dayNumber d) + 0.5)

    -- Strand activity, in weekly bins.
    nBins = ceil (nDays / 7.0)
    binOf d = floor (toNumber (dayNumber d - d0) / 7.0)
    activity :: Map.Map UmbrellaId (Array Int)
    activity = Map.fromFoldable (map (\u -> Tuple u.id (binCounts u.id)) tl.umbrellas)
    binCounts uid =
      let
        sets = foldl
          ( \m (Thread t) ->
              if t.umbrella /= uid then m
              else foldl (\m' d -> Map.alter (Just <<< Set.insert t.name <<< fromMaybe Set.empty) (binOf d) m') m t.days
          )
          Map.empty
          tl.threads
      in
        map (\b -> maybeSize (Map.lookup b sets)) (range 0 (nBins - 1))
    maybeSize = case _ of
      Just s -> Set.size s
      Nothing -> 0
    peak = fromMaybe 1 (maximum (Array.concat (Array.fromFoldable (Map.values activity))))
    widthFor c =
      if c <= 0 then dm.minW
      else dm.minW + (dm.maxW - dm.minW) * sqrt (toNumber c / toNumber (max 1 peak))

    strandCx i = ribbonLeft + (toNumber i + 0.5) * dm.slot
    strands = mapWithIndex
      ( \i u ->
          let
            cx = strandCx i
            counts = fromMaybe [] (Map.lookup u.id activity)
            pts = mapWithIndex (\b c -> { y: yOfDay (toNumber (d0 + b * 7) + 3.5), hw: widthFor c / 2.0 }) counts
          in
            { umbrella: u, index: i, cx, outline: strandPath cx top timeBottom pts, halfMax: dm.maxW / 2.0 }
      )
      tl.umbrellas
    strandIndex uid = fromMaybe 0 (findIndex (\u -> u.id == uid) tl.umbrellas)

    -- Card columns.
    leftEdge = 0.0
    leftLimit = ribbonLeft - dm.gutter - dm.reach
    rightStart = ribbonRight + dm.reach
    rightLimit = width
    cardWFor side = case side of
      LeftSide -> min dm.cardMax (leftLimit - leftEdge)
      RightSide -> min dm.cardMax (rightLimit - rightStart)

    wants = map (\a -> { a, want: yOfDate a.date - 14.0, h: cardHeight a }) annotations
    -- Wide: each card goes to the side where it can sit nearest its date; ties go to
    -- the side its strand is on.
    assign = case mode of
      Narrow -> map (\x -> Tuple RightSide x) wants
      Wide -> (foldl step { l: top - 1000.0, r: top - 1000.0, out: [] } wants).out
        where
        step acc x =
          let
            costL = max 0.0 (acc.l + dm.gap - x.want)
            costR = max 0.0 (acc.r + dm.gap - x.want)
            prefer = if 2 * strandIndex x.a.umbrella < nStrands - 1 then LeftSide else RightSide
            side
              | costL < costR = LeftSide
              | costR < costL = RightSide
              | otherwise = prefer
            bottom = max x.want ((if side == LeftSide then acc.l else acc.r) + dm.gap) + x.h
          in
            if side == LeftSide then acc { l = bottom, out = snoc acc.out (Tuple side x) }
            else acc { r = bottom, out = snoc acc.out (Tuple side x) }

    placeSide side =
      let
        xs = map snd (filter (\p -> fst p == side) assign)
        ys = dodge top dm.gap (map (\x -> { want: x.want, size: x.h }) xs)
        w = cardWFor side
        cx = case side of
          LeftSide -> leftLimit - w
          RightSide -> rightStart
      in
        zipWith (\x y -> mkCard side cx w y x) xs ys

    mkCard side x w y item =
      let
        a = item.a
        Thread t = a.thread
        sx = strandCx (strandIndex a.umbrella)
        sy = yOfDate a.date
        cy = y + 16.0
        leader = case side of
          RightSide ->
            "M" <> show sx <> " " <> show sy <> "H" <> show (ribbonRight + 6.0)
              <> "L" <> show (x - 8.0) <> " " <> show cy <> "H" <> show x
          LeftSide ->
            "M" <> show sx <> " " <> show sy <> "H" <> show (ribbonLeft - dm.gutter + 4.0)
              <> "L" <> show (x + w + 8.0) <> " " <> show cy <> "H" <> show (x + w)
      in
        { annotation: a, side, x, y, w, h: item.h, anchorX: sx, anchorY: sy, leader
        , spanTop: yOfDay (toNumber (dayNumber t.first))
        , spanBottom: yOfDay (toNumber (dayNumber t.last + 1))
        , dayYs: map yOfDate t.days
        }

    cards = placeSide LeftSide <> placeSide RightSide
    cardsBottom = fromMaybe 0.0 (maximum (map (\c -> c.y + c.h) cards))
    -- Say so when the line starts with weeks of nothing, rather than leave it unexplained.
    firstDay = Array.head (Array.sort (map (\(Thread t) -> t.first) tl.threads))
    caption = firstDay >>= \d ->
      if dayNumber d - d0 >= 21 then Just { y: top + 28.0, text: "Nothing on the map before " <> fmtDate d <> "." }
      else Nothing
  in
    { mode
    , width
    , height: max timeBottom cardsBottom + 40.0
    , top
    , timeBottom
    , ribbonLeft
    , ribbonRight
    , strands
    , months: monthMarks (mode == Narrow) yOfDay d0 d1
    , cards
    , caption
    }

byDate :: Annotation -> Annotation -> Ordering
byDate a b = compare a.date b.date

clampN :: Number -> Number -> Number -> Number
clampN lo hi x = max lo (min hi x)

-- The strand body: down the right side, back up the left. Ends are squared off at the
-- period's start and end.
strandPath :: Number -> Number -> Number -> Array { y :: Number, hw :: Number } -> String
strandPath cx top bottom pts =
  let
    firstHw = fromMaybe 1.0 (_.hw <$> Array.head pts)
    lastHw = fromMaybe 1.0 (_.hw <$> Array.last pts)
    down = [ { y: top, hw: firstHw } ] <> pts <> [ { y: bottom, hw: lastHw } ]
    right = map (\p -> show (cx + p.hw) <> " " <> show p.y) down
    left = map (\p -> show (cx - p.hw) <> " " <> show p.y) (Array.reverse down)
  in
    "M" <> Array.intercalate " L" (right <> left) <> "Z"

monthMarks :: Boolean -> (Number -> Number) -> Int -> Int -> Array MonthMark
monthMarks short yOfDay d0 d1 = case dateFromDayNumber d0 of
  Nothing -> []
  Just start ->
    let
      y0 = fromEnum (year start)
      m0 = fromEnum (month start)
      cands = map (\i -> { y: y0 + (m0 - 1 + i) / 12, m: (m0 - 1 + i) `mod` 12 + 1 }) (range 0 15)
      mark c = do
        yy <- toEnum c.y
        mm <- toEnum c.m
        dd <- toEnum 1
        d <- exactDate yy mm dd
        let n = dayNumber d
        if n >= d0 && n < d1 then
          let yearStart = c.m == 1 || (c.y == y0 && c.m == m0)
          in Just { y: yOfDay (toNumber n), label: label c.m <> (if not yearStart then "" else if short then " \x2019" <> show (c.y `mod` 100) else " " <> show c.y), yearStart }
        else Nothing
    in
      catMaybes (map mark cands)
  where
  label m = case toEnum m of
    Just January -> "Jan"
    Just February -> "Feb"
    Just March -> "Mar"
    Just April -> "Apr"
    Just May -> "May"
    Just June -> "Jun"
    Just July -> "Jul"
    Just August -> "Aug"
    Just September -> "Sep"
    Just October -> "Oct"
    Just November -> "Nov"
    Just December -> "Dec"
    Nothing -> ""
