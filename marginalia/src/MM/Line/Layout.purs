-- | Pure layout for the Line view: one vertical line of time, oldest at the top, drawn
-- | as a ribbon cable of umbrella strands, with annotations beside it.
-- |
-- | Oldest at the top so the page reads as a story in reading order: a thread's
-- | annotation sits below the threads that came before it, a half-done thread "goes
-- | quiet" further down the page than it started, and the shifts of attention (the
-- | April turn from Hylograph to music) read downwards, the way the swimlane Timeline
-- | reads left to right.
-- |
-- | * Months are segments. An unfolded month gets the shared scale (px per day); a
-- |   FOLDED month is compressed to a short fixed height, keeping its ribbon (so the
-- |   balance between strands still shows) but none of its cards.
-- | * Strands: one per umbrella, side by side in the map's order. Each strand's width
-- |   follows how many of that umbrella's threads were worked on that week (sqrt, on
-- |   one scale shared by all strands); a quiet week shrinks to a hairline.
-- | * Annotations: one per chosen thread, at its telling moment (see `momentOf`),
-- |   placed without overlap by `MM.Line.Dodge` within each run of unfolded months,
-- |   tied to the strand by a leader. If a run's cards need more room than its days
-- |   give, the run ends with a little blank space rather than spilling onto the next.
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
  , Segment
  , Geometry
  , chooseAnnotations
  , cardHeight
  , monthKeys
  , layoutLine
  ) where

import Prelude

import Data.Array (filter, findIndex, foldl, length, mapWithIndex, range, snoc, sortBy, zipWith)
import Data.Array as Array
import Data.Date (Date, Month(..), exactDate, month, year)
import Data.Either (Either(..))
import Data.Enum (fromEnum, toEnum)
import Data.Foldable (any, maximum, sum)
import Data.Int (ceil, floor, toNumber)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Number (sqrt)
import Data.Set as Set
import Data.Tuple (Tuple(..), fst, snd)
import Hylograph.Scale.Pure (applyScale, domain, linear, range) as Scale
import MM.Line.Dodge (dodge)
import MM.Line.Options (LineOptions, MonthKey(..), groupOf)
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

isQuiet :: Fate -> Boolean
isQuiet f = f == HalfDone || f == Abandoned

-- | The density rule. A card for every half-done and abandoned thread; for any other
-- | thread, a card if it had at least `minDays` active days, or if Andrew wrote it a
-- | note or gave it an image. Then the fate filter.
chooseAnnotations :: LineOptions -> Array Thread -> Array Annotation
chooseAnnotations opts = map annotate <<< filter chosen
  where
  chosen (Thread t) =
    Set.member (groupOf t.fate) opts.fates
      && (isQuiet t.fate || length t.days >= opts.minDays || isJust t.note || isJust t.image)
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

-- | A month on the line. `y0`..`yTimeEnd` is its time; `yTimeEnd`..`y1` is any blank
-- | room its run's cards needed.
type Segment =
  { key :: MonthKey
  , label :: String -- "Oct 2025", "Nov"
  , longLabel :: String -- "October 2025"
  , yearStart :: Boolean
  , folded :: Boolean
  , d0 :: Int
  , d1 :: Int
  , y0 :: Number
  , yTimeEnd :: Number
  , y1 :: Number
  , threads :: Int -- threads worked on that month (shown umbrellas)
  , quiet :: Int -- of which went quiet that month
  }

type Geometry =
  { mode :: Mode
  , width :: Number
  , height :: Number
  , top :: Number
  , timeBottom :: Number
  , ribbonLeft :: Number
  , ribbonRight :: Number
  , strands :: Array Strand
  , segments :: Array Segment
  , cards :: Array Card
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
  Wide -> { slot: 26.0, maxW: 22.0, minW: 1.6, gutter: 58.0, reach: 34.0, cardMax: 360.0, gap: 10.0 }
  Narrow -> { slot: 14.0, maxW: 12.0, minW: 1.2, gutter: 54.0, reach: 18.0, cardMax: 520.0, gap: 8.0 }

headerHeight :: Number
headerHeight = 34.0

-- | Height of a folded month.
foldHeight :: Number
foldHeight = 34.0

-- Months ----------------------------------------------------------------------------

type MonthSpan = { key :: MonthKey, y :: Int, m :: Int, d0 :: Int, d1 :: Int }

monthSpans :: Int -> Int -> Array MonthSpan
monthSpans d0 d1 = case dateFromDayNumber d0 of
  Nothing -> []
  Just start ->
    let
      y0 = fromEnum (year start)
      m0 = fromEnum (month start)
      ym i = { y: y0 + (m0 - 1 + i) / 12, m: (m0 - 1 + i) `mod` 12 + 1 }
      firstOf c = fromMaybe d1 do
        yy <- toEnum c.y
        mm <- toEnum c.m
        dd <- toEnum 1
        dayNumber <$> exactDate yy mm dd
      spanOf i =
        let
          c = ym i
          s = max d0 (firstOf c)
          e = min d1 (firstOf (ym (i + 1)))
        in
          if s < e then Just { key: MonthKey (show c.y <> "-" <> pad c.m), y: c.y, m: c.m, d0: s, d1: e } else Nothing
    in
      Array.catMaybes (map spanOf (range 0 15))
  where
  pad n = if n < 10 then "0" <> show n else show n

-- | Every month of the period, for "fold all".
monthKeys :: Timeline -> Array MonthKey
monthKeys (Timeline tl) = map _.key (monthSpans (dayNumber tl.period.from) (dayNumber tl.period.to + 1))

monthName :: Int -> { short :: String, long :: String }
monthName m = case toEnum m of
  Just January -> { short: "Jan", long: "January" }
  Just February -> { short: "Feb", long: "February" }
  Just March -> { short: "Mar", long: "March" }
  Just April -> { short: "Apr", long: "April" }
  Just May -> { short: "May", long: "May" }
  Just June -> { short: "Jun", long: "June" }
  Just July -> { short: "Jul", long: "July" }
  Just August -> { short: "Aug", long: "August" }
  Just September -> { short: "Sep", long: "September" }
  Just October -> { short: "Oct", long: "October" }
  Just November -> { short: "Nov", long: "November" }
  Just December -> { short: "Dec", long: "December" }
  Nothing -> { short: "", long: "" }

-- Layout ------------------------------------------------------------------------------

-- | `width` is the pixel width available; `keep` picks the umbrellas whose cards show
-- | (all strands are always drawn, so the ribbon keeps its shape when filtered).
layoutLine :: Number -> (UmbrellaId -> Boolean) -> LineOptions -> Timeline -> Geometry
layoutLine width keep opts (Timeline tl) =
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
    top = headerHeight

    spans = monthSpans d0 d1
    isFolded s = Set.member s.key opts.folded
    spanOfDay n = Array.find (\s -> s.d0 <= n && n < s.d1) spans
    keyOfDate d = _.key <$> spanOfDay (dayNumber d)

    shownThreads = filter (\(Thread t) -> keep t.umbrella) tl.threads
    annotations = sortBy byDate (filter (\a -> keep a.umbrella) (chooseAnnotations opts tl.threads))
    inUnfolded a = case spanOfDay (dayNumber a.date) of
      Just s -> not (isFolded s)
      Nothing -> false
    cardAnns = filter inUnfolded annotations

    sides = case mode of
      Wide -> 2.0
      Narrow -> 1.0
    unfoldedDays = sum (map (\s -> toNumber (s.d1 - s.d0)) (filter (not <<< isFolded) spans))
    totalCardH = foldl (\acc a -> acc + cardHeight a + dm.gap) 0.0 cardAnns
    -- One shared scale for unfolded months; its density is set so the cards roughly fit.
    pxPerDay = clampN (if mode == Wide then 5.0 else 7.0) 40.0 (totalCardH / sides / max 1.0 unfoldedDays * 1.1)

    -- Group months into folded months and runs of unfolded ones.
    runs :: Array (Either MonthSpan (Array MonthSpan))
    runs = foldl
      ( \acc s ->
          if isFolded s then snoc acc (Left s)
          else case Array.unsnoc acc of
            Just { init, last: Right run } -> snoc init (Right (snoc run s))
            _ -> snoc acc (Right [ s ])
      )
      []
      spans

    strandIndex uid = fromMaybe 0 (findIndex (\u -> u.id == uid) tl.umbrellas)
    strandCx i = ribbonLeft + (toNumber i + 0.5) * dm.slot

    -- Card columns.
    leftLimit = ribbonLeft - dm.gutter - dm.reach
    rightStart = ribbonRight + dm.reach
    cardWFor side = case side of
      LeftSide -> min dm.cardMax leftLimit
      RightSide -> min dm.cardMax (width - rightStart)

    monthCounts s =
      { threads: length (filter (\(Thread t) -> any (\d -> let n = dayNumber d in s.d0 <= n && n < s.d1) t.days) shownThreads)
      , quiet: length (filter (\(Thread t) -> isQuiet t.fate && let n = dayNumber t.last in s.d0 <= n && n < s.d1) shownThreads)
      }
    mkSegment s y0 yEnd y1 =
      let
        names = monthName s.m
        yearStart = s.m == 1 || s.d0 == d0
        c = monthCounts s
      in
        { key: s.key
        , label: names.short <> (if not yearStart then "" else if mode == Narrow then " \x2019" <> show (s.y `mod` 100) else " " <> show s.y)
        , longLabel: names.long <> " " <> show s.y
        , yearStart
        , folded: isFolded s
        , d0: s.d0
        , d1: s.d1
        , y0
        , yTimeEnd: yEnd
        , y1
        , threads: c.threads
        , quiet: c.quiet
        }

    -- Walk the months top to bottom.
    walk = foldl step { y: top, segments: [], cards: [] } runs
    step acc = case _ of
      Left s ->
        acc { y = acc.y + foldHeight, segments = snoc acc.segments (mkSegment s acc.y (acc.y + foldHeight) (acc.y + foldHeight)) }
      Right run ->
        let
          heights = map (\s -> toNumber (s.d1 - s.d0) * pxPerDay) run
          starts = Array.scanl (+) acc.y heights
          tops = [ acc.y ] <> starts
          segs0 = zipWith (\s t -> mkSegment s t (t + toNumber (s.d1 - s.d0) * pxPerDay) (t + toNumber (s.d1 - s.d0) * pxPerDay)) run tops
          runEnd = fromMaybe acc.y (Array.last starts)
          yOf = yOfDayIn segs0
          runKeys = Set.fromFoldable (map _.key run)
          anns = filter (\a -> maybe false (\k -> Set.member k runKeys) (keyOfDate a.date)) cardAnns
          runCards = placeCards mode dm yOf strandCx strandIndex nStrands leftLimit rightStart cardWFor ribbonLeft ribbonRight acc.y anns
          cardsBottom = fromMaybe runEnd (maximum (map (\c -> c.y + c.h + dm.gap) runCards))
          overflow = max 0.0 (cardsBottom - runEnd)
          segs = case Array.unsnoc segs0 of
            Just { init, last } -> snoc init (last { y1 = last.y1 + overflow })
            Nothing -> segs0
        in
          { y: runEnd + overflow, segments: acc.segments <> segs, cards: acc.cards <> runCards }

    segments = walk.segments
    yOfDay = yOfDayIn segments
    yOfDate d = yOfDay (toNumber (dayNumber d) + 0.5)
    timeBottom = walk.y
    cards = map
      ( \c ->
          let Thread t = c.annotation.thread
          in
            c
              { spanTop = yOfDay (toNumber (dayNumber t.first))
              , spanBottom = yOfDay (toNumber (dayNumber t.last + 1))
              , dayYs = map yOfDate t.days
              }
      )
      walk.cards

    -- Strand activity, in weekly bins.
    nBins = ceil (toNumber (d1 - d0) / 7.0)
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
        map (\b -> maybe 0 Set.size (Map.lookup b sets)) (range 0 (nBins - 1))
    peak = fromMaybe 1 (maximum (Array.concat (Array.fromFoldable (Map.values activity))))
    widthFor c =
      if c <= 0 then dm.minW
      else dm.minW + (dm.maxW - dm.minW) * sqrt (toNumber c / toNumber (max 1 peak))
    strands = mapWithIndex
      ( \i u ->
          let
            cx = strandCx i
            counts = fromMaybe [] (Map.lookup u.id activity)
            pts = mapWithIndex (\b c -> { y: yOfDay (min (toNumber d1 - 0.01) (toNumber (d0 + b * 7) + 3.5)), hw: widthFor c / 2.0 }) counts
          in
            { umbrella: u, index: i, cx, outline: strandPath cx top timeBottom pts, halfMax: dm.maxW / 2.0 }
      )
      tl.umbrellas
  in
    { mode
    , width
    , height: timeBottom + 40.0
    , top
    , timeBottom
    , ribbonLeft
    , ribbonRight
    , strands
    , segments
    , cards
    }

-- Day number (fractional) to y, through whichever month segment holds it.
yOfDayIn :: Array Segment -> Number -> Number
yOfDayIn segs n =
  case Array.find (\s -> toNumber s.d0 <= n && n < toNumber s.d1) segs of
    Just s ->
      Scale.applyScale (Scale.linear # Scale.domain [ toNumber s.d0, toNumber s.d1 ] # Scale.range [ s.y0, s.yTimeEnd ]) n
    Nothing -> case Array.head segs, Array.last segs of
      Just f, _ | n < toNumber f.d0 -> f.y0
      _, Just l -> l.yTimeEnd
      _, _ -> 0.0

-- Cards for one run of unfolded months. Wide: each card goes to the side where it can
-- sit nearest its date; ties go to the side its strand is on.
placeCards
  :: Mode
  -> Dims
  -> (Number -> Number)
  -> (Int -> Number)
  -> (UmbrellaId -> Int)
  -> Int
  -> Number
  -> Number
  -> (Side -> Number)
  -> Number
  -> Number
  -> Number
  -> Array Annotation
  -> Array Card
placeCards mode dm yOfDay strandCx strandIndex nStrands leftLimit rightStart cardWFor ribbonLeft ribbonRight runTop anns =
  let
    yOfDate d = yOfDay (toNumber (dayNumber d) + 0.5)
    wants = map (\a -> { a, want: yOfDate a.date - 14.0, h: cardHeight a }) anns
    assign = case mode of
      Narrow -> map (\x -> Tuple RightSide x) wants
      Wide -> (foldl choose { l: runTop - 1000.0, r: runTop - 1000.0, out: [] } wants).out
    choose acc x =
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
        ys = dodge (runTop + 6.0) dm.gap (map (\x -> { want: x.want, size: x.h }) xs)
        w = cardWFor side
        cx = case side of
          LeftSide -> leftLimit - w
          RightSide -> rightStart
      in
        zipWith (\x y -> mkCard side cx w y x) xs ys
    mkCard side x w y item =
      let
        a = item.a
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
        , spanTop: sy, spanBottom: sy, dayYs: []
        }
  in
    placeSide LeftSide <> placeSide RightSide

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
