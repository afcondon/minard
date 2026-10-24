-- | The Git view: the year's commits as a calendar in GitHub's manner, with the lines
-- | of code and of prose underneath, week by week, under the same columns.
-- |
-- | This view says more about Claude Code than the others do: the ideas and the
-- | direction were Andrew's, the lines nearly all Claude's. The heading says so, and
-- | the share of commits with a Claude co-author is one of the headline numbers.
-- |
-- | Selecting an umbrella (a chip here, a cell on the map, a band on the timeline)
-- | redraws the whole view from that umbrella's counts; the selection lives in MM.App.
module MM.Git.Component
  ( Input
  , Output(..)
  , component
  ) where

import Prelude

import Affjax.ResponseFormat as ARF
import Affjax.Web as AX
import Data.Array (concatMap, foldl, length, mapWithIndex, (!!))
import Data.Either (Either(..))
import Data.Int (round, toNumber)
import Data.Maybe (Maybe(..), maybe)
import Data.String (joinWith)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HPA
import MM.Git.Layout (Cell, GitChart, Shading(..), Strip, Week, layoutGit)
import MM.Git.Types (Counts, GitActivity(..), Totals, parseGitActivity)
import MM.Marks (attr, fmtDate, lineAt, num, pathD, rectAt, svgEl, textAt)
import MM.Palette as Palette
import MM.Timeline.Types (Umbrella, UmbrellaId)
import Web.HTML (window)
import Web.HTML.Window (innerWidth)
import Web.UIEvent.MouseEvent (MouseEvent, clientX, clientY)

type Input = { selected :: Maybe UmbrellaId, dataUrl :: String }

data Output = SelectUmbrella (Maybe UmbrellaId)

-- | What the pointer is over.
data Target = OnDay Cell | OnWeek Week

type Tip = { target :: Target, x :: Int, y :: Int, flip :: Boolean, pinned :: Boolean }

type Loaded =
  { activity :: GitActivity
  , chart :: GitChart
  , shading :: Shading
  , tip :: Maybe Tip
  }

data Status
  = Loading
  | Failed String
  | Ready Loaded

type State = { input :: Input, status :: Status }

data Action
  = Initialize
  | Receive Input
  | SetShading Shading
  | Select (Maybe UmbrellaId)
  | Hover Target MouseEvent
  | Leave
  | Tap Target MouseEvent

component :: forall q m. MonadAff m => H.Component q Input Output m
component =
  H.mkComponent
    { initialState: \input -> { input, status: Loading }
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        , receive = Just <<< Receive
        }
    }

chartFor :: Maybe UmbrellaId -> GitActivity -> GitChart
chartFor sel a@(GitActivity g) = layoutGit g.umbrellas sel a

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  Initialize -> do
    st <- H.get
    resp <- H.liftAff $ AX.get ARF.string st.input.dataUrl
    case resp of
      Left err -> H.modify_ _ { status = Failed (AX.printError err) }
      Right r -> case parseGitActivity r.body of
        Left e -> H.modify_ _ { status = Failed e }
        Right a -> H.modify_ _ { status = Ready { activity: a, chart: chartFor st.input.selected a, shading: ByActivity, tip: Nothing } }
  Receive input -> do
    st <- H.get
    when (input.selected /= st.input.selected) do
      H.modify_ \s -> s
        { input = input
        , status = case s.status of
            Ready l -> Ready (l { chart = chartFor input.selected l.activity, tip = Nothing })
            other -> other
        }
  SetShading sh -> modifyLoaded _ { shading = sh }
  Select u -> H.raise (SelectUmbrella u)
  Hover t e -> do
    tip <- tipAt t false e
    modifyLoaded \l -> case l.tip of
      Just p | p.pinned -> l
      _ -> l { tip = Just tip }
  Leave ->
    modifyLoaded \l -> case l.tip of
      Just p | p.pinned -> l
      _ -> l { tip = Nothing }
  Tap t e -> do
    tip <- tipAt t true e
    modifyLoaded \l -> case l.tip of
      Just p | p.pinned && sameTarget p.target t -> l { tip = Nothing }
      _ -> l { tip = Just tip }
  where
  modifyLoaded f = H.modify_ \st -> case st.status of
    Ready l -> st { status = Ready (f l) }
    _ -> st
  tipAt target pinned e = do
    w <- liftEffect (window >>= innerWidth)
    let x = clientX e
    pure { target, x, y: clientY e, flip: x * 2 > w, pinned }

sameTarget :: Target -> Target -> Boolean
sameTarget a b = case a, b of
  OnDay x, OnDay y -> x.date == y.date
  OnWeek x, OnWeek y -> x.start == y.start
  _, _ -> false

-- Render -------------------------------------------------------------------------

render :: forall m. State -> H.ComponentHTML Action () m
render st = case st.status of
  Loading -> HH.div [ HP.class_ (ClassName "mm-msg") ] [ HH.text "Loading the commits..." ]
  Failed e -> HH.div [ HP.class_ (ClassName "mm-msg mm-err") ] [ HH.text ("Could not load the commits: " <> e) ]
  Ready l ->
    let
      GitActivity g = l.activity
      sel = st.input.selected
    in
      HH.div [ HP.class_ (ClassName "gt") ]
        ( [ stats g.totals sel l.chart
          , HH.div [ HP.class_ (ClassName "gt-bar") ]
              [ umbrellaChips g.umbrellas sel
              , HH.div [ HP.class_ (ClassName "ln-group ln-seg") ]
                  [ HH.span [ HP.class_ (ClassName "ln-group-label") ] [ HH.text "shade days by" ]
                  , shadeButton "activity" ByActivity l.shading
                  , shadeButton "umbrella" ByUmbrella l.shading
                  ]
              , levelKey l.chart sel l.shading
              ]
          , HH.div [ HP.class_ (ClassName "gt-scroll") ]
              [ chartSvg g.umbrellas sel l.shading l.chart ]
          , footnote g.totals
          ]
            <> tooltip g.umbrellas l.tip
        )

shadeButton :: forall w. String -> Shading -> Shading -> HH.HTML w Action
shadeButton label sh current =
  HH.button
    [ HP.class_ (ClassName ("mm-btn" <> if sh == current then " on" else ""))
    , HPA.pressed (if sh == current then "true" else "false")
    , HE.onClick \_ -> SetShading sh
    ]
    [ HH.text label ]

-- The headline numbers. With an umbrella selected they are that umbrella's share.
stats :: forall w i. Totals -> Maybe UmbrellaId -> GitChart -> HH.HTML w i
stats totals sel chart =
  let
    c = foldl (\acc w -> addTo acc w.counts) { commits: 0, claude: 0, codeAdded: 0, codeDeleted: 0, proseAdded: 0, proseDeleted: 0 } chart.weeks
    days = length (filterCells chart.cells)
    share = if c.commits == 0 then 0 else round (100.0 * toNumber c.claude / toNumber c.commits)
    stat big small = HH.div [ HP.class_ (ClassName "gt-stat") ]
      [ HH.div [ HP.class_ (ClassName "gt-big") ] [ HH.text big ]
      , HH.div [ HP.class_ (ClassName "gt-small") ] [ HH.text small ]
      ]
  in
    HH.div [ HP.class_ (ClassName "gt-stats") ]
      [ stat (thousands c.commits) ("commits" <> if sel == Nothing then " in " <> show totals.repos <> " repositories" else "")
      , stat (show days) ("days with a commit, of " <> show (length chart.cells))
      , stat (show share <> "%") "of commits co-authored by Claude"
      , stat ("+" <> compact c.codeAdded) ("lines of code added, " <> compact c.codeDeleted <> " deleted")
      , stat ("+" <> compact c.proseAdded) "lines of prose (Markdown)"
      ]
  where
  filterCells = foldl (\acc x -> if x.counts.commits > 0 then acc <> [ x ] else acc) []
  addTo a b =
    { commits: a.commits + b.commits, claude: a.claude + b.claude
    , codeAdded: a.codeAdded + b.codeAdded, codeDeleted: a.codeDeleted + b.codeDeleted
    , proseAdded: a.proseAdded + b.proseAdded, proseDeleted: a.proseDeleted + b.proseDeleted
    }

umbrellaChips :: forall w. Array Umbrella -> Maybe UmbrellaId -> HH.HTML w Action
umbrellaChips umbrellas sel =
  HH.div [ HP.class_ (ClassName "ln-chips") ]
    (map chip umbrellas)
  where
  chip u =
    let on = sel == Just u.id
    in
      HH.button
        [ HP.class_ (ClassName ("mm-btn ln-chip" <> if on then " on" else ""))
        , HPA.pressed (if on then "true" else "false")
        , HE.onClick \_ -> Select (if on then Nothing else Just u.id)
        ]
        [ swatch (Palette.umbrellaColor u.id), HH.text u.name ]

swatch :: forall w i. String -> HH.HTML w i
swatch c =
  svgEl "svg" [ attr "viewBox" "0 0 12 12", attr "width" "12", attr "height" "12", attr "aria-hidden" "true" ]
    [ rectAt 0.5 0.5 11.0 11.0 [ attr "fill" c, attr "stroke" Palette.ink, attr "stroke-width" "1" ] ]

-- "Less □□□□□ More", with the commit counts each step starts at.
levelKey :: forall w i. GitChart -> Maybe UmbrellaId -> Shading -> HH.HTML w i
levelKey chart sel shading =
  HH.div [ HP.class_ (ClassName "gt-key") ]
    ( [ HH.span_ [ HH.text "commits a day" ] ]
        <> mapWithIndex step ([ 0 ] <> chart.thresholds)
    )
  where
  step i t =
    HH.span [ HP.class_ (ClassName "gt-key-step") ]
      [ svgEl "svg" [ attr "viewBox" "0 0 12 12", attr "width" "12", attr "height" "12", attr "aria-hidden" "true" ]
          [ rectAt 0.5 0.5 11.0 11.0 (cellPaint shading sel i) ]
      , HH.text (if i == 0 then "0" else show t <> "+")
      ]

-- How a cell of a given level is painted. By activity: ink, darker for busier. By
-- umbrella: the lead umbrella's colour, more opaque for busier.
cellPaint :: forall r i. Shading -> Maybe UmbrellaId -> Int -> Array (HP.IProp r i)
cellPaint shading lead level =
  if level == 0 then [ attr "fill" "#e7e1d3" ]
  else case shading, lead of
    ByUmbrella, Just u ->
      [ attr "fill" (Palette.barFill (Palette.umbrellaColor u)), attr "fill-opacity" (opacity level)
      , attr "stroke" Palette.ink, attr "stroke-opacity" "0.25", attr "stroke-width" "0.6"
      ]
    _, _ -> [ attr "fill" Palette.ink, attr "fill-opacity" (opacity level) ]
  where
  opacity = case _ of
    1 -> "0.22"
    2 -> "0.45"
    3 -> "0.7"
    _ -> "1"

otherGrey :: String
otherGrey = "#a39e93"

stackColour :: Maybe UmbrellaId -> String
stackColour = maybe otherGrey (Palette.barFill <<< Palette.umbrellaColor)

chartSvg :: forall m. Array Umbrella -> Maybe UmbrellaId -> Shading -> GitChart -> H.ComponentHTML Action () m
chartSvg _ _ shading chart =
  svgEl "svg"
    [ attr "class" "gt-svg"
    , attr "viewBox" ("0 0 " <> num chart.width <> " " <> num chart.height)
    , attr "preserveAspectRatio" "xMinYMin meet"
    , attr "role" "img"
    , attr "aria-label" "Commits per day as a calendar, with lines of code and prose per week underneath"
    ]
    ( monthLabels chart
        <> weekdayLabels chart
        <> map (cellMark shading) chart.cells
        <> stripLabel chart chart.code (compact chart.code.peak <> " a week at most")
        <> [ lineAt chart.gutter chart.codeBaseline chart.width chart.codeBaseline [ attr "stroke" Palette.ink, attr "stroke-width" "0.8" ] ]
        <> concatMap (codeBars chart) chart.weeks
        <> stripLabel chart chart.net (netNote chart)
        <> netLine chart
        <> stripLabel chart chart.prose (compact chart.prose.peak <> " a week at most")
        <> [ lineAt chart.gutter chart.prose.bottom chart.width chart.prose.bottom [ attr "stroke" Palette.ink, attr "stroke-width" "0.8" ] ]
        <> map (proseBar chart) chart.weeks
        <> map (weekHit chart) chart.weeks
    )

monthLabels :: forall w i. GitChart -> Array (HH.HTML w i)
monthLabels chart =
  concatMap
    ( \m ->
        [ textAt m.x 11.0 m.label
            [ attr "fill" Palette.ink, attr "font-size" "10.5", attr "letter-spacing" "0.04em"
            , attr "font-weight" (if m.yearStart then "600" else "400")
            ]
        ]
    )
    chart.months

weekdayLabels :: forall w i. GitChart -> Array (HH.HTML w i)
weekdayLabels chart =
  map
    ( \d ->
        textAt 0.0 (chart.calendarTop + toNumber d.row * chart.step + chart.step * 0.62) d.label
          [ attr "fill" Palette.ink, attr "fill-opacity" "0.7", attr "font-size" "9.5" ]
    )
    [ { row: 0, label: "Mon" }, { row: 2, label: "Wed" }, { row: 4, label: "Fri" } ]

cellMark :: forall m. Shading -> Cell -> H.ComponentHTML Action () m
cellMark shading c =
  svgEl "rect"
    ( [ attr "x" (num c.x), attr "y" (num c.y), attr "width" (num c.size), attr "height" (num c.size)
      , attr "rx" "1.5", attr "class" "gt-cell"
      , HE.onMouseMove (Hover (OnDay c))
      , HE.onMouseLeave \_ -> Leave
      , HE.onClick (Tap (OnDay c))
      ]
        <> cellPaint shading c.lead c.level
    )
    []

stripLabel :: forall w i. GitChart -> Strip -> String -> Array (HH.HTML w i)
stripLabel chart s note =
  [ textAt chart.gutter (s.top - 12.0) s.label
      [ attr "fill" Palette.ink, attr "font-size" "10", attr "font-weight" "600", attr "letter-spacing" "0.12em" ]
  , textAt chart.width (s.top - 12.0) note
      [ attr "fill" Palette.ink, attr "fill-opacity" "0.7", attr "font-size" "10", attr "text-anchor" "end" ]
  ]

netNote :: GitChart -> String
netNote chart = compact chart.netFinal <> " net by the last week"

codeBars :: forall w i. GitChart -> Week -> Array (HH.HTML w i)
codeBars chart w =
  map (\s -> rectAt w.x s.y1 w.width (s.y0 - s.y1) [ attr "fill" (stackColour s.umbrella) ]) w.added
    <> [ rectAt w.x chart.codeBaseline w.width (w.deletedTo - chart.codeBaseline) [ attr "fill" Palette.ink, attr "fill-opacity" "0.4" ] ]

netLine :: forall w i. GitChart -> Array (HH.HTML w i)
netLine chart =
  let
    pts = map (\w -> num (w.x + w.width / 2.0) <> " " <> num w.netY) chart.weeks
    lastX = maybe chart.width (\w -> w.x + w.width / 2.0) (chart.weeks !! (length chart.weeks - 1))
    firstX = maybe chart.gutter (\w -> w.x + w.width / 2.0) (chart.weeks !! 0)
    line = "M" <> joinWith "L" pts
  in
    [ pathD (line <> "L" <> num lastX <> " " <> num chart.net.bottom <> "L" <> num firstX <> " " <> num chart.net.bottom <> "Z")
        [ attr "fill" Palette.blue, attr "fill-opacity" "0.12" ]
    , pathD line [ attr "fill" "none", attr "stroke" Palette.blue, attr "stroke-width" "1.6", attr "stroke-linejoin" "round" ]
    , lineAt chart.gutter chart.net.bottom chart.width chart.net.bottom [ attr "stroke" Palette.ink, attr "stroke-width" "0.8" ]
    ]

proseBar :: forall w i. GitChart -> Week -> HH.HTML w i
proseBar chart w =
  rectAt w.x w.proseTop w.width (chart.prose.bottom - w.proseTop) [ attr "fill" Palette.ink, attr "fill-opacity" "0.55" ]

-- An invisible column over the three weekly strips, so a thin bar is easy to hover.
weekHit :: forall m. GitChart -> Week -> H.ComponentHTML Action () m
weekHit chart w =
  svgEl "rect"
    [ attr "x" (num (w.x - 1.0)), attr "y" (num (chart.code.top - 4.0))
    , attr "width" (num (chart.step)), attr "height" (num (chart.prose.bottom - chart.code.top + 4.0))
    , attr "fill" "transparent", attr "class" "gt-week"
    , HE.onMouseMove (Hover (OnWeek w))
    , HE.onMouseLeave \_ -> Leave
    , HE.onClick (Tap (OnWeek w))
    ]
    []

footnote :: forall w i. Totals -> HH.HTML w i
footnote totals =
  HH.p [ HP.class_ (ClassName "gt-note") ]
    [ HH.text
        ( "What is counted: every commit of mine on two machines since year zero, each counted once however many clones it sits in. "
            <> "Lines are lines of source code and of Markdown prose. Left out: compiled output, bundles, lockfiles, data files and built sites, "
            <> "and the lines (not the commits) of "
            <> show totals.bulkCommits
            <> " bulk commits that imported or reorganised code wholesale. Commits made by a hook or a timer, such as automatic backups, are not counted at all. "
            <> "I supplied the ideas and the direction; Claude Code wrote nearly all of the lines."
        )
    ]

-- Tooltip ------------------------------------------------------------------------

tooltip :: forall w i. Array Umbrella -> Maybe Tip -> Array (HH.HTML w i)
tooltip umbrellas = case _ of
  Nothing -> []
  Just tip ->
    let
      pos =
        if tip.flip then "right:calc(100vw - " <> show (tip.x - 14) <> "px); top:" <> show (tip.y + 16) <> "px"
        else "left:" <> show (tip.x + 14) <> "px; top:" <> show (tip.y + 16) <> "px"
      body = case tip.target of
        OnDay c ->
          [ HH.div [ HP.class_ (ClassName "mm-tip-title") ] [ HH.text (fmtDate c.date) ] ]
            <> countsLines c.counts
            <> case c.lead of
              Just u | c.counts.commits > 0 -> [ HH.div [ HP.class_ (ClassName "tl-tip-meta") ] [ HH.text ("mostly " <> nameOf u) ] ]
              _ -> []
        OnWeek w ->
          [ HH.div [ HP.class_ (ClassName "mm-tip-title") ] [ HH.text ("Week of " <> fmtDate w.start) ] ]
            <> countsLines w.counts
    in
      [ HH.div [ HP.class_ (ClassName "mm-tip"), HP.style pos ] body ]
  where
  nameOf u = maybe "" _.name (foldl (\acc x -> if x.id == u then Just x else acc) Nothing umbrellas)

countsLines :: forall w i. Counts -> Array (HH.HTML w i)
countsLines c =
  if c.commits == 0 then [ HH.div [ HP.class_ (ClassName "tl-tip-meta") ] [ HH.text "no commits" ] ]
  else
    [ HH.div [ HP.class_ (ClassName "mm-tip-body") ]
        [ HH.text (thousands c.commits <> (if c.commits == 1 then " commit" else " commits") <> ", " <> thousands c.claude <> " with Claude") ]
    , HH.div [ HP.class_ (ClassName "tl-tip-meta") ]
        [ HH.text ("code +" <> thousands c.codeAdded <> " \x2212" <> thousands c.codeDeleted <> " \x00b7 prose +" <> thousands c.proseAdded) ]
    ]

-- Numbers ------------------------------------------------------------------------

-- | 1234567 -> "1,234,567"
thousands :: Int -> String
thousands n
  | n < 0 = "\x2212" <> thousands (negate n)
  | n < 1000 = show n
  | otherwise = thousands (n `div` 1000) <> "," <> pad3 (n `mod` 1000)
  where
  pad3 r
    | r < 10 = "00" <> show r
    | r < 100 = "0" <> show r
    | otherwise = show r

-- | 1348741 -> "1.35M", 62682 -> "63k", 4802 -> "4.8k", 480 -> "480"
compact :: Int -> String
compact n
  | n < 0 = "\x2212" <> compact (negate n)
  | n >= 1000000 = decimal 2 (toNumber n / 1000000.0) <> "M"
  | n >= 10000 = decimal 0 (toNumber n / 1000.0) <> "k"
  | n >= 1000 = decimal 1 (toNumber n / 1000.0) <> "k"
  | otherwise = show n

-- | A number to a fixed number of decimal places (0, 1 or 2).
decimal :: Int -> Number -> String
decimal places x =
  let
    scale = if places >= 2 then 100 else if places == 1 then 10 else 1
    r = round (x * toNumber scale)
    whole = r `div` scale
    frac = r - whole * scale
    fracText = if places >= 2 && frac < 10 then "0" <> show frac else show frac
  in
    if places == 0 then show r else show whole <> "." <> fracText
