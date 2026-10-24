-- | The Timeline view: WHEN the effort went, where the map shows WHERE.
-- |
-- | One swimlane band per map umbrella, in its Mondrian colour; one bar per thread
-- | from its first to its last day, with its active days as ticks underneath. Fate
-- | is drawn as STRUCTURE, not just colour, so the chart survives a black-and-white
-- | print: shipped solid; active solid with an open arrowed end; parked outlined
-- | with a pause mark; half-done hatched; abandoned faded and dashed; superseded a
-- | half-height bar pointing at its successor.
-- |
-- | Selecting an umbrella (a band header here, a cell on the map) filters this view
-- | to that band; the selection lives in MM.App and is shared with the map.
module MM.Timeline.Component
  ( Input
  , Output(..)
  , component
  ) where

import Prelude

import Affjax.ResponseFormat as ARF
import Affjax.Web as AX
import Data.Array (concatMap, find, length, mapMaybe, null)
import Data.Either (Either(..))
import Data.Int (toNumber)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String (length) as Str
import Data.Newtype (unwrap)
import Data.String.Common (toUpper)
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HPA
import MM.Marks (attr, barHeight, endMarkWidth, fateMark, fmtDate, hatchDef, lineAt, num, pathD, rectAt, svgEl, textAt, unfinished)
import MM.Palette as Palette
import MM.Timeline.Layout (Bar, Band, Chart, FateCounts, LabelSide(..), defaultConfig, layoutTimeline)
import MM.Timeline.Types (Fate(..), Thread(..), ThreadName, Timeline(..), UmbrellaId, allFates, fateLabel, parseTimeline)
import Web.HTML (window)
import Web.HTML.Window (innerWidth)
import Web.UIEvent.MouseEvent (MouseEvent, clientX, clientY)

type Input = { selected :: Maybe UmbrellaId, dataUrl :: String }

data Output = SelectUmbrella (Maybe UmbrellaId)

-- | Which threads to draw.
data Scope = Everything | OnlyUnfinished

derive instance Eq Scope

type Tip =
  { thread :: Thread
  , umbrellaName :: String
  , x :: Int
  , y :: Int
  , flip :: Boolean -- open to the left of the pointer (pointer is in the right half)
  , pinned :: Boolean
  }

type Loaded =
  { timeline :: Timeline
  , chart :: Chart
  , scope :: Scope
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
  | SetScope Scope
  | Select (Maybe UmbrellaId)
  | Hover Thread String MouseEvent
  | Leave
  | Tap Thread String MouseEvent

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

-- The chart is cached in state and rebuilt only when what it shows changes, not on
-- every pointer move.
chartFor :: Maybe UmbrellaId -> Scope -> Timeline -> Chart
chartFor selected scope =
  layoutTimeline defaultConfig keep shown
  where
  keep (Thread t) = case scope of
    Everything -> true
    OnlyUnfinished -> t.fate == HalfDone || t.fate == Abandoned
  shown u = case selected of
    Just s -> s == u
    Nothing -> true

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  Initialize -> do
    st <- H.get
    resp <- H.liftAff $ AX.get ARF.string st.input.dataUrl
    case resp of
      Left err -> H.modify_ _ { status = Failed (AX.printError err) }
      Right r -> case parseTimeline r.body of
        Left e -> H.modify_ _ { status = Failed e }
        Right tl ->
          H.modify_ _ { status = Ready { timeline: tl, chart: chartFor st.input.selected Everything tl, scope: Everything, tip: Nothing } }
  Receive input -> do
    st <- H.get
    when (input.selected /= st.input.selected) do
      H.modify_ _ { input = input, status = relayout input.selected st.status }
  SetScope scope ->
    H.modify_ \st -> case st.status of
      Ready l -> st { status = Ready (l { scope = scope, chart = chartFor st.input.selected scope l.timeline, tip = Nothing }) }
      _ -> st
  Select u -> H.raise (SelectUmbrella u)
  Hover th uname e -> do
    t <- tipAt th uname false e
    modifyLoaded \l -> case l.tip of
      Just p | p.pinned -> l
      _ -> l { tip = Just t }
  Leave ->
    modifyLoaded \l -> case l.tip of
      Just p | p.pinned -> l
      _ -> l { tip = Nothing }
  Tap th uname e -> do
    t <- tipAt th uname true e
    modifyLoaded \l -> case l.tip of
      Just p | p.pinned && sameThread p.thread th -> l { tip = Nothing }
      _ -> l { tip = Just t }
  where
  relayout sel = case _ of
    Ready l -> Ready (l { chart = chartFor sel l.scope l.timeline, tip = Nothing })
    s -> s
  modifyLoaded f = H.modify_ \st -> case st.status of
    Ready l -> st { status = Ready (f l) }
    _ -> st
  tipAt th uname pinned e = do
    w <- liftEffect (window >>= innerWidth)
    let x = clientX e
    pure { thread: th, umbrellaName: uname, x, y: clientY e, flip: x * 2 > w, pinned }

sameThread :: Thread -> Thread -> Boolean
sameThread (Thread a) (Thread b) = a.name == b.name

-- Render -------------------------------------------------------------------------

render :: forall m. State -> H.ComponentHTML Action () m
render st = case st.status of
  Loading -> HH.div [ HP.class_ (ClassName "mm-msg") ] [ HH.text "Loading the timeline..." ]
  Failed e -> HH.div [ HP.class_ (ClassName "mm-msg mm-err") ] [ HH.text ("Could not load the timeline: " <> e) ]
  Ready l ->
    let
      Timeline tl = l.timeline
      selectedName = st.input.selected >>= \s -> _.name <$> find (\u -> u.id == s) tl.umbrellas
      drawn = concatMap _.bars l.chart.bands
    in
      HH.div [ HP.class_ (ClassName "tl") ]
        ( [ HH.div [ HP.class_ (ClassName "tl-bar") ]
              ( [ legend
                , HH.div [ HP.class_ (ClassName "tl-tools") ]
                    ( [ HH.button
                          [ HP.class_ (ClassName ("mm-btn" <> if l.scope == OnlyUnfinished then " on" else ""))
                          , HPA.pressed (if l.scope == OnlyUnfinished then "true" else "false")
                          , HE.onClick \_ -> SetScope (if l.scope == OnlyUnfinished then Everything else OnlyUnfinished)
                          ]
                          [ HH.text "only half-done & abandoned" ]
                      , HH.span [ HP.class_ (ClassName "tl-count") ]
                          [ HH.text (show (length drawn) <> " of " <> show (length tl.threads) <> " threads") ]
                      ]
                        <> case selectedName of
                          Just n ->
                            [ HH.button [ HP.class_ (ClassName "mm-btn"), HE.onClick \_ -> Select Nothing ]
                                [ HH.text (n <> " only \x00d7") ]
                            ]
                          Nothing -> []
                    )
                ]
              )
          , HH.div [ HP.class_ (ClassName "tl-scroll") ]
              [ axis l.chart
              , chartSvg l.timeline l.chart st.input.selected
              ]
          ]
            <> tooltip l.tip
        )

legend :: forall w i. HH.HTML w i
legend =
  HH.ul [ HP.class_ (ClassName "tl-legend") ]
    (map item allFates)
  where
  item f =
    HH.li_
      [ svgEl "svg"
          [ attr "viewBox" "0 0 34 12", attr "width" "34", attr "height" "12", attr "aria-hidden" "true" ]
          ( [ svgEl "defs" [] [ hatchDef "legend" Palette.blue ] ]
              <> fateMark "legend" Palette.blue f 1.0 (25.0 - endMarkWidth f) 2.0
          )
      , HH.text (fateLabel f)
      ]

axisHeight :: Number
axisHeight = 30.0

axis :: forall w i. Chart -> HH.HTML w i
axis chart =
  svgEl "svg"
    [ attr "class" "tl-svg tl-axis"
    , attr "viewBox" ("0 0 " <> num chart.width <> " " <> num axisHeight)
    , attr "preserveAspectRatio" "xMinYMin meet"
    ]
    ( [ rectAt 0.0 0.0 chart.width axisHeight [ attr "fill" Palette.cream ] ]
        <> concatMap tick chart.months
        <> [ lineAt 0.0 (axisHeight - 0.5) chart.width (axisHeight - 0.5) [ attr "stroke" Palette.ink, attr "stroke-width" "1" ] ]
    )
  where
  tick m =
    [ lineAt m.x (if m.yearStart then 6.0 else 18.0) m.x axisHeight
        [ attr "stroke" Palette.ink, attr "stroke-width" (if m.yearStart then "1.2" else "0.7") ]
    , textAt (m.x + 4.0) 20.0 m.label
        [ attr "fill" Palette.ink, attr "font-size" "11"
        , attr "font-weight" (if m.yearStart then "600" else "400")
        , attr "letter-spacing" "0.04em"
        ]
    ]

chartSvg :: forall m. Timeline -> Chart -> Maybe UmbrellaId -> H.ComponentHTML Action () m
chartSvg (Timeline tl) chart selected =
  let
    bars = concatMap _.bars chart.bands
    -- where each drawn thread's bar is, for successor links
    byName = Map.fromFoldable (map (\b -> Tuple (threadName b.thread) b) bars)
    defs = svgEl "defs" [] (map (\u -> hatchDef (show (unwrap u.id)) (Palette.umbrellaColor u.id)) tl.umbrellas)
    grid = concatMap
      ( \m ->
          [ lineAt m.x 0.0 m.x chart.height
              [ attr "stroke" Palette.ink, attr "stroke-opacity" (if m.yearStart then "0.28" else "0.08"), attr "stroke-width" "1" ]
          ]
      )
      chart.months
    links = mapMaybe (successorLink byName) bars
  in
    svgEl "svg"
      [ attr "class" "tl-svg tl-body"
      , attr "viewBox" ("0 0 " <> num chart.width <> " " <> num (max 1.0 chart.height))
      , attr "preserveAspectRatio" "xMinYMin meet"
      , attr "role" "img"
      , attr "aria-label" "Timeline of threads by umbrella, October 2025 to October 2026"
      ]
      ( [ defs ]
          <> concatMap bandBackground chart.bands
          <> grid
          <> links
          <> concatMap (bandMarks selected) chart.bands
          <> (if null bars then [ emptyNote chart ] else [])
      )

emptyNote :: forall w i. Chart -> HH.HTML w i
emptyNote chart =
  textAt (chart.plotLeft + 8.0) 60.0 "Nothing to show here with this filter."
    [ attr "fill" Palette.ink, attr "font-size" "13", attr "font-weight" "300" ]

threadName :: Thread -> ThreadName
threadName (Thread t) = t.name

successorLink :: forall w i. Map.Map ThreadName Bar -> Bar -> Maybe (HH.HTML w i)
successorLink byName b = do
  let Thread t = b.thread
  s <- t.successor
  to <- Map.lookup s byName
  let
    ya = b.rect.y + 4.0 + barHeight / 2.0
    yb = to.rect.y + 4.0 + barHeight / 2.0
    xa = b.x1
    xb = to.x0
    mid = (xa + xb) / 2.0
  pure $ pathD
    ("M" <> num xa <> " " <> num ya <> "C" <> num mid <> " " <> num ya <> " " <> num mid <> " " <> num yb <> " " <> num xb <> " " <> num yb)
    [ attr "fill" "none", attr "stroke" Palette.ink, attr "stroke-width" "0.9"
    , attr "stroke-dasharray" "1 2.5", attr "stroke-linecap" "round", attr "stroke-opacity" "0.6"
    ]

bandBackground :: forall w i. Band -> Array (HH.HTML w i)
bandBackground band =
  let
    c = Palette.umbrellaColor band.umbrella.id
    tint = if c == Palette.white then [] else [ rectAt band.body.x band.body.y band.body.width band.body.height [ attr "fill" c, attr "fill-opacity" "0.05" ] ]
  in
    tint

bandMarks :: forall m. Maybe UmbrellaId -> Band -> Array (H.ComponentHTML Action () m)
bandMarks selected band =
  let
    u = band.umbrella
    c = Palette.umbrellaColor u.id
    key = show (unwrap u.id)
    h = band.header
    isSelected = selected == Just u.id
    countsText = summary band.countsAll band.counts (band.counts.total /= band.countsAll.total)
    header =
      svgEl "g"
        [ attr "class" "tl-head"
        , attr "role" "button"
        , attr "tabindex" "0"
        , attr "aria-pressed" (if isSelected then "true" else "false")
        , HE.onClick \_ -> Select (if isSelected then Nothing else Just u.id)
        ]
        [ rectAt 0.0 h.y h.width h.height [ attr "fill" "transparent" ]
        , lineAt 0.0 (h.y + 3.0) h.width (h.y + 3.0) [ attr "stroke" Palette.ink, attr "stroke-width" "3" ]
        , rectAt (h.x + 14.0) (h.y + 13.0) 16.0 16.0 [ attr "fill" c, attr "stroke" Palette.ink, attr "stroke-width" "1.5" ]
        , textAt (h.x + 38.0) (h.y + 27.0) (toUpper u.name)
            [ attr "fill" Palette.ink, attr "font-size" "14", attr "font-weight" "600", attr "letter-spacing" "0.12em" ]
        , textAt (h.x + 44.0 + 11.2 * toNumber (lengthOf u.name)) (h.y + 27.0) countsText
            [ attr "fill" Palette.ink, attr "fill-opacity" "0.7", attr "font-size" "11.5", attr "font-weight" "400" ]
        ]
  in
    [ header ] <> concatMap (barMarks key c u.name) band.bars

lengthOf :: String -> Int
lengthOf = Str.length

summary :: FateCounts -> FateCounts -> Boolean -> String
summary all drawn filtered =
  let
    base = show all.total <> " threads \x00b7 " <> show all.halfDone <> " half-done \x00b7 " <> show all.abandoned <> " abandoned \x00b7 " <> show all.parked <> " parked"
  in
    if filtered then base <> "  (" <> show drawn.total <> " shown)" else base

barMarks :: forall m. String -> String -> String -> Bar -> Array (H.ComponentHTML Action () m)
barMarks key colour uname b =
  let
    Thread t = b.thread
    y = b.rect.y + 4.0
    tickTop = y + barHeight + 1.5
    ticks = map (\x -> lineAt x tickTop x (tickTop + 3.0) [ attr "stroke" Palette.ink, attr "stroke-width" "0.8", attr "stroke-opacity" "0.75" ]) b.ticks
    labelProps =
      [ attr "fill" Palette.ink
      , attr "font-size" "10.5"
      , attr "font-weight" (if unfinished t.fate then "600" else "400")
      , attr "fill-opacity" (if t.fate == Abandoned then "0.7" else if unfinished t.fate then "1" else "0.82")
      ]
    label = case b.labelSide of
      LabelAfter -> textAt (b.x1 + endMarkWidth t.fate + 5.0) (y + barHeight - 0.5) b.label labelProps
      LabelBefore -> textAt (b.x0 - 5.0) (y + barHeight - 0.5) b.label (labelProps <> [ attr "text-anchor" "end" ])
      LabelInside ->
        -- on the bar itself; legible ink for the bar's fill
        let solidBar = t.fate == Shipped || t.fate == Active
        in textAt (b.x0 + 6.0) (y + barHeight - 0.8) b.label
             (labelProps <> [ attr "fill" (if solidBar then Palette.inkOn (Palette.barFill colour) else Palette.ink), attr "font-size" "8.5" ])
    -- an invisible hit area over the whole row slot, so a 3px bar is still easy to hover/tap
    hit = rectAt b.rect.x b.rect.y (max 8.0 (b.x1 - b.x0 + endMarkWidth t.fate)) b.rect.height [ attr "fill" "transparent" ]
  in
    [ svgEl "g"
        [ attr "class" "tl-thread"
        , HE.onMouseMove (Hover b.thread uname)
        , HE.onMouseLeave \_ -> Leave
        , HE.onClick (Tap b.thread uname)
        ]
        ([ hit ] <> fateMark key colour t.fate b.x0 b.x1 y <> ticks <> [ label ])
    ]

-- Tooltip ------------------------------------------------------------------------

tooltip :: forall w i. Maybe Tip -> Array (HH.HTML w i)
tooltip = case _ of
  Nothing -> []
  Just tip ->
    let
      Thread t = tip.thread
      where_ = tip.umbrellaName <> case t.project of
        Just p -> " \x203a " <> p.name
        Nothing -> ""
      nDays = length t.days
      dates =
        (if t.first == t.last then fmtDate t.first else fmtDate t.first <> " \x2013 " <> fmtDate t.last)
          <> " \x00b7 " <> show nDays <> (if nDays == 1 then " active day" else " active days")
      fateLine = fateLabel t.fate <> case t.successor of
        Just s -> " \x2192 superseded by " <> unwrap s
        Nothing -> ""
      pos =
        if tip.flip then "right:calc(100vw - " <> show (tip.x - 14) <> "px); top:" <> show (tip.y + 16) <> "px"
        else "left:" <> show (tip.x + 14) <> "px; top:" <> show (tip.y + 16) <> "px"
    in
      [ HH.div [ HP.class_ (ClassName "mm-tip tl-tip"), HP.style pos ]
          ( [ HH.div [ HP.class_ (ClassName "mm-tip-title") ] [ HH.text (unwrap t.name) ]
            , HH.div [ HP.class_ (ClassName "tl-tip-meta") ] [ HH.text where_ ]
            ]
              <> (if t.what == "" then [] else [ HH.div [ HP.class_ (ClassName "mm-tip-body") ] [ HH.text t.what ] ])
              <>
                [ HH.div [ HP.class_ (ClassName "tl-tip-meta") ] [ HH.text dates ]
                , HH.div [ HP.class_ (ClassName ("tl-tip-fate" <> if unfinished t.fate then " is-unfinished" else "")) ] [ HH.text fateLine ]
                ]
          )
      ]
