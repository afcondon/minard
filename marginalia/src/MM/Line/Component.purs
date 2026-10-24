-- | The Line view: one vertical line of time down the page, drawn as a ribbon cable of
-- | umbrella strands, with annotations for the threads as they were worked on.
-- |
-- | A strand swells in the weeks its umbrella was busy and thins to a hairline when it
-- | was quiet, so shifts of attention read at a glance. Each strand also carries its
-- | own texture (solid, dotted, striped, ruled) and keeps a fixed place in the cable,
-- | so the ribbon survives a black-and-white print.
-- |
-- | Hover or tap an annotation to light its strand and the thread's span on it;
-- | a strand head (or chip) selects that umbrella, shared with the other views.
module MM.Line.Component
  ( Input
  , Output(..)
  , component
  ) where

import Prelude

import Affjax.ResponseFormat as ARF
import Affjax.Web as AX
import Data.Array (concatMap, find, length)
import Data.Either (Either(..))
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.String.Common (toUpper)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HPA
import Halogen.Query.Event (eventListener)
import MM.Line.Layout (Annotation, Card, Geometry, Mode(..), Moment(..), Segment, Strand, cardHeight, layoutLine, monthKeys, momentLabel)
import MM.Line.Options (FateGroup, LineOptions, MonthKey, allGroups, densitySteps, groupLabel)
import Data.Set as Set
import MM.Marks (attr, fateMark, fmtDate, hatchDef, lineAt, num, pathD, rectAt, svgEl, textAt)
import MM.Palette as Palette
import MM.Timeline.Types (Fate(..), Thread(..), ThreadName, Timeline(..), Umbrella, UmbrellaId, fateLabel, parseTimeline)
import Web.Event.Event (EventType(..))
import Web.HTML (window)
import Web.HTML.Window (innerWidth, toEventTarget)

type Input = { selected :: Maybe UmbrellaId, options :: LineOptions, dataUrl :: String }

-- | Selection and options live in MM.App (and the URL); this view asks for changes.
data Output = SelectUmbrella (Maybe UmbrellaId) | SetOptions LineOptions

type Focus = { name :: ThreadName, umbrella :: UmbrellaId, pinned :: Boolean }

type Loaded = { timeline :: Timeline, geometry :: Geometry, focus :: Maybe Focus }

data Status
  = Loading
  | Failed String
  | Ready Loaded

type State = { input :: Input, width :: Number, status :: Status }

data Action
  = Initialize
  | Receive Input
  | Resize
  | Select (Maybe UmbrellaId)
  | Enter Annotation
  | Leave
  | Tap Annotation
  | ToggleMonth MonthKey
  | FoldAll
  | UnfoldAll
  | ToggleFate FateGroup
  | SetDensity Int

component :: forall q m. MonadAff m => H.Component q Input Output m
component =
  H.mkComponent
    { initialState: \input -> { input, width: 1200.0, status: Loading }
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        , receive = Just <<< Receive
        }
    }

geometryFor :: Number -> Input -> Timeline -> Geometry
geometryFor width input = layoutLine width shown input.options
  where
  shown u = case input.selected of
    Just s -> s == u
    Nothing -> true

-- The width the chart gets: the window, less the shell's gutters (styles.css
-- --gutter) and, on desktop, a scrollbar.
availableWidth :: Int -> Number
availableWidth w =
  let
    wn = toNumber w
    gutter = max 16.0 (min 40.0 (wn * 0.024))
    scrollbar = if wn >= 760.0 then 16.0 else 0.0
  in
    wn - 2.0 * gutter - scrollbar

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  Initialize -> do
    w <- liftEffect window
    void $ H.subscribe (eventListener (EventType "resize") (toEventTarget w) (const (Just Resize)))
    width <- availableWidth <$> liftEffect (innerWidth w)
    st <- H.modify _ { width = width }
    resp <- H.liftAff $ AX.get ARF.string st.input.dataUrl
    case resp of
      Left err -> H.modify_ _ { status = Failed (AX.printError err) }
      Right r -> case parseTimeline r.body of
        Left e -> H.modify_ _ { status = Failed e }
        Right tl -> H.modify_ _ { status = Ready { timeline: tl, geometry: geometryFor width st.input tl, focus: Nothing } }
  Receive input -> do
    st <- H.get
    when (input.selected /= st.input.selected || input.options /= st.input.options) do
      H.modify_ _ { input = input, status = relayout st.width input st.status }
  Resize -> do
    width <- availableWidth <$> liftEffect (window >>= innerWidth)
    st <- H.get
    when (width /= st.width) do
      H.modify_ _ { width = width, status = relayout width st.input st.status }
  Select u -> H.raise (SelectUmbrella u)
  ToggleMonth k -> setOptions \o -> o { folded = toggle k o.folded }
  FoldAll -> do
    st <- H.get
    case st.status of
      Ready l -> setOptions _ { folded = Set.fromFoldable (monthKeys l.timeline) }
      _ -> pure unit
  UnfoldAll -> setOptions _ { folded = Set.empty }
  ToggleFate g -> setOptions \o -> o { fates = toggle g o.fates }
  SetDensity n -> setOptions _ { minDays = n }
  Enter a -> modifyLoaded \l -> case l.focus of
    Just f | f.pinned -> l
    _ -> l { focus = Just (focusOf false a) }
  Leave -> modifyLoaded \l -> case l.focus of
    Just f | f.pinned -> l
    _ -> l { focus = Nothing }
  Tap a -> modifyLoaded \l -> case l.focus of
    Just f | f.pinned && f.name == nameOf a -> l { focus = Nothing }
    _ -> l { focus = Just (focusOf true a) }
  where
  relayout width input = case _ of
    Ready l -> Ready (l { geometry = geometryFor width input l.timeline, focus = Nothing })
    s -> s
  modifyLoaded f = H.modify_ \st -> case st.status of
    Ready l -> st { status = Ready (f l) }
    s -> st { status = s }
  focusOf pinned a = { name: nameOf a, umbrella: a.umbrella, pinned }
  setOptions f = do
    st <- H.get
    H.raise (SetOptions (f st.input.options))
  toggle :: forall a. Ord a => a -> Set.Set a -> Set.Set a
  toggle x set = if Set.member x set then Set.delete x set else Set.insert x set

nameOf :: Annotation -> ThreadName
nameOf a = let Thread t = a.thread in t.name

-- Strand textures ------------------------------------------------------------------
--
-- Colour plus a texture per strand, so strands stay distinct in greyscale.

strandPatternId :: UmbrellaId -> String
strandPatternId u = "ln-strand-" <> show (unwrap u)

strandFill :: UmbrellaId -> String
strandFill u =
  let c = Palette.umbrellaColor u
  in if c == Palette.red || c == Palette.black then c else "url(#" <> strandPatternId u <> ")"

strandPattern :: forall w i. UmbrellaId -> Array (HH.HTML w i)
strandPattern u =
  let
    c = Palette.umbrellaColor u
    pat size rot kids =
      [ svgEl "pattern"
          ( [ attr "id" (strandPatternId u), attr "patternUnits" "userSpaceOnUse"
            , attr "width" (num size), attr "height" (num size)
            ] <> (if rot then [ attr "patternTransform" "rotate(45)" ] else [])
          )
          ([ rectAt 0.0 0.0 size size [ attr "fill" c ] ] <> kids)
      ]
  in
    if c == Palette.yellow then pat 4.0 false [ svgEl "circle" [ attr "cx" "2", attr "cy" "2", attr "r" "0.85", attr "fill" Palette.ink ] [] ]
    else if c == Palette.blue then pat 4.0 true [ lineAt 0.0 0.0 0.0 4.0 [ attr "stroke" Palette.cream, attr "stroke-width" "1.1" ] ]
    else if c == Palette.white then pat 3.0 false [ lineAt 0.0 1.5 3.0 1.5 [ attr "stroke" Palette.ink, attr "stroke-width" "0.6" ] ]
    else []

-- A strand swatch for chips and legends.
strandSwatch :: forall w i. UmbrellaId -> HH.HTML w i
strandSwatch u =
  svgEl "svg" [ attr "viewBox" "0 0 14 14", attr "width" "14", attr "height" "14", attr "aria-hidden" "true" ]
    [ svgEl "defs" [] (strandPattern u)
    , rectAt 0.5 0.5 13.0 13.0 [ attr "fill" (strandFill u), attr "stroke" Palette.ink, attr "stroke-width" "1" ]
    ]

-- Anchor marks: where on the strand an annotation's moment falls.
anchorMark :: forall w i. Moment -> Number -> Number -> Array (HH.HTML w i)
anchorMark m x y = case m of
  Started -> [ circle 3.2 Palette.cream ]
  ShippedAt -> [ circle 3.2 Palette.ink ]
  WentQuiet ->
    [ circle 4.2 Palette.cream
    , lineAt (x - 2.6) (y - 2.6) (x + 2.6) (y + 2.6) inkLine
    , lineAt (x - 2.6) (y + 2.6) (x + 2.6) (y - 2.6) inkLine
    ]
  SupersededAt ->
    [ pathD ("M" <> num (x - 3.0) <> " " <> num (y - 3.5) <> "L" <> num (x + 3.5) <> " " <> num y <> "L" <> num (x - 3.0) <> " " <> num (y + 3.5) <> "Z")
        [ attr "fill" Palette.ink, attr "stroke" Palette.cream, attr "stroke-width" "0.8" ]
    ]
  ParkedAt ->
    [ rectAt (x - 3.5) (y - 3.5) 7.0 7.0 [ attr "fill" Palette.cream, attr "stroke" Palette.ink, attr "stroke-width" "0.9" ]
    , lineAt (x - 1.2) (y - 2.2) (x - 1.2) (y + 2.2) inkLine
    , lineAt (x + 1.2) (y - 2.2) (x + 1.2) (y + 2.2) inkLine
    ]
  where
  circle r fill = svgEl "circle" [ attr "cx" (num x), attr "cy" (num y), attr "r" (num r), attr "fill" fill, attr "stroke" Palette.ink, attr "stroke-width" "1.2" ] []
  inkLine = [ attr "stroke" Palette.ink, attr "stroke-width" "1.3" ]

-- Render ----------------------------------------------------------------------------

render :: forall m. State -> H.ComponentHTML Action () m
render st = case st.status of
  Loading -> HH.div [ HP.class_ (ClassName "mm-msg") ] [ HH.text "Loading the line..." ]
  Failed e -> HH.div [ HP.class_ (ClassName "mm-msg mm-err") ] [ HH.text ("Could not load the timeline: " <> e) ]
  Ready l ->
    let
      Timeline tl = l.timeline
      g = l.geometry
      selected = st.input.selected
      opts = st.input.options
    in
      HH.div [ HP.class_ (ClassName "ln") ]
        [ HH.div [ HP.class_ (ClassName "ln-bar") ]
            [ HH.div [ HP.class_ (ClassName "ln-chips") ] (map (chip selected) tl.umbrellas)
            , HH.div [ HP.class_ (ClassName "ln-controls") ]
                [ HH.div [ HP.class_ (ClassName "ln-group"), HPA.label "Show cards for" ]
                    (map (fateChip opts) allGroups)
                , HH.div [ HP.class_ (ClassName "ln-group ln-seg"), HPA.label "Minimum active days" ]
                    ( [ HH.span [ HP.class_ (ClassName "ln-group-label") ] [ HH.text "days \x2265" ] ]
                        <> map (densityButton opts) densitySteps
                    )
                , HH.div [ HP.class_ (ClassName "ln-group ln-seg") ]
                    [ HH.button [ HP.class_ (ClassName "mm-btn"), HE.onClick \_ -> FoldAll ] [ HH.text "fold all" ]
                    , HH.button [ HP.class_ (ClassName "mm-btn"), HE.onClick \_ -> UnfoldAll ] [ HH.text "unfold all" ]
                    ]
                ]
            , HH.ul [ HP.class_ (ClassName "ln-legend") ]
                ( map anchorLegend [ Started, ShippedAt, WentQuiet, SupersededAt, ParkedAt ]
                    <> [ HH.li [ HP.class_ (ClassName "ln-rule") ]
                           [ HH.text
                               ( show (length g.cards) <> " notes: every half-done or abandoned thread, and any other with "
                                   <> show opts.minDays
                                   <> "+ active days. A strand's width is how many of its threads were worked on that week; click a month to fold it."
                               )
                           ]
                       ]
                )
            ]
        , HH.div
            [ HP.class_ (ClassName ("ln-chart " <> modeClass g.mode))
            , HP.style ("height:" <> show g.height <> "px")
            ]
            ([ ribbonSvg g selected l.focus ] <> map (cardHtml l.focus) g.cards)
        ]

fateChip :: forall w. LineOptions -> FateGroup -> HH.HTML w Action
fateChip opts grp =
  let on = Set.member grp opts.fates
  in
    HH.button
      [ HP.class_ (ClassName ("mm-btn" <> if on then " on" else ""))
      , HPA.pressed (if on then "true" else "false")
      , HE.onClick \_ -> ToggleFate grp
      ]
      [ HH.text (groupLabel grp) ]

densityButton :: forall w. LineOptions -> Int -> HH.HTML w Action
densityButton opts n =
  let on = opts.minDays == n
  in
    HH.button
      [ HP.class_ (ClassName ("mm-btn" <> if on then " on" else ""))
      , HPA.pressed (if on then "true" else "false")
      , HE.onClick \_ -> SetDensity n
      ]
      [ HH.text (show n) ]

modeClass :: Mode -> String
modeClass = case _ of
  Wide -> "is-wide"
  Narrow -> "is-narrow"

chip :: forall w. Maybe UmbrellaId -> Umbrella -> HH.HTML w Action
chip selected u =
  let on = selected == Just u.id
  in
    HH.button
      [ HP.class_ (ClassName ("mm-btn ln-chip" <> if on then " on" else ""))
      , HPA.pressed (if on then "true" else "false")
      , HE.onClick \_ -> Select (if on then Nothing else Just u.id)
      ]
      [ strandSwatch u.id, HH.text u.name ]

anchorLegend :: forall w i. Moment -> HH.HTML w i
anchorLegend m =
  HH.li_
    [ svgEl "svg" [ attr "viewBox" "0 0 12 12", attr "width" "12", attr "height" "12", attr "aria-hidden" "true" ] (anchorMark m 6.0 6.0)
    , HH.text (momentLabel m)
    ]

ribbonSvg :: forall m. Geometry -> Maybe UmbrellaId -> Maybe Focus -> H.ComponentHTML Action () m
ribbonSvg g selected focus =
  let
    lit u = case focus of
      Just f -> f.umbrella == u
      Nothing -> case selected of
        Just s -> s == u
        Nothing -> true
    bands = concatMap (foldBand g) g.segments
    months = concatMap (monthHeader g) g.segments
    strandEls = concatMap (strandEl lit) g.strands
    heads = map (strandHead selected) g.strands
    focusCard = focus >>= \f -> find (\c -> nameOf c.annotation == f.name) g.cards
    leaders = map (leaderEl focus) g.cards
    anchors = concatMap (\c -> anchorMark c.annotation.moment c.anchorX c.anchorY) g.cards
    -- the focused thread's span on its strand, with its active days as ticks
    spanEls = case focusCard of
      Just c ->
        [ rectAt (c.anchorX - 1.5) c.spanTop 3.0 (max 2.0 (c.spanBottom - c.spanTop)) [ attr "fill" Palette.ink ] ]
          <> map (\y -> lineAt (c.anchorX - 7.0) y (c.anchorX + 7.0) y [ attr "stroke" Palette.ink, attr "stroke-width" "1.2" ]) c.dayYs
      Nothing -> []
  in
    svgEl "svg"
      [ attr "class" "ln-svg"
      , attr "width" (num g.width)
      , attr "height" (num g.height)
      , attr "viewBox" ("0 0 " <> num g.width <> " " <> num g.height)
      , attr "role" "img"
      , attr "aria-label" "A line of time, October 2025 at the top to October 2026, with a strand per umbrella"
      ]
      ( [ svgEl "defs" [] (concatMap (\s -> strandPattern s.umbrella.id) g.strands <> [ hatchDef "ln" Palette.blue ]) ]
          <> bands
          <> strandEls
          <> heads
          <> leaders
          <> spanEls
          <> anchors
          <> months
      )

-- A folded month: a faint band across the page behind its compressed ribbon.
foldBand :: forall w i. Geometry -> Segment -> Array (HH.HTML w i)
foldBand g seg =
  if seg.folded then [ rectAt 0.0 seg.y0 g.width (seg.y1 - seg.y0) [ attr "fill" Palette.ink, attr "fill-opacity" "0.045" ] ]
  else []

-- A month's header: its rule across the ribbon and its label in the gutter, which
-- folds or unfolds it. A folded month also says what it holds.
monthHeader :: forall m. Geometry -> Segment -> Array (H.ComponentHTML Action () m)
monthHeader g seg =
  let
    labelX = g.ribbonLeft - 8.0
    glyph = if seg.folded then "\x25B8 " else "\x25BE "
    summary = case g.mode of
      Wide ->
        seg.longLabel <> " \x00b7 " <> show seg.threads <> (if seg.threads == 1 then " thread" else " threads")
          <> (if seg.quiet > 0 then " (" <> show seg.quiet <> " went quiet)" else "")
      Narrow ->
        show seg.threads <> " threads" <> (if seg.quiet > 0 then ", " <> show seg.quiet <> " went quiet" else "")
    hitW = g.ribbonLeft
  in
    [ lineAt (g.ribbonLeft - 4.0) seg.y0 (g.ribbonRight + 4.0) seg.y0
        [ attr "stroke" Palette.ink, attr "stroke-width" (if seg.yearStart then "1.4" else "0.6"), attr "stroke-opacity" (if seg.yearStart then "0.9" else "0.45") ]
    , svgEl "g"
        [ attr "class" "ln-month", attr "role" "button", attr "tabindex" "0"
        , attr "aria-expanded" (if seg.folded then "false" else "true")
        , attr "aria-label" ((if seg.folded then "Unfold " else "Fold ") <> seg.longLabel)
        , HE.onClick \_ -> ToggleMonth seg.key
        ]
        ( [ rectAt (max 0.0 (labelX - 80.0)) (seg.y0 - 10.0) (min 80.0 hitW) 22.0 [ attr "fill" "transparent" ]
          , textAt labelX (seg.y0 + 4.0) (glyph <> seg.label)
              [ attr "fill" Palette.ink, attr "font-size" (if g.mode == Narrow then "10" else "11"), attr "text-anchor" "end"
              , attr "font-weight" (if seg.yearStart then "600" else "400"), attr "letter-spacing" "0.03em"
              ]
          ]
            <>
              if seg.folded then
                [ textAt (g.ribbonRight + 14.0) ((seg.y0 + seg.y1) / 2.0 + 4.0) summary
                    [ attr "fill" Palette.ink, attr "fill-opacity" "0.8", attr "font-size" "12" ]
                ]
              else []
        )
    ]

strandEl :: forall w i. (UmbrellaId -> Boolean) -> Strand -> Array (HH.HTML w i)
strandEl lit s =
  [ svgEl "g" [ attr "opacity" (if lit s.umbrella.id then "1" else "0.22") ]
      [ pathD s.outline [ attr "fill" (strandFill s.umbrella.id), attr "stroke" Palette.ink, attr "stroke-width" "0.7", attr "stroke-linejoin" "round" ]
      ]
  ]

strandHead :: forall m. Maybe UmbrellaId -> Strand -> H.ComponentHTML Action () m
strandHead selected s =
  let
    u = s.umbrella.id
    on = selected == Just u
    w = 2.0 * s.halfMax
  in
    svgEl "g"
      [ attr "class" "ln-head", attr "role" "button", attr "tabindex" "0"
      , attr "aria-label" (s.umbrella.name <> if on then " (selected)" else "")
      , attr "aria-pressed" (if on then "true" else "false")
      , HE.onClick \_ -> Select (if on then Nothing else Just u)
      ]
      [ svgEl "title" [] [ HH.text s.umbrella.name ]
      , rectAt (s.cx - w / 2.0) 6.0 w 18.0
          [ attr "fill" (strandFill u), attr "stroke" Palette.ink, attr "stroke-width" (if on then "2.5" else "1") ]
      ]

leaderEl :: forall w i. Maybe Focus -> Card -> HH.HTML w i
leaderEl focus c =
  let
    on = case focus of
      Just f -> f.name == nameOf c.annotation
      Nothing -> false
  in
    pathD c.leader
      [ attr "fill" "none", attr "stroke" Palette.ink
      , attr "stroke-width" (if on then "1.8" else "0.7")
      , attr "stroke-opacity" (if on then "1" else "0.5")
      ]

kicker :: Annotation -> String
kicker a =
  let Thread t = a.thread
  in
    toUpper case a.moment of
      WentQuiet -> "went quiet \x00b7 " <> fateLabel t.fate
      Started -> if t.fate == Active then "started \x00b7 still active" else "started"
      SupersededAt -> case t.successor of
        Just s -> "superseded \x2192 " <> unwrap s
        Nothing -> "superseded"
      m -> momentLabel m

cardHtml :: forall m. Maybe Focus -> Card -> H.ComponentHTML Action () m
cardHtml focus c =
  let
    a = c.annotation
    Thread t = a.thread
    on = case focus of
      Just f -> f.name == t.name
      Nothing -> false
    colour = Palette.umbrellaColor a.umbrella
    thumb = case t.image, t.cover of
      Just src, _ -> Just src
      Nothing, Just src -> Just src
      Nothing, Nothing -> Nothing
    nDays = length t.days
    unfinishedFate = t.fate == HalfDone || t.fate == Abandoned
    sideClass = if c.x < c.anchorX then " is-left" else " is-right"
  in
    HH.div
      [ HP.class_ (ClassName ("ln-card" <> sideClass <> (if on then " is-focus" else "") <> (if unfinishedFate then " is-unfinished" else "")))
      , HP.style
          ( "left:" <> show c.x <> "px;top:" <> show c.y <> "px;width:" <> show c.w <> "px;height:" <> show (cardHeight a) <> "px;"
              <> "--strand:" <> (if colour == Palette.white then Palette.ink else colour)
          )
      , HE.onMouseEnter \_ -> Enter a
      , HE.onMouseLeave \_ -> Leave
      , HE.onClick \_ -> Tap a
      ]
      ( [ HH.div [ HP.class_ (ClassName "ln-kicker") ]
            [ svgEl "svg" [ attr "viewBox" "0 0 24 12", attr "width" "24", attr "height" "12", attr "aria-hidden" "true" ]
                ([ svgEl "defs" [] [ hatchDef ("ln-" <> show (unwrap a.umbrella)) colour ] ] <> fateMark ("ln-" <> show (unwrap a.umbrella)) colour t.fate 1.0 16.0 2.0)
            , HH.span [ HP.class_ (ClassName "ln-moment") ] [ HH.text (kicker a) ]
            , HH.span [ HP.class_ (ClassName "ln-date") ]
                [ HH.text (fmtDate a.date)
                , HH.span [ HP.class_ (ClassName "ln-days") ] [ HH.text (" \x00b7 " <> show nDays <> (if nDays == 1 then " day" else " days")) ]
                ]
            ]
        , HH.div [ HP.class_ (ClassName "ln-main") ]
            ( ( case thumb of
                  Just src -> [ HH.img [ HP.src src, HP.alt "", HP.class_ (ClassName "ln-thumb") ] ]
                  Nothing -> []
              )
                <>
                  [ HH.div [ HP.class_ (ClassName "ln-text") ]
                      [ HH.div [ HP.class_ (ClassName "ln-name") ] [ HH.text (unwrap t.name) ]
                      , HH.div [ HP.class_ (ClassName "ln-what") ] [ HH.text t.what ]
                      ]
                  ]
            )
        ]
          <> case t.note of
            Just n -> [ HH.div [ HP.class_ (ClassName "ln-note") ] [ HH.text n ] ]
            Nothing -> []
      )
