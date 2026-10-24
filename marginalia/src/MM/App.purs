-- | The shell: one page, two views of the same umbrellas. The map shows WHERE the
-- | effort went; the timeline shows WHEN. The shell owns the two things they share,
-- | which view is showing and which umbrella is selected, and keeps both in the URL
-- | hash so a view can be linked to:
-- |
-- |   #map   #timeline   #line   #git   and /<umbrella id> on any of them (#timeline/125),
-- |   plus the Line view's folds and filters after a ? (#line/125?fold=2025-10&min=12)
-- |
-- | Four views: Map (where), Timeline (when, as swimlanes), Line (one line of time
-- | down the page with annotations), Git (the commits and lines, as a calendar).
-- |
-- | All views stay mounted (the hidden one is `display:none`), so switching keeps
-- | the timeline's scroll position and neither refetches its data.
module MM.App (component) where

import Prelude

import Data.Array (drop, index)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (unwrap)
import Data.String (Pattern(..), split, stripPrefix)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HPA
import Halogen.Query.Event (eventListener)
import MM.Component as MapView
import MM.Git.Component as GitView
import MM.Line.Component as LineView
import MM.Line.Options (LineOptions, defaultOptions, parseOptions, printOptions)
import MM.Timeline.Component as TimelineView
import MM.Timeline.Types (UmbrellaId(..))
import Type.Proxy (Proxy(..))
import Web.HTML (window)
import Web.HTML.Event.HashChangeEvent.EventTypes (hashchange)
import Web.HTML.Location (hash, setHash)
import Web.HTML.Window (location, toEventTarget)

foreign import readTimelineUrl :: Effect String
foreign import readGitUrl :: Effect String

data View = MapView | TimelineView | LineView | GitView

derive instance Eq View

-- | `line` is the Line view's fold/filter state, after a `?` in the hash.
type Route = { view :: View, selected :: Maybe UmbrellaId, line :: LineOptions }

type State = { route :: Route, timelineUrl :: String, gitUrl :: String }

data Action
  = Initialize
  | HashChanged
  | Show View
  | Select (Maybe UmbrellaId)
  | SetLine LineOptions

type Slots =
  ( map :: forall q. H.Slot q MapView.Output Unit
  , timeline :: forall q. H.Slot q TimelineView.Output Unit
  , line :: forall q. H.Slot q LineView.Output Unit
  , git :: forall q. H.Slot q GitView.Output Unit
  )

component :: forall q i o m. MonadAff m => H.Component q i o m
component =
  H.mkComponent
    { initialState: const { route: { view: MapView, selected: Nothing, line: defaultOptions }, timelineUrl: "timeline-public.json", gitUrl: "git-activity.json" }
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }

-- Routing -----------------------------------------------------------------------

parseRoute :: String -> Route
parseRoute h =
  let
    full = fromMaybe h (stripPrefix (Pattern "#") h)
    pathAndQuery = split (Pattern "?") full
    body = fromMaybe "" (index pathAndQuery 0)
    line = maybe defaultOptions parseOptions (index pathAndQuery 1)
    parts = split (Pattern "/") body
    view = case index parts 0 of
      Just "timeline" -> TimelineView
      Just "line" -> LineView
      Just "git" -> GitView
      _ -> MapView
    selected = UmbrellaId <$> (index (drop 1 parts) 0 >>= fromString)
  in
    { view, selected, line }

printRoute :: Route -> String
printRoute r =
  (case r.view of
    MapView -> "map"
    TimelineView -> "timeline"
    LineView -> "line"
    GitView -> "git")
    <> (case r.selected of
      Just u -> "/" <> show (unwrap u)
      Nothing -> "")
    <> case printOptions r.line of
      "" -> ""
      q -> "?" <> q

readRoute :: Effect Route
readRoute = parseRoute <$> (window >>= location >>= hash)

writeRoute :: Route -> Effect Unit
writeRoute r = do
  loc <- window >>= location
  current <- hash loc
  let next = printRoute r
  when (fromMaybe current (stripPrefix (Pattern "#") current) /= next) (setHash next loc)

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action Slots o m Unit
handleAction = case _ of
  Initialize -> do
    url <- liftEffect readTimelineUrl
    gitUrl <- liftEffect readGitUrl
    route <- liftEffect readRoute
    H.modify_ _ { route = route, timelineUrl = url, gitUrl = gitUrl }
    w <- liftEffect window
    void $ H.subscribe (eventListener hashchange (toEventTarget w) (const (Just HashChanged)))
  HashChanged -> do
    route <- liftEffect readRoute
    H.modify_ _ { route = route }
  Show v -> go \r -> r { view = v }
  Select u -> go \r -> r { selected = u }
  SetLine o -> go \r -> r { line = o }
  where
  go f = do
    st <- H.modify \s -> s { route = f s.route }
    liftEffect (writeRoute st.route)

-- Render ------------------------------------------------------------------------

render :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
render st =
  let
    r = st.route
    pane v content =
      HH.div [ HP.class_ (ClassName ("mm-pane mm-pane-" <> viewKey v <> if r.view == v then "" else " is-hidden")) ] [ content ]
    title = case r.view of
      MapView -> "What I've been working on\x2026"
      TimelineView -> "\x2026" <> "and when."
      LineView -> "\x2026" <> "and as it happened."
      GitView -> "\x2026" <> "and the code."
    sub = case r.view of
      MapView -> "A handful of long-running efforts. Each block is one umbrella; its size echoes how many sub-projects it holds. Select one to follow it into the timeline."
      TimelineView -> "A year of work with Claude Code, from 14 October 2025 to October 2026: every thread, including the ones that didn't finish."
      LineView -> "One line of time, oldest at the top. Each umbrella is a strand that swells in the weeks it was busy; the notes mark where threads began, shipped, or went quiet."
      GitView -> "Every commit since 14 October 2025, day by day, and the lines of code and prose they changed, week by week. The ideas and the direction were mine; Claude Code wrote nearly all of the lines."
  in
    HH.div [ HP.class_ (ClassName "mm-app") ]
      [ HH.header [ HP.class_ (ClassName "mm-head") ]
          [ HH.div [ HP.class_ (ClassName "mm-head-text") ]
              [ HH.h1 [ HP.class_ (ClassName "mm-title") ]
                  [ HH.text title ]
              , HH.p [ HP.class_ (ClassName "mm-sub") ]
                  [ HH.text sub ]
              ]
          , HH.nav [ HP.class_ (ClassName "mm-switch"), HPA.label "View" ]
              [ switchButton "Map" MapView r.view
              , switchButton "Timeline" TimelineView r.view
              , switchButton "Line" LineView r.view
              , switchButton "Git" GitView r.view
              ]
          ]
      , HH.main [ HP.class_ (ClassName "mm-main") ]
          [ pane MapView (HH.slot (Proxy :: _ "map") unit MapView.component { selected: r.selected } fromMap)
          , pane TimelineView
              (HH.slot (Proxy :: _ "timeline") unit TimelineView.component { selected: r.selected, dataUrl: st.timelineUrl } fromTimeline)
          , pane LineView
              (HH.slot (Proxy :: _ "line") unit LineView.component { selected: r.selected, options: r.line, dataUrl: st.timelineUrl } fromLine)
          , pane GitView
              (HH.slot (Proxy :: _ "git") unit GitView.component { selected: r.selected, dataUrl: st.gitUrl } fromGit)
          ]
      ]
  where
  fromMap (MapView.SelectUmbrella u) = Select u
  fromTimeline (TimelineView.SelectUmbrella u) = Select u
  fromLine (LineView.SelectUmbrella u) = Select u
  fromLine (LineView.SetOptions o) = SetLine o
  fromGit (GitView.SelectUmbrella u) = Select u

viewKey :: View -> String
viewKey = case _ of
  MapView -> "map"
  TimelineView -> "timeline"
  LineView -> "line"
  GitView -> "git"

switchButton :: forall w. String -> View -> View -> HH.HTML w Action
switchButton label v current =
  HH.button
    [ HP.class_ (ClassName ("mm-btn" <> if v == current then " on" else ""))
    , HPA.pressed (if v == current then "true" else "false")
    , HE.onClick \_ -> Show v
    ]
    [ HH.text label ]
