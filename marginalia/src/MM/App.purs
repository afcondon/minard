-- | The shell: one page, two views of the same umbrellas. The map shows WHERE the
-- | effort went; the timeline shows WHEN. The shell owns the two things they share,
-- | which view is showing and which umbrella is selected, and keeps both in the URL
-- | hash so a view can be linked to:
-- |
-- |   #map            #timeline            #timeline/125   (125 = Hylograph's id)
-- |
-- | Both views stay mounted (the hidden one is `display:none`), so switching keeps
-- | the timeline's scroll position and neither refetches its data.
module MM.App (component) where

import Prelude

import Data.Array (drop, index)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe)
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
import MM.Timeline.Component as TimelineView
import MM.Timeline.Types (UmbrellaId(..))
import Type.Proxy (Proxy(..))
import Web.HTML (window)
import Web.HTML.Event.HashChangeEvent.EventTypes (hashchange)
import Web.HTML.Location (hash, setHash)
import Web.HTML.Window (location, toEventTarget)

foreign import readTimelineUrl :: Effect String

data View = MapView | TimelineView

derive instance Eq View

type Route = { view :: View, selected :: Maybe UmbrellaId }

type State = { route :: Route, timelineUrl :: String }

data Action
  = Initialize
  | HashChanged
  | Show View
  | Select (Maybe UmbrellaId)

type Slots =
  ( map :: forall q. H.Slot q MapView.Output Unit
  , timeline :: forall q. H.Slot q TimelineView.Output Unit
  )

component :: forall q i o m. MonadAff m => H.Component q i o m
component =
  H.mkComponent
    { initialState: const { route: { view: MapView, selected: Nothing }, timelineUrl: "timeline-public.json" }
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
    body = fromMaybe h (stripPrefix (Pattern "#") h)
    parts = split (Pattern "/") body
    view = case index parts 0 of
      Just "timeline" -> TimelineView
      _ -> MapView
    selected = UmbrellaId <$> (index (drop 1 parts) 0 >>= fromString)
  in
    { view, selected }

printRoute :: Route -> String
printRoute r =
  (case r.view of
    MapView -> "map"
    TimelineView -> "timeline")
    <> case r.selected of
      Just u -> "/" <> show (unwrap u)
      Nothing -> ""

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
    route <- liftEffect readRoute
    H.modify_ _ { route = route, timelineUrl = url }
    w <- liftEffect window
    void $ H.subscribe (eventListener hashchange (toEventTarget w) (const (Just HashChanged)))
  HashChanged -> do
    route <- liftEffect readRoute
    H.modify_ _ { route = route }
  Show v -> go \r -> r { view = v }
  Select u -> go \r -> r { selected = u }
  where
  go f = do
    st <- H.modify \s -> s { route = f s.route }
    liftEffect (writeRoute st.route)

-- Render ------------------------------------------------------------------------

render :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
render st =
  let
    r = st.route
    isMap = r.view == MapView
    pane v content =
      HH.div [ HP.class_ (ClassName ("mm-pane" <> if r.view == v then "" else " is-hidden")) ] [ content ]
  in
    HH.div [ HP.class_ (ClassName "mm-app") ]
      [ HH.header [ HP.class_ (ClassName "mm-head") ]
          [ HH.div [ HP.class_ (ClassName "mm-head-text") ]
              [ HH.h1 [ HP.class_ (ClassName "mm-title") ]
                  [ HH.text (if isMap then "What I've been working on\x2026" else "\x2026" <> "and when.") ]
              , HH.p [ HP.class_ (ClassName "mm-sub") ]
                  [ HH.text
                      ( if isMap then "A handful of long-running efforts. Each block is one umbrella; its size echoes how many sub-projects it holds. Select one to follow it into the timeline."
                        else "Thirteen months of work with Claude Code, September 2025 to October 2026: every thread, including the ones that didn't finish."
                      )
                  ]
              ]
          , HH.nav [ HP.class_ (ClassName "mm-switch"), HPA.label "View" ]
              [ switchButton "Map" MapView r.view
              , switchButton "Timeline" TimelineView r.view
              ]
          ]
      , HH.main [ HP.class_ (ClassName "mm-main") ]
          [ pane MapView (HH.slot (Proxy :: _ "map") unit MapView.component { selected: r.selected } fromMap)
          , pane TimelineView
              (HH.slot (Proxy :: _ "timeline") unit TimelineView.component { selected: r.selected, dataUrl: st.timelineUrl } fromTimeline)
          ]
      ]
  where
  fromMap (MapView.SelectUmbrella u) = Select u
  fromTimeline (TimelineView.SelectUmbrella u) = Select u

switchButton :: forall w. String -> View -> View -> HH.HTML w Action
switchButton label v current =
  HH.button
    [ HP.class_ (ClassName ("mm-btn" <> if v == current then " on" else ""))
    , HPA.pressed (if v == current then "true" else "false")
    , HE.onClick \_ -> Show v
    ]
    [ HH.text label ]
