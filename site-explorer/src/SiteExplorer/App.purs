module SiteExplorer.App where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import SiteExplorer.Types (State, RouteInfo, RouteStatus(..), RouteFilter(..))
import SiteExplorer.ForceGraph as ForceGraph

-- | Input to the component
type Input =
  { routes :: Array RouteInfo
  , baseUrl :: String
  }

-- | Actions for the app
data Action
  = Initialize
  | SelectRoute String
  | ClearSelection
  | SetFilter RouteFilter

-- | The main app component
component :: forall q o m. MonadAff m => H.Component q Input o m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }

initialState :: Input -> State
initialState input =
  { routes: input.routes
  , selectedRoute: Nothing
  , filter: ShowAll
  , baseUrl: input.baseUrl
  }

-- | Rose Adler palette colors
colors ::
  { reachable :: String
  , unreachable :: String
  , extra :: String
  , archived :: String
  }
colors =
  { reachable: "#c9a227"    -- Gold
  , unreachable: "#722f37"  -- Burgundy
  , extra: "#d4a574"        -- Blush/coral
  , archived: "#8a7f72"     -- Taupe
  }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div
    [ HP.style "display: flex; height: 100vh; overflow: hidden;" ]
    [ -- Left: Graph container (force graph renders here via FFI)
      HH.div [ HP.id "graph" ] []
    , -- Right: Sidebar
      HH.div [ HP.id "sidebar" ]
        [ HH.h1_ [ HH.text "Site Explorer" ]
        , renderSummary state
        , renderLegend
        , renderDetails state
        ]
    ]

renderSummary :: forall m. State -> H.ComponentHTML Action () m
renderSummary state =
  let
    reachable = Array.length $ Array.filter (\r -> r.status == Reachable) state.routes
    unreachable = Array.length $ Array.filter (\r -> r.status == Unreachable) state.routes
    extra = Array.length $ Array.filter (\r -> r.status == Extra) state.routes
    total = Array.length state.routes
  in
  HH.div_
    [ HH.h2_ [ HH.text "Summary" ]
    , renderStat "Total routes" (show total)
    , renderStat "Reachable" (show reachable)
    , renderStat "Unreachable" (show unreachable)
    , renderStat "Extra (undocumented)" (show extra)
    ]

renderStat :: forall m. String -> String -> H.ComponentHTML Action () m
renderStat label value =
  HH.div
    [ HP.class_ $ HH.ClassName "stat" ]
    [ HH.text label
    , HH.span [ HP.class_ $ HH.ClassName "stat-value" ] [ HH.text value ]
    ]

renderLegend :: forall m. H.ComponentHTML Action () m
renderLegend =
  HH.div
    [ HP.class_ $ HH.ClassName "legend" ]
    [ HH.h2_ [ HH.text "Legend" ]
    , renderLegendItem colors.reachable "Reachable & Defined"
    , renderLegendItem colors.unreachable "Defined but Unreachable"
    , renderLegendItem colors.extra "Discovered (not defined)"
    , renderLegendItem colors.archived "Archived"
    ]

renderLegendItem :: forall m. String -> String -> H.ComponentHTML Action () m
renderLegendItem color label =
  HH.div
    [ HP.class_ $ HH.ClassName "legend-item" ]
    [ HH.div
        [ HP.class_ $ HH.ClassName "legend-dot"
        , HP.style $ "background: " <> color
        ]
        []
    , HH.text label
    ]

renderDetails :: forall m. State -> H.ComponentHTML Action () m
renderDetails state =
  HH.div
    [ HP.id "details" ]
    case state.selectedRoute of
      Nothing ->
        [ HH.h2_ [ HH.text "Details" ]
        , HH.p
            [ HP.style "color: #94a3b8; font-size: 0.85rem;" ]
            [ HH.text "Click a node to see details" ]
        ]
      Just routeName ->
        case Array.find (\r -> r.name == routeName) state.routes of
          Nothing ->
            [ HH.h2_ [ HH.text "Details" ]
            , HH.text "Route not found"
            ]
          Just route ->
            [ HH.h2_ [ HH.text "Details" ]
            , renderRouteDetail state.baseUrl route
            ]

renderRouteDetail :: forall m. String -> RouteInfo -> H.ComponentHTML Action () m
renderRouteDetail baseUrl route =
  let
    fullUrl = baseUrl <> "/#" <> route.urlPath
    statusColor = case route.status of
      Reachable -> colors.reachable
      Unreachable -> colors.unreachable
      Extra -> colors.extra
      Archived -> colors.archived
  in
  HH.div_
    [ renderField "Route Name" (HH.text route.name)
    , renderField "URL Pattern"
        (HH.a
          [ HP.href fullUrl
          , HP.target "_blank"
          ]
          [ HH.text $ route.urlPath <> " ↗" ])
    , renderField "Module" (HH.text route.componentModule)
    , renderField "Status"
        (HH.span
          [ HP.style $ "color: " <> statusColor <> "; font-weight: 600;" ]
          [ HH.text $ statusLabel route.status ])
    , renderField "Discovery Depth"
        (HH.text $ fromMaybe "Not discovered" (map show route.depth))
    , renderField "Found From"
        (HH.text $ fromMaybe "Entry point / Not discovered" route.foundFrom)
    ]

renderField :: forall m. String -> H.ComponentHTML Action () m -> H.ComponentHTML Action () m
renderField label value =
  HH.div
    [ HP.class_ $ HH.ClassName "field" ]
    [ HH.div [ HP.class_ $ HH.ClassName "label" ] [ HH.text label ]
    , HH.div [ HP.class_ $ HH.ClassName "value" ] [ value ]
    ]

statusLabel :: RouteStatus -> String
statusLabel = case _ of
  Reachable -> "REACHABLE"
  Unreachable -> "UNREACHABLE"
  Extra -> "EXTRA (NOT DEFINED)"
  Archived -> "ARCHIVED"

-- | Handle actions
handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> do
    state <- H.get

    -- Create subscription for HATS -> Halogen communication
    { emitter, listener } <- liftEffect HS.create

    -- Initialize force graph after mount
    handle <- liftEffect $ ForceGraph.initForceGraph "#graph" state.routes

    -- Wire up node clicks to emit SelectRoute actions
    liftEffect $ handle.onNodeClick \routeName ->
      HS.notify listener (SelectRoute routeName)

    -- Subscribe to the emitter so we receive the actions
    void $ H.subscribe emitter

  SelectRoute name ->
    H.modify_ _ { selectedRoute = Just name }

  ClearSelection ->
    H.modify_ _ { selectedRoute = Nothing }

  SetFilter filter ->
    H.modify_ _ { filter = filter }
