module SiteExplorer.Types where

import Prelude
import Data.Maybe (Maybe(..))

-- | Route status based on comparison
data RouteStatus
  = Reachable      -- Found in both static and spidered
  | Unreachable    -- Defined but not spidered
  | Extra          -- Spidered but not defined
  | Archived       -- Marked as archived

derive instance eqRouteStatus :: Eq RouteStatus

-- | A route with its analysis data
type RouteInfo =
  { name :: String
  , urlPath :: String
  , componentModule :: String
  , status :: RouteStatus
  , depth :: Maybe Int        -- Spider depth (if discovered)
  , foundFrom :: Maybe String -- Where it was linked from
  }

-- | Application state
type State =
  { routes :: Array RouteInfo
  , selectedRoute :: Maybe String
  , filter :: RouteFilter
  , baseUrl :: String
  }

-- | Filter for route display
data RouteFilter
  = ShowAll
  | ShowReachable
  | ShowUnreachable
  | ShowExtra

derive instance eqRouteFilter :: Eq RouteFilter

-- | Initial state
initialState :: State
initialState =
  { routes: []
  , selectedRoute: Nothing
  , filter: ShowAll
  , baseUrl: ""
  }
