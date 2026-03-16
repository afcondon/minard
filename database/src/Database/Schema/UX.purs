-- | Database.Schema.UX
-- |
-- | Typed row definitions for Pausanias UX tables.
-- | Shared by pausanias-analyzer, pausanias-spider, and pausanias-server.
-- | The single source of truth for what the UX schema looks like in PureScript.
-- |
-- | If you change a column, both analyzer and spider will fail to compile
-- | until they're updated to match.
module Database.Schema.UX
  ( RouteRow
  , PageComponentRow
  , AffordanceRow
  , NavEdgeRow
  , RouteInsert
  , PageComponentInsert
  , AffordanceInsert
  , NavEdgeInsert
  ) where

import Data.Maybe (Maybe)

-- =============================================================================
-- Full row types (what you get back from SELECT)
-- =============================================================================

type RouteRow =
  { id              :: Int
  , snapshotId      :: Maybe Int
  , urlPattern      :: String
  , routeName       :: Maybe String
  , componentModule :: Maybe String
  , spiderDepth     :: Maybe Int
  , discoveredFrom  :: Maybe String
  , staticDefined   :: Boolean
  , spiderFound     :: Boolean
  , reachability    :: Maybe String    -- "reachable" | "unreachable" | "extra" | "archived"
  }

type PageComponentRow =
  { id                :: Int
  , routeId           :: Int
  , componentModule   :: String
  , parentComponentId :: Maybe Int
  , slotName          :: Maybe String
  , depth             :: Maybe Int
  }

type AffordanceRow =
  { id            :: Int
  , routeId       :: Int
  , componentId   :: Maybe Int
  , elementType   :: String        -- "button" | "link" | "input" | "toggle" | "tab" | "dropdown" | "modal-trigger"
  , actionType    :: Maybe String  -- "navigate" | "mutate-state" | "open-panel" | "submit" | "download" | "external"
  , label         :: Maybe String
  , targetRouteId :: Maybe Int
  , cssSelector   :: Maybe String
  , source        :: String        -- "static" | "spider" | "both"
  , inSource      :: Boolean
  , inSpider      :: Boolean
  }

type NavEdgeRow =
  { id             :: Int
  , fromRouteId    :: Int
  , toRouteId      :: Int
  , viaAffordance  :: Maybe Int
  , edgeType       :: Maybe String -- "link" | "button" | "programmatic" | "redirect"
  }

-- =============================================================================
-- Insert types (what you pass to INSERT — no id, DB generates it)
-- =============================================================================

type RouteInsert =
  { snapshotId      :: Maybe Int
  , urlPattern      :: String
  , routeName       :: Maybe String
  , componentModule :: Maybe String
  , spiderDepth     :: Maybe Int
  , discoveredFrom  :: Maybe String
  , staticDefined   :: Boolean
  , spiderFound     :: Boolean
  , reachability    :: Maybe String
  }

type PageComponentInsert =
  { routeId           :: Int
  , componentModule   :: String
  , parentComponentId :: Maybe Int
  , slotName          :: Maybe String
  , depth             :: Maybe Int
  }

type AffordanceInsert =
  { routeId       :: Int
  , componentId   :: Maybe Int
  , elementType   :: String
  , actionType    :: Maybe String
  , label         :: Maybe String
  , targetRouteId :: Maybe Int
  , cssSelector   :: Maybe String
  , source        :: String
  , inSource      :: Boolean
  , inSpider      :: Boolean
  }

type NavEdgeInsert =
  { fromRouteId    :: Int
  , toRouteId      :: Int
  , viaAffordance  :: Maybe Int
  , edgeType       :: Maybe String
  }
