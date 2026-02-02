module HalogenSpider.Types where

import Prelude

import Data.Map (Map)
import Data.Set (Set)

-- | A module name like "Component.Home" or "Main"
type ModuleName = String

-- | A route name like "Home" or "HowtoTransitions"
type RouteName = String

-- | An edge in a directed graph
type Edge = { from :: String, to :: String }

-- | Labeled edge for multi-graph scenarios
type LabeledEdge = { from :: String, to :: String, label :: String }

-- | Module dependency information extracted from a file
type ModuleInfo =
  { moduleName :: ModuleName
  , filePath :: String
  , imports :: Array ModuleName
  }

-- | Route definition with its associated component
type RouteMapping =
  { routeName :: RouteName
  , componentModule :: ModuleName
  , urlPattern :: String  -- extracted from comments or routeToPath
  }

-- | Analysis result for a codebase
type AnalysisResult =
  { modules :: Map ModuleName ModuleInfo
  , routes :: Array RouteMapping
  , routeUsages :: Map RouteName (Set ModuleName)  -- which modules reference each route
  , moduleGraph :: Array Edge  -- import dependencies
  }

-- | A URL path like "/howto/transitions" or "/"
type UrlPath = String

-- | A discovered route from spidering
type DiscoveredRoute =
  { urlPath :: UrlPath
  , depth :: Int
  , foundFrom :: UrlPath
  }

-- | Spider results
type SpiderResult =
  { baseUrl :: String
  , discoveredRoutes :: Array DiscoveredRoute
  , anchorLinks :: Array UrlPath  -- in-page anchors (start with /_)
  }

-- | Comparison between static analysis and spidering
type RouteComparison =
  { definedRoutes :: Set RouteName          -- from static analysis
  , reachableRoutes :: Set UrlPath          -- from spidering
  , unreachableRoutes :: Array RouteName    -- defined but not spidered
  , extraRoutes :: Array UrlPath            -- spidered but not defined
  , archivedRoutes :: Array RouteName       -- marked as ARCHIVED
  }
