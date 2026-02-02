-- | Types for the spider module with proper newtypes for type safety
module HalogenSpider.Spider.Types
  ( -- * Newtypes
    BaseUrl(..)
  , UrlPath(..)
  , Depth(..)
  , CssSelector(..)
  -- * Configuration
  , SpiderConfig
  , defaultConfig
  , configWithUrl
  -- * Results
  , DiscoveredRoute
  , mkDiscoveredRoute
  , SpiderResult
  , SpiderProgress(..)
  , SpiderError(..)
  -- * Opaque handles
  , BrowserHandle
  , PageHandle
  ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Newtype (class Newtype)

-- | A base URL like "http://localhost:8080"
newtype BaseUrl = BaseUrl String

derive instance Newtype BaseUrl _
derive instance Eq BaseUrl
derive instance Ord BaseUrl
derive newtype instance Show BaseUrl

-- | A URL path like "/howto/transitions" or "/"
newtype UrlPath = UrlPath String

derive instance Newtype UrlPath _
derive instance Eq UrlPath
derive instance Ord UrlPath
derive newtype instance Show UrlPath

-- | Depth in the BFS traversal (non-negative)
newtype Depth = Depth Int

derive instance Newtype Depth _
derive instance Eq Depth
derive instance Ord Depth
derive newtype instance Show Depth
derive newtype instance Semiring Depth

-- | A CSS selector for finding links
newtype CssSelector = CssSelector String

derive instance Newtype CssSelector _
derive newtype instance Show CssSelector

-- | Opaque handle to a Puppeteer browser instance
foreign import data BrowserHandle :: Type

-- | Opaque handle to a Puppeteer page instance
foreign import data PageHandle :: Type

-- | Configuration for spidering with sensible defaults
type SpiderConfig =
  { baseUrl :: BaseUrl
  , maxDepth :: Depth           -- ^ Maximum BFS depth (default: 5)
  , maxPages :: Int             -- ^ Safety limit on pages visited (default: 150)
  , suspiciousDepth :: Depth    -- ^ Depth that triggers cycle warning (default: 7)
  , renderWaitMs :: Int         -- ^ Time to wait for JS rendering (default: 500)
  , pageTimeoutMs :: Int        -- ^ Navigation timeout (default: 10000)
  , linkSelector :: CssSelector -- ^ CSS selector for links (default: a[href^="#"])
  , startPath :: UrlPath        -- ^ Starting path (default: /)
  , headless :: Boolean         -- ^ Run browser headless (default: true)
  , verbose :: Boolean          -- ^ Log progress to console (default: true)
  }

-- | Default configuration - only requires base URL
defaultConfig :: SpiderConfig
defaultConfig =
  { baseUrl: BaseUrl "http://localhost:8080"
  , maxDepth: Depth 5
  , maxPages: 150
  , suspiciousDepth: Depth 7
  , renderWaitMs: 500
  , pageTimeoutMs: 10000
  , linkSelector: CssSelector "a[href^=\"#\"]"
  , startPath: UrlPath "/"
  , headless: true
  , verbose: true
  }

-- | Create config with just a URL, using defaults for everything else
configWithUrl :: String -> SpiderConfig
configWithUrl url = defaultConfig { baseUrl = BaseUrl url }

-- | A discovered route from spidering
type DiscoveredRoute =
  { urlPath :: UrlPath
  , depth :: Depth
  , foundFrom :: UrlPath
  }

-- | Smart constructor for DiscoveredRoute
mkDiscoveredRoute :: UrlPath -> Depth -> UrlPath -> DiscoveredRoute
mkDiscoveredRoute path depth from =
  { urlPath: path, depth: depth, foundFrom: from }

-- | Spider results
type SpiderResult =
  { baseUrl :: BaseUrl
  , discoveredRoutes :: Array DiscoveredRoute
  , anchorLinks :: Array UrlPath     -- ^ In-page anchors (paths starting with /_)
  , errors :: Array SpiderError      -- ^ Errors encountered during spidering
  , pagesVisited :: Int
  , maxDepthReached :: Depth
  }

-- | Progress update during spidering
data SpiderProgress
  = Visiting UrlPath Depth Int Int   -- ^ path, depth, visited count, queue size
  | SuspiciousDepth UrlPath Depth    -- ^ path exceeding suspicious depth
  | PageError UrlPath String         -- ^ path and error message
  | Completed Int                    -- ^ total routes discovered

derive instance Generic SpiderProgress _
instance Show SpiderProgress where
  show = genericShow

-- | Errors that can occur during spidering
data SpiderError
  = NavigationError UrlPath String   -- ^ Failed to navigate to path
  | ExtractionError UrlPath String   -- ^ Failed to extract links
  | BrowserError String              -- ^ Browser-level error
  | DepthLimitExceeded UrlPath Depth -- ^ Path exceeded max depth
  | PageLimitExceeded Int            -- ^ Hit max pages limit

derive instance Generic SpiderError _
instance Show SpiderError where
  show = genericShow
