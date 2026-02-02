-- | High-level spidering API
-- | This is the main public interface for the spider module
module HalogenSpider.Spider
  ( -- * Main API
    spider
  , spiderWithProgress
  -- * Configuration
  , module ConfigExports
  -- * Types (re-exported from Spider.Types)
  , module TypeExports
  -- * Utilities
  , urlPathToRouteName
  , uniquePaths
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Effect.Aff (Aff)
import HalogenSpider.Spider.Algorithm (spiderBFS)
import HalogenSpider.Spider.Puppeteer (defaultBrowserOptions, withBrowser, withPage)
import HalogenSpider.Spider.Types
  ( SpiderConfig
  , SpiderProgress
  , SpiderResult
  , UrlPath(..)
  )
import HalogenSpider.Spider.Types
  ( SpiderConfig
  , defaultConfig
  , configWithUrl
  ) as ConfigExports
import HalogenSpider.Spider.Types
  ( BaseUrl(..)
  , UrlPath(..)
  , Depth(..)
  , DiscoveredRoute
  , SpiderResult
  , SpiderProgress(..)
  , SpiderError(..)
  ) as TypeExports

-- | Spider a website using default progress handling (no-op)
-- | This is the simple interface for most use cases
spider :: SpiderConfig -> Aff SpiderResult
spider config = spiderWithProgress config (\_ -> pure unit)

-- | Spider a website with a progress callback
-- | Use this for UI progress updates or logging
spiderWithProgress
  :: SpiderConfig
  -> (SpiderProgress -> Aff Unit)
  -> Aff SpiderResult
spiderWithProgress config onProgress = do
  let browserOpts = defaultBrowserOptions { headless = config.headless }

  withBrowser browserOpts \browser ->
    withPage browser \page ->
      spiderBFS config page onProgress

-- | Convert a URL path to a likely PureScript route constructor name
-- | /howto/transitions -> HowtoTransitions
-- | /tour/scrolly -> TourScrolly
-- | / -> Home
urlPathToRouteName :: UrlPath -> String
urlPathToRouteName (UrlPath path)
  | path == "/" || path == "" = "Home"
  | otherwise =
      let parts = String.split (String.Pattern "/") path
          cleaned = Array.filter (not <<< String.null) parts
          capitalized = map capitalizeSegment cleaned
      in String.joinWith "" capitalized
  where
    capitalizeSegment :: String -> String
    capitalizeSegment s =
      let parts' = String.split (String.Pattern "-") s
          capped = map capitalize parts'
      in String.joinWith "" capped

    capitalize :: String -> String
    capitalize s = case String.uncons s of
      Nothing -> ""
      Just { head, tail } -> String.toUpper (String.singleton head) <> tail

-- | Get unique URL paths from spider result
uniquePaths :: SpiderResult -> Set UrlPath
uniquePaths result = Set.fromFoldable $ map _.urlPath result.discoveredRoutes
