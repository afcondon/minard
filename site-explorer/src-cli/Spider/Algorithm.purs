-- | BFS spidering algorithm in pure PureScript
-- | Separates the algorithm from the FFI for testability
module HalogenSpider.Spider.Algorithm
  ( spiderBFS
  , normalizeLink
  , isAnchorLink
  , buildFullUrl
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap, wrap)
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff, attempt)
import Effect.Class.Console as Console
import HalogenSpider.Spider.Puppeteer (extractLinks, navigateTo, waitForRender)
import HalogenSpider.Spider.Types
  ( BaseUrl(..)
  , Depth(..)
  , DiscoveredRoute
  , PageHandle
  , SpiderConfig
  , SpiderError(..)
  , SpiderProgress(..)
  , SpiderResult
  , UrlPath(..)
  , mkDiscoveredRoute
  )

-- | Internal state for BFS traversal
type BFSState =
  { discovered :: Map UrlPath DiscoveredRoute  -- All discovered routes
  , visited :: Set UrlPath                      -- Already visited paths
  , queue :: Array { path :: UrlPath, depth :: Depth, from :: UrlPath }
  , errors :: Array SpiderError
  , maxDepthSeen :: Depth
  }

-- | Initialize BFS state
initState :: UrlPath -> BFSState
initState startPath =
  let startRoute = mkDiscoveredRoute startPath (Depth 0) (UrlPath "start")
  in { discovered: Map.singleton startPath startRoute
     , visited: Set.empty
     , queue: [{ path: startPath, depth: Depth 0, from: UrlPath "start" }]
     , errors: []
     , maxDepthSeen: Depth 0
     }

-- | Run BFS spidering with a page handle
-- | Returns the spider result
spiderBFS
  :: SpiderConfig
  -> PageHandle
  -> (SpiderProgress -> Aff Unit)  -- Progress callback
  -> Aff SpiderResult
spiderBFS config page onProgress = do
  finalState <- bfsLoop config page onProgress (initState config.startPath)

  -- Separate anchor links from real routes
  let allRoutes = Array.fromFoldable $ Map.values finalState.discovered
      { yes: anchors, no: routes } = Array.partition (isAnchorLink <<< _.urlPath) allRoutes

  onProgress $ Completed (Array.length routes)

  pure
    { baseUrl: config.baseUrl
    , discoveredRoutes: routes
    , anchorLinks: map _.urlPath anchors
    , errors: finalState.errors
    , pagesVisited: Set.size finalState.visited
    , maxDepthReached: finalState.maxDepthSeen
    }

-- | Main BFS loop
bfsLoop
  :: SpiderConfig
  -> PageHandle
  -> (SpiderProgress -> Aff Unit)
  -> BFSState
  -> Aff BFSState
bfsLoop config page onProgress state = case Array.uncons state.queue of
  Nothing -> pure state  -- Queue empty, done
  Just { head: current, tail: rest } -> do
    let state' = state { queue = rest }

    -- Skip if already visited
    if Set.member current.path state'.visited
      then bfsLoop config page onProgress state'
      else do
        -- Check depth limit
        if current.depth > config.maxDepth
          then do
            let err = DepthLimitExceeded current.path current.depth
            bfsLoop config page onProgress $ state' { errors = Array.snoc state'.errors err }
          else do
            -- Check page limit
            if Set.size state'.visited >= config.maxPages
              then do
                let err = PageLimitExceeded config.maxPages
                when config.verbose $ Console.log $ "\n⚠️  Reached max page limit (" <> show config.maxPages <> ")"
                pure $ state' { errors = Array.snoc state'.errors err }
              else do
                -- Visit this page
                newState <- visitPage config page onProgress current state'
                bfsLoop config page onProgress newState

-- | Visit a single page and extract links
visitPage
  :: SpiderConfig
  -> PageHandle
  -> (SpiderProgress -> Aff Unit)
  -> { path :: UrlPath, depth :: Depth, from :: UrlPath }
  -> BFSState
  -> Aff BFSState
visitPage config page onProgress current state = do
  let visited' = Set.insert current.path state.visited
      maxDepth' = max state.maxDepthSeen current.depth

  -- Report progress
  when config.verbose $
    Console.log $ "[" <> show (unwrap current.depth) <> "] "
      <> unwrap current.path
      <> " (" <> show (Set.size visited') <> "/" <> show config.maxPages
      <> ", queue: " <> show (Array.length state.queue) <> ")"

  onProgress $ Visiting current.path current.depth (Set.size visited') (Array.length state.queue)

  -- Warn about suspicious depth
  when (current.depth > config.suspiciousDepth) do
    when config.verbose $ Console.warn $ "  ⚠️  Suspicious depth " <> show (unwrap current.depth) <> " - possible cycle?"
    onProgress $ SuspiciousDepth current.path current.depth

  -- Navigate and extract links
  let fullUrl = buildFullUrl config.baseUrl current.path
  result <- attempt do
    navigateTo page fullUrl config.pageTimeoutMs
    waitForRender config.renderWaitMs
    extractLinks page config.linkSelector

  case result of
    Left err -> do
      let errMsg = show err
      when config.verbose $ Console.error $ "  Error: " <> errMsg
      onProgress $ PageError current.path errMsg
      pure $ state
        { visited = visited'
        , maxDepthSeen = maxDepth'
        , errors = Array.snoc state.errors (NavigationError current.path errMsg)
        }

    Right rawLinks -> do
      -- Process links
      let normalized = Array.mapMaybe (normalizeLink) rawLinks
          newDepth = wrap (unwrap current.depth + 1)
          newRoutes = Array.filter (\p -> not $ Map.member p state.discovered) normalized
          newEntries = map (\p -> Tuple p (mkDiscoveredRoute p newDepth current.path)) newRoutes
          newQueue = map (\p -> { path: p, depth: newDepth, from: current.path }) newRoutes

      pure $ state
        { discovered = foldl (\m (Tuple k v) -> Map.insert k v m) state.discovered newEntries
        , visited = visited'
        , queue = state.queue <> newQueue
        , maxDepthSeen = maxDepth'
        }

-- | Build full URL from base URL and path (for hash-based routing)
buildFullUrl :: BaseUrl -> UrlPath -> String
buildFullUrl (BaseUrl base) (UrlPath path) = base <> "/#" <> path

-- | Normalize a raw href to a UrlPath
-- | Handles: "#/path", "#path", "/path", etc.
normalizeLink :: String -> Maybe UrlPath
normalizeLink href
  | String.null href = Nothing
  | otherwise =
      let -- Remove leading # if present
          withoutHash =
            if String.take 1 href == "#"
              then String.drop 1 href
              else href
          -- Ensure leading /
          withSlash =
            if String.take 1 withoutHash == "/"
              then withoutHash
              else "/" <> withoutHash
          -- Remove trailing slashes (except for root)
          normalized = dropTrailingSlashes withSlash
      in if String.null normalized then Nothing else Just (UrlPath normalized)
  where
    -- Drop trailing slashes, preserving at least "/"
    dropTrailingSlashes :: String -> String
    dropTrailingSlashes s =
      let len = String.length s
          lastChar = String.drop (len - 1) s
      in if len <= 1
           then s
           else if lastChar == "/"
                  then dropTrailingSlashes (String.take (len - 1) s)
                  else s

-- | Check if a path is an anchor link (starts with /_)
isAnchorLink :: UrlPath -> Boolean
isAnchorLink (UrlPath path) = String.take 2 path == "/_"
