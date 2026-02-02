module HalogenSpider.Compare
  ( compareRoutes
  , RouteNameToPath
  , UrlPath
  , defaultRouteNameToPath
  , routeNameToUrlPath
  ) where

import Prelude

import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Newtype (unwrap)
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Data.Tuple (Tuple(..))
import HalogenSpider.Types (RouteComparison, RouteMapping, RouteName)
import HalogenSpider.Spider.Types (SpiderResult)

-- | Local type alias for URL paths (plain strings for comparison logic)
type UrlPath = String

-- | Check if a string contains a substring
stringContains :: String -> String -> Boolean
stringContains needle haystack = isJust (String.indexOf (String.Pattern needle) haystack)

-- | Mapping from route names to URL paths
type RouteNameToPath = Map RouteName UrlPath

-- | Default mapping based on common Halogen routing conventions
-- | This handles the camelCase to kebab-case conversion
defaultRouteNameToPath :: Array RouteMapping -> RouteNameToPath
defaultRouteNameToPath mappings =
  Map.fromFoldable $ Array.mapMaybe toPathMapping mappings
  where
    toPathMapping :: RouteMapping -> Maybe (Tuple RouteName UrlPath)
    toPathMapping mapping =
      let path = routeNameToUrlPath mapping.routeName
      in Just (Tuple mapping.routeName path)

-- | Convert a route name to its expected URL path
-- | HowtoTransitions -> /howto/transitions
-- | TourScrolly -> /tour/scrolly
-- | Home -> /
routeNameToUrlPath :: RouteName -> UrlPath
routeNameToUrlPath name = case name of
  "Home" -> "/"
  "GettingStarted" -> "/getting-started"
  "HowtoIndex" -> "/howto"
  "Understanding" -> "/understanding"
  "Reference" -> "/reference"
  "TourIndex" -> "/tour"
  "Showcase" -> "/showcase"
  "Examples" -> "/examples"
  "Acknowledgements" -> "/acknowledgements"
  "Wizard" -> "/wizard"
  "NotFound" -> "/not-found"
  "ARCHIVED" -> "/archived"
  _ -> camelToKebabPath name

-- | Convert CamelCase route name to /kebab/case path
-- | HowtoTransitions -> /howto/transitions
-- | TourMotionScrolly -> /tour/motion-scrolly
-- | UnderstandingGrammar -> /understanding/grammar
camelToKebabPath :: String -> UrlPath
camelToKebabPath name =
  let -- Known prefixes that should become path segments
      prefixes = ["Howto", "Tour", "Understanding", "Showcase", "Example", "ForceConfig", "TreeBuilder"]
      -- Try to split on known prefix
      result = Array.foldl tryPrefix Nothing prefixes
  in fromMaybe (defaultKebab name) result
  where
    tryPrefix :: Maybe UrlPath -> String -> Maybe UrlPath
    tryPrefix (Just p) _ = Just p
    tryPrefix Nothing prefix =
      if String.take (String.length prefix) name == prefix
        then
          let rest = String.drop (String.length prefix) name
              prefixKebab = camelToKebab prefix
              restKebab = camelToKebab rest
          in if String.null rest
             then Just ("/" <> prefixKebab)
             else Just ("/" <> prefixKebab <> "/" <> restKebab)
        else Nothing

    defaultKebab :: String -> UrlPath
    defaultKebab s = "/" <> camelToKebab s

-- | Convert CamelCase to kebab-case
-- | HowtoTransitions -> howto-transitions
-- | But: Howto -> howto (no trailing dash)
camelToKebab :: String -> String
camelToKebab s =
  let chars = String.toCodePointArray s
      result = Array.foldl addChar { first: true, result: "" } chars
  in String.toLower result.result
  where
    addChar :: { first :: Boolean, result :: String } -> String.CodePoint -> { first :: Boolean, result :: String }
    addChar acc cp =
      let char = String.singleton cp
          isUpper = char >= "A" && char <= "Z"
      in if isUpper && not acc.first
         then { first: false, result: acc.result <> "-" <> char }
         else { first: false, result: acc.result <> char }

-- | Compare static analysis results with spider results
compareRoutes
  :: Array RouteMapping      -- from static analysis
  -> SpiderResult            -- from spidering
  -> RouteComparison
compareRoutes mappings spiderResult =
  let -- Build route name to path mapping
      routeToPath = defaultRouteNameToPath mappings

      -- Set of defined route names
      definedRoutes = Set.fromFoldable $ map _.routeName mappings

      -- Set of expected paths (from defined routes)
      expectedPaths = Set.fromFoldable $ Map.values routeToPath

      -- Set of spidered paths (unwrap newtypes)
      spideredPaths = Set.fromFoldable $ map (unwrap <<< _.urlPath) spiderResult.discoveredRoutes

      -- Filter out parameterized routes (Example/*)
      isParameterized path = stringContains "/example/" path

      realSpideredPaths = Set.filter (not <<< isParameterized) spideredPaths

      -- Archived routes
      archivedRoutes = Array.filter (\m -> m.componentModule == "ARCHIVED") mappings
                       # map _.routeName

      -- Routes defined but not reachable
      unreachableRoutes = Array.filter (isUnreachable realSpideredPaths routeToPath) $
                          Array.fromFoldable definedRoutes

      -- Routes spidered but not in definitions
      extraRoutes = Array.filter (not <<< isExpected expectedPaths) $
                    Array.fromFoldable realSpideredPaths

  in { definedRoutes
     , reachableRoutes: realSpideredPaths
     , unreachableRoutes
     , extraRoutes
     , archivedRoutes
     }
  where
    isUnreachable :: Set UrlPath -> RouteNameToPath -> RouteName -> Boolean
    isUnreachable spidered routeToPath routeName =
      case Map.lookup routeName routeToPath of
        Nothing -> true  -- No path mapping, assume unreachable
        Just path -> not (Set.member path spidered)

    isExpected :: Set UrlPath -> UrlPath -> Boolean
    isExpected expected path = Set.member path expected
