module HalogenSpider.Parser.Routes where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.String as String
import Data.String.Pattern (Pattern(..))
import Data.String.Utils (lines)
import HalogenSpider.Types (RouteName, RouteMapping, ModuleName)

-- | Extract route constructor names from a Route ADT definition
-- | Looks for pattern: "data Route = Foo | Bar | Baz String"
extractRouteConstructors :: String -> Array RouteName
extractRouteConstructors content =
  let allLines = lines content
      -- Find the data Route = ... definition (may span multiple lines)
      routeLines = findRouteDefinition allLines
      constructors = Array.concatMap parseConstructors routeLines
  in constructors

-- | Find lines that are part of the Route data type definition
findRouteDefinition :: Array String -> Array String
findRouteDefinition allLines =
  let indexed = Array.mapWithIndex (\i l -> { i, l }) allLines
      startIdx = Array.findIndex (\{ l } -> isRouteStart l) indexed
  in case startIdx of
    Nothing -> []
    Just idx ->
      -- Take lines until we hit another top-level definition or empty line after content
      let remaining = Array.drop idx allLines
      in Array.takeWhile isRouteContinuation remaining

isRouteStart :: String -> Boolean
isRouteStart line =
  let trimmed = String.trim line
  in String.contains (Pattern "data Route") trimmed
     || String.contains (Pattern "data Route =") trimmed

isRouteContinuation :: String -> Boolean
isRouteContinuation line =
  let trimmed = String.trim line
  in String.null trimmed
     || String.take 1 trimmed == "|"
     || String.take 1 trimmed == "="
     || String.contains (Pattern "data Route") trimmed
     -- Indented lines are continuations
     || (not (String.null line) && String.take 1 line == " ")

-- | Parse constructors from a line like "  | Foo" or "= Bar | Baz String"
parseConstructors :: String -> Array RouteName
parseConstructors line =
  let -- Split on | and extract constructor names
      parts = String.split (Pattern "|") line
      cleaned = map extractConstructorName parts
  in Array.mapMaybe identity cleaned

-- | Extract constructor name from "  Foo" or "Foo String" or "= Foo"
extractConstructorName :: String -> Maybe RouteName
extractConstructorName part =
  let trimmed = String.trim part
      -- Remove leading = if present
      withoutEq = if String.take 1 trimmed == "="
                  then String.trim (String.drop 1 trimmed)
                  else trimmed
      -- Take first word (constructor name)
      name = takeFirstWord withoutEq
  in if isValidConstructor name then Just name else Nothing

takeFirstWord :: String -> String
takeFirstWord s =
  let chars = String.toCodePointArray s
      isWordChar cp =
        let c = String.singleton cp
        in c >= "A" && c <= "Z"
           || c >= "a" && c <= "z"
           || c >= "0" && c <= "9"
      taken = Array.takeWhile isWordChar chars
  in String.fromCodePointArray taken

isValidConstructor :: String -> Boolean
isValidConstructor name =
  not (String.null name)
  && isUpperCase (String.take 1 name)
  && name /= "Route"  -- exclude the type name itself

isUpperCase :: String -> Boolean
isUpperCase s = s >= "A" && s <= "Z"

-- | Extract route -> component mappings from Main.purs renderPage function
-- | Pattern spans two lines:
-- |   RouteName ->
-- |     HH.slot_ _slotName unit ModuleName.component unit
extractRouteMappings :: String -> Array RouteMapping
extractRouteMappings content =
  let allLines = lines content
      indexed = Array.mapWithIndex (\i l -> { idx: i, line: l }) allLines
      routeLines = Array.filter (\x -> isRouteCaseLine x.line) indexed
  in Array.mapMaybe (parseRoutePair allLines) routeLines

-- | Check if a line is a route case pattern: "  RouteName ->" (ending with arrow)
isRouteCaseLine :: String -> Boolean
isRouteCaseLine line =
  let trimmed = String.trim line
      len = String.length trimmed
      -- Must end with "->" and start with uppercase
      lastTwo = String.drop (len - 2) trimmed
      endsWithArrow = lastTwo == "->"
      startsUpper = isUpperCase (String.take 1 trimmed)
      -- Exclude comment lines
      notComment = not (String.take 2 trimmed == "--")
  in endsWithArrow && startsUpper && notComment

-- | Parse a route case that spans multiple lines
-- | Line i: "  RouteName ->" or "  RouteName _ ->" (wildcard) or "  RouteName param ->"
-- | Line i+1 (or i+2 if comment): "    HH.slot_ _name unit Module.component unit"
parseRoutePair :: Array String -> { idx :: Int, line :: String } -> Maybe RouteMapping
parseRoutePair allLines { idx, line } = do
  -- Extract route name from "  RouteName ->" or "  RouteName _ ->" or "  RouteName param ->"
  let trimmed = String.trim line
  -- Drop the trailing " ->" (3 chars) and extract route name
  let withoutArrow = String.take (String.length trimmed - 3) trimmed
  let routeName = takeFirstWord withoutArrow
  -- Find the next non-comment line for component info
  let componentLine = findNextCodeLine allLines (idx + 1)
  let componentModule = extractComponentModule componentLine
  if isValidConstructor routeName
    then pure { routeName, componentModule, urlPattern: "" }
    else Nothing

-- | Find the next non-comment, non-empty line starting from index
findNextCodeLine :: Array String -> Int -> String
findNextCodeLine allLines startIdx =
  let checkLine idx = case Array.index allLines idx of
        Nothing -> ""  -- End of file
        Just line ->
          let trimmed = String.trim line
          in if String.null trimmed || String.take 2 trimmed == "--"
             then checkLine (idx + 1)  -- Skip empty/comment lines
             else trimmed
  in checkLine startIdx

-- | Check if a string contains a pattern
stringContains :: String -> String -> Boolean
stringContains needle haystack = isJust (String.indexOf (Pattern needle) haystack)

-- | Extract component module from "HH.slot_ _home unit Home.component unit"
-- | Returns "ARCHIVED" for routes using HH.div_ (no component)
-- | Returns "REDIRECT" for routes that redirect to another component
extractComponentModule :: String -> ModuleName
extractComponentModule rhs
  -- Check for archived routes (using HH.div_ instead of HH.slot_)
  | stringContains "HH.div_" rhs = "ARCHIVED"
  -- Check for HH.slot_ pattern
  | stringContains ".component" rhs =
      let parts = String.split (Pattern ".component") rhs
      in case Array.head parts of
        Nothing -> "unknown"
        Just before ->
          -- Get the last word before .component
          let words = String.split (Pattern " ") (String.trim before)
              lastWord = fromMaybe "unknown" (Array.last words)
          in lastWord
  -- Fallback
  | otherwise = "unknown"

-- | Find all routeToPath usages in a file
-- | Returns array of route names used
extractRouteUsages :: String -> Array RouteName
extractRouteUsages content =
  let allLines = lines content
      usageLines = Array.filter hasRouteToPath allLines
  in Array.concatMap parseRouteToPathUsage usageLines

hasRouteToPath :: String -> Boolean
hasRouteToPath line = String.contains (Pattern "routeToPath") line

-- | Parse "routeToPath Home" or "routeToPath HowtoIndex" from a line
parseRouteToPathUsage :: String -> Array RouteName
parseRouteToPathUsage line =
  let -- Find all occurrences after "routeToPath "
      parts = String.split (Pattern "routeToPath ") line
      afterKeyword = Array.drop 1 parts  -- drop everything before first match
  in Array.mapMaybe extractRouteName afterKeyword

extractRouteName :: String -> Maybe RouteName
extractRouteName s =
  let name = takeFirstWord (String.trim s)
  in if isValidConstructor name then Just name else Nothing
