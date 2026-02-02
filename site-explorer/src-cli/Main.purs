module Main where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Set as Set
import Data.String (Pattern(..), indexOf)
import Data.Foldable (for_)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_, attempt)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Node.Process as Process
import Node.Buffer as Buffer
import Node.Encoding (Encoding(..))
import Node.FS.Sync as FS

import Data.Newtype (unwrap)
import HalogenSpider.Types (ModuleInfo, RouteMapping, RouteComparison)
import HalogenSpider.FileSystem (findPursFiles, readTextFile)
import HalogenSpider.Parser.Imports (parseModuleInfo, extractImportAliases)
import HalogenSpider.Parser.Routes (extractRouteConstructors, extractRouteMappings, extractRouteUsages)
import HalogenSpider.Graph.Builder (buildImportGraph, graphToEdges, routeDependencies, unusedRoutes, exclusiveDependencies)
import HalogenSpider.Output.CSV (edgesToCSV, routeMappingsToCSV, routeDepsToCSV, unusedRoutesToCSV, orphanedModulesToCSV)
import HalogenSpider.Output.DOT (moduleGraphToDOT, routeGraphToDOT, moduleGraphWithHighlightsToDOT)
import HalogenSpider.Spider (spider, configWithUrl, SpiderResult)
import HalogenSpider.Compare (compareRoutes, routeNameToUrlPath)
import HalogenSpider.Output.HTML (tabbedReportHTML)

-- | Check if a string contains a pattern
contains :: String -> String -> Boolean
contains needle haystack = isJust (indexOf (Pattern needle) haystack)

main :: Effect Unit
main = launchAff_ do
  args <- liftEffect Process.argv
  let command = fromMaybe "help" (Array.index args 2)

  case command of
    "analyze" -> do
      let targetDir = fromMaybe "." (Array.index args 3)
      analyze targetDir

    "spider" -> do
      let baseUrl = fromMaybe "http://localhost:8080" (Array.index args 3)
      spiderCommand baseUrl

    "compare" -> do
      let targetDir = fromMaybe "." (Array.index args 3)
      let baseUrl = fromMaybe "http://localhost:8080" (Array.index args 4)
      compareCommand targetDir baseUrl

    "html" -> do
      let targetDir = fromMaybe "." (Array.index args 3)
      let baseUrl = fromMaybe "http://localhost:8080" (Array.index args 4)
      htmlCommand targetDir baseUrl

    "help" -> showHelp
    "--help" -> showHelp
    "-h" -> showHelp

    _ -> do
      Console.log $ "Unknown command: " <> command
      showHelp

showHelp :: Aff Unit
showHelp = do
  Console.log "halogen-spider - Analyze Halogen routing for dead code detection"
  Console.log ""
  Console.log "Usage:"
  Console.log "  node run.js analyze <project-dir>       Static analysis of routes and modules"
  Console.log "  node run.js spider <base-url>           Spider a live site for reachable routes"
  Console.log "  node run.js compare <dir> <url>         Compare static analysis with spidering"
  Console.log "  node run.js html <dir> <url>            Generate HTML page with route links"
  Console.log ""
  Console.log "Examples:"
  Console.log "  node run.js analyze ./site/website"
  Console.log "  node run.js spider http://localhost:8080"
  Console.log "  node run.js compare ./site/website http://localhost:8080"
  Console.log "  node run.js html ./site/website http://localhost:8080"

-- | Static analysis command
analyze :: String -> Aff Unit
analyze targetDir = do
  Console.log $ "Analyzing Halogen project: " <> targetDir

  -- Find all PureScript files
  Console.log "Finding .purs files..."
  pursFiles <- findPursFiles (targetDir <> "/src")
  Console.log $ "Found " <> show (Array.length pursFiles) <> " files"

  -- Parse all modules for imports
  Console.log "Parsing module imports..."
  moduleInfos <- parseAllModules pursFiles
  Console.log $ "Parsed " <> show (Array.length moduleInfos) <> " modules"

  -- Build import graph
  let importGraph = buildImportGraph moduleInfos
  let edges = graphToEdges importGraph
  Console.log $ "Built graph with " <> show (Array.length edges) <> " import edges"

  -- Find Types.purs for route definitions
  Console.log "Looking for route definitions..."
  routeConstructors <- findRouteConstructors pursFiles
  Console.log $ "Found " <> show (Array.length routeConstructors) <> " route constructors"

  -- Find Main.purs for route -> component mappings
  routeMappings <- findRouteMappings pursFiles
  Console.log $ "Found " <> show (Array.length routeMappings) <> " route -> component mappings"

  -- Find route usages across all files
  Console.log "Finding route usages..."
  usageMap <- findAllRouteUsages pursFiles
  let usedRoutes = Set.fromFoldable $ Map.keys usageMap
  Console.log $ "Found " <> show (Set.size usedRoutes) <> " routes with usages"

  -- Compute route dependencies
  let routeDeps = routeDependencies importGraph routeMappings
  Console.log "Computed transitive dependencies per route"

  -- Find unused routes
  let allRoutes = Set.fromFoldable routeConstructors
  let unused = unusedRoutes allRoutes usageMap
  Console.log $ "Found " <> show (Set.size unused) <> " routes with no usages"

  -- Output results
  Console.log "\n--- Writing output files ---"

  -- Module import graph
  writeOutput (targetDir <> "/module-graph.csv") (edgesToCSV edges)
  writeOutput (targetDir <> "/module-graph.dot") (moduleGraphToDOT "Module Dependencies" edges)

  -- Route mappings
  writeOutput (targetDir <> "/route-mappings.csv") (routeMappingsToCSV routeMappings)
  writeOutput (targetDir <> "/route-graph.dot") (routeGraphToDOT routeMappings)

  -- Route dependencies
  writeOutput (targetDir <> "/route-dependencies.csv") (routeDepsToCSV routeDeps)

  -- Unused routes
  writeOutput (targetDir <> "/unused-routes.csv") (unusedRoutesToCSV unused)

  -- If we have unused routes, compute what would be orphaned
  when (not $ Set.isEmpty unused) do
    let allModules = Set.fromFoldable $ map _.moduleName moduleInfos
    let orphaned = exclusiveDependencies importGraph routeDeps unused allModules
    writeOutput (targetDir <> "/orphaned-modules.csv") (orphanedModulesToCSV orphaned)
    writeOutput (targetDir <> "/module-graph-orphans.dot")
      (moduleGraphWithHighlightsToDOT "Modules (orphans highlighted)" edges orphaned)
    Console.log $ "Found " <> show (Set.size orphaned) <> " modules that would be orphaned"

  Console.log "\nDone! Output files written to target directory."

-- | Spider command
spiderCommand :: String -> Aff Unit
spiderCommand baseUrl = do
  result <- spider (configWithUrl baseUrl)

  -- Write spider results
  let csv = spiderResultToCSV result
  writeOutput "discovered-routes.csv" csv
  Console.log "Wrote: discovered-routes.csv"

-- | Compare command - runs both analyze and spider, then compares
compareCommand :: String -> String -> Aff Unit
compareCommand targetDir baseUrl = do
  Console.log "=== STATIC ANALYSIS ==="
  Console.log ""

  -- Get route mappings from static analysis
  pursFiles <- findPursFiles (targetDir <> "/src")
  routeMappings <- findRouteMappings pursFiles
  Console.log $ "Found " <> show (Array.length routeMappings) <> " route mappings"

  Console.log ""
  Console.log "=== SPIDERING ==="
  Console.log ""

  -- Spider the site
  spiderResult <- spider (configWithUrl baseUrl)

  Console.log ""
  Console.log "=== COMPARISON ==="
  Console.log ""

  -- Compare
  let comparison = compareRoutes routeMappings spiderResult
  printComparison comparison

  -- Write comparison results
  writeOutput (targetDir <> "/comparison.csv") (comparisonToCSV comparison)
  Console.log $ "\nWrote: " <> targetDir <> "/comparison.csv"

printComparison :: RouteComparison -> Aff Unit
printComparison c = do
  Console.log $ "Defined routes: " <> show (Set.size c.definedRoutes)
  Console.log $ "Reachable routes: " <> show (Set.size c.reachableRoutes)
  Console.log $ "Archived routes: " <> show (Array.length c.archivedRoutes)

  Console.log ""
  Console.log "--- UNREACHABLE ROUTES (defined but not spidered) ---"
  for_ c.unreachableRoutes \r ->
    Console.log $ "  " <> r
  Console.log $ "Total: " <> show (Array.length c.unreachableRoutes)

  Console.log ""
  Console.log "--- EXTRA ROUTES (spidered but not defined) ---"
  for_ c.extraRoutes \r ->
    Console.log $ "  " <> r
  Console.log $ "Total: " <> show (Array.length c.extraRoutes)

-- | HTML command - generate tabbed route review page with static, spidered, and comparison
htmlCommand :: String -> String -> Aff Unit
htmlCommand targetDir baseUrl = do
  Console.log "=== Generating Route Analysis Report ==="
  Console.log ""

  -- Static analysis
  Console.log "Step 1: Static analysis..."
  pursFiles <- findPursFiles (targetDir <> "/src")
  routeMappings <- findRouteMappings pursFiles
  Console.log $ "  Found " <> show (Array.length routeMappings) <> " route definitions"

  -- Add URL patterns to mappings
  let withPaths = map addUrlPath routeMappings

  -- Spider the site
  Console.log ""
  Console.log "Step 2: Spidering live site..."
  spiderResult <- spider (configWithUrl baseUrl)
  Console.log $ "  Discovered " <> show (Array.length spiderResult.discoveredRoutes) <> " routes"

  -- Compare
  Console.log ""
  Console.log "Step 3: Comparing..."
  let comparison = compareRoutes withPaths spiderResult

  -- Generate tabbed HTML report
  let html = tabbedReportHTML baseUrl withPaths spiderResult comparison
  writeOutput (targetDir <> "/routes.html") html

  Console.log ""
  Console.log $ "Report written to: " <> targetDir <> "/routes.html"
  Console.log "Open in browser to review all routes with tabs for static/spidered/comparison"
  where
    addUrlPath mapping = mapping { urlPattern = routeNameToUrlPath mapping.routeName }

-- CSV formatters for spider results (unwrapping newtypes)

spiderResultToCSV :: SpiderResult -> String
spiderResultToCSV result =
  let header = "url_path,depth,found_from"
      rows = map routeToLine result.discoveredRoutes
  in Array.intercalate "\n" ([header] <> rows)
  where
    routeToLine r =
      unwrap r.urlPath <> "," <> show (unwrap r.depth) <> "," <> unwrap r.foundFrom

comparisonToCSV :: RouteComparison -> String
comparisonToCSV c =
  let unreachableSection = ["# Unreachable routes (defined but not spidered)"]
        <> map (\r -> "unreachable," <> r) c.unreachableRoutes
      extraSection = ["# Extra routes (spidered but not defined)"]
        <> map (\r -> "extra," <> r) c.extraRoutes
      archivedSection = ["# Archived routes"]
        <> map (\r -> "archived," <> r) c.archivedRoutes
  in Array.intercalate "\n" (unreachableSection <> [""] <> extraSection <> [""] <> archivedSection)

-- Helper functions (same as before)

parseAllModules :: Array String -> Aff (Array ModuleInfo)
parseAllModules files = do
  results <- traverse parseFile files
  pure $ Array.catMaybes results
  where
    parseFile :: String -> Aff (Maybe ModuleInfo)
    parseFile path = do
      result <- attempt $ readTextFile path
      case result of
        Left _ -> pure Nothing
        Right content -> pure $ parseModuleInfo path content

findRouteConstructors :: Array String -> Aff (Array String)
findRouteConstructors files = do
  let candidates = Array.filter isLikelyRouteFile files
  results <- traverse extractFromFile candidates
  pure $ Array.concat results
  where
    isLikelyRouteFile :: String -> Boolean
    isLikelyRouteFile path =
      contains "Types.purs" path || contains "Route" path

    extractFromFile :: String -> Aff (Array String)
    extractFromFile path = do
      result <- attempt $ readTextFile path
      case result of
        Left _ -> pure []
        Right content ->
          if contains "data Route" content
            then pure $ extractRouteConstructors content
            else pure []

findRouteMappings :: Array String -> Aff (Array RouteMapping)
findRouteMappings files = do
  let candidates = Array.filter (\p -> contains "Main.purs" p) files
  results <- traverse extractFromFile candidates
  pure $ Array.concat results
  where
    extractFromFile :: String -> Aff (Array RouteMapping)
    extractFromFile path = do
      result <- attempt $ readTextFile path
      case result of
        Left _ -> pure []
        Right content ->
          let aliases = extractImportAliases content
              rawMappings = extractRouteMappings content
              resolved = map (resolveAlias aliases) rawMappings
          in pure resolved

    resolveAlias :: Array { alias :: String, moduleName :: String } -> RouteMapping -> RouteMapping
    resolveAlias aliases mapping =
      let foundAlias = Array.find (\a -> a.alias == mapping.componentModule) aliases
      in case foundAlias of
        Just { moduleName } -> mapping { componentModule = moduleName }
        Nothing -> mapping

findAllRouteUsages :: Array String -> Aff (Map.Map String (Set.Set String))
findAllRouteUsages files = do
  usages <- traverse extractUsages files
  pure $ mergeUsages (Array.concat usages)
  where
    extractUsages :: String -> Aff (Array { route :: String, module_ :: String })
    extractUsages path = do
      result <- attempt $ readTextFile path
      case result of
        Left _ -> pure []
        Right content -> do
          let moduleInfo = parseModuleInfo path content
          case moduleInfo of
            Nothing -> pure []
            Just info ->
              let routes = extractRouteUsages content
              in pure $ map (\r -> { route: r, module_: info.moduleName }) routes

    mergeUsages :: Array { route :: String, module_ :: String } -> Map.Map String (Set.Set String)
    mergeUsages usages =
      Array.foldl addUsage Map.empty usages

    addUsage :: Map.Map String (Set.Set String) -> { route :: String, module_ :: String } -> Map.Map String (Set.Set String)
    addUsage acc { route, module_ } =
      Map.insertWith Set.union route (Set.singleton module_) acc

writeOutput :: String -> String -> Aff Unit
writeOutput path content = do
  liftEffect do
    buf <- Buffer.fromString content UTF8
    FS.writeFile path buf
  Console.log $ "Wrote: " <> path
