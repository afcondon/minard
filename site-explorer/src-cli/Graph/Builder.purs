module HalogenSpider.Graph.Builder where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import HalogenSpider.Types (Edge, ModuleName, ModuleInfo, RouteName, RouteMapping)

-- | Build adjacency map from module infos (module -> its imports)
buildImportGraph :: Array ModuleInfo -> Map ModuleName (Set ModuleName)
buildImportGraph modules =
  foldl addModule Map.empty modules
  where
    addModule :: Map ModuleName (Set ModuleName) -> ModuleInfo -> Map ModuleName (Set ModuleName)
    addModule graph info =
      Map.insert info.moduleName (Set.fromFoldable info.imports) graph

-- | Convert adjacency map to edge list
graphToEdges :: Map ModuleName (Set ModuleName) -> Array Edge
graphToEdges graph =
  let entries = Map.toUnfoldable graph :: Array (Tuple ModuleName (Set ModuleName))
  in Array.concatMap entryToEdges entries
  where
    entryToEdges :: Tuple ModuleName (Set ModuleName) -> Array Edge
    entryToEdges (Tuple from imports) =
      map (\to -> { from, to }) (Set.toUnfoldable imports)

-- | Compute transitive closure (all reachable modules from a starting point)
transitiveClosure :: Map ModuleName (Set ModuleName) -> ModuleName -> Set ModuleName
transitiveClosure graph start = go Set.empty (Set.singleton start)
  where
    go :: Set ModuleName -> Set ModuleName -> Set ModuleName
    go visited frontier
      | Set.isEmpty frontier = visited
      | otherwise =
          let newVisited = Set.union visited frontier
              nextFrontier = Set.difference (expandFrontier frontier) newVisited
          in go newVisited nextFrontier

    expandFrontier :: Set ModuleName -> Set ModuleName
    expandFrontier frontier =
      let frontierArray = Set.toUnfoldable frontier :: Array ModuleName
      in foldl addNeighbors Set.empty frontierArray

    addNeighbors :: Set ModuleName -> ModuleName -> Set ModuleName
    addNeighbors acc node =
      let neighbors = fromMaybe Set.empty (Map.lookup node graph)
      in Set.union acc neighbors

-- | For each route, compute all modules it depends on transitively
routeDependencies
  :: Map ModuleName (Set ModuleName)
  -> Array RouteMapping
  -> Map RouteName (Set ModuleName)
routeDependencies importGraph routes =
  Map.fromFoldable $ map computeDeps routes
  where
    computeDeps :: RouteMapping -> Tuple RouteName (Set ModuleName)
    computeDeps mapping =
      -- The component module is our entry point
      let componentModule = mapping.componentModule
          deps = transitiveClosure importGraph componentModule
      in Tuple mapping.routeName deps

-- | Find all modules reachable from a set of entry points (e.g., Main module)
allReachableModules
  :: Map ModuleName (Set ModuleName)
  -> Array ModuleName
  -> Set ModuleName
allReachableModules graph entryPoints =
  foldl addReachable Set.empty entryPoints
  where
    addReachable :: Set ModuleName -> ModuleName -> Set ModuleName
    addReachable acc entry = Set.union acc (transitiveClosure graph entry)

-- | Find modules that are ONLY reachable via specific routes
-- | (i.e., would become orphaned if those routes were removed)
exclusiveDependencies
  :: Map ModuleName (Set ModuleName)
  -> Map RouteName (Set ModuleName)  -- route -> its transitive deps
  -> Set RouteName                    -- routes to consider removing
  -> Set ModuleName                   -- all modules in the codebase
  -> Set ModuleName                   -- modules only used by the removed routes
exclusiveDependencies _importGraph routeDeps removedRoutes _allModules =
  let -- Modules used by removed routes
      removedArray = Set.toUnfoldable removedRoutes :: Array RouteName
      removedDeps = foldl addRouteDeps Set.empty removedArray
      -- Modules used by remaining routes
      remainingRoutes = Set.difference (Set.fromFoldable $ Map.keys routeDeps) removedRoutes
      remainingArray = Set.toUnfoldable remainingRoutes :: Array RouteName
      remainingDeps = foldl addRouteDeps Set.empty remainingArray
      -- Exclusive = used by removed but not by remaining
  in Set.difference removedDeps remainingDeps
  where
    addRouteDeps :: Set ModuleName -> RouteName -> Set ModuleName
    addRouteDeps acc route =
      let deps = fromMaybe Set.empty (Map.lookup route routeDeps)
      in Set.union acc deps

-- | Find routes that have NO usages (never referenced via routeToPath)
unusedRoutes
  :: Set RouteName                    -- all defined routes
  -> Map RouteName (Set ModuleName)   -- routes that are used -> modules using them
  -> Set RouteName                    -- routes with no usages
unusedRoutes allRoutes usages =
  Set.difference allRoutes (Set.fromFoldable $ Map.keys usages)

-- | Build reverse dependency graph (module -> modules that import it)
reverseGraph :: Map ModuleName (Set ModuleName) -> Map ModuleName (Set ModuleName)
reverseGraph graph =
  let edges = graphToEdges graph
  in foldl addReverseEdge Map.empty edges
  where
    addReverseEdge :: Map ModuleName (Set ModuleName) -> Edge -> Map ModuleName (Set ModuleName)
    addReverseEdge acc edge =
      Map.insertWith Set.union edge.to (Set.singleton edge.from) acc
