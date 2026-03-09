-- | Layer Discovery from Directed Import Graph
-- |
-- | Uses directed import edges + namespace prefixes to suggest
-- | architectural layers for a codebase. Produces a draft architecture.yml.
-- |
-- | Key insight: the undirected block-cut tree can't differentiate modules
-- | within a dominant block. But the directed import graph can — modules
-- | imported by many others but importing few are "foundation", modules
-- | that import many but are imported by few are "entry".
module CE2.Data.LayerDiscovery
  ( DiscoveredLayer
  , LayerDiscoveryResult
  , Violation
  , discoverLayers
  , discoverLayersFromImports
  , generateYaml
  ) where

import Prelude

import Data.Array as Array
import Data.Array (sortBy)
import Data.Foldable (foldl)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Data.Tuple (Tuple(..), fst, snd)

import CE2.Data.Decomposition as Dec

-- | A discovered architectural layer
type DiscoveredLayer =
  { name :: String
  , order :: Int
  , modules :: Set String
  , pattern :: String
  , namespaces :: Array String
  }

-- | An edge that violates the discovered layer ordering
type Violation =
  { from :: String
  , to :: String
  , fromLayer :: Int
  , toLayer :: Int
  }

type LayerDiscoveryResult =
  { layers :: Array DiscoveredLayer
  , violations :: Array Violation
  , moduleLayer :: Map String Int
  , treelikenessWithout :: Number
  }

-- | The raw directed import data we receive
type DirectedImport = { moduleName :: String, imports :: Array String }

-- | Discover layers from directed imports + namespace structure.
-- |
-- | Algorithm:
-- | 1. Build directed import graph and compute import depth per module
-- |    (longest path from sources — modules with zero imports)
-- | 2. Group modules by namespace prefix (e.g., CE2.Component, CE2.Data)
-- | 3. Assign each namespace group a layer based on its median import depth
-- | 4. Compact layer numbers (remove gaps)
-- | 5. Detect violations: imports going from lower layer to higher layer
-- |    (a foundation module importing an entry module)
discoverLayers :: Dec.SimpleGraph String -> Dec.DecompInfo -> LayerDiscoveryResult
discoverLayers graph info = discoverLayersFromImports graph info []

-- | Version that takes directed imports for proper layer ordering
discoverLayersFromImports :: Dec.SimpleGraph String -> Dec.DecompInfo -> Array DirectedImport -> LayerDiscoveryResult
discoverLayersFromImports graph _info directedImports =
  let
    -- Step 1: Compute import depth from directed edges
    -- Build directed adjacency: module → modules it imports
    dirAdj = foldl (\acc di ->
      Map.insert di.moduleName (Set.fromFoldable di.imports) acc
    ) Map.empty directedImports

    -- Compute longest-path depth from sources (BFS on reverse edges)
    depths = importDepths dirAdj graph.nodes

    -- Step 2: Group by namespace and compute median depth per namespace
    nsMap = buildNamespaceMap graph.nodes
    nsMedian = namespaceMedianDepth nsMap depths

    -- Step 3: Assign each module its namespace's median depth
    moduleDepthRaw = foldl (\acc name ->
      let ns = moduleNamespace name
          d = case Map.lookup ns nsMedian of
            Just med -> med
            Nothing -> fromMaybe 0 (Map.lookup name depths)
      in Map.insert name d acc
    ) Map.empty graph.nodes

    -- Step 4: Compact — renumber so layers are 0, 1, 2, ... with no gaps
    uniqueDepths = Array.sort $ Array.nub $
      map snd (Map.toUnfoldable moduleDepthRaw :: Array (Tuple String Int))
    depthToLayer = Map.fromFoldable $
      Array.mapWithIndex (\i d -> Tuple d i) uniqueDepths

    moduleLayer = map (\d -> fromMaybe 0 (Map.lookup d depthToLayer)) moduleDepthRaw

    -- Step 5: Build layer records
    layerGroups = groupByLayer moduleLayer
    maxOrder = fromMaybe 0 $ map fst $ Array.last layerGroups

    layers = layerGroups <#> \(Tuple order modules) ->
      let
        nss = namespacesInSet modules
        layerName = suggestLayerName nss order maxOrder
        pat = suggestPattern nss
      in
        { name: layerName
        , order
        , modules
        , pattern: pat
        , namespaces: nss
        }

    -- Step 6: Find violations
    -- In a clean layered architecture, imports go downward (higher layer → lower layer).
    -- A violation is when a module in a lower layer imports from a higher layer.
    violations = findViolations directedImports moduleLayer

    -- Step 7: Treelikeness without violations
    treelikenessWithout = computeTreelikenessWithout graph violations
  in
    { layers, violations, moduleLayer, treelikenessWithout }

-- | Compute import depth for each module using longest-path BFS.
-- | Sources (modules with no imports within the set) get depth 0.
-- | Each module's depth = 1 + max depth of its imports.
importDepths :: Map String (Set String) -> Array String -> Map String Int
importDepths dirAdj allNodes =
  let
    nodeSet = Set.fromFoldable allNodes

    -- Only count imports within the analyzed set
    localImports = map (Set.filter (\imp -> Set.member imp nodeSet)) dirAdj

    -- Build reverse adjacency: module → modules that import it
    revAdj = foldl (\acc (Tuple importer imports) ->
      foldl (\a imp ->
        Map.alter (Just <<< Set.insert importer <<< fromMaybe Set.empty) imp a
      ) acc (Set.toUnfoldable imports :: Array String)
    ) Map.empty (Map.toUnfoldable localImports :: Array (Tuple String (Set String)))

    -- Sources: modules with no local imports
    sources = Array.filter (\n ->
      let imps = fromMaybe Set.empty (Map.lookup n localImports)
      in Set.isEmpty imps
    ) allNodes

    -- BFS from sources, propagating max depth
    initDepths = foldl (\acc s -> Map.insert s 0 acc) Map.empty sources
  in
    propagateDepths localImports revAdj initDepths sources

-- | Propagate depths through the directed graph using iterative relaxation.
-- | Each module's depth = 1 + max depth among its imports.
propagateDepths :: Map String (Set String) -> Map String (Set String) -> Map String Int -> Array String -> Map String Int
propagateDepths dirAdj _revAdj initDepths _sources =
  let
    -- Iterative: keep updating until stable (handles cycles gracefully)
    allNodes = Array.nub $ Array.concat
      [ map fst (Map.toUnfoldable dirAdj :: Array (Tuple String (Set String)))
      , Array.concatMap (\(Tuple _ s) -> Set.toUnfoldable s :: Array String)
          (Map.toUnfoldable dirAdj :: Array (Tuple String (Set String)))
      ]

    -- Initialize all nodes
    depths0 = foldl (\acc n ->
      if Map.member n acc then acc
      else Map.insert n 0 acc
    ) initDepths allNodes

    -- Relaxation pass: for each node, depth = 1 + max(depth of imports)
    relax depths =
      foldl (\acc (Tuple node imports) ->
        let
          impDepths = Array.mapMaybe (\imp -> Map.lookup imp acc)
                        (Set.toUnfoldable imports :: Array String)
          maxImpDepth = fromMaybe (-1) $ Array.last $ Array.sort impDepths
          newDepth = if Array.length impDepths > 0 then maxImpDepth + 1 else 0
          curDepth = fromMaybe 0 (Map.lookup node acc)
        in Map.insert node (max curDepth newDepth) acc
      ) depths (Map.toUnfoldable dirAdj :: Array (Tuple String (Set String)))

    -- Run enough iterations (V times handles any DAG; cycles stabilize)
    maxIter = min 20 (Array.length allNodes)
    result = iterateN maxIter relax depths0
  in result

iterateN :: forall a. Int -> (a -> a) -> a -> a
iterateN 0 _ x = x
iterateN n f x = iterateN (n - 1) f (f x)

-- | Extract the namespace prefix from a module name.
-- | "CE2.Component.SceneCoordinator" → "CE2.Component"
-- | "CE2.Types" → "CE2" (single-segment after root)
-- | "Main" → "Main"
moduleNamespace :: String -> String
moduleNamespace name =
  let parts = String.split (String.Pattern ".") name
  in case Array.length parts of
    0 -> name
    1 -> name
    _ -> String.joinWith "." (fromMaybe [] (Array.init parts))

-- | Build map: namespace prefix → set of module names
buildNamespaceMap :: Array String -> Map String (Set String)
buildNamespaceMap nodes = foldl (\acc name ->
  let ns = moduleNamespace name
  in Map.alter (Just <<< Set.insert name <<< fromMaybe Set.empty) ns acc
) Map.empty nodes

-- | For each namespace, find the median import depth of its modules
namespaceMedianDepth :: Map String (Set String) -> Map String Int -> Map String Int
namespaceMedianDepth nsMap modDepth =
  Map.mapMaybeWithKey (\_ modules ->
    let
      depths = Array.sort $ Array.mapMaybe (\m -> Map.lookup m modDepth)
                 (Set.toUnfoldable modules :: Array String)
      n = Array.length depths
    in if n == 0 then Nothing
       else depths Array.!! (n / 2)  -- median
  ) nsMap

-- | Group modules by their layer order, returning sorted pairs
groupByLayer :: Map String Int -> Array (Tuple Int (Set String))
groupByLayer moduleLayer =
  let
    groups = foldl (\acc (Tuple name layer) ->
      Map.alter (Just <<< Set.insert name <<< fromMaybe Set.empty) layer acc
    ) (Map.empty :: Map Int (Set String)) (Map.toUnfoldable moduleLayer :: Array (Tuple String Int))
  in sortBy (\a b -> compare (fst a) (fst b))
       (Map.toUnfoldable groups :: Array (Tuple Int (Set String)))

-- | Extract unique namespace prefixes from a set of modules
namespacesInSet :: Set String -> Array String
namespacesInSet modules =
  let nss = Set.fromFoldable $ (Set.toUnfoldable modules :: Array String) <#> moduleNamespace
  in Array.sort (Set.toUnfoldable nss :: Array String)

-- | Suggest a human-readable layer name from namespace prefixes
suggestLayerName :: Array String -> Int -> Int -> String
suggestLayerName namespaces order maxOrder =
  case Array.length namespaces of
    0 -> "Layer " <> show order
    1 -> fromMaybe ("Layer " <> show order) (Array.head namespaces)
    _ ->
      let common = commonPrefix namespaces
      in if String.length common > 2
         then common <> " (Layer " <> show order <> ")"
         else if order == 0 then "Foundation"
              else if order == maxOrder then "Entry"
              else "Layer " <> show order

-- | Find the longest common prefix of an array of strings
commonPrefix :: Array String -> String
commonPrefix strs = case Array.head strs of
  Nothing -> ""
  Just first -> foldl (\pfx s ->
    let len = min (String.length pfx) (String.length s)
        go i = if i >= len then String.take i pfx
               else if String.take (i + 1) pfx == String.take (i + 1) s
                    then go (i + 1)
                    else String.take i pfx
    in go 0
  ) first (fromMaybe [] (Array.tail strs))

-- | Suggest a regex pattern matching the modules in a layer
suggestPattern :: Array String -> String
suggestPattern namespaces =
  let escaped = namespaces <#> \ns ->
        String.replaceAll (String.Pattern ".") (String.Replacement "\\\\.") ns <> "\\\\..*"
  in String.joinWith "|" escaped

-- | Find violations: a module importing from a higher layer number
-- | (higher = further from foundation, more "entry-like")
-- | In clean architecture, imports go downward: layer N imports from layer < N.
-- | Violation: a module in layer X imports from layer Y where Y > X.
findViolations :: Array DirectedImport -> Map String Int -> Array Violation
findViolations dirImports moduleLayer =
  Array.concatMap (\di ->
    let
      fromL = fromMaybe 0 (Map.lookup di.moduleName moduleLayer)
    in Array.mapMaybe (\imp ->
      let toL = fromMaybe 0 (Map.lookup imp moduleLayer)
      -- Violation: importing from a HIGHER layer (upward dependency)
      in if toL > fromL
         then Just { from: di.moduleName, to: imp, fromLayer: fromL, toLayer: toL }
         else Nothing
    ) di.imports
  ) dirImports

-- | Compute treelikeness of graph with violation edges removed
computeTreelikenessWithout :: Dec.SimpleGraph String -> Array Violation -> Number
computeTreelikenessWithout graph violations =
  let
    violationEdges = Set.fromFoldable $ violations <#> \v -> Tuple v.from v.to
    cleanEdges = foldl (\acc (Tuple src targets) ->
      let cleaned = Set.filter (\tgt ->
            not (Set.member (Tuple src tgt) violationEdges) &&
            not (Set.member (Tuple tgt src) violationEdges)
          ) targets
      in if Set.isEmpty cleaned then acc
         else Map.insert src cleaned acc
    ) Map.empty (Map.toUnfoldable graph.edges :: Array (Tuple String (Set String)))
    cleanGraph = { nodes: graph.nodes, edges: cleanEdges }
    cleanMetrics = Dec.decompositionMetrics cleanGraph
  in cleanMetrics.treelikeness

-- | Generate architecture.yml content from discovered layers
generateYaml :: Array DiscoveredLayer -> String
generateYaml layers =
  let
    header = "# Auto-discovered layers from structural decomposition\n"
          <> "# Review and adjust before committing\n"
          <> "layers:\n"
    layerLines = Array.concatMap (\layer ->
      [ "  - name: \"" <> layer.name <> "\""
      , "    order: " <> show layer.order
      , "    pattern: \"" <> layer.pattern <> "\""
      , "    # " <> show (Set.size layer.modules) <> " modules"
      , ""
      ]
    ) layers
  in header <> String.joinWith "\n" layerLines
