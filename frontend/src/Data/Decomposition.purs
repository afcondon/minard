-- | Graph decomposition algorithms for structural analysis.
-- |
-- | Copied from hylograph-graph (pending 0.2.0 release) with SimpleGraph inlined.
-- | Algorithms: biconnected components, articulation points, bridges,
-- | bipartiteness, block-cut tree, decomposition metrics.
-- | All O(V + E).
module CE2.Data.Decomposition
  ( SimpleGraph
  , biconnectedComponents
  , articulationPoints
  , bridges
  , detectBipartite
  , BlockCutNode(..)
  , blockCutTree
  , DecompositionMetrics
  , decompositionMetrics
  -- Analysis helpers
  , DecompInfo
  , BlockInfo
  , BlockShape(..)
  , analyzeGraph
  , classifyBlock
  , importsToSimpleGraph
  ) where

import Prelude

import Data.Array (foldl, length, snoc, uncons)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Int as Int
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))

-- | Simple graph as adjacency list (node -> set of targets)
type SimpleGraph node =
  { nodes :: Array node
  , edges :: Map node (Set node)
  }

-- =============================================================================
-- Convert Minard imports to SimpleGraph
-- =============================================================================

-- | Convert V2ModuleImports data to an undirected SimpleGraph.
-- | Each module is a node; import relationships are edges.
importsToSimpleGraph :: Array { moduleName :: String, imports :: Array String } -> SimpleGraph String
importsToSimpleGraph allImports =
  let
    -- Collect all module names (both importers and imported)
    allNames = Set.fromFoldable $ Array.concat
      [ allImports <#> _.moduleName
      , Array.concat $ allImports <#> _.imports
      ]
    nodes = Set.toUnfoldable allNames :: Array String

    -- Build undirected adjacency (import A→B creates edge in both directions)
    edges = foldl (\acc mi ->
      foldl (\acc' imp ->
        Map.alter (Just <<< Set.insert imp <<< fromMaybe Set.empty) mi.moduleName
          (Map.alter (Just <<< Set.insert mi.moduleName <<< fromMaybe Set.empty) imp acc')
      ) acc mi.imports
    ) Map.empty allImports
  in { nodes, edges }

-- =============================================================================
-- Analysis (from demo Render.purs)
-- =============================================================================

type BlockInfo =
  { index :: Int
  , nodes :: Set String
  , edges :: Set (Tuple String String)
  , isBridge :: Boolean
  }

type DecompInfo =
  { nodeBlock :: Map String Int
  , edgeBlock :: Map (Tuple String String) Int
  , aps :: Set String
  , bridgeSet :: Set (Tuple String String)
  , blocks :: Array BlockInfo
  , metrics :: DecompositionMetrics
  , bipartite :: Map Int (Either (Array String) { partA :: Set String, partB :: Set String })
  }

data BlockShape
  = ShapeDense
  | ShapeBipartite
  | ShapeTree
  | ShapeCycle
  | ShapeSparse

derive instance Eq BlockShape

-- | Run all decomposition algorithms and return unified analysis
analyzeGraph :: SimpleGraph String -> DecompInfo
analyzeGraph graph =
  let
    comps = biconnectedComponents graph
    aps_ = articulationPoints graph
    br = bridges graph
    met = decompositionMetrics graph

    bridgeSet_ = Set.fromFoldable $ Array.concat $
      br <#> \(Tuple a b) -> [Tuple a b, Tuple b a]

    blocks_ = Array.mapWithIndex (\i edgeSet ->
      let
        ns = foldl (\acc (Tuple a b) -> Set.insert a (Set.insert b acc)) Set.empty
               (Set.toUnfoldable edgeSet :: Array (Tuple String String))
      in
        { index: i
        , nodes: ns
        , edges: edgeSet
        , isBridge: Set.size edgeSet == 1
        }
    ) comps

    nodeBlock_ = foldl (\acc block ->
      foldl (\a n -> Map.insert n block.index a) acc
        (Set.toUnfoldable block.nodes :: Array String)
    ) Map.empty blocks_

    edgeBlock_ = foldl (\acc block ->
      foldl (\a edge@(Tuple x y) ->
        Map.insert edge block.index (Map.insert (Tuple y x) block.index a)
      ) acc (Set.toUnfoldable block.edges :: Array (Tuple String String))
    ) Map.empty blocks_

    -- Test bipartiteness per non-bridge block
    bipartite_ = foldl (\acc block ->
      if block.isBridge then acc
      else
        let
          blockNodes = Set.toUnfoldable block.nodes :: Array String
          blockEdges = Map.fromFoldable $ blockNodes <#> \n ->
            Tuple n (Set.intersection
              (fromMaybe Set.empty $ Map.lookup n graph.edges)
              block.nodes)
          subgraph = { nodes: blockNodes, edges: blockEdges }
        in Map.insert block.index (detectBipartite subgraph) acc
    ) Map.empty blocks_
  in
    { nodeBlock: nodeBlock_
    , edgeBlock: edgeBlock_
    , aps: aps_
    , bridgeSet: bridgeSet_
    , blocks: blocks_
    , metrics: met
    , bipartite: bipartite_
    }

-- | Classify a block's structural shape for visualization
classifyBlock :: DecompInfo -> BlockInfo -> BlockShape
classifyBlock info block
  | block.isBridge = ShapeTree
  | otherwise =
    let
      n = Set.size block.nodes
      e = Set.size block.edges
      maxEdges = n * (n - 1) / 2
      density = if maxEdges == 0 then 0.0
                else Int.toNumber e / Int.toNumber maxEdges
    in
      if density > 0.5 then ShapeDense
      else case Map.lookup block.index info.bipartite of
        Just (Right _) | n > 3 -> ShapeBipartite
        _ ->
          if e == n then ShapeCycle
          else ShapeSparse

-- =============================================================================
-- Undirected adjacency helper
-- =============================================================================

undirectedAdj :: forall node. Ord node => SimpleGraph node -> Map node (Set node)
undirectedAdj graph =
  foldl (\acc (Tuple src targets) ->
    let
      acc' = Map.alter (Just <<< Set.union targets <<< fromMaybe Set.empty) src acc
      acc'' = foldl (\a tgt ->
        Map.alter (Just <<< Set.insert src <<< fromMaybe Set.empty) tgt a
      ) acc' (Set.toUnfoldable targets :: Array node)
    in acc''
  ) Map.empty (Map.toUnfoldable graph.edges :: Array (Tuple node (Set node)))

neighbors :: forall node. Ord node => node -> Map node (Set node) -> Array node
neighbors v adj = case Map.lookup v adj of
  Nothing -> []
  Just s -> Set.toUnfoldable s

-- =============================================================================
-- Biconnected Components + Articulation Points (Tarjan's bridge-finding DFS)
-- =============================================================================

type BCCState node =
  { timer :: Int
  , disc :: Map node Int
  , low :: Map node Int
  , parent :: Map node node
  , stack :: Array (Tuple node node)
  , components :: Array (Set (Tuple node node))
  , aps :: Set node
  }

biconnectedComponents :: forall node. Ord node => SimpleGraph node -> Array (Set (Tuple node node))
biconnectedComponents graph =
  let adj = undirectedAdj graph
      result = foldl (\state node ->
        if Map.member node state.disc
        then state
        else bccDFS adj node state
      ) initState graph.nodes
  in result.components
  where
  initState :: BCCState node
  initState =
    { timer: 0
    , disc: Map.empty
    , low: Map.empty
    , parent: Map.empty
    , stack: []
    , components: []
    , aps: Set.empty
    }

articulationPoints :: forall node. Ord node => SimpleGraph node -> Set node
articulationPoints graph =
  let adj = undirectedAdj graph
      result = foldl (\state node ->
        if Map.member node state.disc
        then state
        else bccDFS adj node state
      ) initState graph.nodes
  in result.aps
  where
  initState :: BCCState node
  initState =
    { timer: 0
    , disc: Map.empty
    , low: Map.empty
    , parent: Map.empty
    , stack: []
    , components: []
    , aps: Set.empty
    }

bccDFS :: forall node. Ord node => Map node (Set node) -> node -> BCCState node -> BCCState node
bccDFS adj u state0 =
  let
    state1 = state0
      { timer = state0.timer + 1
      , disc = Map.insert u state0.timer state0.disc
      , low = Map.insert u state0.timer state0.low
      }
    nbrs = neighbors u adj
    childCount = 0
    isRoot = not (Map.member u state0.parent)
  in
    processNeighbors adj u nbrs isRoot childCount state1

processNeighbors :: forall node. Ord node =>
  Map node (Set node) -> node -> Array node -> Boolean -> Int -> BCCState node -> BCCState node
processNeighbors adj u nbrs isRoot childCount state =
  case uncons nbrs of
    Nothing ->
      if isRoot && childCount >= 2
      then state { aps = Set.insert u state.aps }
      else state
    Just { head: v, tail: rest } ->
      let uDisc = fromMaybe 0 $ Map.lookup u state.disc
      in case Map.lookup v state.disc of
        Nothing ->
          let
            state' = state
              { parent = Map.insert v u state.parent
              , stack = snoc state.stack (Tuple u v)
              }
            state'' = bccDFS adj v state'
            uLow = fromMaybe 0 $ Map.lookup u state''.low
            vLow = fromMaybe 0 $ Map.lookup v state''.low
            state3 = state'' { low = Map.insert u (min uLow vLow) state''.low }
            state4 =
              if not isRoot && vLow >= uDisc
              then
                let popped = popEdgesUntil state3.stack (Tuple u v)
                in state3
                    { aps = Set.insert u state3.aps
                    , stack = popped.remaining
                    , components = snoc state3.components popped.component
                    }
              else if isRoot
              then
                let popped = popEdgesUntil state3.stack (Tuple u v)
                in state3
                    { stack = popped.remaining
                    , components = snoc state3.components popped.component
                    }
              else state3
          in processNeighbors adj u rest isRoot (childCount + 1) state4
        Just vDisc ->
          let parentOfU = Map.lookup u state.parent
          in if Just v /= parentOfU && vDisc < uDisc
             then
               let
                 uLow = fromMaybe 0 $ Map.lookup u state.low
                 state' = state
                   { low = Map.insert u (min uLow vDisc) state.low
                   , stack = snoc state.stack (Tuple u v)
                   }
               in processNeighbors adj u rest isRoot childCount state'
             else
               processNeighbors adj u rest isRoot childCount state

popEdgesUntil :: forall node. Ord node =>
  Array (Tuple node node) -> Tuple node node -> { component :: Set (Tuple node node), remaining :: Array (Tuple node node) }
popEdgesUntil stack target = go stack Set.empty
  where
  go stk acc = case Array.last stk of
    Nothing -> { component: acc, remaining: [] }
    Just edge ->
      let stk' = fromMaybe [] (Array.init stk)
          acc' = Set.insert edge acc
      in if edge == target
         then { component: acc', remaining: stk' }
         else go stk' acc'

-- =============================================================================
-- Bridge Detection
-- =============================================================================

bridges :: forall node. Ord node => SimpleGraph node -> Array (Tuple node node)
bridges graph =
  let comps = biconnectedComponents graph
  in Array.concatMap (\comp ->
    if Set.size comp == 1
    then Set.toUnfoldable comp
    else []
  ) comps

-- =============================================================================
-- Bipartiteness Testing (BFS 2-coloring)
-- =============================================================================

detectBipartite :: forall node. Ord node => SimpleGraph node ->
  Either (Array node) { partA :: Set node, partB :: Set node }
detectBipartite graph =
  let adj = undirectedAdj graph
  in foldl (\result node ->
    case result of
      Left _ -> result
      Right parts ->
        if Map.member node parts.colors
        then result
        else
          case bfsColor adj node parts.colors of
            Left cycle -> Left cycle
            Right colors' ->
              let
                partA' = foldl (\s (Tuple n c) -> if c then Set.insert n s else s) parts.partA
                           (Map.toUnfoldable colors' :: Array (Tuple node Boolean))
                partB' = foldl (\s (Tuple n c) -> if not c then Set.insert n s else s) parts.partB
                           (Map.toUnfoldable colors' :: Array (Tuple node Boolean))
              in Right { colors: colors', partA: partA', partB: partB' }
  ) (Right { colors: Map.empty, partA: Set.empty, partB: Set.empty }) graph.nodes
  <#> \parts -> { partA: parts.partA, partB: parts.partB }

bfsColor :: forall node. Ord node =>
  Map node (Set node) -> node -> Map node Boolean -> Either (Array node) (Map node Boolean)
bfsColor adj start colors0 =
  go [start] (Map.insert start true colors0)
  where
  go queue colors =
    case uncons queue of
      Nothing -> Right colors
      Just { head: v, tail: rest } ->
        let
          vColor = fromMaybe true $ Map.lookup v colors
          nbrs = neighbors v adj
        in processColorNeighbors adj v vColor nbrs rest colors

processColorNeighbors :: forall node. Ord node =>
  Map node (Set node) -> node -> Boolean -> Array node -> Array node -> Map node Boolean -> Either (Array node) (Map node Boolean)
processColorNeighbors adj v vColor nbrs queue colors =
  case uncons nbrs of
    Nothing -> go queue colors
      where
      go q c = case uncons q of
        Nothing -> Right c
        Just { head: u, tail: rest } ->
          let
            uColor = fromMaybe true $ Map.lookup u c
            ns = neighbors u adj
          in processColorNeighbors adj u uColor ns rest c
    Just { head: w, tail: rest } ->
      case Map.lookup w colors of
        Nothing ->
          processColorNeighbors adj v vColor rest (snoc queue w) (Map.insert w (not vColor) colors)
        Just wColor ->
          if wColor == vColor
          then Left [v, w]
          else processColorNeighbors adj v vColor rest queue colors

-- =============================================================================
-- Block-Cut Tree
-- =============================================================================

data BlockCutNode node
  = Block Int (Set node)
  | CutVertex node

blockCutTree :: forall node. Ord node => SimpleGraph node ->
  { blocks :: Array (Set node)
  , cutVertices :: Set node
  , tree :: Array { from :: Int, to :: Int }
  }
blockCutTree graph =
  let
    edgeComps = biconnectedComponents graph
    aps_ = articulationPoints graph

    blocks_ = map (\edgeSet ->
      foldl (\acc (Tuple a b) -> Set.insert a (Set.insert b acc)) Set.empty
        (Set.toUnfoldable edgeSet :: Array (Tuple node node))
    ) edgeComps

    nBlocks = length blocks_
    apArray = Set.toUnfoldable aps_ :: Array node
    apIndices = foldl (\m (Tuple i ap) -> Map.insert ap (nBlocks + i) m) Map.empty
                  (Array.mapWithIndex (\i ap -> Tuple i ap) apArray)

    treeEdges = Array.concatMap (\(Tuple apNode apIdx) ->
      Array.mapWithIndex (\blockIdx block ->
        if Set.member apNode block
        then [{ from: blockIdx, to: apIdx }]
        else []
      ) blocks_ # Array.concat
    ) (Map.toUnfoldable apIndices :: Array (Tuple node Int))
  in
    { blocks: blocks_, cutVertices: aps_, tree: treeEdges }

-- =============================================================================
-- Decomposition Metrics
-- =============================================================================

type DecompositionMetrics =
  { biconnectedComponentCount :: Int
  , articulationPointCount :: Int
  , bridgeCount :: Int
  , isBipartite :: Boolean
  , isTree :: Boolean
  , maxBlockSize :: Int
  , treelikeness :: Number
  }

decompositionMetrics :: forall node. Ord node => SimpleGraph node -> DecompositionMetrics
decompositionMetrics graph =
  let
    comps = biconnectedComponents graph
    aps_ = articulationPoints graph
    br = bridges graph
    bip = case detectBipartite graph of
      Left _ -> false
      Right _ -> true
    edgeCount = foldl (\acc (Tuple _ targets) -> acc + Set.size targets) 0
                  (Map.toUnfoldable graph.edges :: Array (Tuple node (Set node))) / 2
    nodeCount = length graph.nodes
    blockSizes = map (\edgeSet ->
      Set.size $ foldl (\acc (Tuple a b) -> Set.insert a (Set.insert b acc)) Set.empty
        (Set.toUnfoldable edgeSet :: Array (Tuple node node))
    ) comps
    maxBlock = fromMaybe 0 $ Array.last $ Array.sort blockSizes
  in
    { biconnectedComponentCount: length comps
    , articulationPointCount: Set.size aps_
    , bridgeCount: length br
    , isBipartite: bip
    , isTree: edgeCount == nodeCount - 1 && nodeCount > 0
    , maxBlockSize: maxBlock
    , treelikeness: if edgeCount == 0 then 1.0
                    else Int.toNumber (length br) / Int.toNumber edgeCount
    }
