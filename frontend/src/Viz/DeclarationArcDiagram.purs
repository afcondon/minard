-- | Declaration Arc Diagram — Pure Layout
-- |
-- | Computes node positions and arc paths for an intra-module
-- | function call diagram. No Effect, no DOM — takes declarations
-- | + calls, returns positioned nodes and pre-computed SVG path strings.
module CE2.Viz.DeclarationArcDiagram
  ( ArcLayout
  , ArcNode
  , ArcEdge
  , computeLayout
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldMap)
import Data.Int (toNumber)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (abs) as Num
import Data.Set as Set
import Data.Tuple (Tuple(..))

import CE2.Data.Loader (V2Declaration, V2FunctionCall)

-- =============================================================================
-- Types
-- =============================================================================

type ArcNode =
  { name :: String
  , kind :: String
  , x :: Number
  , sourceLine :: Int
  }

type ArcEdge =
  { fromName :: String
  , toName :: String
  , fromX :: Number
  , toX :: Number
  , arcHeight :: Number
  , count :: Int
  , pathD :: String
  }

type ArcLayout =
  { nodes :: Array ArcNode
  , edges :: Array ArcEdge
  , width :: Number
  , height :: Number
  , baselineY :: Number
  }

-- =============================================================================
-- Layout computation
-- =============================================================================

-- | Compute an arc diagram layout from declarations and function calls.
-- | Returns an empty layout (no nodes/edges) when there are no intra-module calls.
computeLayout
  :: { moduleName :: String
     , declarations :: Array V2Declaration
     , functionCalls :: Map Int (Array V2FunctionCall)
     , layoutWidth :: Number
     }
  -> ArcLayout
computeLayout { moduleName, declarations, functionCalls, layoutWidth } =
  let
    -- Build a kind lookup from exported declarations
    declKindMap :: Map String String
    declKindMap = Map.fromFoldable $
      declarations # map (\d -> Tuple d.name d.kind)

    declLineMap :: Map String Int
    declLineMap = Map.fromFoldable $
      declarations # map (\d -> Tuple d.name (declSourceLine d))

    -- Extract intra-module calls: flatten all calls, keep only calls belonging
    -- to this module (calleeModule matches moduleName for intra-module calls),
    -- exclude self-calls
    allCalls :: Array V2FunctionCall
    allCalls = foldMap identity functionCalls

    intraCalls :: Array { from :: String, to :: String, count :: Int }
    intraCalls = allCalls
      # Array.filter (\c -> not c.isCrossModule)
      # Array.filter (\c -> c.calleeModule == moduleName)
      # Array.filter (\c -> c.callerName /= c.calleeName)
      # map (\c -> { from: c.callerName, to: c.calleeName, count: c.callCount })
      # deduplicateCalls

  in
    if Array.null intraCalls then
      { nodes: [], edges: [], width: layoutWidth, height: 0.0, baselineY: 0.0 }
    else
      let
        -- Collect all unique names participating in intra-module calls
        callNames = intraCalls
          # foldMap (\c -> Set.insert c.from (Set.singleton c.to))
        allNames = Set.toUnfoldable callNames :: Array String

        -- Sort names: exported declarations first (by source line), then
        -- internal functions alphabetically
        sorted = allNames # Array.sortBy (\a b ->
          case Tuple (Map.lookup a declLineMap) (Map.lookup b declLineMap) of
            Tuple (Just la) (Just lb) -> compare la lb
            Tuple (Just _) Nothing -> LT
            Tuple Nothing (Just _) -> GT
            Tuple Nothing Nothing -> compare a b)

        padding = 40.0
        usableWidth = layoutWidth - padding * 2.0
        nodeCount = Array.length sorted
        spacing = if nodeCount <= 1 then 0.0
                  else usableWidth / toNumber (nodeCount - 1)

        nodes :: Array ArcNode
        nodes = Array.mapWithIndex (\i name ->
          { name
          , kind: fromMaybe "value" (Map.lookup name declKindMap)
          , x: padding + toNumber i * spacing
          , sourceLine: fromMaybe 0 (Map.lookup name declLineMap)
          }) sorted

        -- Build name -> x position map
        nodeXMap :: Map String Number
        nodeXMap = Map.fromFoldable $
          nodes # map (\n -> Tuple n.name n.x)

        -- Baseline sits below arcs, with room for labels below
        labelSpace = 80.0
        maxArcH = 95.0
        baseY = maxArcH + 10.0
        totalHeight = baseY + labelSpace

        edges :: Array ArcEdge
        edges = intraCalls # Array.mapMaybe (\c ->
          case Tuple (Map.lookup c.from nodeXMap) (Map.lookup c.to nodeXMap) of
            Tuple (Just fx) (Just tx) ->
              let
                span = Num.abs (tx - fx)
                arcH = 15.0 + (span / layoutWidth) * 80.0
                midX = (fx + tx) / 2.0
                pathD = "M " <> show fx <> " " <> show baseY
                     <> " Q " <> show midX <> " " <> show (baseY - arcH)
                     <> " " <> show tx <> " " <> show baseY
              in Just
                { fromName: c.from
                , toName: c.to
                , fromX: fx
                , toX: tx
                , arcHeight: arcH
                , count: c.count
                , pathD
                }
            _ -> Nothing
          )
      in
        { nodes, edges, width: layoutWidth, height: totalHeight, baselineY: baseY }

-- =============================================================================
-- Helpers
-- =============================================================================

-- | Extract the start line from a declaration's source span, defaulting to 0.
declSourceLine :: V2Declaration -> Int
declSourceLine d = case d.sourceSpan of
  Just ss -> fromMaybe 0 (Array.index ss.start 0)
  Nothing -> 0

-- | Deduplicate calls by (from, to), summing counts.
deduplicateCalls
  :: Array { from :: String, to :: String, count :: Int }
  -> Array { from :: String, to :: String, count :: Int }
deduplicateCalls calls =
  let
    grouped = Array.foldl (\m c ->
      let key = c.from <> "\x00" <> c.to
      in case Map.lookup key m of
        Just existing -> Map.insert key (existing { count = existing.count + c.count }) m
        Nothing -> Map.insert key c m
      ) Map.empty calls
  in Array.fromFoldable (Map.values grouped)
