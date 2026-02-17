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
  , heatColor
  , isEffectful
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldMap)
import Data.Int (round, toNumber)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (abs, sqrt) as Num
import Data.Set as Set
import Data.String.Common (split) as Str
import Data.String.CodeUnits as SCU
import Data.String.Pattern (Pattern(..))
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
  , heat :: Number  -- 0.0 (cold) to 1.0 (hot), based on in-degree
  , effectful :: Boolean  -- return type is Effect, Aff, ST, HalogenM, etc.
  }

type ArcEdge =
  { fromName :: String
  , toName :: String
  , fromX :: Number
  , toX :: Number
  , arcHeight :: Number
  , count :: Int
  , pathD :: String
  , color :: String  -- heat color of the destination node
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

    declSigMap :: Map String (Maybe String)
    declSigMap = Map.fromFoldable $
      declarations # map (\d -> Tuple d.name d.typeSignature)

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

        -- Compute in-degree (number of callers) per node
        inDegreeMap :: Map String Int
        inDegreeMap = Array.foldl (\m c ->
          Map.insertWith add c.to 1 m) Map.empty intraCalls

        maxInDeg = toNumber $ Array.foldl (\mx c ->
          max mx (fromMaybe 0 (Map.lookup c.to inDegreeMap))) 1 intraCalls

        nodeHeat :: String -> Number
        nodeHeat name =
          let deg = toNumber $ fromMaybe 0 (Map.lookup name inDegreeMap)
          in if maxInDeg <= 1.0 then 0.0
             else let r = deg / maxInDeg in r * r

        nodes :: Array ArcNode
        nodes = Array.mapWithIndex (\i name ->
          { name
          , kind: fromMaybe "value" (Map.lookup name declKindMap)
          , x: padding + toNumber i * spacing
          , sourceLine: fromMaybe 0 (Map.lookup name declLineMap)
          , heat: nodeHeat name
          , effectful: isEffectful (join (Map.lookup name declSigMap))
          }) sorted

        -- Build name -> x position map and heat map
        nodeXMap :: Map String Number
        nodeXMap = Map.fromFoldable $
          nodes # map (\n -> Tuple n.name n.x)

        nodeHeatMap :: Map String Number
        nodeHeatMap = Map.fromFoldable $
          nodes # map (\n -> Tuple n.name n.heat)

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
                destHeat = fromMaybe 0.0 (Map.lookup c.to nodeHeatMap)
              in Just
                { fromName: c.from
                , toName: c.to
                , fromX: fx
                , toX: tx
                , arcHeight: arcH
                , count: c.count
                , pathD
                , color: heatColor destHeat
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

-- | Does a type signature's return type start with a known effectful constructor?
isEffectful :: Maybe String -> Boolean
isEffectful Nothing = false
isEffectful (Just sig) =
  let segments = Str.split (Pattern " -> ") sig
      ret = fromMaybe "" (Array.last segments)
      prefixes = ["Effect ", "Effect\n", "Aff ", "Aff\n", "ST ", "HalogenM ", "H.HalogenM ", "MonadEffect ", "MonadAff "]
      exact = ["Effect", "Aff"]
  in Array.any (\p -> SCU.take (SCU.length p) ret == p) prefixes
     || Array.any (\e -> ret == e) exact

-- | Map a heat value (0.0–1.0) to a color on a cool→hot ramp.
-- | 0.0 = slate (#94a3b8), 0.5 = amber (#d97706), 1.0 = red (#dc2626).
heatColor :: Number -> String
heatColor t
  | t <= 0.0 = "rgb(148,163,184)"  -- slate-400
  | t >= 1.0 = "rgb(220,38,38)"    -- red-600
  | t <= 0.5 =
      let f = t * 2.0  -- 0→1 over first half
          r = round (148.0 + (217.0 - 148.0) * f)
          g = round (163.0 + (119.0 - 163.0) * f)
          b = round (184.0 + (6.0 - 184.0) * f)
      in "rgb(" <> show r <> "," <> show g <> "," <> show b <> ")"
  | otherwise =
      let f = (t - 0.5) * 2.0  -- 0→1 over second half
          r = round (217.0 + (220.0 - 217.0) * f)
          g = round (119.0 + (38.0 - 119.0) * f)
          b = round (6.0 + (38.0 - 6.0) * f)
      in "rgb(" <> show r <> "," <> show g <> "," <> show b <> ")"

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
