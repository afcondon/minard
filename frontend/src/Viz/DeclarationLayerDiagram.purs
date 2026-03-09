-- | Declaration Layer Diagram — Pure Layout
-- |
-- | Computes a layered layout of declarations within a module based on the
-- | directed internal call graph. Layer 0 = leaf functions (call nothing),
-- | Layer N = entry points that transitively call lower layers.
module CE2.Viz.DeclarationLayerDiagram
  ( LayerLayout
  , LayerNode
  , LayerEdge
  , computeLayout
  ) where

import Prelude

import Data.Array as Array
import Data.Array (mapWithIndex)
import Data.Foldable (foldl, foldMap)
import Data.Int (toNumber) as Int
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Number (sqrt) as Num
import Data.Set as Set
import Data.Tuple (Tuple(..))

import CE2.Data.Loader (V2Declaration, V2FunctionCall)
import CE2.Viz.DeclarationArcDiagram (isEffectful)

-- =============================================================================
-- Types
-- =============================================================================

type LayerNode =
  { name :: String
  , kind :: String
  , x :: Number
  , y :: Number
  , r :: Number           -- circle radius (scaled by fan-out)
  , layer :: Int
  , effectful :: Boolean
  }

type LayerEdge =
  { fromName :: String
  , toName :: String
  , fromX :: Number
  , fromY :: Number
  , toX :: Number
  , toY :: Number
  , crossesLayers :: Int  -- how many layers the edge spans (0 = same layer, 1 = adjacent)
  }

type LayerLayout =
  { nodes :: Array LayerNode
  , edges :: Array LayerEdge
  , layers :: Array { layer :: Int, count :: Int }
  , width :: Number
  , height :: Number
  , maxLayer :: Int
  }

-- =============================================================================
-- Layout computation
-- =============================================================================

computeLayout
  :: { moduleName :: String
     , declarations :: Array V2Declaration
     , functionCalls :: Map Int (Array V2FunctionCall)
     , layoutWidth :: Number
     }
  -> LayerLayout
computeLayout { moduleName, declarations, functionCalls, layoutWidth } =
  let
    -- Build kind lookup
    declKindMap = Map.fromFoldable $
      declarations <#> \d -> Tuple d.name d.kind

    declSigMap = Map.fromFoldable $
      declarations <#> \d -> Tuple d.name d.typeSignature

    -- Extract intra-module directed calls
    allCalls :: Array V2FunctionCall
    allCalls = foldMap identity functionCalls

    intraCalls :: Array { from :: String, to :: String }
    intraCalls = allCalls
      # Array.filter (\c -> not c.isCrossModule)
      # Array.filter (\c -> c.calleeModule == moduleName)
      # Array.filter (\c -> c.callerName /= c.calleeName)
      # Array.nubBy (\a b -> compare (Tuple a.callerName a.calleeName) (Tuple b.callerName b.calleeName))
      # map (\c -> { from: c.callerName, to: c.calleeName })

  in
    if Array.null intraCalls then
      { nodes: [], edges: [], layers: [], width: layoutWidth, height: 0.0, maxLayer: 0 }
    else
      let
        -- All names participating in calls
        callNames = intraCalls
          # foldMap (\c -> Set.insert c.from (Set.singleton c.to))
        allNames = Set.toUnfoldable callNames :: Array String

        -- Directed adjacency: caller → Set of callees (within set)
        calleeMap = foldl (\acc c ->
          if Set.member c.to callNames
          then Map.alter (Just <<< Set.insert c.to <<< fromMaybe Set.empty) c.from acc
          else acc
        ) Map.empty intraCalls

        -- Compute layers via iterative relaxation
        -- Layer 0 = declarations that call nothing within the set
        initLayers = foldl (\acc name -> Map.insert name 0 acc) Map.empty allNames

        relax layers =
          foldl (\acc name ->
            let callees = fromMaybe Set.empty (Map.lookup name calleeMap)
                calleeLayers = Array.mapMaybe (\c -> Map.lookup c acc)
                                 (Set.toUnfoldable callees :: Array String)
                maxCallee = fromMaybe (-1) $ Array.last $ Array.sort calleeLayers
                newLayer = if Array.length calleeLayers > 0 then maxCallee + 1 else 0
                curLayer = fromMaybe 0 (Map.lookup name acc)
            in Map.insert name (max curLayer newLayer) acc
          ) layers allNames

        maxIter = min 20 (Array.length allNames)
        finalLayers = iterN maxIter relax initLayers

        -- Max layer
        maxLay = foldl (\acc (Tuple _ l) -> max acc l) 0
                   (Map.toUnfoldable finalLayers :: Array (Tuple String Int))

        -- Group members by layer
        layerMembers = Array.range 0 maxLay <#> \l ->
          { layer: l
          , members: Array.sort $ Array.filter (\name ->
              fromMaybe 0 (Map.lookup name finalLayers) == l
            ) allNames
          }

        -- Layout parameters
        layerHeight = 60.0
        topPad = 30.0
        bottomPad = 30.0
        sidePad = 40.0
        totalHeight = topPad + bottomPad + Int.toNumber (maxLay + 1) * layerHeight

        -- Position nodes: high layers at top, layer 0 at bottom
        nodePositions = foldl (\acc lm ->
          let
            n = Array.length lm.members
            y = topPad + Int.toNumber (maxLay - lm.layer) * layerHeight + layerHeight / 2.0
            usableWidth = layoutWidth - sidePad * 2.0
            spacing = if n <= 1 then 0.0 else usableWidth / Int.toNumber (n - 1)
          in foldl (\a (Tuple i name) ->
            let
              x = if n <= 1 then layoutWidth / 2.0
                  else sidePad + Int.toNumber i * spacing
              fanOut = Set.size (fromMaybe Set.empty (Map.lookup name calleeMap))
              r = max 4.0 (min 10.0 (Num.sqrt (Int.toNumber (fanOut + 1)) * 3.5))
              kind = fromMaybe "value" (Map.lookup name declKindMap)
              sig = maybe Nothing identity (Map.lookup name declSigMap)
              eff = isEffectful sig
            in Map.insert name { name, kind, x, y, r, layer: lm.layer, effectful: eff } a
          ) acc (mapWithIndex Tuple lm.members)
        ) Map.empty layerMembers

        -- Layer summary
        layerSummary = layerMembers <#> \lm ->
          { layer: lm.layer, count: Array.length lm.members }

        -- Edges with positions
        edgeList = Array.mapMaybe (\call ->
          case Map.lookup call.from nodePositions, Map.lookup call.to nodePositions of
            Just fromN, Just toN -> Just
              { fromName: call.from
              , toName: call.to
              , fromX: fromN.x
              , fromY: fromN.y
              , toX: toN.x
              , toY: toN.y
              , crossesLayers: fromN.layer - toN.layer
              }
            _, _ -> Nothing
        ) intraCalls

        nodes = map (\(Tuple _ n) -> n)
                  (Map.toUnfoldable nodePositions :: Array (Tuple String LayerNode))
      in
        { nodes
        , edges: edgeList
        , layers: layerSummary
        , width: layoutWidth
        , height: totalHeight
        , maxLayer: maxLay
        }

-- =============================================================================
-- Helpers
-- =============================================================================

iterN :: forall a. Int -> (a -> a) -> a -> a
iterN 0 _ x = x
iterN n f x = iterN (n - 1) f (f x)

