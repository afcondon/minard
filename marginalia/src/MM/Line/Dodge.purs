-- | One-dimensional label placement: put boxes along an axis without overlap, each
-- | as close as possible to where it wants to be.
-- |
-- | Given boxes in order of their wanted positions, `dodge` returns positions y that
-- | minimise Σ (y_i − want_i)² subject to y_{i+1} ≥ y_i + size_i + gap and y_0 ≥ lo.
-- | Shift each box by the room the boxes before it need (o_i); the constraint becomes
-- | "z = y − o is non-decreasing", and the least-squares fit is isotonic regression,
-- | solved exactly by pool-adjacent-violators. So crowded runs fan out evenly around
-- | their dates instead of all being pushed downwards, and order is always kept.
-- |
-- | Not in hylograph-layout (checked API-INDEX for label placement / dodge / collision,
-- | 2026-10-24); a candidate to move there.
module MM.Line.Dodge (dodge) where

import Prelude

import Data.Array (concatMap, foldl, replicate, scanl, snoc, unsnoc, zipWith)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))

type Block = { sum :: Number, count :: Int }

mean :: Block -> Number
mean b = b.sum / toNumber b.count

dodge :: Number -> Number -> Array { want :: Number, size :: Number } -> Array Number
dodge lo gap items =
  let
    steps = map (\i -> i.size + gap) items
    offsets = [ 0.0 ] <> scanl (+) 0.0 steps
    targets = zipWith (\i o -> i.want - o) items offsets
    blocks = foldl push [] targets
    fitted = concatMap (\b -> replicate b.count (max lo (mean b))) blocks
  in
    zipWith (+) fitted offsets

-- Push a value as its own block, then merge backwards while the order is violated.
push :: Array Block -> Number -> Array Block
push stack t = settle (snoc stack { sum: t, count: 1 })

settle :: Array Block -> Array Block
settle stack = case unsnoc stack of
  Just { init, last } -> case unsnoc init of
    Just { init: rest, last: prev } | mean prev > mean last ->
      settle (snoc rest { sum: prev.sum + last.sum, count: prev.count + last.count })
    _ -> stack
  Nothing -> stack
