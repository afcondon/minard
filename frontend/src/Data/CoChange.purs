-- | Co-Change Analysis
-- |
-- | Pure computations on the commit-module incidence matrix.
-- | Reordering, frequency summaries, co-occurrence matrix (M^T * M).
module CE2.Data.CoChange
  ( OrderMode(..)
  , reorderModules
  , moduleFrequencies
  , commitBreadths
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as Set
import Data.Tuple (Tuple(..), fst, snd)
import CE2.Data.Loader as Loader

-- | Module column ordering modes
data OrderMode
  = Alphabetical
  | ByFrequency
  | ByCosimilarity

derive instance eqOrderMode :: Eq OrderMode

-- | Reorder module names according to the selected mode
reorderModules :: OrderMode -> Array Loader.CommitFileEntry -> Array String -> Array String
reorderModules mode commits modules = case mode of
  Alphabetical -> Array.sort modules
  ByFrequency -> sortByFrequency commits modules
  ByCosimilarity -> seriateByCoChange commits modules

-- | Per-module change frequency: how many commits touch each module
moduleFrequencies :: Array Loader.CommitFileEntry -> Map String Int
moduleFrequencies commits =
  foldl (\acc commit ->
    foldl (\acc' modName ->
      Map.alter (Just <<< (_ + 1) <<< fromMaybe 0) modName acc'
    ) acc commit.modules
  ) Map.empty commits

-- | Per-commit breadth: how many modules each commit touches
commitBreadths :: Array Loader.CommitFileEntry -> Map String Int
commitBreadths commits =
  Map.fromFoldable $ commits <#> \c -> Tuple c.hash (Array.length c.modules)

-- =============================================================================
-- Frequency Ordering
-- =============================================================================

-- | Sort modules by change frequency, most-changed first
sortByFrequency :: Array Loader.CommitFileEntry -> Array String -> Array String
sortByFrequency commits modules =
  let freqs = moduleFrequencies commits
      withFreq = modules <#> \m -> Tuple (fromMaybe 0 (Map.lookup m freqs)) m
  in map snd $ Array.sortBy (\a b -> compare (fst b) (fst a)) withFreq

-- =============================================================================
-- Co-Change Seriation
-- =============================================================================

-- | Greedy nearest-neighbor seriation on co-change similarity.
-- | Produces an ordering where modules that frequently change together
-- | are placed adjacent.
seriateByCoChange :: Array Loader.CommitFileEntry -> Array String -> Array String
seriateByCoChange commits modules =
  let
    n = Array.length modules
    coMatrix = buildCoOccurrenceMatrix commits modules
    freqs = moduleFrequencies commits

    -- Start with the highest-frequency module
    startModule = case Array.head (sortByFrequency commits modules) of
      Just m -> m
      Nothing -> fromMaybe "" (Array.head modules)

    -- Greedy: repeatedly pick the unplaced module most similar to the last placed
    go :: Array String -> Set.Set String -> String -> Array String
    go placed remaining lastPlaced =
      if Set.isEmpty remaining then placed
      else
        let
          candidates = Array.fromFoldable remaining
          scored = candidates <#> \c ->
            Tuple (coOccurrence coMatrix lastPlaced c) c
          -- Sort descending by co-occurrence score, break ties by frequency
          best = case Array.head (Array.sortBy compareCandidates scored) of
            Just (Tuple _ m) -> m
            Nothing -> fromMaybe "" (Array.head candidates)
          compareCandidates (Tuple scoreA modA) (Tuple scoreB modB) =
            case compare scoreB scoreA of
              EQ -> compare
                (fromMaybe 0 (Map.lookup modB freqs))
                (fromMaybe 0 (Map.lookup modA freqs))
              ord -> ord
        in go (Array.snoc placed best) (Set.delete best remaining) best
  in
    if n <= 2 then modules
    else go [startModule] (Set.delete startModule (Set.fromFoldable modules)) startModule

-- | Co-occurrence matrix: Map (pair of module names) -> count of commits touching both
-- | Uses sorted pair keys to avoid duplicates
type CoOccurrenceMatrix = Map (Tuple String String) Int

buildCoOccurrenceMatrix :: Array Loader.CommitFileEntry -> Array String -> CoOccurrenceMatrix
buildCoOccurrenceMatrix commits _modules =
  foldl (\acc commit ->
    let mods = Array.sort commit.modules
        pairs = do
          i <- Array.range 0 (Array.length mods - 2)
          j <- Array.range (i + 1) (Array.length mods - 1)
          case Array.index mods i, Array.index mods j of
            Just a, Just b -> [Tuple (Tuple a b) 1]
            _, _ -> []
    in foldl (\acc' (Tuple pair count) ->
        Map.alter (Just <<< (_ + count) <<< fromMaybe 0) pair acc'
      ) acc pairs
  ) Map.empty commits

-- | Look up co-occurrence count for a pair of modules
coOccurrence :: CoOccurrenceMatrix -> String -> String -> Int
coOccurrence matrix a b =
  let key = if a <= b then Tuple a b else Tuple b a
  in fromMaybe 0 (Map.lookup key matrix)
