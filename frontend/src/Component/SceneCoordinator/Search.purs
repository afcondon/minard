-- | Search typeahead handlers for SceneCoordinator.
-- |
-- | Handles search input (with debounced async search), result sorting,
-- | and dismiss. SearchKeyDown and SearchConfirmIndex stay in the
-- | coordinator since they call handleAction (NavigateTo ...).
module CE2.Component.SceneCoordinator.Search
  ( handleSearchInput
  , handleSearchDismiss
  , searchEntityPriority
  , resolveSearchSelection
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String as String
import Effect.Aff (Milliseconds(..))
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class.Console (log)
import Halogen as H

import CE2.Component.SceneCoordinator.Types (State, Action, Slots, Output)
import CE2.Component.SceneCoordinator.Pure as Pure
import CE2.Data.Loader as Loader
import CE2.Scene (Scene)

-- | Priority for sorting search results: packages > modules > declarations
searchEntityPriority :: Loader.UnifiedSearchResult -> Int
searchEntityPriority r = case r.entityType of
  "package" -> 0
  "module" -> 1
  _ -> 2

-- | Resolve a search selection to a target scene (pure)
resolveSearchSelection :: Array Loader.UnifiedSearchResult -> Int -> Maybe Scene
resolveSearchSelection results idx =
  case Array.index results idx of
    Nothing -> Nothing
    Just result -> Just (Pure.sceneForResult result)

-- | Handle search input with debounced async search
handleSearchInput :: forall m. MonadAff m => String -> H.HalogenM State Action Slots Output m Unit
handleSearchInput query = do
  state <- H.get
  let seqId = state.searchSeqId + 1
  if String.length query < 2
    then
      H.modify_ _ { searchQuery = query, searchResults = [], searchOpen = false, searchSeqId = seqId }
    else do
      H.modify_ _ { searchQuery = query, searchOpen = true, searchSelectedIndex = 0, searchSeqId = seqId }
      -- Fork async search with simple debounce: delay then check if seqId still matches
      void $ H.fork do
        liftAff $ Aff.delay (Milliseconds 150.0)
        currentState <- H.get
        when (currentState.searchSeqId == seqId) do
          result <- liftAff $ Loader.searchAll query
          case result of
            Right results ->
              -- Sort: packages first, modules second, declarations last
              let sorted = Array.sortBy (comparing searchEntityPriority) results
              -- Only apply if seqId still matches (user hasn't typed more)
              in H.modify_ _ { searchResults = sorted, searchSelectedIndex = 0 }
            Left err ->
              log $ "[SceneCoordinator] Search error: " <> err

-- | Handle search dismiss (with small delay for mousedown events)
handleSearchDismiss :: forall m. MonadAff m => H.HalogenM State Action Slots Output m Unit
handleSearchDismiss = do
  -- Small delay to allow mousedown events on results to fire first
  void $ H.fork do
    liftAff $ Aff.delay (Milliseconds 200.0)
    H.modify_ _ { searchOpen = false }
