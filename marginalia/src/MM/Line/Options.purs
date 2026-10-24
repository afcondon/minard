-- | The Line view's folding and filtering state, kept in the URL so a view can be
-- | linked: `#line/125?fold=2025-10,2025-11&fates=quiet,shipped&min=12`.
-- | Defaults (nothing folded, every fate, 8 days) print as nothing.
module MM.Line.Options
  ( FateGroup(..)
  , allGroups
  , groupLabel
  , groupOf
  , MonthKey(..)
  , LineOptions
  , defaultOptions
  , densitySteps
  , printOptions
  , parseOptions
  ) where

import Prelude

import Data.Array (catMaybes, elem, filter, find, null)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Set (Set)
import Data.Set as Set
import Data.String (Pattern(..), joinWith, split)
import MM.Timeline.Types (Fate(..))

-- | The fate filter's chips. Half-done and abandoned share one, "went quiet",
-- | because the Line view marks both at the same moment; unknown threads (few) ride
-- | with active, since both are annotated where they started.
data FateGroup = QuietGroup | ShippedGroup | SupersededGroup | ParkedGroup | ActiveGroup

derive instance Eq FateGroup
derive instance Ord FateGroup

allGroups :: Array FateGroup
allGroups = [ QuietGroup, ShippedGroup, SupersededGroup, ParkedGroup, ActiveGroup ]

groupLabel :: FateGroup -> String
groupLabel = case _ of
  QuietGroup -> "went quiet"
  ShippedGroup -> "shipped"
  SupersededGroup -> "superseded"
  ParkedGroup -> "parked"
  ActiveGroup -> "active"

groupKey :: FateGroup -> String
groupKey = case _ of
  QuietGroup -> "quiet"
  ShippedGroup -> "shipped"
  SupersededGroup -> "superseded"
  ParkedGroup -> "parked"
  ActiveGroup -> "active"

groupOf :: Fate -> FateGroup
groupOf = case _ of
  HalfDone -> QuietGroup
  Abandoned -> QuietGroup
  Shipped -> ShippedGroup
  Superseded -> SupersededGroup
  Parked -> ParkedGroup
  Active -> ActiveGroup
  Unknown -> ActiveGroup

-- | A calendar month, "YYYY-MM".
newtype MonthKey = MonthKey String

derive instance Newtype MonthKey _
derive instance Eq MonthKey
derive instance Ord MonthKey

type LineOptions =
  { folded :: Set MonthKey
  , fates :: Set FateGroup
  , minDays :: Int -- density: active days a non-quiet thread needs for a card
  }

densitySteps :: Array Int
densitySteps = [ 5, 8, 12, 20 ]

defaultOptions :: LineOptions
defaultOptions = { folded: Set.empty, fates: Set.fromFoldable allGroups, minDays: 8 }

printOptions :: LineOptions -> String
printOptions o =
  joinWith "&" $ catMaybes
    [ if Set.isEmpty o.folded then Nothing
      else Just ("fold=" <> joinWith "," (map unwrap (Set.toUnfoldable o.folded)))
    , if o.fates == defaultOptions.fates then Nothing
      else if Set.isEmpty o.fates then Just "fates=none"
      else Just ("fates=" <> joinWith "," (map groupKey (filter (\g -> Set.member g o.fates) allGroups)))
    , if o.minDays == defaultOptions.minDays then Nothing
      else Just ("min=" <> show o.minDays)
    ]

parseOptions :: String -> LineOptions
parseOptions q =
  let
    pairs = catMaybes $ map pair (split (Pattern "&") q)
    pair kv = case split (Pattern "=") kv of
      [ k, v ] -> Just { k, v }
      _ -> Nothing
    get k = _.v <$> find (\p -> p.k == k) pairs
    list v = filter (_ /= "") (split (Pattern ",") v)
    fromKey k = find (\g -> groupKey g == k) allGroups
  in
    { folded: maybe Set.empty (Set.fromFoldable <<< map MonthKey <<< list) (get "fold")
    , fates: maybe defaultOptions.fates
        (\v -> let gs = catMaybes (map fromKey (list v)) in if null gs && v /= "none" then defaultOptions.fates else Set.fromFoldable gs)
        (get "fates")
    , minDays: fromMaybe defaultOptions.minDays do
        v <- get "min"
        n <- fromString v
        if elem n densitySteps then Just n else Nothing
    }
