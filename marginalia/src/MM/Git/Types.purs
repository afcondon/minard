-- | The public Git contract, `git-activity.json`, written by `build-git-activity.mjs`:
-- |
-- |   { period: { from, to }, generatedAt, umbrellas: [{ id, name }],
-- |     totals: { commits, claude, codeAdded, codeDeleted, proseAdded, proseDeleted,
-- |               bulkCommits, repos },
-- |     days: [{ date, total: [6 counts], byUmbrella: [[umbrella id, 6 counts]] }] }
-- |
-- | The six counts are, in order: commits, commits with a Claude co-author, code lines
-- | added, deleted, prose lines added, deleted. Umbrella id 0 is "not under an umbrella
-- | on the map", which is `Nothing` here.
module MM.Git.Types
  ( Counts
  , zeroCounts
  , addCounts
  , UmbrellaCounts
  , DayActivity
  , Totals
  , GitActivityFields
  , GitActivity(..)
  , parseGitActivity
  ) where

import Prelude

import Data.Argonaut.Parser (jsonParser)
import Data.Array (drop, index, length)
import Data.Bifunctor (lmap)
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Date (Date)
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import MM.Timeline.Types (Umbrella, UmbrellaId(..), dateCodec, umbrellaCodec)

type Counts =
  { commits :: Int
  , claude :: Int
  , codeAdded :: Int
  , codeDeleted :: Int
  , proseAdded :: Int
  , proseDeleted :: Int
  }

zeroCounts :: Counts
zeroCounts = { commits: 0, claude: 0, codeAdded: 0, codeDeleted: 0, proseAdded: 0, proseDeleted: 0 }

addCounts :: Counts -> Counts -> Counts
addCounts a b =
  { commits: a.commits + b.commits
  , claude: a.claude + b.claude
  , codeAdded: a.codeAdded + b.codeAdded
  , codeDeleted: a.codeDeleted + b.codeDeleted
  , proseAdded: a.proseAdded + b.proseAdded
  , proseDeleted: a.proseDeleted + b.proseDeleted
  }

type UmbrellaCounts = { umbrella :: Maybe UmbrellaId, counts :: Counts }

type DayActivity =
  { date :: Date
  , total :: Counts
  , byUmbrella :: Array UmbrellaCounts
  }

type Totals =
  { commits :: Int
  , claude :: Int
  , codeAdded :: Int
  , codeDeleted :: Int
  , proseAdded :: Int
  , proseDeleted :: Int
  , bulkCommits :: Int
  , repos :: Int
  }

type GitActivityFields =
  { period :: { from :: Date, to :: Date }
  , generatedAt :: String
  , umbrellas :: Array Umbrella
  , totals :: Totals
  , days :: Array DayActivity
  }

newtype GitActivity = GitActivity GitActivityFields

derive instance Newtype GitActivity _

-- Codecs ---------------------------------------------------------------------

countsFrom :: Array Int -> Maybe Counts
countsFrom xs =
  if length xs /= 6 then Nothing
  else do
    commits <- index xs 0
    claude <- index xs 1
    codeAdded <- index xs 2
    codeDeleted <- index xs 3
    proseAdded <- index xs 4
    proseDeleted <- index xs 5
    pure { commits, claude, codeAdded, codeDeleted, proseAdded, proseDeleted }

countsTo :: Counts -> Array Int
countsTo c = [ c.commits, c.claude, c.codeAdded, c.codeDeleted, c.proseAdded, c.proseDeleted ]

countsCodec :: JsonCodec Counts
countsCodec = CA.prismaticCodec "Counts" countsFrom countsTo (CA.array CA.int)

umbrellaCountsCodec :: JsonCodec UmbrellaCounts
umbrellaCountsCodec = CA.prismaticCodec "UmbrellaCounts" from to (CA.array CA.int)
  where
  from xs = do
    u <- index xs 0
    counts <- countsFrom (drop 1 xs)
    pure { umbrella: if u == 0 then Nothing else Just (UmbrellaId u), counts }
  to uc = [ maybe0 uc.umbrella ] <> countsTo uc.counts
  maybe0 = case _ of
    Just u -> unwrap u
    Nothing -> 0

dayCodec :: JsonCodec DayActivity
dayCodec = CAR.object "DayActivity"
  { date: dateCodec
  , total: countsCodec
  , byUmbrella: CA.array umbrellaCountsCodec
  }

totalsCodec :: JsonCodec Totals
totalsCodec = CAR.object "Totals"
  { commits: CA.int
  , claude: CA.int
  , codeAdded: CA.int
  , codeDeleted: CA.int
  , proseAdded: CA.int
  , proseDeleted: CA.int
  , bulkCommits: CA.int
  , repos: CA.int
  }

gitActivityCodec :: JsonCodec GitActivity
gitActivityCodec = CA.coercible "GitActivity" fields
  where
  fields :: JsonCodec GitActivityFields
  fields = CAR.object "GitActivity"
    { period: CAR.object "Period" { from: dateCodec, to: dateCodec }
    , generatedAt: CA.string
    , umbrellas: CA.array umbrellaCodec
    , totals: totalsCodec
    , days: CA.array dayCodec
    }

parseGitActivity :: String -> Either String GitActivity
parseGitActivity s = do
  j <- jsonParser s
  lmap CA.printJsonDecodeError (CA.decode gitActivityCodec j)
