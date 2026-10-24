-- | The public timeline contract, `timeline-public.json`, written by
-- | `build-public-timeline.mjs`:
-- |
-- |   { period: { from, to }, generatedAt, umbrellas: [{ id, name }],
-- |     threads: [{ name, what, kind, fate, first, last, days[], umbrella,
-- |                 project: { id, name } | null, successor: name | null,
-- |                 cover?, note?, image? }] }
-- |
-- | Dates are "YYYY-MM-DD" on the wire and `Date` here. Fate and kind are closed
-- | sets, so they are ADTs, and an unknown string fails the decode rather than
-- | drawing as something it isn't.
module MM.Timeline.Types
  ( Fate(..)
  , Kind(..)
  , UmbrellaId(..)
  , ThreadName(..)
  , Umbrella
  , MapProject
  , ThreadFields
  , Thread(..)
  , TimelineFields
  , Timeline(..)
  , allFates
  , fateLabel
  , kindLabel
  , dayNumber
  , dateFromDayNumber
  , parseTimeline
  ) where

import Prelude

import Data.Argonaut.Parser (jsonParser)
import Data.Array (index, length)
import Data.Bifunctor (lmap)
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Compat as CAC
import Data.Codec.Argonaut.Record as CAR
import Data.Date (Date, adjust, diff, exactDate, day, month, year)
import Data.Either (Either)
import Data.Enum (fromEnum, toEnum)
import Data.Int (floor, fromString, toNumber)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.String (Pattern(..), split)
import Data.Time.Duration (Days(..))

-- | What became of a thread. The order is the legend's order: finished, still
-- | going, then the four ways of not finishing, then not known.
data Fate = Shipped | Active | Parked | HalfDone | Abandoned | Superseded | Unknown

derive instance Eq Fate
derive instance Ord Fate

allFates :: Array Fate
allFates = [ Shipped, Active, Parked, HalfDone, Abandoned, Superseded, Unknown ]

fateLabel :: Fate -> String
fateLabel = case _ of
  Shipped -> "shipped"
  Active -> "active"
  Parked -> "parked"
  HalfDone -> "half-done"
  Abandoned -> "abandoned"
  Superseded -> "superseded"
  Unknown -> "unknown"

fateFromLabel :: String -> Maybe Fate
fateFromLabel = case _ of
  "shipped" -> Just Shipped
  "active" -> Just Active
  "parked" -> Just Parked
  "half-done" -> Just HalfDone
  "abandoned" -> Just Abandoned
  "superseded" -> Just Superseded
  "unknown" -> Just Unknown
  _ -> Nothing

data Kind = Repo | Feature | Idea | Infra | Doc

derive instance Eq Kind

kindLabel :: Kind -> String
kindLabel = case _ of
  Repo -> "repo"
  Feature -> "feature"
  Idea -> "idea"
  Infra -> "infra"
  Doc -> "doc"

kindFromLabel :: String -> Maybe Kind
kindFromLabel = case _ of
  "repo" -> Just Repo
  "feature" -> Just Feature
  "idea" -> Just Idea
  "infra" -> Just Infra
  "doc" -> Just Doc
  _ -> Nothing

-- | A top-level project on the map: the key that joins the two views.
newtype UmbrellaId = UmbrellaId Int

derive instance Newtype UmbrellaId _
derive instance Eq UmbrellaId
derive instance Ord UmbrellaId

newtype ThreadName = ThreadName String

derive instance Newtype ThreadName _
derive instance Eq ThreadName
derive instance Ord ThreadName

type Umbrella = { id :: UmbrellaId, name :: String }

-- | The project on the map a thread joined, when that is below its umbrella.
type MapProject = { id :: Int, name :: String }

type ThreadFields =
  { name :: ThreadName
  , what :: String
  , kind :: Kind
  , fate :: Fate
  , first :: Date
  , last :: Date
  , days :: Array Date
  , umbrella :: UmbrellaId
  , project :: Maybe MapProject
  , successor :: Maybe ThreadName
  , cover :: Maybe String -- screenshot of the thread's own map project (Line view)
  , note :: Maybe String -- Andrew's hand-written note, from overrides.json (Line view)
  , image :: Maybe String -- Andrew's chosen image, from overrides.json (Line view)
  }

newtype Thread = Thread ThreadFields

derive instance Newtype Thread _

type TimelineFields =
  { period :: { from :: Date, to :: Date }
  , generatedAt :: String
  , umbrellas :: Array Umbrella
  , threads :: Array Thread
  }

newtype Timeline = Timeline TimelineFields

derive instance Newtype Timeline _

-- Dates ----------------------------------------------------------------------

-- | Days since 1970-01-01. The timeline's x axis is measured in these.
dayNumber :: Date -> Int
dayNumber d = case epoch of
  Just e -> floor (unDays (diff d e))
  Nothing -> 0
  where
  unDays (Days n) = n

dateFromDayNumber :: Int -> Maybe Date
dateFromDayNumber n = epoch >>= adjust (Days (toNumber n))

epoch :: Maybe Date
epoch = do
  y <- toEnum 1970
  m <- toEnum 1
  d <- toEnum 1
  exactDate y m d

isoFromString :: String -> Maybe Date
isoFromString s =
  let parts = split (Pattern "-") s
  in
    if length parts /= 3 then Nothing
    else do
      y <- index parts 0 >>= fromString >>= toEnum
      m <- index parts 1 >>= fromString >>= toEnum
      d <- index parts 2 >>= fromString >>= toEnum
      exactDate y m d

isoToString :: Date -> String
isoToString d =
  show (fromEnum (year d)) <> "-" <> pad (fromEnum (month d)) <> "-" <> pad (fromEnum (day d))
  where
  pad n = if n < 10 then "0" <> show n else show n

-- Codecs ---------------------------------------------------------------------

dateCodec :: JsonCodec Date
dateCodec = CA.prismaticCodec "Date" isoFromString isoToString CA.string

fateCodec :: JsonCodec Fate
fateCodec = CA.prismaticCodec "Fate" fateFromLabel fateLabel CA.string

kindCodec :: JsonCodec Kind
kindCodec = CA.prismaticCodec "Kind" kindFromLabel kindLabel CA.string

umbrellaIdCodec :: JsonCodec UmbrellaId
umbrellaIdCodec = CA.coercible "UmbrellaId" CA.int

threadNameCodec :: JsonCodec ThreadName
threadNameCodec = CA.coercible "ThreadName" CA.string

umbrellaCodec :: JsonCodec Umbrella
umbrellaCodec = CAR.object "Umbrella" { id: umbrellaIdCodec, name: CA.string }

mapProjectCodec :: JsonCodec MapProject
mapProjectCodec = CAR.object "MapProject" { id: CA.int, name: CA.string }

threadCodec :: JsonCodec Thread
threadCodec = CA.coercible "Thread" fields
  where
  fields :: JsonCodec ThreadFields
  fields = CAR.object "Thread"
    { name: threadNameCodec
    , what: CA.string
    , kind: kindCodec
    , fate: fateCodec
    , first: dateCodec
    , last: dateCodec
    , days: CA.array dateCodec
    , umbrella: umbrellaIdCodec
    , project: CAC.maybe mapProjectCodec
    , successor: CAC.maybe threadNameCodec
    , cover: CAR.optional CA.string
    , note: CAR.optional CA.string
    , image: CAR.optional CA.string
    }

timelineCodec :: JsonCodec Timeline
timelineCodec = CA.coercible "Timeline" fields
  where
  fields :: JsonCodec TimelineFields
  fields = CAR.object "Timeline"
    { period: CAR.object "Period" { from: dateCodec, to: dateCodec }
    , generatedAt: CA.string
    , umbrellas: CA.array umbrellaCodec
    , threads: CA.array threadCodec
    }

parseTimeline :: String -> Either String Timeline
parseTimeline s = do
  j <- jsonParser s
  lmap CA.printJsonDecodeError (CA.decode timelineCodec j)
