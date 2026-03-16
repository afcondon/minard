-- | Database.Schema.Core
-- |
-- | Typed row definitions for shared tables (projects, snapshots, annotations).
-- | These tables are used by both Minard and Pausanias.
module Database.Schema.Core
  ( ProjectRow
  , SnapshotRow
  , AnnotationRow
  , AnnotationInsert
  ) where

import Data.Maybe (Maybe)

type ProjectRow =
  { id       :: Int
  , name     :: String
  , repoPath :: Maybe String
  }

type SnapshotRow =
  { id        :: Int
  , projectId :: Int
  , gitHash   :: Maybe String
  , gitRef    :: Maybe String
  , label     :: Maybe String
  }

type AnnotationRow =
  { id         :: Int
  , targetType :: String     -- "module" | "package" | "route" | "component" | "affordance"
  , targetId   :: String
  , targetId2  :: Maybe String
  , kind       :: String     -- "summary" | "architecture" | "quality" | "discoverability" | "flow" etc.
  , value      :: String
  , source     :: String     -- "ai" | "human"
  , confidence :: Number
  , status     :: String     -- "proposed" | "confirmed" | "rejected" | "stale"
  , supersedes :: Maybe Int
  , sessionId  :: Maybe String
  }

type AnnotationInsert =
  { targetType :: String
  , targetId   :: String
  , kind       :: String
  , value      :: String
  , source     :: String
  , supersedes :: Maybe Int
  }
