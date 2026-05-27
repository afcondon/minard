-- | Annotation CRUD API Endpoints
-- |
-- | Provides GET/POST/PATCH operations for the annotation layer.
-- | Annotations attach semantic metadata (tags, summaries, quality notes)
-- | to codebase entities (declarations, modules, packages, relationships).
module API.Annotations
  ( list
  , get
  , create
  , update
  , report
  ) where

import Prelude

import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toMaybe, toNullable)
import Database.DuckDB (Database, queryAll, queryAllParams, run, firstRow)
import Effect.Aff (Aff)
import Foreign (Foreign, unsafeToForeign)
import Foreign.Object (Object)
import Foreign.Object as Object
import HTTPurple (Response, ok', notFound, badRequest', response')
import HTTPurple.Headers (ResponseHeaders, headers)
import HTTPurple.Status as Status

-- | JSON content type header with CORS
jsonHeaders :: ResponseHeaders
jsonHeaders = headers { "Content-Type": "application/json", "Access-Control-Allow-Origin": "*" }

-- | Markdown content type header with CORS
markdownHeaders :: ResponseHeaders
markdownHeaders = headers { "Content-Type": "text/markdown; charset=utf-8", "Access-Control-Allow-Origin": "*" }

-- =============================================================================
-- FFI Imports
-- =============================================================================

foreign import buildAnnotationsJson :: Array Foreign -> String
foreign import buildAnnotationJson :: Foreign -> String
foreign import buildReportMarkdown :: Array Foreign -> Array Foreign -> String
foreign import parseAnnotationBody :: String -> Nullable Foreign
foreign import validateCreateFields :: Foreign -> Nullable
  { project_id :: Nullable Int
  , target_type :: String
  , target_id :: String
  , target_id_2 :: Nullable String
  , kind :: String
  , value :: String
  , source :: String
  , confidence :: Number
  , session_id :: Nullable String
  , supersedes :: Nullable Int
  }
foreign import buildUpdateParts :: Foreign -> Nullable
  { setClause :: String
  , setParams :: Array Foreign
  }

-- =============================================================================
-- GET /api/v2/annotations?target_type=&target_id=&kind=&status=&session_id=
-- =============================================================================

-- | List annotations with optional filters
list :: Database -> Object String -> Aff Response
list db query = do
  let targetType = toNullable (Object.lookup "target_type" query)
      targetId = toNullable (Object.lookup "target_id" query)
      kind = toNullable (Object.lookup "kind" query)
      status = toNullable (Object.lookup "status" query)
      sessionId = toNullable (Object.lookup "session_id" query)
      projectId = toNullable (Object.lookup "project_id" query >>= Int.fromString)
  rows <- queryAllParams db """
    SELECT * FROM annotations
    WHERE (? IS NULL OR target_type = ?)
      AND (? IS NULL OR target_id = ?)
      AND (? IS NULL OR kind = ?)
      AND (? IS NULL OR status = ?)
      AND (? IS NULL OR session_id = ?)
      AND (? IS NULL OR project_id = ?)
    ORDER BY created_at DESC
    LIMIT 500
  """ [ unsafeToForeign targetType, unsafeToForeign targetType
      , unsafeToForeign targetId, unsafeToForeign targetId
      , unsafeToForeign kind, unsafeToForeign kind
      , unsafeToForeign status, unsafeToForeign status
      , unsafeToForeign sessionId, unsafeToForeign sessionId
      , unsafeToForeign projectId, unsafeToForeign projectId
      ]
  ok' jsonHeaders (buildAnnotationsJson rows)

-- =============================================================================
-- GET /api/v2/annotations/:id
-- =============================================================================

-- | Get a single annotation by ID
get :: Database -> Int -> Aff Response
get db annId = do
  rows <- queryAllParams db
    "SELECT * FROM annotations WHERE id = ?"
    [unsafeToForeign annId]
  case firstRow rows of
    Nothing -> notFound
    Just row -> ok' jsonHeaders (buildAnnotationJson row)

-- =============================================================================
-- POST /api/v2/annotations
-- =============================================================================

-- | Create a new annotation
create :: Database -> String -> Aff Response
create db bodyStr =
  case toMaybe (parseAnnotationBody bodyStr) of
    Nothing -> badRequest' jsonHeaders """{"error":"Invalid JSON body"}"""
    Just body ->
      case toMaybe (validateCreateFields body) of
        Nothing -> badRequest' jsonHeaders """{"error":"Missing required fields: target_type, target_id, kind, value, source"}"""
        Just v -> do
          run db """
            INSERT INTO annotations (project_id, target_type, target_id, target_id_2, kind, value, source, confidence, session_id, supersedes)
            VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
          """ [ unsafeToForeign v.project_id
              , unsafeToForeign v.target_type
              , unsafeToForeign v.target_id
              , unsafeToForeign v.target_id_2
              , unsafeToForeign v.kind
              , unsafeToForeign v.value
              , unsafeToForeign v.source
              , unsafeToForeign v.confidence
              , unsafeToForeign v.session_id
              , unsafeToForeign v.supersedes
              ]
          rows <- queryAll db "SELECT * FROM annotations ORDER BY id DESC LIMIT 1"
          case firstRow rows of
            Nothing -> ok' jsonHeaders """{"error":"Failed to retrieve created annotation"}"""
            Just row -> response' Status.created jsonHeaders (buildAnnotationJson row)

-- =============================================================================
-- PATCH /api/v2/annotations/:id
-- =============================================================================

-- | Update an existing annotation (status, confidence, value, kind)
update :: Database -> Int -> String -> Aff Response
update db annId bodyStr =
  case toMaybe (parseAnnotationBody bodyStr) of
    Nothing -> badRequest' jsonHeaders """{"error":"Invalid JSON body"}"""
    Just body ->
      case toMaybe (buildUpdateParts body) of
        Nothing -> badRequest' jsonHeaders """{"error":"No updateable fields provided"}"""
        Just { setClause, setParams } -> do
          let sql = "UPDATE annotations " <> setClause <> " WHERE id = ?"
              allParams = setParams <> [unsafeToForeign annId]
          run db sql allParams
          rows <- queryAllParams db
            "SELECT * FROM annotations WHERE id = ?"
            [unsafeToForeign annId]
          case firstRow rows of
            Nothing -> notFound
            Just row -> ok' jsonHeaders (buildAnnotationJson row)

-- =============================================================================
-- GET /api/v2/report
-- =============================================================================

-- | Generate a markdown codebase report, scoped to a project. Annotations are
-- | filtered by project_id directly. Module stats come from the project's
-- | latest snapshot for declaration counts and LOC.
report :: Database -> Maybe Int -> Maybe Int -> Aff Response
report db mProject mSnapshot = do
  let projectParam = unsafeToForeign (toNullable mProject)
      snapshotParams = [projectParam, unsafeToForeign (toNullable mSnapshot)]
  annotationRows <- queryAllParams db """
    WITH target AS (
      SELECT COALESCE(?, (SELECT MIN(id) FROM projects)) as project_id
    ),
    target_snapshot AS (
      SELECT COALESCE(?,
        (SELECT s.id FROM snapshots s
         WHERE s.project_id = (SELECT project_id FROM target)
         ORDER BY s.id DESC LIMIT 1)
      ) as snapshot_id
    ),
    project_modules AS (
      SELECT m.name as module_name, pv.name as package_name
      FROM modules m
      JOIN package_versions pv ON m.package_version_id = pv.id
      JOIN snapshot_packages sp ON sp.package_version_id = pv.id
      WHERE sp.snapshot_id = (SELECT snapshot_id FROM target_snapshot)
    )
    SELECT a.*,
           pm.module_name as module_name,
           pm.package_name as package_name
    FROM annotations a
    JOIN project_modules pm ON a.target_type = 'module' AND a.target_id = pm.module_name
    WHERE a.project_id = (SELECT project_id FROM target)
    UNION ALL
    SELECT a.*,
           a.target_id as module_name,
           a.target_id as package_name
    FROM annotations a
    WHERE a.target_type = 'package'
      AND a.project_id = (SELECT project_id FROM target)
    ORDER BY package_name, target_id, kind
  """ snapshotParams
  moduleStatsRows <- queryAllParams db """
    WITH target AS (
      SELECT COALESCE(?, (SELECT MIN(id) FROM projects)) as project_id
    ),
    target_snapshot AS (
      SELECT COALESCE(?,
        (SELECT s.id FROM snapshots s
         WHERE s.project_id = (SELECT project_id FROM target)
         ORDER BY s.id DESC LIMIT 1)
      ) as snapshot_id
    )
    SELECT m.name as module_name, pv.name as package_name,
           COUNT(d.id) as decl_count, COALESCE(m.loc, 0) as loc
    FROM modules m
    JOIN package_versions pv ON m.package_version_id = pv.id
    JOIN snapshot_packages sp ON sp.package_version_id = pv.id
    LEFT JOIN declarations d ON d.module_id = m.id
    WHERE sp.snapshot_id = (SELECT snapshot_id FROM target_snapshot)
    GROUP BY m.id, m.name, pv.name, m.loc
    ORDER BY pv.name, m.name
  """ snapshotParams
  ok' markdownHeaders (buildReportMarkdown annotationRows moduleStatsRows)
