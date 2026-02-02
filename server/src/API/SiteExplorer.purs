-- | Site Explorer API Endpoints
-- |
-- | Routes, spider results, and annotations for Site Explorer.
module API.SiteExplorer
  ( listRoutes
  , listAnnotations
  , createAnnotation
  , getClassificationReport
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toMaybe)
import Database.DuckDB (Database, queryAll, queryAllParams, firstRow, run)
import Effect.Aff (Aff)
import Foreign (Foreign, unsafeToForeign)
import HTTPurple (Response, ok', badRequest', response')
import HTTPurple.Headers (ResponseHeaders, headers)
import HTTPurple.Status as Status
import HTTPurple.Body (RequestBody, toString)

-- | JSON content type header with CORS
jsonHeaders :: ResponseHeaders
jsonHeaders = headers { "Content-Type": "application/json", "Access-Control-Allow-Origin": "*" }

-- =============================================================================
-- GET /api/site-explorer/routes/:snapshotId
-- =============================================================================

-- | List routes with reachability status for a snapshot
listRoutes :: Database -> Int -> Aff Response
listRoutes db snapshotId = do
  rows <- queryAllParams db """
    SELECT
      r.id,
      r.route_name,
      r.url_pattern,
      r.module_name,
      r.is_archived,
      dp.id IS NOT NULL as is_reachable,
      dp.depth as discovery_depth,
      dp.found_from,
      a.classification,
      a.note,
      a.action_hint
    FROM routes r
    LEFT JOIN spider_runs sr ON sr.snapshot_id = r.snapshot_id
    LEFT JOIN discovered_pages dp ON dp.route_id = r.id AND dp.spider_run_id = sr.id
    LEFT JOIN annotations a ON a.target_type = 'route' AND a.target_id = r.id
    WHERE r.snapshot_id = ?
    ORDER BY r.route_name
  """ [unsafeToForeign snapshotId]

  ok' jsonHeaders (buildRoutesJson rows)

foreign import buildRoutesJson :: Array Foreign -> String

-- =============================================================================
-- GET /api/site-explorer/annotations
-- =============================================================================

-- | List all annotations
listAnnotations :: Database -> Aff Response
listAnnotations db = do
  rows <- queryAll db """
    SELECT
      id,
      target_type,
      target_id,
      classification,
      note,
      action_hint,
      created_at
    FROM annotations
    ORDER BY created_at DESC
  """

  ok' jsonHeaders (buildAnnotationsJson rows)

foreign import buildAnnotationsJson :: Array Foreign -> String

-- =============================================================================
-- POST /api/site-explorer/annotations
-- =============================================================================

-- | Annotation input from JSON body
type AnnotationInput =
  { targetType :: String
  , targetId :: Int
  , classification :: String
  , note :: String
  , actionHint :: String
  }

-- | Create a new annotation
createAnnotation :: Database -> RequestBody -> Aff Response
createAnnotation db body = do
  bodyStr <- toString body
  case toMaybe (parseAnnotationJsonImpl bodyStr) of
    Nothing -> badRequest' jsonHeaders """{"error": "Invalid JSON body"}"""
    Just input -> do
      -- Get next ID
      maxIdRow <- firstRow <$> queryAll db "SELECT COALESCE(MAX(id), 0) as max_id FROM annotations"
      let nextId = case maxIdRow of
            Nothing -> 1
            Just row -> (getMaxId row) + 1

      -- Insert annotation
      run db """
        INSERT INTO annotations (id, target_type, target_id, classification, note, action_hint, created_at)
        VALUES (?, ?, ?, ?, ?, ?, NOW())
      """ [ unsafeToForeign nextId
          , unsafeToForeign input.targetType
          , unsafeToForeign input.targetId
          , unsafeToForeign input.classification
          , unsafeToForeign input.note
          , unsafeToForeign input.actionHint
          ]

      -- Return created annotation
      response' Status.created jsonHeaders (buildCreatedJson nextId input)

foreign import parseAnnotationJsonImpl :: String -> Nullable AnnotationInput
foreign import getMaxId :: Foreign -> Int
foreign import buildCreatedJson :: Int -> AnnotationInput -> String

-- =============================================================================
-- GET /api/site-explorer/classification-report
-- =============================================================================

-- | Get classification report for LLM consumption
getClassificationReport :: Database -> Aff Response
getClassificationReport db = do
  rows <- queryAll db """
    SELECT * FROM classification_report
  """

  ok' jsonHeaders (buildReportJson rows)

foreign import buildReportJson :: Array Foreign -> String
