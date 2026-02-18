-- | Project Management API Endpoints (V2)
-- |
-- | Provides endpoints for in-browser project onboarding:
-- | - List loaded projects with stats
-- | - Validate a project path (prerequisites check)
-- | - Load a project via the Rust loader (execSync)
-- | - Delete a project and its data
module API.Projects
  ( listProjects
  , validatePath
  , loadProject
  , deleteProject
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toMaybe, toNullable)
import Data.Traversable (for_)
import Database.DuckDB (Database, queryAll, exec)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Foreign (Foreign)
import HTTPurple (Response, ok', badRequest', notFound)
import HTTPurple.Headers (ResponseHeaders, headers)

-- | JSON content type header with CORS
jsonHeaders :: ResponseHeaders
jsonHeaders = headers
  { "Content-Type": "application/json"
  , "Access-Control-Allow-Origin": "*"
  , "Access-Control-Allow-Methods": "GET, POST, DELETE, OPTIONS"
  , "Access-Control-Allow-Headers": "Content-Type"
  }

-- =============================================================================
-- FFI Imports
-- =============================================================================

foreign import listProjectsJson :: Array Foreign -> String
foreign import validatePathJson :: String -> Effect String
foreign import runLoaderSync :: String -> String -> Nullable String -> Nullable String -> Effect String
foreign import deleteProjectSql :: Int -> Array String
foreign import parseProjectBody :: String -> Nullable Foreign
foreign import validateLoadFields :: Foreign -> Nullable
  { path :: String
  , name :: Nullable String
  , label :: Nullable String
  }
foreign import validatePathFields :: Foreign -> Nullable
  { path :: String
  }

-- =============================================================================
-- GET /api/v2/projects
-- =============================================================================

-- | List all projects with aggregate stats from their latest snapshot
listProjects :: Database -> Aff Response
listProjects db = do
  rows <- queryAll db """
    SELECT p.id, p.name, p.repo_path, p.primary_backend, p.created_at,
           COALESCE(stats.package_count, 0) as package_count,
           COALESCE(stats.module_count, 0) as module_count,
           COALESCE(stats.declaration_count, 0) as declaration_count
    FROM projects p
    LEFT JOIN LATERAL (
      SELECT
        COUNT(DISTINCT pv.id) as package_count,
        COUNT(DISTINCT m.id) as module_count,
        COUNT(DISTINCT d.id) as declaration_count
      FROM snapshots s
      JOIN snapshot_packages sp ON sp.snapshot_id = s.id
      JOIN package_versions pv ON sp.package_version_id = pv.id
      LEFT JOIN modules m ON m.package_version_id = pv.id
      LEFT JOIN declarations d ON d.module_id = m.id
      WHERE s.project_id = p.id
    ) stats ON true
    ORDER BY p.created_at DESC
  """
  ok' jsonHeaders (listProjectsJson rows)

-- =============================================================================
-- POST /api/v2/projects/validate
-- =============================================================================

-- | Validate a project path: check prerequisites for loading
validatePath :: String -> Aff Response
validatePath bodyStr =
  case toMaybe (parseProjectBody bodyStr) of
    Nothing -> badRequest' jsonHeaders """{"error":"Invalid JSON body"}"""
    Just body ->
      case toMaybe (validatePathFields body) of
        Nothing -> badRequest' jsonHeaders """{"error":"Missing required field: path"}"""
        Just v -> do
          result <- liftEffect (validatePathJson v.path)
          ok' jsonHeaders result

-- =============================================================================
-- POST /api/v2/projects/load
-- =============================================================================

-- | Load a project by running the Rust loader synchronously.
-- | The execSync call blocks the Node event loop, which is intentional:
-- | it prevents concurrent DuckDB access issues.
loadProject :: Database -> String -> String -> Aff Response
loadProject _db bodyStr dbPath =
  case toMaybe (parseProjectBody bodyStr) of
    Nothing -> badRequest' jsonHeaders """{"error":"Invalid JSON body"}"""
    Just body ->
      case toMaybe (validateLoadFields body) of
        Nothing -> badRequest' jsonHeaders """{"error":"Missing required field: path"}"""
        Just v -> do
          result <- liftEffect (runLoaderSync v.path dbPath (toNullable (toMaybe v.name)) (toNullable (toMaybe v.label)))
          ok' jsonHeaders result

-- =============================================================================
-- DELETE /api/v2/projects/:id
-- =============================================================================

-- | Delete a project and all its associated data (cascade)
deleteProject :: Database -> Int -> Aff Response
deleteProject db projectId = do
  rows <- queryAll db ("SELECT id FROM projects WHERE id = " <> show projectId)
  case rows of
    [] -> notFound
    _ -> do
      let statements = deleteProjectSql projectId
      for_ statements (exec db)
      ok' jsonHeaders ("{\"deleted\":true,\"projectId\":" <> show projectId <> "}")
