-- | Projects API Endpoints
-- |
-- | Multi-project and snapshot management endpoints.
module API.Projects
  ( listProjects
  , getProject
  , listSnapshots
  , getSnapshot
  , getLatestSnapshotId
  , listPackageSets
  , getPackageSet
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toMaybe)
import Database.DuckDB (Database, queryAll, queryAllParams, firstRow)
import Effect.Aff (Aff)
import Foreign (Foreign, unsafeToForeign)
import HTTPurple (Response, ok', notFound)
import HTTPurple.Headers (ResponseHeaders, headers)

-- | JSON content type header with CORS
jsonHeaders :: ResponseHeaders
jsonHeaders = headers { "Content-Type": "application/json", "Access-Control-Allow-Origin": "*" }

-- =============================================================================
-- GET /api/projects
-- =============================================================================

-- | List all projects with snapshot counts
listProjects :: Database -> Aff Response
listProjects db = do
  rows <- queryAll db """
    SELECT
      p.id,
      p.name,
      p.repo_path,
      p.description,
      p.created_at,
      COUNT(s.id) as snapshot_count,
      MAX(s.snapshot_at) as latest_snapshot_at
    FROM projects p
    LEFT JOIN snapshots s ON p.id = s.project_id
    GROUP BY p.id, p.name, p.repo_path, p.description, p.created_at
    ORDER BY p.name
  """
  let json = buildProjectsListJson rows
  ok' jsonHeaders json

foreign import buildProjectsListJson :: Array Foreign -> String

-- =============================================================================
-- GET /api/projects/:id
-- =============================================================================

-- | Get a single project with its snapshots
getProject :: Database -> Int -> Aff Response
getProject db projectId = do
  -- Get project details
  projectRows <- queryAllParams db """
    SELECT id, name, repo_path, description, created_at
    FROM projects
    WHERE id = ?
  """ [unsafeToForeign projectId]

  case firstRow projectRows of
    Nothing -> notFound
    Just project -> do
      -- Get snapshots for this project
      snapshots <- queryAllParams db """
        SELECT
          s.id,
          s.git_hash,
          s.git_ref,
          s.label,
          s.snapshot_at,
          s.created_at,
          (SELECT COUNT(*) FROM modules m WHERE m.snapshot_id = s.id) as module_count,
          (SELECT COUNT(DISTINCT m2.package) FROM modules m2 WHERE m2.snapshot_id = s.id) as package_count,
          (SELECT COUNT(*) FROM declarations d WHERE d.snapshot_id = s.id) as declaration_count
        FROM snapshots s
        WHERE s.project_id = ?
        ORDER BY s.snapshot_at DESC
      """ [unsafeToForeign projectId]

      let json = buildProjectWithSnapshotsJson project snapshots
      ok' jsonHeaders json

foreign import buildProjectWithSnapshotsJson :: Foreign -> Array Foreign -> String

-- =============================================================================
-- GET /api/projects/:id/snapshots
-- =============================================================================

-- | List snapshots for a project
listSnapshots :: Database -> Int -> Aff Response
listSnapshots db projectId = do
  rows <- queryAllParams db """
    SELECT
      s.id,
      s.project_id,
      s.git_hash,
      s.git_ref,
      s.label,
      s.snapshot_at,
      s.created_at,
      (SELECT COUNT(*) FROM modules m WHERE m.snapshot_id = s.id) as module_count,
      (SELECT COUNT(DISTINCT m2.package) FROM modules m2 WHERE m2.snapshot_id = s.id) as package_count,
      (SELECT COUNT(*) FROM declarations d WHERE d.snapshot_id = s.id) as declaration_count
    FROM snapshots s
    WHERE s.project_id = ?
    ORDER BY s.snapshot_at DESC
  """ [unsafeToForeign projectId]
  let json = buildSnapshotsListJson rows
  ok' jsonHeaders json

foreign import buildSnapshotsListJson :: Array Foreign -> String

-- =============================================================================
-- GET /api/snapshots/:id
-- =============================================================================

-- | Get a single snapshot with summary
getSnapshot :: Database -> Int -> Aff Response
getSnapshot db snapshotId = do
  rows <- queryAllParams db """
    SELECT
      s.id,
      s.project_id,
      s.git_hash,
      s.git_ref,
      s.label,
      s.snapshot_at,
      s.created_at,
      p.name as project_name,
      (SELECT COUNT(*) FROM modules m WHERE m.snapshot_id = s.id) as module_count,
      (SELECT COUNT(DISTINCT m2.package) FROM modules m2 WHERE m2.snapshot_id = s.id) as package_count,
      (SELECT COUNT(*) FROM declarations d WHERE d.snapshot_id = s.id) as declaration_count
    FROM snapshots s
    JOIN projects p ON s.project_id = p.id
    WHERE s.id = ?
  """ [unsafeToForeign snapshotId]

  case firstRow rows of
    Nothing -> notFound
    Just snapshot -> do
      let json = buildSnapshotJson snapshot
      ok' jsonHeaders json

foreign import buildSnapshotJson :: Foreign -> String

-- =============================================================================
-- Helper: Get latest snapshot ID for a project
-- =============================================================================

-- | Get the ID of the latest snapshot for a project (or any project if none specified)
-- | Orders by snapshot_at DESC, then id DESC to handle same-timestamp snapshots
getLatestSnapshotId :: Database -> Maybe Int -> Aff (Maybe Int)
getLatestSnapshotId db mProjectId = do
  rows <- case mProjectId of
    Just pid -> queryAllParams db """
      SELECT id FROM snapshots
      WHERE project_id = ?
      ORDER BY snapshot_at DESC, id DESC
      LIMIT 1
    """ [unsafeToForeign pid]
    Nothing -> queryAll db """
      SELECT id FROM snapshots
      ORDER BY snapshot_at DESC, id DESC
      LIMIT 1
    """
  pure $ toMaybe <<< getIdFromRow_ =<< firstRow rows

foreign import getIdFromRow_ :: Foreign -> Nullable Int

-- =============================================================================
-- Package Sets (Registry)
-- =============================================================================

-- | List all package sets
listPackageSets :: Database -> Aff Response
listPackageSets db = do
  rows <- queryAll db """
    SELECT
      id,
      version as name,
      compiler as compiler_version,
      source,
      published_at,
      package_count,
      created_at
    FROM package_sets
    ORDER BY version DESC
  """
  let json = buildPackageSetsListJson rows
  ok' jsonHeaders json

foreign import buildPackageSetsListJson :: Array Foreign -> String

-- | Get a single package set with all its packages
getPackageSet :: Database -> Int -> Aff Response
getPackageSet db packageSetId = do
  -- Get package set info (aliasing to match legacy API field names)
  setRows <- queryAllParams db """
    SELECT id, version as name, compiler as compiler_version, source, published_at, package_count, created_at
    FROM package_sets
    WHERE id = ?
  """ [unsafeToForeign packageSetId]

  case firstRow setRows of
    Nothing -> notFound
    Just packageSet -> do
      -- Get all packages in this set with module counts and LOC
      -- (populated by analyze-package-set.cjs from registry tarballs)
      packages <- queryAllParams db """
        SELECT
          id,
          name,
          version,
          description,
          license,
          repository_owner,
          repository_name,
          depends,
          topo_layer,
          published_at,
          release_number,
          COALESCE(module_count, 0) as module_count,
          COALESCE(total_loc, 0) as total_loc
        FROM package_set_packages
        WHERE package_set_id = ?
        ORDER BY topo_layer, name
      """ [unsafeToForeign packageSetId]

      let json = buildPackageSetWithPackagesJson packageSet packages
      ok' jsonHeaders json

foreign import buildPackageSetWithPackagesJson :: Foreign -> Array Foreign -> String
