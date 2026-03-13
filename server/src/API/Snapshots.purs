-- | Snapshot Management API
-- |
-- | Git commit log, worktree lifecycle, and snapshot CRUD.
-- | Enables frontend-driven snapshot creation and cleanup.
module API.Snapshots
  ( getGitLog
  , createSnapshot
  , deleteSnapshots
  , listSnapshotDetails
  , getCommitFiles
  , getModuleNumstat
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable (Nullable, toMaybe, toNullable)
import Data.String.CodeUnits as SCU
import Data.Traversable (traverse)
import Database.DuckDB (Database, queryAll, queryAllParams, exec, firstRow, closeDB, openDB)
import Effect (Effect)
import Effect.Uncurried (EffectFn2, EffectFn3, runEffectFn2, runEffectFn3, runEffectFn4)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Foreign (Foreign, unsafeToForeign)
import HTTPurple (Response, ok', badRequest')
import HTTPurple.Headers (ResponseHeaders, headers)

import API.Projects (runLoaderSync)

-- =============================================================================
-- FFI Imports
-- =============================================================================

-- Git log: returns JSON string with commits + hasMore
foreign import getGitLogWithSnapshots :: EffectFn3 Int Int (Array Foreign) String

-- Ref resolution
foreign import resolveRef :: String -> Effect String

-- Worktree operations (return JSON result strings)
foreign import createWorktree :: EffectFn2 String String String
foreign import hasCompiledOutput :: String -> Effect Boolean
foreign import buildInWorktree :: String -> Effect String
foreign import removeWorktree :: EffectFn2 String String String

-- JSON builders
foreign import buildSnapshotDetailsJson :: Array Foreign -> String
foreign import buildDeleteResultsJson
  :: Array { snapshotId :: Int, deleted :: Boolean, warning :: Nullable String, error :: Nullable String }
  -> String

-- Body parsing
foreign import parseBody :: String -> Nullable Foreign
foreign import getBodyRef :: Foreign -> Nullable String
foreign import getBodyLabel :: Foreign -> Nullable String
foreign import getBodySnapshotIds :: Foreign -> Nullable (Array Int)

-- JSON result helpers
foreign import isJsonSuccess :: String -> Boolean
foreign import getJsonField :: EffectFn2 String String (Nullable String)

-- Commit-module grid
foreign import getCommitFilesImpl :: EffectFn3 Int Int (Array String) String

-- Module numstat (per-commit line additions/deletions by module)
foreign import getModuleNumstatImpl :: EffectFn2 Int (Array String) String

-- Row field access (JS objects from DuckDB query results)
foreign import getRowString :: Foreign -> String -> String
foreign import getRowInt :: Foreign -> String -> Int

-- =============================================================================
-- Helpers
-- =============================================================================

jsonHeaders :: ResponseHeaders
jsonHeaders = headers
  { "Content-Type": "application/json"
  , "Access-Control-Allow-Origin": "*"
  }

-- | The server runs from the minard/ directory — that's our project root.
projectRoot :: String
projectRoot = "."

-- =============================================================================
-- GET /api/v2/git/log?count=30&offset=0
-- =============================================================================

getGitLog :: Database -> Int -> Int -> Aff Response
getGitLog db count offset = do
  -- Query existing snapshot hashes to annotate which commits are already loaded
  snapshotRows <- queryAll db """
    SELECT DISTINCT s.git_hash FROM snapshots s
    WHERE s.git_hash IS NOT NULL AND s.git_hash != ''
  """
  json <- liftEffect $ runEffectFn3 getGitLogWithSnapshots count offset snapshotRows
  ok' jsonHeaders json

-- =============================================================================
-- POST /api/v2/snapshots/create
-- Body: { "ref": "main" | "28a4808...", "label": "optional" }
-- =============================================================================

createSnapshot :: Ref Database -> String -> String -> Aff Response
createSnapshot dbRef bodyStr dbPath = case toMaybe (parseBody bodyStr) of
  Nothing -> badRequest' jsonHeaders """{"error":"Invalid JSON body"}"""
  Just body -> case toMaybe (getBodyRef body) of
    Nothing -> badRequest' jsonHeaders """{"error":"Missing required field: ref"}"""
    Just ref -> do
      liftEffect $ log $ "[Snapshots] Creating snapshot for ref: " <> ref
      let label = fromMaybe ref (toMaybe (getBodyLabel body))

      -- 1. Resolve ref to full commit hash
      hash <- liftEffect $ resolveRef ref
      if hash == ""
        then badRequest' jsonHeaders $ """{"error":"Could not resolve git ref: """ <> ref <> """"}"""
        else do
          liftEffect $ log $ "[Snapshots] Resolved to: " <> hash

          -- 2. Create worktree (detached HEAD at commit hash)
          wtResult <- liftEffect $ runEffectFn2 createWorktree hash projectRoot
          if not (isJsonSuccess wtResult)
            then do
              mErr <- liftEffect $ runEffectFn2 getJsonField wtResult "error"
              badRequest' jsonHeaders $ """{"error":"Failed to create worktree: """ <> fromMaybe "unknown" (toMaybe mErr) <> """"}"""
            else do
              mPath <- liftEffect $ runEffectFn2 getJsonField wtResult "worktreePath"
              let worktreePath = fromMaybe "" (toMaybe mPath)
              liftEffect $ log $ "[Snapshots] Worktree at: " <> worktreePath

              -- 3. Check for compiled output, build if missing
              hasOutput <- liftEffect $ hasCompiledOutput worktreePath
              when (not hasOutput) do
                liftEffect $ log "[Snapshots] No output/ found, running spago build..."
                buildResult <- liftEffect $ buildInWorktree worktreePath
                when (not (isJsonSuccess buildResult)) do
                  mErr <- liftEffect $ runEffectFn2 getJsonField buildResult "error"
                  liftEffect $ log $ "[Snapshots] Build warning: " <> fromMaybe "unknown" (toMaybe mErr)

              -- 4. Close DB, run loader, reopen
              let shortHash = SCU.take 7 hash
                  projectName = "minard-" <> shortHash
              db <- liftEffect $ Ref.read dbRef
              closeDB db
              result <- liftEffect $ runEffectFn4 runLoaderSync worktreePath dbPath
                (toNullable (Just projectName))
                (toNullable (Just label))
              newDb <- openDB dbPath
              liftEffect $ Ref.write newDb dbRef
              liftEffect $ log "[Snapshots] Loader complete"

              ok' jsonHeaders result

-- =============================================================================
-- POST /api/v2/snapshots/delete
-- Body: { "snapshotIds": [238, 205] }
-- =============================================================================

deleteSnapshots :: Ref Database -> String -> String -> Aff Response
deleteSnapshots dbRef bodyStr _dbPath = case toMaybe (parseBody bodyStr) of
  Nothing -> badRequest' jsonHeaders """{"error":"Invalid JSON body"}"""
  Just body -> case toMaybe (getBodySnapshotIds body) of
    Nothing -> badRequest' jsonHeaders """{"error":"Missing required field: snapshotIds"}"""
    Just ids -> do
      db <- liftEffect $ Ref.read dbRef
      results <- traverse (deleteOne db) ids
      ok' jsonHeaders (buildDeleteResultsJson results)

deleteOne :: Database -> Int -> Aff { snapshotId :: Int, deleted :: Boolean, warning :: Nullable String, error :: Nullable String }
deleteOne db snapshotId = do
  rows <- queryAllParams db """
    SELECT s.id, s.project_id, p.repo_path
    FROM snapshots s
    JOIN projects p ON s.project_id = p.id
    WHERE s.id = ?
  """ [unsafeToForeign snapshotId]
  case firstRow rows of
    Nothing ->
      pure { snapshotId, deleted: false, warning: toNullable Nothing, error: toNullable (Just "Snapshot not found") }
    Just row -> do
      let repoPath = getRowString row "repo_path"
          projectId = getRowInt row "project_id"
      if repoPath == "." || repoPath == ""
        then pure { snapshotId, deleted: false, warning: toNullable Nothing, error: toNullable (Just "Cannot delete current checkout") }
        else do
          -- Remove worktree (tolerates already-removed worktrees)
          wtResult <- liftEffect $ runEffectFn2 removeWorktree repoPath projectRoot
          wtWarning <- liftEffect $ runEffectFn2 getJsonField wtResult "warning"

          -- Cascade delete project + snapshot data from DB
          exec db (cascadeDeleteSql projectId)

          liftEffect $ log $ "[Snapshots] Deleted snapshot " <> show snapshotId <> " (project " <> show projectId <> ")"
          pure { snapshotId, deleted: true, warning: wtWarning, error: toNullable Nothing }

-- =============================================================================
-- GET /api/v2/snapshots/details
-- =============================================================================

listSnapshotDetails :: Database -> Aff Response
listSnapshotDetails db = do
  rows <- queryAll db """
    SELECT s.id, s.project_id, s.git_hash, s.git_ref, s.label,
           p.repo_path, p.name as project_name,
           COUNT(DISTINCT sp.package_version_id) as package_count,
           COUNT(DISTINCT m.id) as module_count,
           COUNT(DISTINCT CASE WHEN pv.source = 'workspace' THEN sp.package_version_id END) as workspace_package_count
    FROM snapshots s
    JOIN projects p ON s.project_id = p.id
    LEFT JOIN snapshot_packages sp ON sp.snapshot_id = s.id
    LEFT JOIN package_versions pv ON sp.package_version_id = pv.id
    LEFT JOIN modules m ON m.package_version_id = pv.id
    GROUP BY s.id, s.project_id, s.git_hash, s.git_ref, s.label, p.repo_path, p.name
    ORDER BY s.id DESC
  """
  ok' jsonHeaders (buildSnapshotDetailsJson rows)

-- =============================================================================
-- GET /api/v2/git/commit-files?count=50&package=minard-frontend
-- =============================================================================

getCommitFiles :: Database -> Int -> Int -> String -> Aff Response
getCommitFiles db count offset pkg = do
  -- Get known module names for this package from the DB
  rows <- queryAllParams db """
    SELECT m.name FROM modules m
    JOIN package_versions pv ON m.package_version_id = pv.id
    WHERE pv.name = ?
    ORDER BY m.name
  """ [unsafeToForeign pkg]
  let moduleNames = map (\row -> getRowString row "name") rows
  json <- liftEffect $ runEffectFn3 getCommitFilesImpl count offset moduleNames
  ok' jsonHeaders json

-- =============================================================================
-- GET /api/v2/git/module-numstat?count=200&package=minard-frontend
-- =============================================================================

getModuleNumstat :: Database -> Int -> String -> Aff Response
getModuleNumstat db count pkg = do
  rows <- queryAllParams db """
    SELECT m.name FROM modules m
    JOIN package_versions pv ON m.package_version_id = pv.id
    WHERE pv.name = ?
    ORDER BY m.name
  """ [unsafeToForeign pkg]
  let moduleNames = map (\row -> getRowString row "name") rows
  json <- liftEffect $ runEffectFn2 getModuleNumstatImpl count moduleNames
  ok' jsonHeaders json

-- =============================================================================
-- Cascade Delete SQL
-- =============================================================================

cascadeDeleteSql :: Int -> String
cascadeDeleteSql pid =
  let p = show pid
      modSub = "(SELECT m.id FROM modules m JOIN package_versions pv ON m.package_version_id = pv.id JOIN snapshot_packages sp ON sp.package_version_id = pv.id JOIN snapshots s ON sp.snapshot_id = s.id WHERE s.project_id = " <> p <> ")"
  in Array.intercalate ";\n"
    [ "DELETE FROM function_calls WHERE caller_module_id IN " <> modSub
    , "DELETE FROM imports WHERE module_id IN " <> modSub
    , "DELETE FROM child_declarations WHERE declaration_id IN (SELECT d.id FROM declarations d JOIN modules m ON d.module_id = m.id JOIN package_versions pv ON m.package_version_id = pv.id JOIN snapshot_packages sp ON sp.package_version_id = pv.id JOIN snapshots s ON sp.snapshot_id = s.id WHERE s.project_id = " <> p <> ")"
    , "DELETE FROM declarations WHERE module_id IN " <> modSub
    , "DELETE FROM modules WHERE package_version_id IN (SELECT pv.id FROM package_versions pv JOIN snapshot_packages sp ON sp.package_version_id = pv.id JOIN snapshots s ON sp.snapshot_id = s.id WHERE s.project_id = " <> p <> ")"
    , "DELETE FROM package_dependencies WHERE snapshot_package_id IN (SELECT sp.id FROM snapshot_packages sp JOIN snapshots s ON sp.snapshot_id = s.id WHERE s.project_id = " <> p <> ")"
    , "DELETE FROM snapshot_packages WHERE snapshot_id IN (SELECT id FROM snapshots WHERE project_id = " <> p <> ")"
    , "DELETE FROM snapshots WHERE project_id = " <> p
    , "DELETE FROM projects WHERE id = " <> p
    ]
