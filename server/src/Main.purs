module Server.Main where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Int (fromString) as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Database.DuckDB as DB
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Ref as Ref
import Foreign.Object as Object
import HTTPurple (Method(..), serve, ok, ok', toString)
import HTTPurple.Headers (headers)
import Routing.Duplex (RouteDuplex', root, path, int, segment)
import Routing.Duplex.Generic (noArgs, sum)
import API.Annotations as Annotations
import API.Projects as Projects
import API.Snapshots as Snapshots
import API.Unified as Unified

foreign import getPortFromEnv :: Effect Int

-- =============================================================================
-- Routes (V2 API only)
-- =============================================================================

data Route
  -- Stats
  = V2Stats
  -- Packages
  | V2ListPackages
  | V2UnusedPackages
  | V2GetPackage Int
  -- Modules
  | V2ListModules
  | V2GetModule Int
  | V2GetModuleDeclarations Int
  | V2GetModuleImports Int
  | V2GetModuleCalls Int
  | V2GetModuleReexports Int
  | V2GetAllImports
  | V2GetAllCalls
  | V2GetModuleDeclarationStats
  | V2GetModuleStructuralComplexity
  -- Namespaces
  | V2ListNamespaces
  | V2GetNamespace String
  | V2NamespaceTree
  | V2NamespacePackages
  -- Search
  | V2SearchDeclarations String
  | V2Search String
  -- Polyglot
  | V2PolyglotSummary
  -- Type system analysis
  | V2TypeClassStats
  -- Git
  | V2GitStatus
  -- Declaration usage (cross-module call graph)
  | V2GetDeclarationUsage
  -- Module source (read .purs file from disk)
  | V2GetModuleSource
  -- Source location (file path for editor integration)
  | V2GetSourceLocation
  -- Annotations
  | V2Annotations        -- GET (list/filter) and POST (create)
  | V2Annotation Int     -- GET (single) and PATCH (update)
  | V2Report             -- GET markdown codebase report
  -- Projects (V2 management)
  | V2ListProjects       -- GET /api/v2/projects
  | V2ValidatePath       -- POST /api/v2/projects/validate
  | V2LoadProject        -- POST /api/v2/projects/load
  | V2DeleteProject Int  -- DELETE /api/v2/projects/:id
  -- Snapshots
  | V2ListSnapshots       -- GET /api/v2/snapshots?project=N
  | V2GitLog              -- GET /api/v2/git/log?count=30&offset=0
  | V2CommitFiles         -- GET /api/v2/git/commit-files?count=50&package=name
  | V2ModuleNumstat       -- GET /api/v2/git/module-numstat?count=200&package=name
  | V2GitBlame            -- GET /api/v2/git/blame?module=name
  | V2SnapshotDetails     -- GET /api/v2/snapshots/details
  | V2CreateSnapshot      -- POST /api/v2/snapshots/create
  | V2DeleteSnapshots     -- POST /api/v2/snapshots/delete
  -- Health
  | Health

derive instance Generic Route _

route :: RouteDuplex' Route
route = root $ sum
  { "V2Stats": path "api/v2/stats" noArgs
  , "V2ListPackages": path "api/v2/packages" noArgs
  , "V2UnusedPackages": path "api/v2/packages/unused" noArgs
  , "V2GetPackage": path "api/v2/packages" (int segment)
  , "V2ListModules": path "api/v2/modules" noArgs
  , "V2GetModule": path "api/v2/modules" (int segment)
  , "V2GetModuleDeclarations": path "api/v2/module-declarations" (int segment)
  , "V2GetModuleImports": path "api/v2/module-imports" (int segment)
  , "V2GetModuleCalls": path "api/v2/module-calls" (int segment)
  , "V2GetModuleReexports": path "api/v2/module-reexports" (int segment)
  , "V2GetAllImports": path "api/v2/all-imports" noArgs
  , "V2GetAllCalls": path "api/v2/all-calls" noArgs
  , "V2GetModuleDeclarationStats": path "api/v2/module-declaration-stats" noArgs
  , "V2GetModuleStructuralComplexity": path "api/v2/module-structural-complexity" noArgs
  , "V2ListNamespaces": path "api/v2/namespaces" noArgs
  , "V2GetNamespace": path "api/v2/namespaces" segment
  , "V2NamespaceTree": path "api/v2/namespace-tree" noArgs
  , "V2NamespacePackages": path "api/v2/namespace-packages" noArgs
  , "V2SearchDeclarations": path "api/v2/declarations/search" segment
  , "V2Search": path "api/v2/search" segment
  , "V2PolyglotSummary": path "api/v2/polyglot-summary" noArgs
  , "V2TypeClassStats": path "api/v2/type-class-stats" noArgs
  , "V2GitStatus": path "api/v2/git/status" noArgs
  , "V2GetDeclarationUsage": path "api/v2/declaration-usage" noArgs
  , "V2GetModuleSource": path "api/v2/module-source" noArgs
  , "V2GetSourceLocation": path "api/v2/source-location" noArgs
  , "V2Annotations": path "api/v2/annotations" noArgs
  , "V2Annotation": path "api/v2/annotations" (int segment)
  , "V2Report": path "api/v2/report" noArgs
  , "V2ListProjects": path "api/v2/projects" noArgs
  , "V2ValidatePath": path "api/v2/projects/validate" noArgs
  , "V2LoadProject": path "api/v2/projects/load" noArgs
  , "V2DeleteProject": path "api/v2/projects" (int segment)
  , "V2ListSnapshots": path "api/v2/snapshots" noArgs
  , "V2GitLog": path "api/v2/git/log" noArgs
  , "V2CommitFiles": path "api/v2/git/commit-files" noArgs
  , "V2ModuleNumstat": path "api/v2/git/module-numstat" noArgs
  , "V2GitBlame": path "api/v2/git/blame" noArgs
  , "V2SnapshotDetails": path "api/v2/snapshots/details" noArgs
  , "V2CreateSnapshot": path "api/v2/snapshots/create" noArgs
  , "V2DeleteSnapshots": path "api/v2/snapshots/delete" noArgs
  , "Health": path "health" noArgs
  }

-- =============================================================================
-- Server
-- =============================================================================

dbPath :: String
dbPath = "./database/ce-unified.duckdb"

main :: Effect Unit
main = launchAff_ do
  db <- DB.openDB dbPath
  dbRef <- liftEffect $ Ref.new db
  liftEffect $ log $ "Connected to database: " <> dbPath

  -- Ensure annotations table exists (idempotent migration)
  DB.exec db """
    CREATE SEQUENCE IF NOT EXISTS seq_annotation_id START 1;
    CREATE TABLE IF NOT EXISTS annotations (
      id              INTEGER PRIMARY KEY DEFAULT nextval('seq_annotation_id'),
      target_type     VARCHAR NOT NULL,
      target_id       VARCHAR NOT NULL,
      target_id_2     VARCHAR,
      kind            VARCHAR NOT NULL,
      value           TEXT NOT NULL,
      source          VARCHAR NOT NULL,
      confidence      REAL DEFAULT 1.0,
      status          VARCHAR DEFAULT 'proposed',
      supersedes      INTEGER,
      session_id      VARCHAR,
      created_at      TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    );
    CREATE INDEX IF NOT EXISTS idx_annotations_target ON annotations(target_type, target_id);
    CREATE INDEX IF NOT EXISTS idx_annotations_kind ON annotations(kind);
    CREATE INDEX IF NOT EXISTS idx_annotations_status ON annotations(status);
    CREATE INDEX IF NOT EXISTS idx_annotations_session ON annotations(session_id);
  """
  liftEffect $ log "Annotations table ready"

  -- Resolve default snapshot from current git HEAD (try hash first, then branch name)
  headHash <- liftEffect Unified.getGitHeadHash
  branchName <- liftEffect Unified.getGitBranchName
  liftEffect $ log $ "Git HEAD: " <> headHash <> " (" <> branchName <> ")"
  mByHash <- if headHash == "" then pure Nothing
    else Unified.findSnapshotIdByHash db headHash
  mDefaultSnapshot <- case mByHash of
    Just _ -> pure mByHash
    Nothing -> if branchName == "" then pure Nothing
      else Unified.findSnapshotIdByRef db branchName
  liftEffect $ log $ "Default snapshot: " <> show mDefaultSnapshot
  defaultSnapshotRef <- liftEffect $ Ref.new mDefaultSnapshot

  port <- liftEffect getPortFromEnv
  liftEffect do
    _ <- serve { port } { route, router: mkRouter dbRef defaultSnapshotRef }
    log $ "Minard API server running on http://localhost:" <> show port
    log ""
    log "Endpoints:"
    log "  GET /api/v2/stats                        - Database statistics"
    log "  GET /api/v2/packages[?project=N]          - List packages (scoped to project)"
    log "  GET /api/v2/packages/:id                 - Get package with modules"
    log "  GET /api/v2/modules[?project=N]           - List modules (scoped to project)"
    log "  GET /api/v2/modules/:id                  - Get module details"
    log "  GET /api/v2/module-declarations/:id      - Get module declarations"
    log "  GET /api/v2/module-imports/:id           - Get module imports"
    log "  GET /api/v2/module-calls/:id             - Get function calls"
    log "  GET /api/v2/module-reexports/:id         - Get module re-exports"
    log "  GET /api/v2/all-imports                  - Get all module imports (bulk)"
    log "  GET /api/v2/all-calls                    - Get all function calls (bulk)"
    log "  GET /api/v2/module-declaration-stats     - Declaration stats by module"
    log "  GET /api/v2/namespaces                   - List top-level namespaces"
    log "  GET /api/v2/namespaces/:path             - Get namespace with children"
    log "  GET /api/v2/namespace-tree               - Full namespace tree with LOC"
    log "  GET /api/v2/namespace-packages           - Namespace to package mapping"
    log "  GET /api/v2/declarations/search/:query   - Search declarations"
    log "  GET /api/v2/search/:query               - Combined search (decl+module+pkg)"
    log "  GET /api/v2/polyglot-summary             - Polyglot project summary"
    log "  GET /api/v2/type-class-stats            - Type class method/instance counts"
    log "  GET /api/v2/git/status                   - Live git status (modified/staged)"
    log "  GET /api/v2/declaration-usage?module=&decl= - Cross-module usage graph"
    log "  GET /api/v2/module-source?module=         - Read module .purs source file"
    log "  GET /api/v2/source-location?module=      - Resolve module to absolute file path"
    log "  GET/POST /api/v2/annotations             - List/create annotations"
    log "  GET/PATCH /api/v2/annotations/:id        - Get/update annotation"
    log "  GET /api/v2/report                       - Markdown codebase report"
    log "  GET /api/v2/snapshots[?project=N]          - List snapshots for project"
    log "  GET /api/v2/snapshots/details              - Enhanced snapshot listing"
    log "  GET /api/v2/git/log?count=30&offset=0      - Git commit log"
    log "  GET /api/v2/git/commit-files?count=50&package= - Commit-module grid data"
    log "  GET /api/v2/git/module-numstat?count=200&package= - Per-commit line stats by module"
    log "  GET /api/v2/git/blame?module=                - Per-line git blame for a module"
    log "  POST /api/v2/snapshots/create              - Create snapshot from ref"
    log "  POST /api/v2/snapshots/delete              - Delete snapshots + worktrees"
    log "  GET /api/v2/projects                     - List loaded projects"
    log "  POST /api/v2/projects/validate           - Validate project path"
    log "  POST /api/v2/projects/load               - Load project via loader"
    log "  DELETE /api/v2/projects/:id              - Delete project + data"
    log "  GET /health                              - Health check"
  where
  corsHeaders = headers { "Access-Control-Allow-Origin": "*", "Access-Control-Allow-Methods": "GET, POST, PATCH, DELETE, OPTIONS", "Access-Control-Allow-Headers": "Content-Type" }
  mkRouter dbRef defaultSnapshotRef { route: r, query, method, body } = do
    db <- liftEffect $ Ref.read dbRef
    mDefaultSnapshot <- liftEffect $ Ref.read defaultSnapshotRef
    let mProject = Object.lookup "project" query >>= Int.fromString
    let mSnapshot = case Object.lookup "snapshot" query >>= Int.fromString of
          Just s -> Just s
          Nothing -> mDefaultSnapshot
    case r of
      V2Stats -> Unified.getStats db
      V2ListPackages -> Unified.listPackages db mProject mSnapshot
      V2UnusedPackages -> Unified.listUnusedPackages db mProject
      V2GetPackage pkgId -> Unified.getPackage db pkgId
      V2ListModules -> Unified.listModules db mProject mSnapshot
      V2GetModule modId -> Unified.getModule db modId
      V2GetModuleDeclarations modId -> Unified.getModuleDeclarations db modId
      V2GetModuleImports modId -> Unified.getModuleImports db modId
      V2GetModuleCalls modId -> Unified.getModuleCalls db modId
      V2GetModuleReexports modId -> Unified.getModuleReexports db modId
      V2GetAllImports -> Unified.getAllImports db mProject mSnapshot
      V2GetAllCalls -> Unified.getAllCalls db mProject mSnapshot
      V2GetModuleDeclarationStats -> Unified.getModuleDeclarationStats db
      V2GetModuleStructuralComplexity -> Unified.getModuleStructuralComplexity db
      V2ListNamespaces -> Unified.listNamespaces db
      V2GetNamespace nsPath -> Unified.getNamespace db nsPath
      V2NamespaceTree -> Unified.getNamespaceTree db
      V2NamespacePackages -> Unified.getNamespacePackages db
      V2SearchDeclarations q -> Unified.searchDeclarations db q
      V2Search q -> Unified.searchAll db q
      V2PolyglotSummary -> Unified.getPolyglotSummary db
      V2TypeClassStats -> Unified.getTypeClassStats db
      V2GitStatus -> Unified.getGitStatus
      V2GetDeclarationUsage ->
        let mModule = Object.lookup "module" query
            mDecl = Object.lookup "decl" query
        in case mModule, mDecl of
          Just moduleName, Just declName -> Unified.getDeclarationUsage db moduleName declName
          _, _ -> ok "{ \"error\": \"module and decl query params required\" }"
      V2GetModuleSource ->
        case Object.lookup "module" query of
          Just moduleName -> case Object.lookup "snapshot" query >>= Int.fromString of
            Just snapId -> Unified.getModuleSourceFromSnapshot db moduleName snapId
            Nothing -> Unified.getModuleSource db moduleName
          Nothing -> ok "{ \"error\": \"module query param required\" }"
      V2GetSourceLocation ->
        case Object.lookup "module" query of
          Just moduleName -> Unified.getSourceLocation db moduleName
          Nothing -> ok "{ \"error\": \"module query param required\" }"
      V2Annotations -> case method of
        Get -> Annotations.list db query
        Post -> do
          bodyStr <- toString body
          Annotations.create db bodyStr
        Options -> ok' corsHeaders ""
        _ -> ok "{ \"error\": \"Method not allowed\" }"
      V2Annotation annId -> case method of
        Get -> Annotations.get db annId
        Patch -> do
          bodyStr <- toString body
          Annotations.update db annId bodyStr
        Options -> ok' corsHeaders ""
        _ -> ok "{ \"error\": \"Method not allowed\" }"
      V2Report -> Annotations.report db
      V2ListProjects -> Projects.listProjects db
      V2ValidatePath -> case method of
        Post -> do
          bodyStr <- toString body
          Projects.validatePath bodyStr
        Options -> ok' corsHeaders ""
        _ -> ok "{ \"error\": \"Method not allowed\" }"
      V2LoadProject -> case method of
        Post -> do
          bodyStr <- toString body
          Projects.loadProject dbRef bodyStr dbPath
        Options -> ok' corsHeaders ""
        _ -> ok "{ \"error\": \"Method not allowed\" }"
      V2DeleteProject projectId -> case method of
        Delete -> Projects.deleteProject db projectId
        Options -> ok' corsHeaders ""
        _ -> ok "{ \"error\": \"Method not allowed\" }"
      V2ListSnapshots -> Unified.listSnapshots db mProject
      V2GitLog ->
        let count = fromMaybe 30 (Object.lookup "count" query >>= Int.fromString)
            offset = fromMaybe 0 (Object.lookup "offset" query >>= Int.fromString)
        in Snapshots.getGitLog db count offset
      V2CommitFiles ->
        let count = fromMaybe 50 (Object.lookup "count" query >>= Int.fromString)
            offset = fromMaybe 0 (Object.lookup "offset" query >>= Int.fromString)
            pkg = fromMaybe "" (Object.lookup "package" query)
        in Snapshots.getCommitFiles db count offset pkg
      V2ModuleNumstat ->
        let count = fromMaybe 200 (Object.lookup "count" query >>= Int.fromString)
            pkg = fromMaybe "" (Object.lookup "package" query)
        in Snapshots.getModuleNumstat db count pkg
      V2GitBlame ->
        case Object.lookup "module" query of
          Just moduleName -> Unified.getModuleBlame db moduleName
          Nothing -> ok "{ \"error\": \"module query param required\" }"
      V2SnapshotDetails -> Snapshots.listSnapshotDetails db
      V2CreateSnapshot -> case method of
        Post -> do
          bodyStr <- toString body
          Snapshots.createSnapshot dbRef bodyStr dbPath
        Options -> ok' corsHeaders ""
        _ -> ok "{ \"error\": \"Method not allowed\" }"
      V2DeleteSnapshots -> case method of
        Post -> do
          bodyStr <- toString body
          Snapshots.deleteSnapshots dbRef bodyStr dbPath
        Options -> ok' corsHeaders ""
        _ -> ok "{ \"error\": \"Method not allowed\" }"
      Health -> ok "OK"
