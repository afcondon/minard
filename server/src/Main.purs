module Server.Main where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Database.DuckDB as DB
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import HTTPurple (serve, ok, notFound)
import HTTPurple.Method (Method(..))
import Routing.Duplex (RouteDuplex', root, path, int, segment)
import Routing.Duplex.Generic (noArgs, sum)
import API.Legacy as Legacy
import API.Projects as Projects
import API.SiteExplorer as SiteExplorer
import API.Unified as Unified

-- =============================================================================
-- Routes
-- =============================================================================

-- | Application routes
-- | Using flat URL structure for simplicity with RouteDuplex
data Route
  -- Legacy endpoints (default to latest snapshot)
  = ModulesJson
  | PackagesJson
  | LocJson
  | DeclarationsSummaryJson
  | ModuleMetricsJson
  | CommitTimelineJson
  | FunctionCallsJson
  -- Project-specific data endpoints
  | ProjectModulesJson Int
  | ProjectPackagesJson Int
  | ProjectLocJson Int
  | ProjectDeclarationsSummaryJson Int
  -- Granular module endpoints (on-demand loading)
  | GetModuleDeclarations String
  | GetModuleFunctionCalls String
  | GetModuleMetrics String
  -- Batch endpoints (comma-separated module names)
  | GetBatchFunctionCalls String
  | GetBatchDeclarations String
  | GetBatchCoupling String
  -- Project/Snapshot API
  | ListProjects
  | GetProject Int
  | GetSnapshot Int
  -- Coupling metrics
  | DeclarationCoupling
  | ModuleCoupling
  -- Package Sets (registry)
  | ListPackageSets
  | GetPackageSet Int
  -- Unified API v2 (new schema)
  | V2Stats
  | V2ListPackages
  | V2GetPackage Int
  | V2ListModules
  | V2GetModule Int
  | V2GetModuleDeclarations Int
  | V2GetModuleImports Int
  | V2GetModuleCalls Int
  | V2GetAllImports
  | V2GetModuleDeclarationStats
  | V2ListNamespaces
  | V2GetNamespace String
  | V2SearchDeclarations String
  -- Polyglot
  | V2PolyglotSummary
  -- Site Explorer
  | SiteExplorerRoutes Int
  | SiteExplorerAnnotations
  | SiteExplorerReport
  -- Health
  | Health

derive instance Generic Route _

-- | Route mapping
route :: RouteDuplex' Route
route = root $ sum
  -- Legacy endpoints (backward compatible)
  { "ModulesJson": path "data/spago-data/modules.json" noArgs
  , "PackagesJson": path "data/spago-data/packages.json" noArgs
  , "LocJson": path "data/spago-data/LOC.json" noArgs
  , "DeclarationsSummaryJson": path "data/spago-data/declarations-summary.json" noArgs
  , "ModuleMetricsJson": path "data/module-metrics.json" noArgs
  , "CommitTimelineJson": path "data/commit-timeline.json" noArgs
  , "FunctionCallsJson": path "data/function-calls.json" noArgs
  -- Project-specific data endpoints (flat URLs: /api/project-modules/:id)
  , "ProjectModulesJson": path "api/project-modules" (int segment)
  , "ProjectPackagesJson": path "api/project-packages" (int segment)
  , "ProjectLocJson": path "api/project-loc" (int segment)
  , "ProjectDeclarationsSummaryJson": path "api/project-declarations-summary" (int segment)
  -- Granular module endpoints (flat URLs for routing-duplex compatibility)
  , "GetModuleDeclarations": path "api/module-declarations" segment
  , "GetModuleFunctionCalls": path "api/module-function-calls" segment
  , "GetModuleMetrics": path "api/module-metrics" segment
  -- Batch endpoints (comma-separated module names in segment)
  , "GetBatchFunctionCalls": path "api/batch-function-calls" segment
  , "GetBatchDeclarations": path "api/batch-declarations" segment
  , "GetBatchCoupling": path "api/batch-coupling" segment
  -- Project/Snapshot API
  , "ListProjects": path "api/projects" noArgs
  , "GetProject": path "api/projects" (int segment)
  , "GetSnapshot": path "api/snapshots" (int segment)
  -- Coupling metrics
  , "DeclarationCoupling": path "api/declaration-coupling" noArgs
  , "ModuleCoupling": path "api/module-coupling" noArgs
  -- Package Sets (registry)
  , "ListPackageSets": path "api/package-sets" noArgs
  , "GetPackageSet": path "api/package-sets" (int segment)
  -- Unified API v2 (new schema)
  , "V2Stats": path "api/v2/stats" noArgs
  , "V2ListPackages": path "api/v2/packages" noArgs
  , "V2GetPackage": path "api/v2/packages" (int segment)
  , "V2ListModules": path "api/v2/modules" noArgs
  , "V2GetModule": path "api/v2/modules" (int segment)
  , "V2GetModuleDeclarations": path "api/v2/module-declarations" (int segment)
  , "V2GetModuleImports": path "api/v2/module-imports" (int segment)
  , "V2GetModuleCalls": path "api/v2/module-calls" (int segment)
  , "V2GetAllImports": path "api/v2/all-imports" noArgs
  , "V2GetModuleDeclarationStats": path "api/v2/module-declaration-stats" noArgs
  , "V2ListNamespaces": path "api/v2/namespaces" noArgs
  , "V2GetNamespace": path "api/v2/namespaces" segment
  , "V2SearchDeclarations": path "api/v2/declarations/search" segment
  -- Polyglot
  , "V2PolyglotSummary": path "api/v2/polyglot-summary" noArgs
  -- Site Explorer
  , "SiteExplorerRoutes": path "api/site-explorer/routes" (int segment)
  , "SiteExplorerAnnotations": path "api/site-explorer/annotations" noArgs
  , "SiteExplorerReport": path "api/site-explorer/report" noArgs
  -- Health
  , "Health": path "health" noArgs
  }

-- =============================================================================
-- Server Configuration
-- =============================================================================

dbPath :: String
dbPath = "./database/test-polyglot.duckdb"

-- =============================================================================
-- Main
-- =============================================================================

-- | Entry point - opens DB then starts server
main :: Effect Unit
main = launchAff_ do
  -- Open database connection
  db <- DB.openDB dbPath
  liftEffect $ log $ "Connected to database: " <> dbPath

  -- Start server (this returns a close callback, which we ignore)
  liftEffect do
    _ <- serve { port: 3000 } { route, router: mkRouter db }
    log "Server running on http://localhost:3000"
    log ""
    log "=== Unified API v2 (new schema) ==="
    log ""
    log "Stats:"
    log "  GET /api/v2/stats                        - Database statistics"
    log ""
    log "Packages:"
    log "  GET /api/v2/packages                     - List all packages"
    log "  GET /api/v2/packages/:id                 - Get package with modules"
    log ""
    log "Modules:"
    log "  GET /api/v2/modules                      - List all modules"
    log "  GET /api/v2/modules/:id                  - Get module details"
    log "  GET /api/v2/module-declarations/:id      - Get module declarations"
    log "  GET /api/v2/module-imports/:id           - Get module imports"
    log "  GET /api/v2/module-calls/:id             - Get function calls"
    log "  GET /api/v2/all-imports                  - Get all module imports (bulk)"
    log ""
    log "Namespaces:"
    log "  GET /api/v2/namespaces                   - List top-level namespaces"
    log "  GET /api/v2/namespaces/:path             - Get namespace with children"
    log ""
    log "Search:"
    log "  GET /api/v2/declarations/search/:query   - Search declarations"
    log ""
    log "=== Legacy API (old schema) ==="
    log ""
    log "Project API:"
    log "  GET /api/projects           - List all projects"
    log "  GET /api/projects/:id       - Get project with snapshots"
    log "  GET /api/snapshots/:id      - Get snapshot details"
  where
  mkRouter db { route: r, method, body } = case r of
    -- Legacy endpoints - use latest snapshot
    ModulesJson -> withLatestSnapshot db Nothing \sid -> Legacy.modulesJson db sid
    PackagesJson -> withLatestSnapshot db Nothing \sid -> Legacy.packagesJson db sid
    LocJson -> withLatestSnapshot db Nothing \sid -> Legacy.locJson db sid
    DeclarationsSummaryJson -> withLatestSnapshot db Nothing \sid -> Legacy.declarationsSummaryJson db sid
    ModuleMetricsJson -> withLatestSnapshot db Nothing \sid -> Legacy.moduleMetricsJson db sid
    CommitTimelineJson -> withLatestSnapshot db Nothing \sid -> Legacy.commitTimelineJson db sid
    FunctionCallsJson -> withLatestSnapshot db Nothing \sid -> Legacy.functionCallsJson db sid
    -- Project-specific data endpoints
    ProjectModulesJson pid -> withLatestSnapshot db (Just pid) \sid -> Legacy.modulesJson db sid
    ProjectPackagesJson pid -> withLatestSnapshot db (Just pid) \sid -> Legacy.packagesJson db sid
    ProjectLocJson pid -> withLatestSnapshot db (Just pid) \sid -> Legacy.locJson db sid
    ProjectDeclarationsSummaryJson pid -> withLatestSnapshot db (Just pid) \sid -> Legacy.declarationsSummaryJson db sid
    -- Granular module endpoints
    GetModuleDeclarations modName -> withLatestSnapshot db Nothing \sid -> Legacy.moduleDeclarationsJson db sid modName
    GetModuleFunctionCalls modName -> withLatestSnapshot db Nothing \sid -> Legacy.moduleFunctionCallsJson db sid modName
    GetModuleMetrics modName -> withLatestSnapshot db Nothing \sid -> Legacy.moduleMetricsJsonSingle db sid modName
    -- Batch endpoints (comma-separated module names)
    GetBatchFunctionCalls modules -> withLatestSnapshot db Nothing \sid -> Legacy.batchFunctionCallsJson db sid modules
    GetBatchDeclarations modules -> withLatestSnapshot db Nothing \sid -> Legacy.batchDeclarationsJson db sid modules
    GetBatchCoupling modules -> withLatestSnapshot db Nothing \sid -> Legacy.batchCouplingJson db sid modules
    -- Project/Snapshot API
    ListProjects -> Projects.listProjects db
    GetProject pid -> Projects.getProject db pid
    GetSnapshot sid -> Projects.getSnapshot db sid
    -- Coupling metrics
    DeclarationCoupling -> withLatestSnapshot db Nothing \sid -> Legacy.declarationCouplingJson db sid
    ModuleCoupling -> withLatestSnapshot db Nothing \sid -> Legacy.moduleCouplingJson db sid
    -- Package Sets (registry)
    ListPackageSets -> Projects.listPackageSets db
    GetPackageSet psId -> Projects.getPackageSet db psId
    -- Unified API v2 (new schema)
    V2Stats -> Unified.getStats db
    V2ListPackages -> Unified.listPackages db
    V2GetPackage pkgId -> Unified.getPackage db pkgId
    V2ListModules -> Unified.listModules db
    V2GetModule modId -> Unified.getModule db modId
    V2GetModuleDeclarations modId -> Unified.getModuleDeclarations db modId
    V2GetModuleImports modId -> Unified.getModuleImports db modId
    V2GetModuleCalls modId -> Unified.getModuleCalls db modId
    V2GetAllImports -> Unified.getAllImports db
    V2GetModuleDeclarationStats -> Unified.getModuleDeclarationStats db
    V2ListNamespaces -> Unified.listNamespaces db
    V2GetNamespace nsPath -> Unified.getNamespace db nsPath
    V2SearchDeclarations query -> Unified.searchDeclarations db query
    -- Polyglot
    V2PolyglotSummary -> Unified.getPolyglotSummary db
    -- Site Explorer
    SiteExplorerRoutes snapshotId -> SiteExplorer.listRoutes db snapshotId
    SiteExplorerAnnotations -> case method of
      Post -> SiteExplorer.createAnnotation db body
      _ -> SiteExplorer.listAnnotations db
    SiteExplorerReport -> SiteExplorer.getClassificationReport db
    -- Health
    Health -> ok "OK"

-- | Helper to get latest snapshot for a project and run handler, or return 404
withLatestSnapshot :: DB.Database -> Maybe Int -> (Int -> Aff _) -> Aff _
withLatestSnapshot db mProjectId handler = do
  mSnapshotId <- Projects.getLatestSnapshotId db mProjectId
  case mSnapshotId of
    Nothing -> notFound
    Just sid -> handler sid
