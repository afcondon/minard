module Server.Main where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Int (fromString) as Int
import Data.Maybe (Maybe(..))
import Database.DuckDB as DB
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Foreign.Object as Object
import HTTPurple (serve, ok)
import Routing.Duplex (RouteDuplex', root, path, int, segment)
import Routing.Duplex.Generic (noArgs, sum)
import API.Unified as Unified

-- =============================================================================
-- Routes (V2 API only)
-- =============================================================================

data Route
  -- Stats
  = V2Stats
  -- Packages
  | V2ListPackages
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
  -- Namespaces
  | V2ListNamespaces
  | V2GetNamespace String
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
  -- Health
  | Health

derive instance Generic Route _

route :: RouteDuplex' Route
route = root $ sum
  { "V2Stats": path "api/v2/stats" noArgs
  , "V2ListPackages": path "api/v2/packages" noArgs
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
  , "V2ListNamespaces": path "api/v2/namespaces" noArgs
  , "V2GetNamespace": path "api/v2/namespaces" segment
  , "V2SearchDeclarations": path "api/v2/declarations/search" segment
  , "V2Search": path "api/v2/search" segment
  , "V2PolyglotSummary": path "api/v2/polyglot-summary" noArgs
  , "V2TypeClassStats": path "api/v2/type-class-stats" noArgs
  , "V2GitStatus": path "api/v2/git/status" noArgs
  , "V2GetDeclarationUsage": path "api/v2/declaration-usage" noArgs
  , "V2GetModuleSource": path "api/v2/module-source" noArgs
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
  liftEffect $ log $ "Connected to database: " <> dbPath

  liftEffect do
    _ <- serve { port: 3000 } { route, router: mkRouter db }
    log "Minard API server running on http://localhost:3000"
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
    log "  GET /api/v2/declarations/search/:query   - Search declarations"
    log "  GET /api/v2/search/:query               - Combined search (decl+module+pkg)"
    log "  GET /api/v2/polyglot-summary             - Polyglot project summary"
    log "  GET /api/v2/type-class-stats            - Type class method/instance counts"
    log "  GET /api/v2/git/status                   - Live git status (modified/staged)"
    log "  GET /api/v2/declaration-usage?module=&decl= - Cross-module usage graph"
    log "  GET /api/v2/module-source?module=         - Read module .purs source file"
    log "  GET /health                              - Health check"
  where
  mkRouter db { route: r, query } =
    let mProject = Object.lookup "project" query >>= Int.fromString
    in case r of
    V2Stats -> Unified.getStats db
    V2ListPackages -> Unified.listPackages db mProject
    V2GetPackage pkgId -> Unified.getPackage db pkgId
    V2ListModules -> Unified.listModules db mProject
    V2GetModule modId -> Unified.getModule db modId
    V2GetModuleDeclarations modId -> Unified.getModuleDeclarations db modId
    V2GetModuleImports modId -> Unified.getModuleImports db modId
    V2GetModuleCalls modId -> Unified.getModuleCalls db modId
    V2GetModuleReexports modId -> Unified.getModuleReexports db modId
    V2GetAllImports -> Unified.getAllImports db
    V2GetAllCalls -> Unified.getAllCalls db
    V2GetModuleDeclarationStats -> Unified.getModuleDeclarationStats db
    V2ListNamespaces -> Unified.listNamespaces db
    V2GetNamespace nsPath -> Unified.getNamespace db nsPath
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
        Just moduleName -> Unified.getModuleSource db moduleName
        Nothing -> ok "{ \"error\": \"module query param required\" }"
    Health -> ok "OK"
