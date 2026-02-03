module Server.Main where

import Prelude

import Data.Generic.Rep (class Generic)
import Database.DuckDB as DB
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
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
  | V2GetAllImports
  | V2GetAllCalls
  | V2GetModuleDeclarationStats
  -- Namespaces
  | V2ListNamespaces
  | V2GetNamespace String
  -- Search
  | V2SearchDeclarations String
  -- Polyglot
  | V2PolyglotSummary
  -- Type system analysis
  | V2TypeClassStats
  -- Git
  | V2GitStatus
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
  , "V2GetAllImports": path "api/v2/all-imports" noArgs
  , "V2GetAllCalls": path "api/v2/all-calls" noArgs
  , "V2GetModuleDeclarationStats": path "api/v2/module-declaration-stats" noArgs
  , "V2ListNamespaces": path "api/v2/namespaces" noArgs
  , "V2GetNamespace": path "api/v2/namespaces" segment
  , "V2SearchDeclarations": path "api/v2/declarations/search" segment
  , "V2PolyglotSummary": path "api/v2/polyglot-summary" noArgs
  , "V2TypeClassStats": path "api/v2/type-class-stats" noArgs
  , "V2GitStatus": path "api/v2/git/status" noArgs
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
    log "  GET /api/v2/packages                     - List all packages"
    log "  GET /api/v2/packages/:id                 - Get package with modules"
    log "  GET /api/v2/modules                      - List all modules"
    log "  GET /api/v2/modules/:id                  - Get module details"
    log "  GET /api/v2/module-declarations/:id      - Get module declarations"
    log "  GET /api/v2/module-imports/:id           - Get module imports"
    log "  GET /api/v2/module-calls/:id             - Get function calls"
    log "  GET /api/v2/all-imports                  - Get all module imports (bulk)"
    log "  GET /api/v2/all-calls                    - Get all function calls (bulk)"
    log "  GET /api/v2/module-declaration-stats     - Declaration stats by module"
    log "  GET /api/v2/namespaces                   - List top-level namespaces"
    log "  GET /api/v2/namespaces/:path             - Get namespace with children"
    log "  GET /api/v2/declarations/search/:query   - Search declarations"
    log "  GET /api/v2/polyglot-summary             - Polyglot project summary"
    log "  GET /api/v2/type-class-stats            - Type class method/instance counts"
    log "  GET /api/v2/git/status                   - Live git status (modified/staged)"
    log "  GET /health                              - Health check"
  where
  mkRouter db { route: r } = case r of
    V2Stats -> Unified.getStats db
    V2ListPackages -> Unified.listPackages db
    V2GetPackage pkgId -> Unified.getPackage db pkgId
    V2ListModules -> Unified.listModules db
    V2GetModule modId -> Unified.getModule db modId
    V2GetModuleDeclarations modId -> Unified.getModuleDeclarations db modId
    V2GetModuleImports modId -> Unified.getModuleImports db modId
    V2GetModuleCalls modId -> Unified.getModuleCalls db modId
    V2GetAllImports -> Unified.getAllImports db
    V2GetAllCalls -> Unified.getAllCalls db
    V2GetModuleDeclarationStats -> Unified.getModuleDeclarationStats db
    V2ListNamespaces -> Unified.listNamespaces db
    V2GetNamespace nsPath -> Unified.getNamespace db nsPath
    V2SearchDeclarations query -> Unified.searchDeclarations db query
    V2PolyglotSummary -> Unified.getPolyglotSummary db
    V2TypeClassStats -> Unified.getTypeClassStats db
    V2GitStatus -> Unified.getGitStatus
    Health -> ok "OK"
