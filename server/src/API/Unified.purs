-- | Unified Schema API Endpoints (v2)
-- |
-- | API endpoints designed for the unified schema v3.0 where:
-- |   - Package versions are the core identity
-- |   - Modules belong to package versions (not snapshots)
-- |   - Namespaces form a tree independent of packages
module API.Unified
  ( -- Stats
    getStats
  -- Packages
  , listPackages
  , getPackage
  -- Modules
  , listModules
  , getModule
  , getModuleDeclarations
  , getModuleImports
  , getModuleCalls
  -- Bulk data
  , getAllImports
  , getModuleDeclarationStats
  -- Namespaces
  , listNamespaces
  , getNamespace
  -- Declarations
  , searchDeclarations
  -- Polyglot
  , getPolyglotSummary
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Database.DuckDB (Database, queryAll, queryAllParams, firstRow)
import Effect.Aff (Aff)
import Foreign (Foreign, unsafeToForeign)
import HTTPurple (Response, ok', notFound)
import HTTPurple.Headers (ResponseHeaders, headers)

-- | JSON content type header with CORS
jsonHeaders :: ResponseHeaders
jsonHeaders = headers { "Content-Type": "application/json", "Access-Control-Allow-Origin": "*" }

-- =============================================================================
-- GET /api/v2/stats
-- =============================================================================

-- | Get overall database statistics
getStats :: Database -> Aff Response
getStats db = do
  rows <- queryAll db """
    SELECT
      (SELECT COUNT(*) FROM package_versions) as package_count,
      (SELECT COUNT(*) FROM package_versions WHERE source = 'registry') as registry_package_count,
      (SELECT COUNT(*) FROM package_versions WHERE source IN ('workspace', 'extra')) as local_package_count,
      (SELECT COUNT(*) FROM modules) as module_count,
      (SELECT COUNT(*) FROM declarations) as declaration_count,
      (SELECT COUNT(*) FROM child_declarations) as child_declaration_count,
      (SELECT COUNT(*) FROM module_namespaces) as namespace_count,
      (SELECT COUNT(*) FROM module_imports) as import_count,
      (SELECT COUNT(*) FROM function_calls) as function_call_count
  """
  let json = buildStatsJson rows
  ok' jsonHeaders json

foreign import buildStatsJson :: Array Foreign -> String

-- =============================================================================
-- GET /api/v2/packages
-- =============================================================================

-- | List all package versions with module counts, LOC, and dependencies
listPackages :: Database -> Aff Response
listPackages db = do
  rows <- queryAll db """
    SELECT
      pv.id,
      pv.name,
      pv.version,
      pv.description,
      pv.license,
      pv.repository,
      pv.source,
      COUNT(DISTINCT m.id) as module_count,
      COUNT(DISTINCT d.id) as declaration_count,
      (SELECT COALESCE(SUM(m2.loc), 0) FROM modules m2 WHERE m2.package_version_id = pv.id) as total_loc,
      (SELECT STRING_AGG(pd.dependency_name, ',')
       FROM package_dependencies pd
       WHERE pd.dependent_id = pv.id) as depends
    FROM package_versions pv
    LEFT JOIN modules m ON m.package_version_id = pv.id
    LEFT JOIN declarations d ON d.module_id = m.id
    GROUP BY pv.id, pv.name, pv.version, pv.description, pv.license, pv.repository, pv.source
    ORDER BY pv.source DESC, pv.name, pv.version
  """
  let json = buildPackagesJson rows
  ok' jsonHeaders json

foreign import buildPackagesJson :: Array Foreign -> String

-- =============================================================================
-- GET /api/v2/packages/:id
-- =============================================================================

-- | Get a package with its modules
getPackage :: Database -> Int -> Aff Response
getPackage db packageId = do
  -- Get package info
  pkgRows <- queryAllParams db """
    SELECT
      pv.id,
      pv.name,
      pv.version,
      pv.description,
      pv.license,
      pv.repository,
      pv.source
    FROM package_versions pv
    WHERE pv.id = ?
  """ [unsafeToForeign packageId]

  case firstRow pkgRows of
    Nothing -> notFound
    Just pkg -> do
      -- Get modules for this package
      modules <- queryAllParams db """
        SELECT
          m.id,
          m.name,
          m.path,
          m.loc,
          ns.path as namespace_path,
          COUNT(d.id) as declaration_count
        FROM modules m
        LEFT JOIN module_namespaces ns ON m.namespace_id = ns.id
        LEFT JOIN declarations d ON d.module_id = m.id
        WHERE m.package_version_id = ?
        GROUP BY m.id, m.name, m.path, m.loc, ns.path
        ORDER BY m.name
      """ [unsafeToForeign packageId]

      let json = buildPackageWithModulesJson pkg modules
      ok' jsonHeaders json

foreign import buildPackageWithModulesJson :: Foreign -> Array Foreign -> String

-- =============================================================================
-- GET /api/v2/modules
-- =============================================================================

-- | List modules with package info (paginated)
listModules :: Database -> Aff Response
listModules db = do
  rows <- queryAll db """
    SELECT
      m.id,
      m.name as module_name,
      m.path,
      m.loc,
      pv.id as package_id,
      pv.name as package_name,
      pv.version as package_version,
      pv.source as package_source,
      ns.path as namespace_path,
      COUNT(d.id) as declaration_count
    FROM modules m
    JOIN package_versions pv ON m.package_version_id = pv.id
    LEFT JOIN module_namespaces ns ON m.namespace_id = ns.id
    LEFT JOIN declarations d ON d.module_id = m.id
    GROUP BY m.id, m.name, m.path, m.loc, pv.id, pv.name, pv.version, pv.source, ns.path
    ORDER BY pv.source DESC, m.name
  """
  let json = buildModulesJson rows
  ok' jsonHeaders json

foreign import buildModulesJson :: Array Foreign -> String

-- =============================================================================
-- GET /api/v2/modules/:id
-- =============================================================================

-- | Get a module with full details
getModule :: Database -> Int -> Aff Response
getModule db moduleId = do
  rows <- queryAllParams db """
    SELECT
      m.id,
      m.name as module_name,
      m.path,
      m.comments,
      m.loc,
      pv.id as package_id,
      pv.name as package_name,
      pv.version as package_version,
      pv.source as package_source,
      ns.path as namespace_path,
      ns.depth as namespace_depth
    FROM modules m
    JOIN package_versions pv ON m.package_version_id = pv.id
    LEFT JOIN module_namespaces ns ON m.namespace_id = ns.id
    WHERE m.id = ?
  """ [unsafeToForeign moduleId]

  case firstRow rows of
    Nothing -> notFound
    Just mod -> do
      let json = buildModuleJson mod
      ok' jsonHeaders json

foreign import buildModuleJson :: Foreign -> String

-- =============================================================================
-- GET /api/v2/modules/:id/declarations
-- =============================================================================

-- | Get declarations for a module
getModuleDeclarations :: Database -> Int -> Aff Response
getModuleDeclarations db moduleId = do
  rows <- queryAllParams db """
    SELECT
      d.id,
      d.name,
      d.kind,
      d.type_signature,
      d.comments,
      d.data_decl_type,
      d.source_span
    FROM declarations d
    WHERE d.module_id = ?
    ORDER BY
      CASE d.kind
        WHEN 'type_class' THEN 1
        WHEN 'data' THEN 2
        WHEN 'newtype' THEN 3
        WHEN 'type_synonym' THEN 4
        WHEN 'value' THEN 5
        WHEN 'foreign' THEN 6
        ELSE 7
      END,
      d.name
  """ [unsafeToForeign moduleId]

  -- Also get child declarations
  children <- queryAllParams db """
    SELECT
      cd.id,
      cd.declaration_id,
      cd.name,
      cd.kind,
      cd.type_signature,
      cd.comments
    FROM child_declarations cd
    JOIN declarations d ON cd.declaration_id = d.id
    WHERE d.module_id = ?
    ORDER BY cd.declaration_id, cd.name
  """ [unsafeToForeign moduleId]

  let json = buildDeclarationsJson rows children
  ok' jsonHeaders json

foreign import buildDeclarationsJson :: Array Foreign -> Array Foreign -> String

-- =============================================================================
-- GET /api/v2/modules/:id/imports
-- =============================================================================

-- | Get imports for a module
getModuleImports :: Database -> Int -> Aff Response
getModuleImports db moduleId = do
  rows <- queryAllParams db """
    SELECT
      mi.imported_module,
      m2.id as imported_module_id,
      pv.name as imported_package_name,
      pv.version as imported_package_version
    FROM module_imports mi
    LEFT JOIN modules m2 ON m2.name = mi.imported_module
    LEFT JOIN package_versions pv ON m2.package_version_id = pv.id
    WHERE mi.module_id = ?
    ORDER BY mi.imported_module
  """ [unsafeToForeign moduleId]
  let json = buildImportsJson rows
  ok' jsonHeaders json

foreign import buildImportsJson :: Array Foreign -> String

-- =============================================================================
-- GET /api/v2/modules/:id/calls
-- =============================================================================

-- | Get function calls for a module
getModuleCalls :: Database -> Int -> Aff Response
getModuleCalls db moduleId = do
  rows <- queryAllParams db """
    SELECT
      fc.caller_name,
      fc.callee_module,
      fc.callee_name,
      fc.is_cross_module,
      fc.call_count
    FROM function_calls fc
    WHERE fc.caller_module_id = ?
    ORDER BY fc.caller_name, fc.callee_module, fc.callee_name
  """ [unsafeToForeign moduleId]
  let json = buildCallsJson rows
  ok' jsonHeaders json

foreign import buildCallsJson :: Array Foreign -> String

-- =============================================================================
-- GET /api/v2/namespaces
-- =============================================================================

-- | List top-level namespaces with stats
listNamespaces :: Database -> Aff Response
listNamespaces db = do
  rows <- queryAll db """
    SELECT
      ns.id,
      ns.path,
      ns.segment,
      ns.depth,
      ns.parent_id,
      ns.is_leaf,
      COUNT(DISTINCT m.id) as module_count,
      COUNT(DISTINCT pv.id) as package_count
    FROM module_namespaces ns
    LEFT JOIN modules m ON m.namespace_id = ns.id
    LEFT JOIN package_versions pv ON m.package_version_id = pv.id
    WHERE ns.depth <= 1
    GROUP BY ns.id, ns.path, ns.segment, ns.depth, ns.parent_id, ns.is_leaf
    ORDER BY ns.path
  """
  let json = buildNamespacesJson rows
  ok' jsonHeaders json

foreign import buildNamespacesJson :: Array Foreign -> String

-- =============================================================================
-- GET /api/v2/namespaces/:path
-- =============================================================================

-- | Get a namespace with children and modules
getNamespace :: Database -> String -> Aff Response
getNamespace db nsPath = do
  -- Get namespace info
  nsRows <- queryAllParams db """
    SELECT id, path, segment, depth, parent_id, is_leaf
    FROM module_namespaces
    WHERE path = ?
  """ [unsafeToForeign nsPath]

  case firstRow nsRows of
    Nothing -> notFound
    Just ns -> do
      -- Get child namespaces
      children <- queryAllParams db """
        SELECT
          ns.id,
          ns.path,
          ns.segment,
          ns.depth,
          ns.is_leaf,
          COUNT(DISTINCT m.id) as module_count
        FROM module_namespaces ns
        LEFT JOIN modules m ON m.namespace_id = ns.id
        WHERE ns.parent_id = (SELECT id FROM module_namespaces WHERE path = ?)
        GROUP BY ns.id, ns.path, ns.segment, ns.depth, ns.is_leaf
        ORDER BY ns.segment
      """ [unsafeToForeign nsPath]

      -- Get modules at this namespace
      modules <- queryAllParams db """
        SELECT
          m.id,
          m.name,
          pv.name as package_name,
          pv.version as package_version,
          COUNT(d.id) as declaration_count
        FROM modules m
        JOIN package_versions pv ON m.package_version_id = pv.id
        JOIN module_namespaces ns ON m.namespace_id = ns.id
        LEFT JOIN declarations d ON d.module_id = m.id
        WHERE ns.path = ?
        GROUP BY m.id, m.name, pv.name, pv.version
        ORDER BY m.name
      """ [unsafeToForeign nsPath]

      let json = buildNamespaceWithChildrenJson ns children modules
      ok' jsonHeaders json

foreign import buildNamespaceWithChildrenJson :: Foreign -> Array Foreign -> Array Foreign -> String

-- =============================================================================
-- GET /api/v2/declarations/search?q=...
-- =============================================================================

-- | Search declarations by name or type signature
searchDeclarations :: Database -> String -> Aff Response
searchDeclarations db query = do
  let searchPattern = "%" <> query <> "%"
  rows <- queryAllParams db """
    SELECT
      d.id,
      d.name,
      d.kind,
      d.type_signature,
      m.name as module_name,
      pv.name as package_name,
      pv.version as package_version
    FROM declarations d
    JOIN modules m ON d.module_id = m.id
    JOIN package_versions pv ON m.package_version_id = pv.id
    WHERE d.name LIKE ? OR d.type_signature LIKE ?
    ORDER BY
      CASE WHEN d.name LIKE ? THEN 0 ELSE 1 END,
      d.name
    LIMIT 100
  """ [unsafeToForeign searchPattern, unsafeToForeign searchPattern, unsafeToForeign searchPattern]
  let json = buildSearchResultsJson rows
  ok' jsonHeaders json

foreign import buildSearchResultsJson :: Array Foreign -> String

-- =============================================================================
-- GET /api/v2/all-imports
-- =============================================================================

-- | Get all module imports (for building dependency graph)
-- | Returns: { imports: [{ moduleId, moduleName, imports: [importedModuleName] }] }
getAllImports :: Database -> Aff Response
getAllImports db = do
  rows <- queryAll db """
    SELECT
      m.id as module_id,
      m.name as module_name,
      mi.imported_module
    FROM modules m
    LEFT JOIN module_imports mi ON mi.module_id = m.id
    ORDER BY m.name, mi.imported_module
  """
  let json = buildAllImportsJson rows
  ok' jsonHeaders json

foreign import buildAllImportsJson :: Array Foreign -> String

-- =============================================================================
-- GET /api/v2/module-declaration-stats
-- =============================================================================

-- | Get declaration counts grouped by kind for all modules
-- | Returns: { stats: [{ moduleId, kinds: { value: N, data: N, ... }, total: N }] }
getModuleDeclarationStats :: Database -> Aff Response
getModuleDeclarationStats db = do
  rows <- queryAll db """
    SELECT
      m.id as module_id,
      d.kind,
      COUNT(*) as count
    FROM modules m
    JOIN declarations d ON d.module_id = m.id
    GROUP BY m.id, d.kind
    ORDER BY m.id, d.kind
  """
  let json = buildModuleDeclarationStatsJson rows
  ok' jsonHeaders json

foreign import buildModuleDeclarationStatsJson :: Array Foreign -> String

-- =============================================================================
-- GET /api/v2/polyglot-summary
-- =============================================================================

-- | Get polyglot summary for sunburst visualization
-- | Returns hierarchical data: backends -> projects -> packages
getPolyglotSummary :: Database -> Aff Response
getPolyglotSummary db = do
  -- Get all projects with their backends
  projects <- queryAll db """
    SELECT
      p.id as project_id,
      p.name as project_name,
      COALESCE(p.primary_backend, 'js') as backend,
      COUNT(DISTINCT sp.package_version_id) as package_count,
      COALESCE(SUM(pv.loc_ffi_js), 0) as ffi_js_loc,
      COALESCE(SUM(pv.loc_ffi_erlang), 0) as ffi_erlang_loc,
      COALESCE(SUM(pv.loc_ffi_python), 0) as ffi_python_loc,
      COALESCE(SUM(pv.loc_ffi_lua), 0) as ffi_lua_loc,
      COALESCE(SUM(pv.loc_ffi_rust), 0) as ffi_rust_loc
    FROM projects p
    LEFT JOIN snapshots s ON s.project_id = p.id
    LEFT JOIN snapshot_packages sp ON sp.snapshot_id = s.id
    LEFT JOIN package_versions pv ON pv.id = sp.package_version_id
    GROUP BY p.id, p.name, p.primary_backend
    ORDER BY p.primary_backend, p.name
  """

  -- Get packages per project for hierarchy
  packages <- queryAll db """
    SELECT DISTINCT
      p.id as project_id,
      pv.id as package_id,
      pv.name as package_name,
      pv.version as package_version,
      pv.source as package_source,
      COALESCE(pv.loc_ffi_js, 0) as ffi_js_loc,
      COALESCE(pv.loc_ffi_erlang, 0) as ffi_erlang_loc,
      COALESCE(pv.loc_ffi_python, 0) as ffi_python_loc,
      COALESCE(pv.loc_ffi_lua, 0) as ffi_lua_loc,
      COALESCE(pv.loc_ffi_rust, 0) as ffi_rust_loc,
      COALESCE(pv.ffi_file_count, 0) as ffi_file_count,
      (SELECT COALESCE(SUM(m.loc), 0) FROM modules m WHERE m.package_version_id = pv.id) as total_loc,
      (SELECT COUNT(*) FROM modules m WHERE m.package_version_id = pv.id) as module_count
    FROM projects p
    JOIN snapshots s ON s.project_id = p.id
    JOIN snapshot_packages sp ON sp.snapshot_id = s.id
    JOIN package_versions pv ON pv.id = sp.package_version_id
    ORDER BY p.id, pv.name
  """

  let json = buildPolyglotSummaryJson projects packages
  ok' jsonHeaders json

foreign import buildPolyglotSummaryJson :: Array Foreign -> Array Foreign -> String
