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
  , listUnusedPackages
  , getPackage
  -- Modules
  , listModules
  , getModule
  , getModuleDeclarations
  , getModuleImports
  , getModuleCalls
  -- Re-exports
  , getModuleReexports
  -- Bulk data
  , getAllImports
  , getAllCalls
  , getModuleDeclarationStats
  -- Namespaces
  , listNamespaces
  , getNamespace
  -- Declarations
  , searchDeclarations
  -- Combined search
  , searchAll
  -- Polyglot
  , getPolyglotSummary
  -- Type system analysis
  , getTypeClassStats
  -- Git
  , getGitStatus
  -- Declaration usage
  , getDeclarationUsage
  -- Module source
  , getModuleSource
  -- Source location (file path only, for editor integration)
  , getSourceLocation
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toMaybe, toNullable)
import Data.String.Common (toLower)
import Data.String.CodeUnits as SCU
import Database.DuckDB (Database, queryAll, queryAllParams, firstRow)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
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
-- | Scoped to a project via ?project=<id>. Defaults to first project when unspecified.
listPackages :: Database -> Maybe Int -> Aff Response
listPackages db mProject = do
  rows <- queryAllParams db """
    WITH target AS (
      SELECT COALESCE(?, (SELECT MIN(id) FROM projects)) as project_id
    )
    SELECT
      pv.id,
      pv.name,
      pv.version,
      pv.description,
      pv.license,
      pv.repository,
      pv.source,
      pv.bundle_module,
      COUNT(DISTINCT m.id) as module_count,
      COUNT(DISTINCT d.id) as declaration_count,
      (SELECT COALESCE(SUM(m2.loc), 0) FROM modules m2 WHERE m2.package_version_id = pv.id) as total_loc,
      (SELECT STRING_AGG(pd.dependency_name, ',')
       FROM package_dependencies pd
       WHERE pd.dependent_id = pv.id) as depends,
      (SELECT MAX(sp.topo_layer) FROM snapshot_packages sp
       JOIN snapshots s ON s.id = sp.snapshot_id
       WHERE sp.package_version_id = pv.id
       AND s.project_id = (SELECT project_id FROM target)) as topo_layer
    FROM package_versions pv
    LEFT JOIN modules m ON m.package_version_id = pv.id
    LEFT JOIN declarations d ON d.module_id = m.id
    WHERE EXISTS (
      SELECT 1 FROM snapshot_packages sp
      JOIN snapshots s ON s.id = sp.snapshot_id
      WHERE sp.package_version_id = pv.id
      AND s.project_id = (SELECT project_id FROM target)
    )
    GROUP BY pv.id, pv.name, pv.version, pv.description, pv.license, pv.repository, pv.source, pv.bundle_module
    ORDER BY pv.source DESC, pv.name, pv.version
  """ [unsafeToForeign (toNullable mProject)]
  let json = buildPackagesJson rows
  ok' jsonHeaders json

foreign import buildPackagesJson :: Array Foreign -> String

-- =============================================================================
-- GET /api/v2/packages/unused
-- =============================================================================

-- | List registry packages NOT used by the current project
-- | These are packages in the DB (from other projects) that this project doesn't depend on
listUnusedPackages :: Database -> Maybe Int -> Aff Response
listUnusedPackages db mProject = do
  rows <- queryAllParams db """
    WITH target AS (
      SELECT COALESCE(?, (SELECT MIN(id) FROM projects)) as project_id
    )
    SELECT
      pv.id,
      pv.name,
      pv.version,
      pv.description,
      pv.license,
      pv.repository,
      pv.source,
      pv.bundle_module,
      COUNT(DISTINCT m.id) as module_count,
      COUNT(DISTINCT d.id) as declaration_count,
      (SELECT COALESCE(SUM(m2.loc), 0) FROM modules m2 WHERE m2.package_version_id = pv.id) as total_loc,
      (SELECT STRING_AGG(pd.dependency_name, ',')
       FROM package_dependencies pd
       WHERE pd.dependent_id = pv.id) as depends,
      0 as topo_layer
    FROM package_versions pv
    LEFT JOIN modules m ON m.package_version_id = pv.id
    LEFT JOIN declarations d ON d.module_id = m.id
    WHERE pv.source = 'registry'
    AND NOT EXISTS (
      SELECT 1 FROM snapshot_packages sp
      JOIN snapshots s ON s.id = sp.snapshot_id
      WHERE sp.package_version_id = pv.id
      AND s.project_id = (SELECT project_id FROM target)
    )
    GROUP BY pv.id, pv.name, pv.version, pv.description, pv.license, pv.repository, pv.source, pv.bundle_module
    ORDER BY pv.name, pv.version
  """ [unsafeToForeign (toNullable mProject)]
  let json = buildPackagesJson rows
  ok' jsonHeaders json

-- =============================================================================
-- GET /api/v2/packages/:id
-- =============================================================================

-- | Get a package with its modules
getPackage :: Database -> Int -> Aff Response
getPackage db packageId = do
  -- Get package info with dependencies
  pkgRows <- queryAllParams db """
    SELECT
      pv.id,
      pv.name,
      pv.version,
      pv.description,
      pv.license,
      pv.repository,
      pv.source,
      (SELECT STRING_AGG(pd.dependency_name, ',')
       FROM package_dependencies pd
       WHERE pd.dependent_id = pv.id) as depends
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

-- | List modules with package info, scoped to a project
listModules :: Database -> Maybe Int -> Aff Response
listModules db mProject = do
  rows <- queryAllParams db """
    WITH target AS (
      SELECT COALESCE(?, (SELECT MIN(id) FROM projects)) as project_id
    )
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
    WHERE EXISTS (
      SELECT 1 FROM snapshot_packages sp
      JOIN snapshots s ON s.id = sp.snapshot_id
      WHERE sp.package_version_id = pv.id
      AND s.project_id = (SELECT project_id FROM target)
    )
    GROUP BY m.id, m.name, m.path, m.loc, pv.id, pv.name, pv.version, pv.source, ns.path
    ORDER BY pv.source DESC, m.name
  """ [unsafeToForeign (toNullable mProject)]
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
      d.source_span,
      d.source_code,
      d.superclasses,
      d.type_arguments
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

  -- Superclass methods: for type_class declarations, find methods of their superclasses
  superMethods <- queryAllParams db """
    SELECT DISTINCT
      sc_name.sc_class_name,
      cd.name as method_name,
      cd.type_signature as method_sig
    FROM declarations d,
    LATERAL (
      SELECT json_extract_string(sc.value, '$.constraintClass[1]') as sc_class_name
      FROM json_each(d.superclasses) sc
    ) sc_name
    JOIN declarations d2 ON d2.name = sc_name.sc_class_name AND d2.kind = 'type_class'
    JOIN child_declarations cd ON cd.declaration_id = d2.id AND cd.kind = 'class_member'
    WHERE d.module_id = ? AND d.kind = 'type_class' AND d.superclasses IS NOT NULL
    ORDER BY sc_name.sc_class_name, cd.name
  """ [unsafeToForeign moduleId]

  let json = buildDeclarationsJson rows children superMethods
  ok' jsonHeaders json

foreign import buildDeclarationsJson :: Array Foreign -> Array Foreign -> Array Foreign -> String

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
-- GET /api/v2/module-reexports/:id
-- =============================================================================

-- | Get re-exports for a module
getModuleReexports :: Database -> Int -> Aff Response
getModuleReexports db moduleId = do
  rows <- queryAllParams db """
    SELECT
      mr.source_module,
      mr.declaration_name
    FROM module_reexports mr
    WHERE mr.module_id = ?
    ORDER BY mr.source_module, mr.declaration_name
  """ [unsafeToForeign moduleId]
  let json = buildReexportsJson rows
  ok' jsonHeaders json

foreign import buildReexportsJson :: Array Foreign -> String

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
-- GET /api/v2/search/:query
-- =============================================================================

-- | Combined search across declarations, modules, and packages
-- | Supports prefix sugar: class:, module:, package:, type:
searchAll :: Database -> String -> Aff Response
searchAll db rawQuery = do
  let { filter, term } = parseSearchPrefix rawQuery
      searchPattern = "%" <> term <> "%"
      prefixPattern = term <> "%"
  rows <- case filter of
    FilterPackage ->
      queryAllParams db """
        SELECT 'package' as entity_type, pv.id, pv.name, NULL as kind, NULL as type_signature,
               NULL as module_name, pv.name as package_name, pv.version as package_version
        FROM package_versions pv
        WHERE EXISTS (SELECT 1 FROM snapshot_packages sp WHERE sp.package_version_id = pv.id)
          AND pv.name LIKE ?
        ORDER BY CASE WHEN pv.name LIKE ? THEN 0 ELSE 1 END, pv.name
        LIMIT 12
      """ [unsafeToForeign searchPattern, unsafeToForeign prefixPattern]

    FilterModule ->
      queryAllParams db """
        SELECT 'module' as entity_type, m.id, m.name, NULL as kind, NULL as type_signature,
               m.name as module_name, pv.name as package_name, pv.version as package_version
        FROM modules m
        JOIN package_versions pv ON m.package_version_id = pv.id
        WHERE m.name LIKE ?
        ORDER BY CASE WHEN m.name LIKE ? THEN 0 ELSE 1 END, m.name
        LIMIT 12
      """ [unsafeToForeign searchPattern, unsafeToForeign prefixPattern]

    FilterClass ->
      queryAllParams db """
        SELECT 'declaration' as entity_type, d.id, d.name, d.kind, d.type_signature,
               m.name as module_name, pv.name as package_name, pv.version as package_version
        FROM declarations d
        JOIN modules m ON d.module_id = m.id
        JOIN package_versions pv ON m.package_version_id = pv.id
        WHERE d.kind = 'type_class' AND d.name LIKE ?
        ORDER BY CASE WHEN d.name LIKE ? THEN 0 ELSE 1 END, d.name
        LIMIT 12
      """ [unsafeToForeign searchPattern, unsafeToForeign prefixPattern]

    FilterType ->
      queryAllParams db """
        SELECT 'declaration' as entity_type, d.id, d.name, d.kind, d.type_signature,
               m.name as module_name, pv.name as package_name, pv.version as package_version
        FROM declarations d
        JOIN modules m ON d.module_id = m.id
        JOIN package_versions pv ON m.package_version_id = pv.id
        WHERE d.type_signature LIKE ?
        ORDER BY d.name
        LIMIT 12
      """ [unsafeToForeign searchPattern]

    FilterNone ->
      queryAllParams db """
        SELECT * FROM (
          SELECT 'declaration' as entity_type, d.id, d.name, d.kind, d.type_signature,
                 m.name as module_name, pv.name as package_name, pv.version as package_version
          FROM declarations d
          JOIN modules m ON d.module_id = m.id
          JOIN package_versions pv ON m.package_version_id = pv.id
          WHERE d.name LIKE ? OR d.type_signature LIKE ?
          ORDER BY CASE WHEN d.name LIKE ? THEN 0 ELSE 1 END, d.name
          LIMIT 6
        )
        UNION ALL
        SELECT * FROM (
          SELECT 'module' as entity_type, m.id, m.name, NULL as kind, NULL as type_signature,
                 m.name as module_name, pv.name as package_name, pv.version as package_version
          FROM modules m
          JOIN package_versions pv ON m.package_version_id = pv.id
          WHERE m.name LIKE ?
          ORDER BY CASE WHEN m.name LIKE ? THEN 0 ELSE 1 END, m.name
          LIMIT 3
        )
        UNION ALL
        SELECT * FROM (
          SELECT 'package' as entity_type, pv.id, pv.name, NULL as kind, NULL as type_signature,
                 NULL as module_name, pv.name as package_name, pv.version as package_version
          FROM package_versions pv
          WHERE EXISTS (SELECT 1 FROM snapshot_packages sp WHERE sp.package_version_id = pv.id)
            AND pv.name LIKE ?
          ORDER BY CASE WHEN pv.name LIKE ? THEN 0 ELSE 1 END, pv.name
          LIMIT 3
        )
      """ [ unsafeToForeign searchPattern, unsafeToForeign searchPattern, unsafeToForeign prefixPattern
          , unsafeToForeign searchPattern, unsafeToForeign prefixPattern
          , unsafeToForeign searchPattern, unsafeToForeign prefixPattern
          ]

  let json = buildSearchAllJson rows
  ok' jsonHeaders json

foreign import buildSearchAllJson :: Array Foreign -> String

-- | Search prefix filter types
data SearchFilter = FilterNone | FilterPackage | FilterModule | FilterClass | FilterType

-- | Parse prefix sugar from search query
parseSearchPrefix :: String -> { filter :: SearchFilter, term :: String }
parseSearchPrefix q =
  let lower = toLower q
  in if SCU.take 8 lower == "package:" then { filter: FilterPackage, term: SCU.drop 8 q }
     else if SCU.take 7 lower == "module:" then { filter: FilterModule, term: SCU.drop 7 q }
     else if SCU.take 6 lower == "class:" then { filter: FilterClass, term: SCU.drop 6 q }
     else if SCU.take 5 lower == "type:" then { filter: FilterType, term: SCU.drop 5 q }
     else { filter: FilterNone, term: q }

-- =============================================================================
-- GET /api/v2/all-imports
-- =============================================================================

-- | Get all module imports (for building dependency graph)
-- | Returns: { imports: [{ moduleId, moduleName, imports: [importedModuleName] }] }
getAllImports :: Database -> Aff Response
getAllImports db = do
  -- Deduplicate modules: when a package exists as both local and registry,
  -- prefer the local version (it has current source). Then deduplicate by
  -- module name, keeping the entry with the most imports.
  -- Note: exclude packages with 0 modules (e.g. git-sourced stubs from
  -- alternative backends) to prevent them shadowing real registry packages.
  rows <- queryAll db """
    WITH active_packages AS (
      SELECT pv.id, pv.name, pv.source,
             ROW_NUMBER() OVER (
               PARTITION BY pv.name
               ORDER BY CASE pv.source WHEN 'local' THEN 0 WHEN 'workspace' THEN 0 ELSE 1 END,
                        pv.id DESC
             ) as rn
      FROM package_versions pv
      WHERE EXISTS (SELECT 1 FROM snapshot_packages sp WHERE sp.package_version_id = pv.id)
        AND EXISTS (SELECT 1 FROM modules m WHERE m.package_version_id = pv.id)
    ),
    best_modules AS (
      SELECT m.name as module_name,
             ARG_MAX(m.id, (SELECT COUNT(*) FROM module_imports mi2 WHERE mi2.module_id = m.id)) as module_id
      FROM modules m
      WHERE m.package_version_id IN (SELECT id FROM active_packages WHERE rn = 1)
      GROUP BY m.name
    )
    SELECT bm.module_id, bm.module_name,
           COALESCE(m2.name, mi.imported_module) as imported_module
    FROM best_modules bm
    LEFT JOIN module_imports mi ON mi.module_id = bm.module_id
    LEFT JOIN modules m2 ON m2.id = mi.imported_module_id
    ORDER BY bm.module_name, imported_module
  """
  let json = buildAllImportsJson rows
  ok' jsonHeaders json

foreign import buildAllImportsJson :: Array Foreign -> String

-- =============================================================================
-- GET /api/v2/all-calls
-- =============================================================================

-- | Get all function calls (for building declaration dependency graph)
-- | Returns: { calls: [{ moduleId, moduleName, calls: [{ callerName, calleeModule, calleeName }] }] }
getAllCalls :: Database -> Aff Response
getAllCalls db = do
  -- Deduplicate modules (same as getAllImports: prefer local over registry,
  -- exclude 0-module packages to prevent git stubs shadowing registry)
  rows <- queryAll db """
    WITH active_packages AS (
      SELECT pv.id, pv.name, pv.source,
             ROW_NUMBER() OVER (
               PARTITION BY pv.name
               ORDER BY CASE pv.source WHEN 'local' THEN 0 WHEN 'workspace' THEN 0 ELSE 1 END,
                        pv.id DESC
             ) as rn
      FROM package_versions pv
      WHERE EXISTS (SELECT 1 FROM snapshot_packages sp WHERE sp.package_version_id = pv.id)
        AND EXISTS (SELECT 1 FROM modules m WHERE m.package_version_id = pv.id)
    ),
    best_modules AS (
      SELECT m.name as module_name,
             ARG_MAX(m.id, (SELECT COUNT(*) FROM function_calls fc2 WHERE fc2.caller_module_id = m.id)) as module_id
      FROM modules m
      WHERE m.package_version_id IN (SELECT id FROM active_packages WHERE rn = 1)
      GROUP BY m.name
    )
    SELECT bm.module_id, bm.module_name, fc.caller_name, fc.callee_module, fc.callee_name,
           fc.is_cross_module, fc.call_count
    FROM best_modules bm
    LEFT JOIN function_calls fc ON fc.caller_module_id = bm.module_id
    ORDER BY bm.module_name, fc.caller_name, fc.callee_module, fc.callee_name
  """
  let json = buildAllCallsJson rows
  ok' jsonHeaders json

foreign import buildAllCallsJson :: Array Foreign -> String

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
    JOIN package_versions pv ON m.package_version_id = pv.id
    JOIN declarations d ON d.module_id = m.id
    WHERE EXISTS (SELECT 1 FROM snapshot_packages sp WHERE sp.package_version_id = pv.id)
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

-- =============================================================================
-- GET /api/v2/type-class-stats
-- =============================================================================

-- | Get all type classes with their method and instance counts
-- | Returns: { typeClasses: [{ id, name, moduleName, packageName, methodCount, instanceCount }], count: N }
getTypeClassStats :: Database -> Aff Response
getTypeClassStats db = do
  -- Get unique type classes (deduplicated by module path + class name)
  -- Note: workspace packages are duplicated across snapshots, so we pick one
  -- instance per unique (module_name, class_name) combination
  rows <- queryAll db """
    WITH unique_classes AS (
      SELECT
        MIN(d.id) as id,
        d.name,
        m.name as module_name,
        MIN(pv.name) as package_name
      FROM declarations d
      JOIN modules m ON d.module_id = m.id
      JOIN package_versions pv ON m.package_version_id = pv.id
      WHERE d.kind = 'type_class'
      GROUP BY m.name, d.name
    ),
    method_counts AS (
      SELECT d.id, COUNT(*) as method_count
      FROM declarations d
      JOIN child_declarations cd ON cd.declaration_id = d.id
      WHERE cd.kind = 'class_member'
      GROUP BY d.id
    ),
    instance_counts AS (
      SELECT SPLIT_PART(instance_type, ' ', 1) as class_name, COUNT(*) as instance_count
      FROM type_class_instances
      GROUP BY SPLIT_PART(instance_type, ' ', 1)
    )
    SELECT
      uc.id,
      uc.name,
      uc.module_name,
      uc.package_name,
      COALESCE(mc.method_count, 0) as method_count,
      COALESCE(ic.instance_count, 0) as instance_count
    FROM unique_classes uc
    LEFT JOIN method_counts mc ON mc.id = uc.id
    LEFT JOIN instance_counts ic ON ic.class_name = uc.name
    ORDER BY instance_count DESC, method_count DESC, uc.name
  """
  let json = buildTypeClassStatsJson rows
  ok' jsonHeaders json

foreign import buildTypeClassStatsJson :: Array Foreign -> String

-- =============================================================================
-- GET /api/v2/declaration-usage?module=&decl=
-- =============================================================================

-- | Get cross-module usage for a declaration (callers + callees, transitive to depth 3)
getDeclarationUsage :: Database -> String -> String -> Aff Response
getDeclarationUsage db moduleName declName = do
  -- Callees: what this declaration calls (transitive, depth 3)
  callees <- queryAllParams db """
    WITH RECURSIVE callees AS (
      SELECT fc.callee_module as module_name, fc.callee_name as decl_name, 1 as hop
      FROM function_calls fc
      JOIN modules m ON fc.caller_module_id = m.id
      WHERE m.name = ? AND fc.caller_name = ?

      UNION

      SELECT fc2.callee_module, fc2.callee_name, c.hop + 1
      FROM callees c
      JOIN modules m2 ON m2.name = c.module_name
      JOIN function_calls fc2 ON fc2.caller_module_id = m2.id AND fc2.caller_name = c.decl_name
      WHERE c.hop < 3
    )
    SELECT DISTINCT 'callee' as direction, module_name, decl_name, MIN(hop) as hop
    FROM callees GROUP BY module_name, decl_name
  """ [unsafeToForeign moduleName, unsafeToForeign declName]

  -- Callers: who calls this declaration (transitive, depth 3)
  callers <- queryAllParams db """
    WITH RECURSIVE callers AS (
      SELECT m.name as module_name, fc.caller_name as decl_name, 1 as hop
      FROM function_calls fc
      JOIN modules m ON fc.caller_module_id = m.id
      WHERE fc.callee_module = ? AND fc.callee_name = ?

      UNION

      SELECT m2.name, fc2.caller_name, c.hop + 1
      FROM callers c
      JOIN function_calls fc2 ON fc2.callee_module = c.module_name AND fc2.callee_name = c.decl_name
      JOIN modules m2 ON fc2.caller_module_id = m2.id
      WHERE c.hop < 3
    )
    SELECT DISTINCT 'caller' as direction, module_name, decl_name, MIN(hop) as hop
    FROM callers GROUP BY module_name, decl_name
  """ [unsafeToForeign moduleName, unsafeToForeign declName]

  let json = buildDeclarationUsageJson callers callees
  ok' jsonHeaders json

foreign import buildDeclarationUsageJson :: Array Foreign -> Array Foreign -> String

-- =============================================================================
-- GET /api/v2/git/status
-- =============================================================================

-- | Get current git working tree status
-- | Returns: { modified: [moduleName], staged: [moduleName], untracked: [moduleName] }
-- | This is a live query - runs git status and maps paths to module names
getGitStatus :: Aff Response
getGitStatus = do
  json <- liftEffect (getGitStatusJson (toNullable Nothing))
  ok' jsonHeaders json

foreign import getGitStatusJson :: Nullable String -> Effect String

-- =============================================================================
-- GET /api/v2/module-source?module=<name>
-- =============================================================================

-- | Read a module's .purs source file from disk
-- | Looks up the file path via source_span in the DB, then reads the file
getModuleSource :: Database -> String -> Aff Response
getModuleSource db moduleName = do
  rows <- queryAllParams db """
    SELECT d.source_span, pr.repo_path
    FROM declarations d
    JOIN modules m ON d.module_id = m.id
    JOIN package_versions pv ON m.package_version_id = pv.id
    JOIN snapshot_packages sp ON sp.package_version_id = pv.id
    JOIN snapshots s ON s.id = sp.snapshot_id
    JOIN projects pr ON pr.id = s.project_id
    WHERE m.name = ? AND d.source_span IS NOT NULL
    LIMIT 1
  """ [unsafeToForeign moduleName]
  case firstRow rows of
    Nothing -> notFound
    Just row -> do
      json <- liftEffect $ buildModuleSourceJson row
      case toMaybe json of
        Nothing -> notFound
        Just j -> ok' jsonHeaders j

foreign import buildModuleSourceJson :: Foreign -> Effect (Nullable String)

-- =============================================================================
-- GET /api/v2/source-location?module=<name>
-- =============================================================================

-- | Resolve a module name to its absolute file path on disk
-- | Returns: { filePath: "/absolute/path/to/Module.purs" }
-- | Used for editor integration (vscode:// URIs)
getSourceLocation :: Database -> String -> Aff Response
getSourceLocation db moduleName = do
  rows <- queryAllParams db """
    SELECT d.source_span, pr.repo_path
    FROM declarations d
    JOIN modules m ON d.module_id = m.id
    JOIN package_versions pv ON m.package_version_id = pv.id
    JOIN snapshot_packages sp ON sp.package_version_id = pv.id
    JOIN snapshots s ON s.id = sp.snapshot_id
    JOIN projects pr ON pr.id = s.project_id
    WHERE m.name = ? AND d.source_span IS NOT NULL
    LIMIT 1
  """ [unsafeToForeign moduleName]
  case firstRow rows of
    Nothing -> notFound
    Just row -> do
      json <- liftEffect $ buildSourceLocationJson row
      case toMaybe json of
        Nothing -> notFound
        Just j -> ok' jsonHeaders j

foreign import buildSourceLocationJson :: Foreign -> Effect (Nullable String)
