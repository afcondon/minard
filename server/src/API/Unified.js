// FFI for API.Unified
// JSON builders for unified schema API responses

// =============================================================================
// Stats
// =============================================================================

export const buildStatsJson = (rows) => {
  if (!rows || rows.length === 0) {
    return JSON.stringify({ error: 'No data' });
  }
  const row = rows[0];
  return JSON.stringify({
    packages: {
      total: Number(row.package_count) || 0,
      registry: Number(row.registry_package_count) || 0,
      local: Number(row.local_package_count) || 0
    },
    modules: Number(row.module_count) || 0,
    declarations: Number(row.declaration_count) || 0,
    childDeclarations: Number(row.child_declaration_count) || 0,
    namespaces: Number(row.namespace_count) || 0,
    imports: Number(row.import_count) || 0,
    functionCalls: Number(row.function_call_count) || 0
  });
};

// =============================================================================
// Packages
// =============================================================================

export const buildPackagesJson = (rows) => {
  const packages = (rows || []).map(row => ({
    id: Number(row.id),
    name: row.name,
    version: row.version,
    description: row.description || null,
    license: row.license || null,
    repository: row.repository || null,
    source: row.source || 'registry',
    moduleCount: Number(row.module_count) || 0,
    declarationCount: Number(row.declaration_count) || 0,
    totalLoc: Number(row.total_loc) || 0,
    depends: row.depends ? row.depends.split(',').filter(d => d.trim()) : [],
    topoLayer: row.topo_layer != null ? Number(row.topo_layer) : 0
  }));
  return JSON.stringify({ packages, count: packages.length });
};

export const buildPackageWithModulesJson = (pkg) => (modules) => {
  return JSON.stringify({
    id: Number(pkg.id),
    name: pkg.name,
    version: pkg.version,
    description: pkg.description || null,
    license: pkg.license || null,
    repository: pkg.repository || null,
    source: pkg.source || 'registry',
    depends: pkg.depends ? pkg.depends.split(',').filter(d => d.trim()) : [],
    modules: (modules || []).map(m => ({
      id: Number(m.id),
      name: m.name,
      path: m.path || null,
      loc: m.loc ? Number(m.loc) : null,
      namespacePath: m.namespace_path || null,
      declarationCount: Number(m.declaration_count) || 0
    }))
  });
};

// =============================================================================
// Modules
// =============================================================================

export const buildModulesJson = (rows) => {
  const modules = (rows || []).map(row => ({
    id: Number(row.id),
    name: row.module_name,
    path: row.path || null,
    loc: row.loc ? Number(row.loc) : null,
    package: {
      id: Number(row.package_id),
      name: row.package_name,
      version: row.package_version,
      source: row.package_source
    },
    namespacePath: row.namespace_path || null,
    declarationCount: Number(row.declaration_count) || 0
  }));
  return JSON.stringify({ modules, count: modules.length });
};

export const buildModuleJson = (mod) => {
  return JSON.stringify({
    id: Number(mod.id),
    name: mod.module_name,
    path: mod.path || null,
    comments: mod.comments || null,
    loc: mod.loc ? Number(mod.loc) : null,
    package: {
      id: Number(mod.package_id),
      name: mod.package_name,
      version: mod.package_version,
      source: mod.package_source
    },
    namespace: mod.namespace_path ? {
      path: mod.namespace_path,
      depth: Number(mod.namespace_depth) || 0
    } : null
  });
};

// =============================================================================
// Declarations
// =============================================================================

export const buildDeclarationsJson = (declarations) => (children) => (superMethods) => {
  // Group children by declaration_id
  const childrenByDecl = {};
  for (const child of (children || [])) {
    const declId = Number(child.declaration_id);
    if (!childrenByDecl[declId]) {
      childrenByDecl[declId] = [];
    }
    childrenByDecl[declId].push({
      id: Number(child.id),
      name: child.name,
      kind: child.kind,
      typeSignature: child.type_signature || null,
      comments: child.comments || null
    });
  }

  // Group superclass methods by class name
  const superMethodsByClass = {};
  for (const sm of (superMethods || [])) {
    const cls = sm.sc_class_name;
    if (!superMethodsByClass[cls]) {
      superMethodsByClass[cls] = [];
    }
    superMethodsByClass[cls].push({
      name: sm.method_name,
      typeSignature: sm.method_sig || null
    });
  }

  const decls = (declarations || []).map(d => {
    const declId = Number(d.id);

    // Extract superclass names and attach methods
    let superclasses = [];
    if (d.superclasses) {
      try {
        const scArray = typeof d.superclasses === 'string'
          ? JSON.parse(d.superclasses) : d.superclasses;
        superclasses = (scArray || []).map(sc => {
          const name = sc.constraintClass ? sc.constraintClass[1] : null;
          return {
            name,
            methods: name && superMethodsByClass[name] ? superMethodsByClass[name] : []
          };
        }).filter(sc => sc.name);
      } catch (e) { /* ignore parse errors */ }
    }

    // Extract type argument names for type classes (e.g. [["m", null]] → ["m"])
    let typeArguments = [];
    if (d.type_arguments) {
      try {
        const taArray = typeof d.type_arguments === 'string'
          ? JSON.parse(d.type_arguments) : d.type_arguments;
        typeArguments = (taArray || []).map(ta => Array.isArray(ta) ? ta[0] : ta).filter(Boolean);
      } catch (e) { /* ignore */ }
    }

    return {
      id: declId,
      name: d.name,
      kind: d.kind,
      typeSignature: d.type_signature || null,
      comments: d.comments || null,
      dataDeclType: d.data_decl_type || null,
      sourceSpan: d.source_span ? JSON.parse(d.source_span) : null,
      sourceCode: d.source_code || null,
      superclasses,
      typeArguments,
      children: childrenByDecl[declId] || []
    };
  });

  return JSON.stringify({ declarations: decls, count: decls.length });
};

// =============================================================================
// Imports
// =============================================================================

export const buildImportsJson = (rows) => {
  const imports = (rows || []).map(row => ({
    moduleName: row.imported_module,
    moduleId: row.imported_module_id ? Number(row.imported_module_id) : null,
    packageName: row.imported_package_name || null,
    packageVersion: row.imported_package_version || null
  }));
  return JSON.stringify({ imports, count: imports.length });
};

// =============================================================================
// Function Calls
// =============================================================================

export const buildCallsJson = (rows) => {
  const calls = (rows || []).map(row => ({
    callerName: row.caller_name,
    calleeModule: row.callee_module,
    calleeName: row.callee_name,
    isCrossModule: Boolean(row.is_cross_module),
    callCount: Number(row.call_count) || 1
  }));
  return JSON.stringify({ calls, count: calls.length });
};

// =============================================================================
// Re-exports
// =============================================================================

export const buildReexportsJson = (rows) => {
  // Group re-exports by source module
  const byModule = new Map();
  for (const row of (rows || [])) {
    const mod = row.source_module;
    if (!byModule.has(mod)) {
      byModule.set(mod, []);
    }
    byModule.get(mod).push(row.declaration_name);
  }

  const reExports = Array.from(byModule.entries()).map(([sourceModule, declarations]) => ({
    sourceModule,
    declarations
  }));

  return JSON.stringify({ reExports, count: rows ? rows.length : 0 });
};

// =============================================================================
// Namespaces
// =============================================================================

export const buildNamespacesJson = (rows) => {
  const namespaces = (rows || []).map(row => ({
    id: Number(row.id),
    path: row.path,
    segment: row.segment,
    depth: Number(row.depth) || 0,
    parentId: row.parent_id ? Number(row.parent_id) : null,
    isLeaf: Boolean(row.is_leaf),
    moduleCount: Number(row.module_count) || 0,
    packageCount: Number(row.package_count) || 0
  }));
  return JSON.stringify({ namespaces, count: namespaces.length });
};

export const buildNamespaceWithChildrenJson = (ns) => (children) => (modules) => {
  return JSON.stringify({
    id: Number(ns.id),
    path: ns.path,
    segment: ns.segment,
    depth: Number(ns.depth) || 0,
    parentId: ns.parent_id ? Number(ns.parent_id) : null,
    isLeaf: Boolean(ns.is_leaf),
    children: (children || []).map(c => ({
      id: Number(c.id),
      path: c.path,
      segment: c.segment,
      depth: Number(c.depth) || 0,
      isLeaf: Boolean(c.is_leaf),
      moduleCount: Number(c.module_count) || 0
    })),
    modules: (modules || []).map(m => ({
      id: Number(m.id),
      name: m.name,
      packageName: m.package_name,
      packageVersion: m.package_version,
      declarationCount: Number(m.declaration_count) || 0
    }))
  });
};

// =============================================================================
// Search
// =============================================================================

export const buildSearchResultsJson = (rows) => {
  const results = (rows || []).map(row => ({
    id: Number(row.id),
    name: row.name,
    kind: row.kind,
    typeSignature: row.type_signature || null,
    moduleName: row.module_name,
    packageName: row.package_name,
    packageVersion: row.package_version
  }));
  return JSON.stringify({ results, count: results.length });
};

// =============================================================================
// Combined Search (declarations + modules + packages)
// =============================================================================

export const buildSearchAllJson = (rows) => {
  const results = (rows || []).map(row => ({
    entityType: row.entity_type,
    id: Number(row.id),
    name: row.name,
    kind: row.kind || null,
    typeSignature: row.type_signature || null,
    moduleName: row.module_name || null,
    packageName: row.package_name,
    packageVersion: row.package_version
  }));
  return JSON.stringify({ results, count: results.length });
};

// =============================================================================
// Module Declaration Stats (bulk)
// =============================================================================

export const buildModuleDeclarationStatsJson = (rows) => {
  // Group declaration counts by module
  const moduleStats = new Map();

  for (const row of (rows || [])) {
    const moduleId = Number(row.module_id);
    const kind = row.kind;
    const count = Number(row.count) || 0;

    if (!moduleStats.has(moduleId)) {
      moduleStats.set(moduleId, {
        moduleId,
        kinds: {},
        total: 0
      });
    }

    const stats = moduleStats.get(moduleId);
    stats.kinds[kind] = count;
    stats.total += count;
  }

  const stats = Array.from(moduleStats.values());
  return JSON.stringify({ stats, count: stats.length });
};

// =============================================================================
// All Imports (bulk)
// =============================================================================

export const buildAllImportsJson = (rows) => {
  // Group imports by module
  const moduleImports = new Map();

  for (const row of (rows || [])) {
    const moduleId = Number(row.module_id);
    const moduleName = row.module_name;

    if (!moduleImports.has(moduleId)) {
      moduleImports.set(moduleId, {
        moduleId,
        moduleName,
        imports: []
      });
    }

    // Add import if not null (LEFT JOIN may produce null)
    if (row.imported_module) {
      moduleImports.get(moduleId).imports.push(row.imported_module);
    }
  }

  const imports = Array.from(moduleImports.values());
  return JSON.stringify({ imports, count: imports.length });
};

// =============================================================================
// All Function Calls (bulk)
// =============================================================================

export const buildAllCallsJson = (rows) => {
  // Group calls by module
  const moduleCalls = new Map();

  for (const row of (rows || [])) {
    const moduleId = Number(row.module_id);
    const moduleName = row.module_name;

    if (!moduleCalls.has(moduleId)) {
      moduleCalls.set(moduleId, {
        moduleId,
        moduleName,
        calls: []
      });
    }

    // Add call if not null (LEFT JOIN may produce null)
    if (row.caller_name && row.callee_name) {
      moduleCalls.get(moduleId).calls.push({
        callerName: row.caller_name,
        calleeModule: row.callee_module,
        calleeName: row.callee_name
      });
    }
  }

  const calls = Array.from(moduleCalls.values());
  return JSON.stringify({ calls, count: calls.length });
};

// =============================================================================
// Polyglot Summary (for sunburst visualization)
// =============================================================================

export const buildPolyglotSummaryJson = (projectRows) => (packageRows) => {
  // Build hierarchical structure: backend -> project -> packages
  const backends = new Map();

  // Process projects
  for (const row of (projectRows || [])) {
    const backend = row.backend || 'js';
    const projectId = Number(row.project_id);

    if (!backends.has(backend)) {
      backends.set(backend, {
        name: backend,
        displayName: getBackendDisplayName(backend),
        projects: new Map(),
        totalLoc: 0,
        packageCount: 0
      });
    }

    const backendData = backends.get(backend);
    backendData.projects.set(projectId, {
      id: projectId,
      name: row.project_name,
      backend,
      packageCount: Number(row.package_count) || 0,
      packages: [],
      ffiLoc: {
        js: Number(row.ffi_js_loc) || 0,
        erlang: Number(row.ffi_erlang_loc) || 0,
        python: Number(row.ffi_python_loc) || 0,
        lua: Number(row.ffi_lua_loc) || 0,
        rust: Number(row.ffi_rust_loc) || 0
      }
    });
  }

  // Process packages and add to their projects
  for (const row of (packageRows || [])) {
    const projectId = Number(row.project_id);

    // Find which backend this project belongs to
    for (const [, backendData] of backends) {
      if (backendData.projects.has(projectId)) {
        const project = backendData.projects.get(projectId);
        const pkgLoc = Number(row.total_loc) || 0;

        project.packages.push({
          id: Number(row.package_id),
          name: row.package_name,
          version: row.package_version,
          source: row.package_source,
          totalLoc: pkgLoc,
          moduleCount: Number(row.module_count) || 0,
          ffiFileCount: Number(row.ffi_file_count) || 0,
          ffiLoc: {
            js: Number(row.ffi_js_loc) || 0,
            erlang: Number(row.ffi_erlang_loc) || 0,
            python: Number(row.ffi_python_loc) || 0,
            lua: Number(row.ffi_lua_loc) || 0,
            rust: Number(row.ffi_rust_loc) || 0
          }
        });

        backendData.totalLoc += pkgLoc;
        backendData.packageCount += 1;
        break;
      }
    }
  }

  // Convert to final structure for D3 sunburst
  // Format: { name: "root", children: [{ name: "js", children: [...] }] }
  const children = Array.from(backends.values()).map(backend => ({
    name: backend.name,
    displayName: backend.displayName,
    totalLoc: backend.totalLoc,
    packageCount: backend.packageCount,
    children: Array.from(backend.projects.values()).map(project => ({
      name: project.name,
      id: project.id,
      backend: project.backend,
      packageCount: project.packageCount,
      ffiLoc: project.ffiLoc,
      children: project.packages.map(pkg => ({
        name: pkg.name,
        id: pkg.id,
        version: pkg.version,
        source: pkg.source,
        value: Math.max(pkg.totalLoc, 100), // Minimum size for visibility
        totalLoc: pkg.totalLoc,
        moduleCount: pkg.moduleCount,
        ffiFileCount: pkg.ffiFileCount,
        ffiLoc: pkg.ffiLoc
      }))
    }))
  }));

  return JSON.stringify({
    name: "polyglot",
    children,
    backendCount: backends.size,
    projectCount: projectRows ? projectRows.length : 0,
    packageCount: packageRows ? packageRows.length : 0
  });
};

function getBackendDisplayName(backend) {
  const names = {
    'js': 'JavaScript',
    'erlang': 'Erlang (Purerl)',
    'python': 'Python (PurePy)',
    'lua': 'Lua (PsLua)',
    'rust': 'Rust/WASM'
  };
  return names[backend] || backend;
}

// =============================================================================
// Type Class Stats
// =============================================================================

export const buildTypeClassStatsJson = (rows) => {
  const typeClasses = (rows || []).map(row => ({
    id: Number(row.id),
    name: row.name,
    moduleName: row.module_name,
    packageName: row.package_name,
    methodCount: Number(row.method_count) || 0,
    instanceCount: Number(row.instance_count) || 0
  }));

  // Compute summary stats
  const totalMethods = typeClasses.reduce((sum, tc) => sum + tc.methodCount, 0);
  const totalInstances = typeClasses.reduce((sum, tc) => sum + tc.instanceCount, 0);

  return JSON.stringify({
    typeClasses,
    count: typeClasses.length,
    summary: {
      totalMethods,
      totalInstances,
      avgMethodsPerClass: typeClasses.length > 0 ? (totalMethods / typeClasses.length).toFixed(1) : 0,
      avgInstancesPerClass: typeClasses.length > 0 ? (totalInstances / typeClasses.length).toFixed(1) : 0
    }
  });
};

// =============================================================================
// Git Status (live query)
// =============================================================================

import { execSync } from 'child_process';
import { existsSync } from 'fs';

/**
 * Get current git status and map file paths to module names.
 * Runs in the project root (minard directory).
 *
 * Returns: { modified: [...], staged: [...], untracked: [...] }
 * Each entry is a module name (e.g., "CE2.Component.SceneCoordinator")
 */
export const getGitStatusJson = () => {
  try {
    // Run git status from the minard root (one level up from server)
    const projectRoot = process.cwd().replace(/\/server$/, '');
    const output = execSync('git status --porcelain', {
      cwd: projectRoot,
      encoding: 'utf8'
    });

    const modified = [];   // Modified but not staged (M in second column or ' M')
    const staged = [];     // Staged for commit (M/A/D in first column)
    const untracked = [];  // Untracked files (??)

    const lines = output.trim().split('\n').filter(line => line.length > 0);

    for (const line of lines) {
      const statusCode = line.substring(0, 2);
      const filePath = line.substring(3).trim();

      // Only process PureScript files in frontend/src
      if (!filePath.endsWith('.purs') || !filePath.startsWith('frontend/src/')) {
        continue;
      }

      // Convert path to module name
      // frontend/src/CE2/Component/SceneCoordinator.purs -> CE2.Component.SceneCoordinator
      const moduleName = filePath
        .replace('frontend/src/', '')
        .replace(/\.purs$/, '')
        .replace(/\//g, '.');

      // Parse git status codes
      // First char = staged status, Second char = working tree status
      const stagedStatus = statusCode[0];
      const workingStatus = statusCode[1];

      if (statusCode === '??') {
        untracked.push(moduleName);
      } else {
        // Check if staged (first column has M, A, D, R, C)
        if ('MADRC'.includes(stagedStatus)) {
          staged.push(moduleName);
        }
        // Check if modified in working tree (second column has M)
        if (workingStatus === 'M') {
          modified.push(moduleName);
        }
      }
    }

    return JSON.stringify({
      modified,
      staged,
      untracked,
      timestamp: Date.now()
    });
  } catch (error) {
    // Git not available - return mock data for UI development
    return getMockGitStatus();
  }
};

/**
 * Generate mock git status for UI development/testing.
 * Uses timestamp-based seed to give semi-random but stable results within a time window.
 * This lets us see different "commits" by refreshing.
 */
function getMockGitStatus() {
  // Pool of realistic module names (minard frontend modules)
  const allModules = [
    'CE2.Component.SceneCoordinator',
    'CE2.Component.AppShell',
    'CE2.Component.BubblePackBeeswarmViz',
    'CE2.Component.GalaxyBeeswarmViz',
    'CE2.Component.ModuleTreemapEnrichedViz',
    'CE2.Data.Loader',
    'CE2.Data.Filter',
    'CE2.Types',
    'CE2.Scene',
    'CE2.Viz.PackageSetTreemap',
    'CE2.Viz.PackageSetBeeswarm',
    'CE2.Viz.ModuleTreemap',
    'CE2.Viz.ModuleTreemapEnriched',
    'CE2.Viz.ModuleBeeswarm',
    'CE2.Viz.ModuleBubblePack',
    'CE2.Viz.BubblePackBeeswarm',
    'CE2.Viz.DependencyMatrix',
    'CE2.Viz.DependencyChord',
    'CE2.Viz.DependencyAdjacency',
    'CE2.Viz.TypeClassGrid',
    'CE2.Viz.PolyglotSunburst',
  ];

  // Use minute-based seed so results change every minute (simulates different commits)
  const seed = Math.floor(Date.now() / 60000);
  const rng = mulberry32(seed);

  // Shuffle and pick random subsets
  const shuffled = [...allModules].sort(() => rng() - 0.5);

  // Pick 2-4 modified, 1-3 staged, 0-2 untracked
  const numModified = 2 + Math.floor(rng() * 3);
  const numStaged = 1 + Math.floor(rng() * 3);
  const numUntracked = Math.floor(rng() * 3);

  const modified = shuffled.slice(0, numModified);
  const staged = shuffled.slice(numModified, numModified + numStaged);
  const untracked = shuffled.slice(numModified + numStaged, numModified + numStaged + numUntracked);

  return JSON.stringify({
    modified,
    staged,
    untracked,
    mock: true,  // Flag so UI knows this is mock data
    timestamp: Date.now()
  });
}

// Simple seeded PRNG (mulberry32)
function mulberry32(seed) {
  return function() {
    let t = seed += 0x6D2B79F5;
    t = Math.imul(t ^ t >>> 15, t | 1);
    t ^= t + Math.imul(t ^ t >>> 7, t | 61);
    return ((t ^ t >>> 14) >>> 0) / 4294967296;
  };
}
