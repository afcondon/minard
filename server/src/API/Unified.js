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
    depends: row.depends ? row.depends.split(',').filter(d => d.trim()) : []
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

export const buildDeclarationsJson = (declarations) => (children) => {
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

  const decls = (declarations || []).map(d => {
    const declId = Number(d.id);
    return {
      id: declId,
      name: d.name,
      kind: d.kind,
      typeSignature: d.type_signature || null,
      comments: d.comments || null,
      dataDeclType: d.data_decl_type || null,
      sourceSpan: d.source_span ? JSON.parse(d.source_span) : null,
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
