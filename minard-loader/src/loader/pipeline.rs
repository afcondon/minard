use rayon::prelude::*;
use serde_json::Value;
use std::collections::{HashMap, HashSet};
use std::path::Path;
use std::sync::Mutex;
use std::time::Instant;

use crate::db::{
    get_max_ids, get_or_create_namespace, get_or_create_project, insert_child_declarations,
    insert_declarations, insert_modules, insert_package_dependencies,
    insert_package_versions_with_ids, insert_snapshot, insert_snapshot_packages,
    package_has_modules, update_package_ffi_stats, IdGenerator,
};
use super::postload::{
    compute_topo_layers, insert_module_imports, insert_function_calls, collect_git_data,
    update_module_metrics, update_coupling_metrics,
};
use crate::error::Result;
use crate::git::get_git_info;
use crate::model::{
    ChildDeclaration, Declaration, LoadStats, Module, PackageDependency, PackageVersion,
    ParsedModule, Snapshot, SnapshotPackage,
};
use crate::parse::{render_type, DocsJson, SpagoLock};
use crate::progress::ProgressReporter;
use crate::registry::{
    get_registry_packages_to_load, load_registry_modules_from_output,
};

use super::detect::scan_ffi_files;
use super::discovery::ProjectDiscovery;

/// Main load pipeline
pub struct LoadPipeline {
    id_gen: IdGenerator,
    verbose: bool,
}

impl LoadPipeline {
    pub fn new(verbose: bool) -> Self {
        Self {
            id_gen: IdGenerator::new(),
            verbose,
        }
    }

    /// Initialize ID generator from database
    pub fn init_ids(&self, conn: &duckdb::Connection) -> Result<()> {
        let max_ids = get_max_ids(conn)?;
        self.id_gen.init_from_db(&max_ids);
        Ok(())
    }

    /// Load a project into the database with project/snapshot tracking
    /// Implements proper deduplication - shared packages are only loaded once
    pub fn load(
        &self,
        conn: &duckdb::Connection,
        discovery: &ProjectDiscovery,
        project_name: &str,
        snapshot_label: Option<&str>,
        progress: &ProgressReporter,
    ) -> Result<LoadStats> {
        let start = Instant::now();
        let mut stats = LoadStats {
            project_name: project_name.to_string(),
            snapshot_label: snapshot_label.map(|s| s.to_string()),
            ..Default::default()
        };

        if self.verbose {
            eprintln!(
                "Found {} docs.json files in {} (backend: {})",
                discovery.module_count(),
                discovery.output_dir.display(),
                discovery.primary_backend
            );
        }

        // Phase 1: Get or create project
        progress.set_message("Creating project record...");
        let project_id = self.id_gen.next_project_id();
        let project_id = get_or_create_project(
            conn,
            project_id,
            project_name,
            discovery.project_path.to_str(),
            discovery.primary_backend,
        )?;

        // Phase 2: Get git info and create snapshot
        progress.set_message("Creating snapshot...");
        let git_info = get_git_info(&discovery.project_path);
        let snapshot_id = self.id_gen.next_snapshot_id();

        let label = snapshot_label
            .map(|s| s.to_string())
            .or_else(|| git_info.as_ref().and_then(|g| g.ref_name.clone()))
            .unwrap_or_else(|| "manual".to_string());

        let snapshot = Snapshot {
            id: snapshot_id,
            project_id,
            git_hash: git_info.as_ref().map(|g| g.hash.clone()),
            git_ref: git_info.as_ref().and_then(|g| g.ref_name.clone()),
            label: Some(label.clone()),
        };
        insert_snapshot(conn, &snapshot)?;
        stats.snapshot_label = Some(label);

        // Phase 3: Parse spago.lock
        progress.set_message("Parsing spago.lock...");
        let spago_lock = SpagoLock::from_path(&discovery.spago_lock_path)?;

        // Phase 4: Create package versions (with deduplication)
        progress.set_message("Creating package versions...");
        let packages = spago_lock.all_packages();
        let mut package_versions = Vec::new();

        for pkg_info in &packages {
            let pkg_id = self.id_gen.next_package_id();
            package_versions.push(PackageVersion {
                id: pkg_id,
                name: pkg_info.name.clone(),
                version: pkg_info.version.clone(),
                description: None,
                license: None,
                repository: None,
                source: pkg_info.source.clone(),
                // FFI stats will be populated by post-load phase
                loc_ffi_js: None,
                loc_ffi_erlang: None,
                loc_ffi_python: None,
                loc_ffi_lua: None,
                ffi_file_count: None,
            });
        }

        // Insert packages and get actual IDs (handles existing packages)
        let pkg_id_map = insert_package_versions_with_ids(conn, &package_versions)?;

        // Track which packages are new vs reused
        let mut packages_to_load: HashSet<i64> = HashSet::new();
        for pkg in &package_versions {
            if let Some(&actual_id) = pkg_id_map.get(&(pkg.name.clone(), pkg.version.clone())) {
                // Check if this package already has modules loaded
                if !package_has_modules(conn, actual_id)? {
                    packages_to_load.insert(actual_id);
                    stats.packages_loaded += 1;
                } else {
                    stats.packages_reused += 1;
                }
            }
        }

        // Build name -> actual_id map for dependencies
        let mut package_map: HashMap<String, i64> = HashMap::new();
        for pkg in &package_versions {
            if let Some(&actual_id) = pkg_id_map.get(&(pkg.name.clone(), pkg.version.clone())) {
                package_map.insert(pkg.name.clone(), actual_id);
            }
        }

        // Create package dependencies using actual IDs (only for new packages)
        let mut package_deps = Vec::new();
        for pkg_info in &packages {
            if let Some(&pkg_id) = package_map.get(&pkg_info.name) {
                if packages_to_load.contains(&pkg_id) {
                    for dep_name in &pkg_info.dependencies {
                        package_deps.push(PackageDependency {
                            dependent_id: pkg_id,
                            dependency_name: dep_name.clone(),
                        });
                    }
                }
            }
        }

        insert_package_dependencies(conn, &package_deps)?;
        stats.dependencies_loaded = package_deps.len();

        // Create workspace package for local modules
        let workspace_pkg_id = self.id_gen.next_package_id();
        let workspace_pkg = PackageVersion {
            id: workspace_pkg_id,
            name: project_name.to_string(),
            version: "0.0.0".to_string(),
            description: Some(format!("Workspace package for {}", project_name)),
            license: None,
            repository: None,
            source: "workspace".to_string(),
            // FFI stats will be populated by post-load phase
            loc_ffi_js: None,
            loc_ffi_erlang: None,
            loc_ffi_python: None,
            loc_ffi_lua: None,
            ffi_file_count: None,
        };
        let workspace_id_map = insert_package_versions_with_ids(conn, &[workspace_pkg.clone()])?;
        let workspace_pkg_id = *workspace_id_map
            .get(&(workspace_pkg.name.clone(), workspace_pkg.version.clone()))
            .unwrap_or(&workspace_pkg_id);

        // Workspace packages are always "new" - we always load their modules
        if !package_has_modules(conn, workspace_pkg_id)? {
            packages_to_load.insert(workspace_pkg_id);
            stats.packages_loaded += 1;
        } else {
            stats.packages_reused += 1;
        }

        // Create snapshot_packages with actual IDs
        let mut snapshot_packages = Vec::new();
        for pkg in &package_versions {
            if let Some(&actual_id) = pkg_id_map.get(&(pkg.name.clone(), pkg.version.clone())) {
                snapshot_packages.push(SnapshotPackage {
                    snapshot_id,
                    package_version_id: actual_id,
                    source: pkg.source.clone(),
                    is_direct: false,
                });
            }
        }

        // Link workspace package to snapshot
        snapshot_packages.push(SnapshotPackage {
            snapshot_id,
            package_version_id: workspace_pkg_id,
            source: "workspace".to_string(),
            is_direct: true,
        });

        insert_snapshot_packages(conn, &snapshot_packages)?;

        // Phase 5: Parse docs.json files in parallel (only for workspace package)
        // Registry packages should be loaded from .spago cache, not output/
        progress.set_message("Parsing docs.json files...");
        progress.set_total(discovery.module_count() as u64);

        let parse_errors = Mutex::new(0usize);

        // Only process if workspace package needs loading
        // Filter to only load LOCAL modules (from src/) not dependency modules (from .spago/)
        let parsed_modules: Vec<ParsedModule> = if packages_to_load.contains(&workspace_pkg_id) {
            let local_modules = Mutex::new(0usize);
            let dep_modules = Mutex::new(0usize);

            let result: Vec<ParsedModule> = discovery
                .docs_json_files
                .par_iter()
                .filter_map(|docs_path| {
                    progress.inc(1);

                    match self.parse_docs_json_if_local(docs_path, workspace_pkg_id) {
                        Ok(Some(parsed)) => {
                            *local_modules.lock().unwrap() += 1;
                            Some(parsed)
                        }
                        Ok(None) => {
                            // Dependency module, skip
                            *dep_modules.lock().unwrap() += 1;
                            None
                        }
                        Err(e) => {
                            if self.verbose {
                                eprintln!("Warning: Failed to parse {}: {}", docs_path.display(), e);
                            }
                            *parse_errors.lock().unwrap() += 1;
                            None
                        }
                    }
                })
                .collect();

            if self.verbose {
                eprintln!(
                    "  Filtered: {} local modules, {} dependency modules skipped",
                    *local_modules.lock().unwrap(),
                    *dep_modules.lock().unwrap()
                );
            }

            result
        } else {
            // Workspace already loaded, skip parsing
            stats.modules_reused = discovery.module_count();
            Vec::new()
        };

        stats.parse_errors = *parse_errors.lock().unwrap();

        // Phase 6: Create namespaces and insert into database
        progress.set_message("Creating namespaces and inserting...");

        let mut all_modules = Vec::new();
        let mut all_declarations = Vec::new();
        let mut all_children = Vec::new();

        // Collect all unique namespace paths
        let mut namespace_paths: HashSet<String> = HashSet::new();
        for parsed in &parsed_modules {
            namespace_paths.insert(parsed.module.name.clone());
        }

        // Create namespaces and build path -> id map
        let mut namespace_map: HashMap<String, i64> = HashMap::new();
        let mut next_ns_id = self.id_gen.current_namespace_id() + 1;

        for path in &namespace_paths {
            let ns_id = get_or_create_namespace(conn, path, &mut next_ns_id)?;
            namespace_map.insert(path.clone(), ns_id);
        }

        // Update the namespace counter
        self.id_gen.set_namespace_counter(next_ns_id - 1);
        stats.namespaces_created = namespace_map.len();

        // Now build modules with namespace IDs
        for mut parsed in parsed_modules {
            // Set namespace_id on the module
            parsed.module.namespace_id = namespace_map.get(&parsed.module.name).copied();

            all_modules.push(parsed.module);
            all_declarations.extend(parsed.declarations);
            all_children.extend(parsed.child_declarations);
        }

        insert_modules(conn, &all_modules)?;
        stats.modules_loaded = all_modules.len();

        insert_declarations(conn, &all_declarations)?;
        stats.declarations_loaded = all_declarations.len();

        insert_child_declarations(conn, &all_children)?;
        stats.child_declarations_loaded = all_children.len();

        // Phase 6b: Load registry packages from output directory
        progress.set_message("Loading registry packages...");
        let registry_packages = get_registry_packages_to_load(conn)?;
        if !registry_packages.is_empty() {
            if self.verbose {
                eprintln!("Loading {} registry packages from output...", registry_packages.len());
            }
            match load_registry_modules_from_output(
                conn,
                &discovery.output_dir,
                &registry_packages,
                &self.id_gen,
                progress,
                self.verbose,
            ) {
                Ok(registry_stats) => {
                    stats.registry_packages_loaded = registry_stats.packages_loaded;
                    stats.registry_modules_loaded = registry_stats.modules_loaded;
                    stats.registry_declarations_loaded = registry_stats.declarations_loaded;
                }
                Err(e) if self.verbose => eprintln!("Warning: Failed to load registry packages: {}", e),
                _ => {}
            }
        }

        // Phase 7: Post-load processing (imports, calls, git data)
        progress.set_message("Extracting module imports...");
        if let Ok(count) = insert_module_imports(conn, &discovery.output_dir, workspace_pkg_id) {
            stats.module_imports_loaded = count;
        }

        progress.set_message("Extracting function calls...");
        match insert_function_calls(conn, &discovery.output_dir, workspace_pkg_id) {
            Ok(count) => stats.function_calls_loaded = count,
            Err(e) if self.verbose => eprintln!("Warning: Failed to extract function calls: {}", e),
            _ => {}
        }

        // Phase 8: Git data collection
        progress.set_message("Collecting git data...");
        match collect_git_data(
            conn,
            &discovery.project_path,
            project_id,
            workspace_pkg_id,
        ) {
            Ok((commits, _links)) => stats.commits_loaded = commits,
            Err(e) if self.verbose => eprintln!("Warning: Failed to collect git data: {}", e),
            _ => {}
        }

        // Phase 8b: Scan FFI files for polyglot statistics
        progress.set_message("Scanning FFI files...");
        let ffi_stats = scan_ffi_files(&discovery.project_path);
        if ffi_stats.file_count > 0 {
            if self.verbose {
                eprintln!(
                    "  FFI: {} files ({} JS, {} Erlang, {} Python, {} Lua LOC)",
                    ffi_stats.file_count,
                    ffi_stats.loc_js,
                    ffi_stats.loc_erlang,
                    ffi_stats.loc_python,
                    ffi_stats.loc_lua
                );
            }
            let _ = update_package_ffi_stats(conn, workspace_pkg_id, &ffi_stats);
        }

        // Phase 9: Update metrics
        progress.set_message("Updating metrics...");
        let _ = update_module_metrics(conn, workspace_pkg_id);
        let _ = update_coupling_metrics(conn, workspace_pkg_id);

        // Phase 10: Compute topological layers
        progress.set_message("Computing topological layers...");
        if let Ok(count) = compute_topo_layers(conn, snapshot_id) {
            stats.topo_layers_computed = count;
        }

        stats.elapsed_ms = start.elapsed().as_millis() as u64;

        Ok(stats)
    }

    /// Parse a single docs.json file, returning None if it's a dependency module
    fn parse_docs_json_if_local(
        &self,
        docs_path: &Path,
        package_id: i64,
    ) -> Result<Option<ParsedModule>> {
        let docs = DocsJson::from_path(docs_path)?;

        // Skip dependency modules - only load local workspace modules
        if !docs.is_local_module() {
            return Ok(None);
        }

        Ok(Some(self.parse_docs_json_inner(docs, docs_path, package_id)?))
    }

    /// Parse a single docs.json file (internal implementation)
    fn parse_docs_json_inner(
        &self,
        docs: DocsJson,
        docs_path: &Path,
        package_id: i64,
    ) -> Result<ParsedModule> {
        use crate::parse::CoreFn;

        let module_id = self.id_gen.next_module_id();

        // Compute LOC: prefer corefn.json (includes all declarations) over docs.json (exports only)
        let corefn_path = docs_path.with_file_name("corefn.json");
        let loc = if corefn_path.exists() {
            CoreFn::from_path(&corefn_path)
                .ok()
                .and_then(|cf| cf.compute_loc())
                .or_else(|| docs.compute_loc())
        } else {
            docs.compute_loc()
        };

        let module = Module {
            id: module_id,
            package_version_id: package_id,
            namespace_id: None, // Will be set later
            name: docs.name.clone(),
            path: docs_path
                .parent()
                .and_then(|p| p.file_name())
                .and_then(|n| n.to_str())
                .map(|s| s.to_string()),
            comments: docs.comments.clone(),
            loc,
        };

        let mut declarations = Vec::new();
        let mut child_declarations = Vec::new();

        for decl in &docs.declarations {
            let decl_id = self.id_gen.next_declaration_id();

            // Render type signature
            let type_signature = decl.type_ast().map(render_type);

            // Convert type arguments to JSON
            let type_arguments: Option<Value> = decl.type_arguments().map(|args| {
                let arr: Vec<Value> = args
                    .iter()
                    .map(|arg| match arg {
                        crate::parse::docs::TypeArgument::Named(arr) => {
                            serde_json::json!(arr)
                        }
                        crate::parse::docs::TypeArgument::Value(v) => v.clone(),
                    })
                    .collect();
                Value::Array(arr)
            });

            declarations.push(Declaration {
                id: decl_id,
                module_id,
                name: decl.title.clone(),
                kind: decl.kind_str().to_string(),
                type_signature,
                type_ast: decl.type_ast().cloned(),
                data_decl_type: decl.data_decl_type().map(|s| s.to_string()),
                type_arguments,
                roles: decl.roles().map(|r| serde_json::to_value(r).unwrap()),
                superclasses: decl.superclasses().map(|s| Value::Array(s.clone())),
                fundeps: decl.fundeps().map(|f| Value::Array(f.clone())),
                synonym_type: match &decl.info {
                    crate::parse::docs::DeclarationInfo::TypeSynonym { type_info, .. } => {
                        type_info.clone()
                    }
                    _ => None,
                },
                comments: decl.comments.clone(),
                source_span: decl.source_span.as_ref().map(|s| {
                    serde_json::json!({
                        "start": s.start,
                        "end": s.end,
                        "name": s.name
                    })
                }),
            });

            // Process child declarations
            for child in &decl.children {
                let child_id = self.id_gen.next_child_id();

                let type_signature = match &child.info {
                    crate::parse::docs::ChildDeclarationInfo::Instance { type_info, .. } => {
                        type_info.as_ref().map(render_type)
                    }
                    crate::parse::docs::ChildDeclarationInfo::TypeClassMember { type_info } => {
                        type_info.as_ref().map(render_type)
                    }
                    _ => None,
                };

                child_declarations.push(ChildDeclaration {
                    id: child_id,
                    declaration_id: decl_id,
                    name: child.title.clone(),
                    kind: child.kind_str().to_string(),
                    type_signature,
                    type_ast: match &child.info {
                        crate::parse::docs::ChildDeclarationInfo::Instance { type_info, .. } => {
                            type_info.clone()
                        }
                        crate::parse::docs::ChildDeclarationInfo::TypeClassMember { type_info } => {
                            type_info.clone()
                        }
                        _ => None,
                    },
                    constructor_args: child.constructor_args().map(|a| Value::Array(a.clone())),
                    instance_chain: None,
                    instance_constraints: child
                        .instance_constraints()
                        .map(|c| Value::Array(c.clone())),
                    comments: child.comments.clone(),
                    source_span: child.source_span.as_ref().map(|s| {
                        serde_json::json!({
                            "start": s.start,
                            "end": s.end,
                            "name": s.name
                        })
                    }),
                });
            }
        }

        Ok(ParsedModule {
            module,
            declarations,
            child_declarations,
        })
    }
}
