use duckdb::params;
use rayon::prelude::*;
use serde_json::Value;
use std::collections::{HashMap, HashSet};
use std::path::Path;
use std::sync::Mutex;
use std::time::Instant;

use crate::db::{
    cleanup_orphaned_package_versions, delete_old_snapshots, delete_package_module_data,
    get_max_ids, get_or_create_namespace, get_or_create_project,
    append_child_declarations, append_declarations, append_modules,
    append_package_dependencies, append_reexports, append_snapshot_packages,
    insert_package_versions_with_ids, insert_snapshot,
    package_has_modules, update_package_ffi_stats, IdGenerator,
};
use super::postload::{
    compute_topo_layers, insert_module_imports, insert_function_calls, collect_git_data,
    update_module_metrics, update_coupling_metrics, resolve_import_module_ids,
};
use crate::error::Result;
use crate::git::get_git_info;
use crate::model::{
    ChildDeclaration, Declaration, LoadStats, Module, PackageDependency, PackageVersion,
    ParsedModule, Snapshot, SnapshotPackage,
};
use crate::parse::{render_type, CstSpanIndex, DocsJson, SpagoLock};
use crate::progress::ProgressReporter;
use crate::registry::{
    get_registry_packages_to_load, load_registry_modules_from_output,
    get_extra_packages_to_load, load_extra_modules_from_output,
};

use super::detect::{scan_ffi_files, extract_bundle_module};
use super::discovery::ProjectDiscovery;

/// Phase timing helper for verbose instrumentation
struct PhaseTimer {
    phase_start: Instant,
    verbose: bool,
}

impl PhaseTimer {
    fn new(verbose: bool) -> Self {
        Self {
            phase_start: Instant::now(),
            verbose,
        }
    }

    /// Mark the end of one phase and the start of the next. Prints elapsed time if verbose.
    fn lap(&mut self, phase_name: &str) {
        if self.verbose {
            let elapsed = self.phase_start.elapsed();
            eprintln!("  [{:>6.1}s] {}", elapsed.as_secs_f64(), phase_name);
        }
        self.phase_start = Instant::now();
    }
}

/// Main load pipeline
pub struct LoadPipeline {
    id_gen: IdGenerator,
    verbose: bool,
    cst_index: CstSpanIndex,
}

impl LoadPipeline {
    pub fn new(verbose: bool) -> Self {
        Self {
            id_gen: IdGenerator::new(),
            verbose,
            cst_index: CstSpanIndex::empty(),
        }
    }

    /// Set the CST span index for accurate declaration source extraction.
    pub fn set_cst_index(&mut self, index: CstSpanIndex) {
        self.cst_index = index;
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

        let mut timer = PhaseTimer::new(self.verbose);

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

        timer.lap("project record");

        // Phase 2: Get git info and create snapshot
        progress.set_message("Creating snapshot...");
        let git_info = get_git_info(&discovery.project_path);
        let snapshot_id = self.id_gen.next_snapshot_id();

        let label = snapshot_label
            .map(|s| s.to_string())
            .or_else(|| git_info.as_ref().and_then(|g| g.ref_name.clone()))
            .unwrap_or_else(|| "manual".to_string());

        // Clean up old snapshots for this project BEFORE inserting the new one.
        // This avoids UNIQUE(project_id, git_hash) conflicts when reloading at the same commit.
        match delete_old_snapshots(conn, project_id, snapshot_id) {
            Ok(deleted) if deleted > 0 => {
                if self.verbose {
                    eprintln!("  Deleted {} old snapshot(s) for project {}", deleted, project_name);
                }
            }
            Err(e) if self.verbose => eprintln!("Warning: Failed to clean old snapshots: {}", e),
            _ => {}
        }

        let snapshot = Snapshot {
            id: snapshot_id,
            project_id,
            git_hash: git_info.as_ref().map(|g| g.hash.clone()),
            git_ref: git_info.as_ref().and_then(|g| g.ref_name.clone()),
            label: Some(label.clone()),
        };
        insert_snapshot(conn, &snapshot)?;
        stats.snapshot_label = Some(label);

        timer.lap("snapshot + git info");

        // Phase 3: Parse spago.lock
        progress.set_message("Parsing spago.lock...");
        let spago_lock = SpagoLock::from_path(&discovery.spago_lock_path)?;

        timer.lap("parse spago.lock");

        // Phase 4: Create package versions (with deduplication)
        progress.set_message("Creating package versions...");
        let packages = spago_lock.all_packages();

        // Build map of workspace package name -> bundle_module (from each package's spago.yaml)
        let mut bundle_modules: HashMap<String, String> = HashMap::new();
        for (name, ws_pkg) in &spago_lock.workspace.packages {
            let ws_yaml_path = discovery.project_path.join(&ws_pkg.path).join("spago.yaml");
            if let Some(bundle_mod) = extract_bundle_module(&ws_yaml_path) {
                bundle_modules.insert(name.clone(), bundle_mod);
            }
        }

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
                bundle_module: bundle_modules.get(&pkg_info.name).cloned(),
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

        // Build set of workspace package names for force-reload detection
        let workspace_names: HashSet<String> = spago_lock
            .workspace
            .packages
            .keys()
            .cloned()
            .collect();

        // Track which packages are new vs reused
        // Workspace packages are always reloaded (source code may have changed)
        // Registry packages are reused if they already have modules (immutable)
        let mut packages_to_load: HashSet<i64> = HashSet::new();
        for pkg in &package_versions {
            if let Some(&actual_id) = pkg_id_map.get(&(pkg.name.clone(), pkg.version.clone())) {
                let is_workspace = workspace_names.contains(&pkg.name);

                if is_workspace && package_has_modules(conn, actual_id)? {
                    // Workspace package with existing data: delete and reload
                    let deleted = delete_package_module_data(conn, actual_id)?;
                    if self.verbose {
                        eprintln!("  Refreshing workspace package {} (deleted {} stale modules)", pkg.name, deleted);
                    }
                    packages_to_load.insert(actual_id);
                    stats.packages_loaded += 1;
                } else if !package_has_modules(conn, actual_id)? {
                    // New package (workspace or registry): load fresh
                    packages_to_load.insert(actual_id);
                    stats.packages_loaded += 1;
                } else {
                    // Registry package with existing data: reuse
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
        // Clear existing deps for packages_to_load to avoid PK violations with Appender
        // (delete_package_module_data handles workspace refreshes, but registry packages
        // shared across projects may already have deps from a prior load)
        for &pkg_id in &packages_to_load {
            conn.execute(
                "DELETE FROM package_dependencies WHERE dependent_id = ?",
                params![pkg_id],
            )?;
        }

        // Dedup within the current batch (all_packages() may list a package twice)
        let mut seen_deps: HashSet<(i64, String)> = HashSet::new();
        let mut package_deps = Vec::new();
        for pkg_info in &packages {
            if let Some(&pkg_id) = package_map.get(&pkg_info.name) {
                if packages_to_load.contains(&pkg_id) {
                    for dep_name in &pkg_info.dependencies {
                        if seen_deps.insert((pkg_id, dep_name.clone())) {
                            package_deps.push(PackageDependency {
                                dependent_id: pkg_id,
                                dependency_name: dep_name.clone(),
                            });
                        }
                    }
                }
            }
        }

        append_package_dependencies(conn, &package_deps)?;
        stats.dependencies_loaded = package_deps.len();

        // Build workspace path -> package_id map for module assignment
        let mut workspace_pkg_ids: Vec<i64> = Vec::new();
        let mut workspace_paths: Vec<(String, i64)> = Vec::new(); // (path_prefix, pkg_id)
        for (ws_name, ws_path) in spago_lock.workspace_package_paths() {
            if let Some(&pkg_id) = package_map.get(&ws_name) {
                workspace_pkg_ids.push(pkg_id);
                workspace_paths.push((ws_path, pkg_id));
            }
        }
        // Sort by path length descending so longer (more specific) paths match first.
        // A root workspace with path "." (1 char) sorts last as a catch-all.
        workspace_paths.sort_by(|a, b| b.0.len().cmp(&a.0.len()));

        // Create snapshot_packages with actual IDs
        let mut snapshot_packages = Vec::new();
        for pkg in &package_versions {
            if let Some(&actual_id) = pkg_id_map.get(&(pkg.name.clone(), pkg.version.clone())) {
                snapshot_packages.push(SnapshotPackage {
                    snapshot_id,
                    package_version_id: actual_id,
                    source: pkg.source.clone(),
                    is_direct: workspace_pkg_ids.contains(&actual_id),
                });
            }
        }

        append_snapshot_packages(conn, &snapshot_packages)?;

        timer.lap(&format!("package versions ({} new, {} reused)", stats.packages_loaded, stats.packages_reused));

        // Phase 5: Parse docs.json files in parallel (for workspace packages)
        // Registry packages should be loaded from .spago cache, not output/
        progress.set_message("Parsing docs.json files...");
        progress.set_total(discovery.module_count() as u64);

        let parse_errors = Mutex::new(0usize);

        // Only process if any workspace package needs loading
        let any_workspace_needs_loading = workspace_pkg_ids
            .iter()
            .any(|id| packages_to_load.contains(id));

        // Filter to only load LOCAL modules (from src/) not dependency modules (from .spago/)
        // Each local module is assigned to its workspace package by matching source paths
        let parsed_modules: Vec<ParsedModule> = if any_workspace_needs_loading {
            let local_modules = Mutex::new(0usize);
            let dep_modules = Mutex::new(0usize);

            let result: Vec<ParsedModule> = discovery
                .docs_json_files
                .par_iter()
                .filter_map(|docs_path| {
                    progress.inc(1);

                    match self.parse_docs_json_for_workspace(docs_path, &workspace_paths) {
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
            // All workspace packages already loaded, skip parsing
            stats.modules_reused = discovery.module_count();
            Vec::new()
        };

        stats.parse_errors = *parse_errors.lock().unwrap();

        timer.lap(&format!("parse docs.json ({} local modules)", parsed_modules.len()));

        // Phase 6: Create namespaces and insert into database
        progress.set_message("Creating namespaces and inserting...");

        let mut all_modules = Vec::new();
        let mut all_declarations = Vec::new();
        let mut all_children = Vec::new();
        let mut all_reexports: Vec<(i64, Vec<(String, String)>)> = Vec::new();

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

            // Collect re-exports with module ID
            if !parsed.re_exports.is_empty() {
                all_reexports.push((parsed.module.id, parsed.re_exports));
            }

            all_modules.push(parsed.module);
            all_declarations.extend(parsed.declarations);
            all_children.extend(parsed.child_declarations);
        }

        append_modules(conn, &all_modules)?;
        stats.modules_loaded = all_modules.len();

        append_declarations(conn, &all_declarations)?;
        stats.declarations_loaded = all_declarations.len();

        append_child_declarations(conn, &all_children)?;
        stats.child_declarations_loaded = all_children.len();

        // Insert re-exports (collect into flat Vec for single append call)
        let flat_reexports: Vec<(i64, String, String)> = all_reexports
            .iter()
            .flat_map(|(module_id, re_exports)| {
                re_exports
                    .iter()
                    .map(move |(src_mod, decl_name)| (*module_id, src_mod.clone(), decl_name.clone()))
            })
            .collect();
        append_reexports(conn, &flat_reexports)?;

        timer.lap(&format!("insert modules/decls ({} modules, {} decls)", stats.modules_loaded, stats.declarations_loaded));

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

        timer.lap(&format!("registry packages ({} pkgs, {} modules)", stats.registry_packages_loaded, stats.registry_modules_loaded));

        // Phase 6c: Load extra (local/git) packages from output directory
        // These are non-registry, non-workspace packages (e.g. local workspace deps from sibling repos)
        progress.set_message("Loading extra packages...");
        let extra_packages = get_extra_packages_to_load(conn)?;
        if !extra_packages.is_empty() {
            // Build path map from spago.lock's packages section
            let extra_pkg_paths: HashMap<String, String> = spago_lock.packages.iter()
                .filter(|(_, entry)| entry.source_type != "registry")
                .filter_map(|(name, entry)| {
                    entry.path.as_ref().map(|p| (name.clone(), p.clone()))
                })
                .collect();

            if self.verbose {
                eprintln!("Loading {} extra packages from output...", extra_packages.len());
            }
            match load_extra_modules_from_output(
                conn,
                &discovery.output_dir,
                &extra_packages,
                &extra_pkg_paths,
                &self.id_gen,
                progress,
                self.verbose,
            ) {
                Ok(extra_stats) => {
                    stats.extra_packages_loaded = extra_stats.packages_loaded;
                    stats.extra_modules_loaded = extra_stats.modules_loaded;
                    stats.extra_declarations_loaded = extra_stats.declarations_loaded;
                }
                Err(e) if self.verbose => eprintln!("Warning: Failed to load extra packages: {}", e),
                _ => {}
            }
        }

        timer.lap(&format!("extra packages ({} pkgs, {} modules)", stats.extra_packages_loaded, stats.extra_modules_loaded));

        // Phase 7: Post-load processing (imports, calls, git data)
        // Process each workspace package separately
        progress.set_message("Extracting module imports...");
        for &ws_pkg_id in &workspace_pkg_ids {
            if let Ok(count) = insert_module_imports(conn, &discovery.output_dir, ws_pkg_id) {
                stats.module_imports_loaded += count;
            }
        }

        timer.lap(&format!("module imports ({} imports)", stats.module_imports_loaded));

        // Phase 7b: Resolve import module IDs (name → id)
        progress.set_message("Resolving import module IDs...");
        match resolve_import_module_ids(conn) {
            Ok(count) => {
                if self.verbose {
                    eprintln!("  Resolved {} import module IDs", count);
                }
            }
            Err(e) if self.verbose => eprintln!("Warning: Failed to resolve import module IDs: {}", e),
            _ => {}
        }

        timer.lap("resolve import module IDs");

        progress.set_message("Extracting function calls...");
        for &ws_pkg_id in &workspace_pkg_ids {
            match insert_function_calls(conn, &discovery.output_dir, ws_pkg_id) {
                Ok(count) => stats.function_calls_loaded += count,
                Err(e) if self.verbose => eprintln!("Warning: Failed to extract function calls: {}", e),
                _ => {}
            }
        }

        timer.lap(&format!("function calls ({} calls)", stats.function_calls_loaded));

        // Phase 8: Git data collection
        progress.set_message("Collecting git data...");
        for &ws_pkg_id in &workspace_pkg_ids {
            match collect_git_data(
                conn,
                &discovery.project_path,
                project_id,
                ws_pkg_id,
            ) {
                Ok((commits, _links)) => stats.commits_loaded += commits,
                Err(e) if self.verbose => eprintln!("Warning: Failed to collect git data: {}", e),
                _ => {}
            }
        }

        timer.lap(&format!("git data ({} commits)", stats.commits_loaded));

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
            // FFI scan is project-wide; assign to first workspace package
            if let Some(&first_ws_id) = workspace_pkg_ids.first() {
                let _ = update_package_ffi_stats(conn, first_ws_id, &ffi_stats);
            }
        }

        timer.lap("FFI scan");

        // Phase 9: Update metrics
        progress.set_message("Updating metrics...");
        for &ws_pkg_id in &workspace_pkg_ids {
            let _ = update_module_metrics(conn, ws_pkg_id);
            let _ = update_coupling_metrics(conn, ws_pkg_id);
        }

        timer.lap("metrics");

        // Phase 10: Compute topological layers
        progress.set_message("Computing topological layers...");
        if let Ok(count) = compute_topo_layers(conn, snapshot_id) {
            stats.topo_layers_computed = count;
        }

        timer.lap(&format!("topo layers ({} layers)", stats.topo_layers_computed));

        // Phase 11: Clean up orphaned package_versions from old snapshots
        progress.set_message("Cleaning up orphaned packages...");
        match cleanup_orphaned_package_versions(conn) {
            Ok(cleaned) if cleaned > 0 => {
                if self.verbose {
                    eprintln!("  Cleaned up {} orphaned package versions", cleaned);
                }
            }
            Err(e) if self.verbose => eprintln!("Warning: Failed to clean up orphans: {}", e),
            _ => {}
        }

        timer.lap("cleanup orphans");

        stats.elapsed_ms = start.elapsed().as_millis() as u64;

        Ok(stats)
    }

    /// Parse a single docs.json file, assigning it to the appropriate workspace package.
    /// Returns None for dependency modules or if no workspace package matches.
    fn parse_docs_json_for_workspace(
        &self,
        docs_path: &Path,
        workspace_paths: &[(String, i64)],
    ) -> Result<Option<ParsedModule>> {
        let docs = DocsJson::from_path(docs_path)?;

        // Skip dependency modules - only load local workspace modules
        if !docs.is_local_module() {
            return Ok(None);
        }

        // Match source path to the correct workspace package
        let source_path = docs.get_source_path();
        let pkg_id = source_path
            .as_ref()
            .and_then(|sp| {
                workspace_paths
                    .iter()
                    .find(|(prefix, _)| {
                        if prefix == "." || prefix == "./" {
                            true // Root workspace matches all local modules
                        } else {
                            sp.starts_with(prefix)
                                && sp
                                    .as_bytes()
                                    .get(prefix.len())
                                    .map_or(true, |&b| b == b'/')
                        }
                    })
                    .map(|(_, id)| *id)
            })
            .or_else(|| workspace_paths.first().map(|(_, id)| *id));

        match pkg_id {
            Some(id) => Ok(Some(self.parse_docs_json_inner(docs, docs_path, id)?)),
            None => Ok(None),
        }
    }

    /// Parse a single docs.json file (internal implementation)
    fn parse_docs_json_inner(
        &self,
        docs: DocsJson,
        docs_path: &Path,
        package_id: i64,
    ) -> Result<ParsedModule> {
        use crate::parse::CoreFn;
        use std::fs;

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

        // Read the source file for extracting declaration source code.
        // The source_span.name field is a path relative to the project root.
        // Project root = output dir's parent (docs_path is output/ModuleName/docs.json)
        let source_name: Option<String> = docs
            .declarations
            .first()
            .and_then(|d| d.source_span.as_ref())
            .and_then(|s| s.name.clone());

        let source_lines: Option<Vec<String>> = source_name
            .as_ref()
            .and_then(|sn| {
                // output/ModuleName/docs.json → output/ModuleName → output → project root
                let project_root = docs_path.parent()?.parent()?.parent()?;
                let source_path = project_root.join(sn);
                fs::read_to_string(&source_path).ok()
            })
            .map(|content| content.lines().map(|l| l.to_string()).collect());

        // Build a list of ALL top-level definition start lines from the source file.
        // We scan for lines that start with a non-space character and look like PureScript
        // definitions (name ::, name =, data, newtype, type, class, foreign, instance, etc).
        // This gives us much better boundaries than using only exported declarations from docs.json.
        let mut top_level_starts: Vec<usize> = Vec::new();
        if let Some(ref lines) = source_lines {
            for (i, line) in lines.iter().enumerate() {
                if line.is_empty() || line.starts_with(' ') || line.starts_with('\t') {
                    continue;
                }
                // Skip comments and module/import lines
                if line.starts_with("--") || line.starts_with("module ") || line.starts_with("import ") {
                    continue;
                }
                // This is a top-level definition line (1-indexed)
                top_level_starts.push(i + 1);
            }
        }
        top_level_starts.sort();
        top_level_starts.dedup();

        let total_lines = source_lines.as_ref().map(|l| l.len()).unwrap_or(0);

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

            // Extract source code from source file using line numbers.
            // Try CST-based spans first (accurate), fall back to heuristic.
            let source_code = decl.source_span.as_ref().and_then(|span| {
                let lines = source_lines.as_ref()?;

                // Try CST-based span first (from minard-cst pre-processing)
                if let Some(ref sn) = source_name {
                    if let Some((cst_start, cst_end)) = self.cst_index.get_span(sn, &decl.title) {
                        let start_idx = (cst_start as usize).saturating_sub(1);
                        let end_idx = (cst_end as usize).min(lines.len());
                        if start_idx < end_idx {
                            let extracted: Vec<&str> = lines[start_idx..end_idx]
                                .iter()
                                .map(|s| s.as_str())
                                .collect();
                            return Some(extracted.join("\n"));
                        }
                    }
                }

                // Fallback: heuristic using top-level definition starts
                let start_line = span.start[0] as usize;
                if start_line == 0 || start_line > lines.len() {
                    return None;
                }

                // Find the next declaration's start line after this one
                let end_line = top_level_starts
                    .iter()
                    .find(|&&l| l > start_line)
                    .map(|&next_start| {
                        // Walk backwards from next decl to skip blank lines
                        let mut end = next_start - 1;
                        while end > start_line && lines.get(end - 1).map_or(true, |l| l.trim().is_empty()) {
                            end -= 1;
                        }
                        end
                    })
                    .unwrap_or(total_lines); // Last declaration: extend to end of file

                let start_idx = start_line - 1;
                let end_idx = end_line.min(lines.len());
                if start_idx >= end_idx {
                    return None;
                }
                let extracted: Vec<&str> = lines[start_idx..end_idx]
                    .iter()
                    .map(|s| s.as_str())
                    .collect();
                Some(extracted.join("\n"))
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
                source_code,
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

        // Extract re-exports
        let re_exports: Vec<(String, String)> = docs
            .re_exports
            .iter()
            .flat_map(|re| {
                let source_mod = re.module_name_string();
                re.declarations
                    .iter()
                    .map(move |d| (source_mod.clone(), d.title.clone()))
            })
            .collect();

        Ok(ParsedModule {
            module,
            declarations,
            child_declarations,
            re_exports,
        })
    }
}
