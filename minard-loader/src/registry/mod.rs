//! Registry package loading from local output directory
//!
//! Instead of downloading from the registry (which only has source, not compiled output),
//! this module loads dependency modules from the project's own output/ directory.
//! When spago builds, it compiles ALL modules (workspace + dependencies) to output/.
//! The source_span in docs.json tells us whether a module is local or from .spago/.

use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};
use std::time::Instant;

use crate::db::{
    get_or_create_namespace, append_child_declarations, append_declarations, append_modules,
    IdGenerator,
};
use crate::error::Result;
use crate::model::{ChildDeclaration, Declaration, Module};
use crate::parse::{render_type, DocsJson};
use crate::progress::ProgressReporter;

/// Statistics from loading registry packages
#[derive(Debug, Default)]
pub struct RegistryStats {
    pub packages_loaded: usize,
    pub modules_loaded: usize,
    pub declarations_loaded: usize,
    pub child_declarations_loaded: usize,
}

impl RegistryStats {
    pub fn merge(&mut self, other: &RegistryStats) {
        self.packages_loaded += other.packages_loaded;
        self.modules_loaded += other.modules_loaded;
        self.declarations_loaded += other.declarations_loaded;
        self.child_declarations_loaded += other.child_declarations_loaded;
    }
}

/// Represents a registry package to load
#[derive(Debug, Clone)]
pub struct RegistryPackage {
    pub name: String,
    pub version: String,
    pub package_version_id: i64,
}

/// Query the database for registry packages that need to be loaded
pub fn get_registry_packages_to_load(conn: &duckdb::Connection) -> Result<Vec<RegistryPackage>> {
    let mut stmt = conn.prepare(
        "SELECT pv.id, pv.name, pv.version
         FROM package_versions pv
         LEFT JOIN modules m ON m.package_version_id = pv.id
         WHERE pv.source = 'registry'
         GROUP BY pv.id, pv.name, pv.version
         HAVING COUNT(m.id) = 0",
    )?;

    let packages: Vec<RegistryPackage> = stmt
        .query_map([], |row| {
            Ok(RegistryPackage {
                package_version_id: row.get(0)?,
                name: row.get(1)?,
                version: row.get(2)?,
            })
        })?
        .filter_map(|r| r.ok())
        .collect();

    Ok(packages)
}

/// Extract package name and version from a .spago path
/// Example: ".spago/p/prelude-6.0.1/src/Prelude.purs" -> ("prelude", "6.0.1")
fn parse_spago_path(path: &str) -> Option<(String, String)> {
    // Patterns:
    // .spago/p/name-version/...
    // ./.spago/p/name-version/...
    let path = path.trim_start_matches("./");

    if !path.starts_with(".spago/p/") {
        return None;
    }

    let after_prefix = &path[".spago/p/".len()..];
    let pkg_dir = after_prefix.split('/').next()?;

    // Package directory is name-version, find the last dash followed by version
    // Version starts with a digit
    let mut last_dash_idx = None;
    for (i, c) in pkg_dir.char_indices() {
        if c == '-' {
            // Check if next char is a digit
            if let Some(next_char) = pkg_dir.chars().nth(i + 1) {
                if next_char.is_ascii_digit() {
                    last_dash_idx = Some(i);
                }
            }
        }
    }

    let dash_idx = last_dash_idx?;
    let name = &pkg_dir[..dash_idx];
    let version = &pkg_dir[dash_idx + 1..];

    Some((name.to_string(), version.to_string()))
}

/// Find all docs.json files in an output directory
fn find_docs_json_files(output_dir: &Path) -> Result<Vec<PathBuf>> {
    let mut result = Vec::new();

    if !output_dir.exists() {
        return Ok(result);
    }

    for entry in walkdir::WalkDir::new(output_dir)
        .max_depth(2)
        .into_iter()
        .flatten()
    {
        if entry.file_name() == "docs.json" {
            result.push(entry.path().to_path_buf());
        }
    }

    Ok(result)
}

/// Load registry package modules from the project's output directory
///
/// This scans docs.json files in output/, identifies those that are dependency modules
/// (by checking source_span paths for .spago/), extracts the package name/version,
/// and loads them with the correct package_version_id.
pub fn load_registry_modules_from_output(
    conn: &duckdb::Connection,
    output_dir: &Path,
    registry_packages: &[RegistryPackage],
    id_gen: &IdGenerator,
    progress: &ProgressReporter,
    verbose: bool,
) -> Result<RegistryStats> {
    let mut stats = RegistryStats::default();

    if registry_packages.is_empty() {
        return Ok(stats);
    }

    // Build a map from (name, version) -> package_version_id
    let mut pkg_map: HashMap<(String, String), i64> = HashMap::new();
    for pkg in registry_packages {
        pkg_map.insert((pkg.name.clone(), pkg.version.clone()), pkg.package_version_id);
    }

    // Find all docs.json files
    let t0 = Instant::now();
    let docs_files = find_docs_json_files(output_dir)?;

    if verbose {
        eprintln!("  Scanning {} docs.json files for dependency modules...", docs_files.len());
    }

    progress.set_message("Loading dependency modules...");
    progress.set_total(docs_files.len() as u64);

    // Group modules by package
    let mut modules_by_package: HashMap<i64, Vec<(DocsJson, PathBuf)>> = HashMap::new();
    let mut matched_packages: HashSet<i64> = HashSet::new();

    for docs_path in &docs_files {
        progress.inc(1);

        // Parse docs.json
        let docs = match DocsJson::from_path(docs_path) {
            Ok(d) => d,
            Err(_) => continue,
        };

        // Check if this is a dependency module
        let spago_path = docs.get_source_path();
        if spago_path.is_none() || !spago_path.as_ref().unwrap().contains(".spago/") {
            continue; // Local module, skip
        }

        // Extract package name and version from path
        let source_path = spago_path.unwrap();
        let (pkg_name, pkg_version) = match parse_spago_path(&source_path) {
            Some((n, v)) => (n, v),
            None => continue,
        };

        // Look up package_version_id
        let pkg_id = match pkg_map.get(&(pkg_name.clone(), pkg_version.clone())) {
            Some(&id) => id,
            None => continue, // Package not in our registry list
        };

        matched_packages.insert(pkg_id);
        modules_by_package
            .entry(pkg_id)
            .or_default()
            .push((docs, docs_path.clone()));
    }

    let scan_elapsed = t0.elapsed();
    if verbose {
        eprintln!(
            "  Found {} dependency modules across {} packages (scan: {:.1}s)",
            modules_by_package.values().map(|v| v.len()).sum::<usize>(),
            matched_packages.len(),
            scan_elapsed.as_secs_f64()
        );
    }

    // Now insert modules for each package
    progress.set_message("Inserting dependency modules...");
    progress.set_total(matched_packages.len() as u64);

    let t1 = Instant::now();
    let mut ns_time = std::time::Duration::ZERO;
    let mut insert_time = std::time::Duration::ZERO;
    let mut parse_time = std::time::Duration::ZERO;

    for (pkg_id, modules_data) in modules_by_package {
        progress.inc(1);

        let tp0 = Instant::now();
        let mut all_modules = Vec::new();
        let mut all_declarations = Vec::new();
        let mut all_children = Vec::new();
        let mut namespace_paths: HashSet<String> = HashSet::new();

        for (docs, docs_path) in modules_data {
            let module_id = id_gen.next_module_id();

            // Compute LOC: prefer corefn.json (includes all declarations) over docs.json (exports only)
            let corefn_path = docs_path.with_file_name("corefn.json");
            let loc = if corefn_path.exists() {
                crate::parse::CoreFn::from_path(&corefn_path)
                    .ok()
                    .and_then(|cf| cf.compute_loc())
                    .or_else(|| docs.compute_loc())
            } else {
                docs.compute_loc()
            };

            namespace_paths.insert(docs.name.clone());

            let module = Module {
                id: module_id,
                package_version_id: pkg_id,
                namespace_id: None,
                name: docs.name.clone(),
                path: docs_path
                    .parent()
                    .and_then(|p| p.file_name())
                    .and_then(|n| n.to_str())
                    .map(|s| s.to_string()),
                comments: docs.comments.clone(),
                loc,
            };

            // Parse declarations
            for decl in &docs.declarations {
                let decl_id = id_gen.next_declaration_id();
                let type_signature = decl.type_ast().map(render_type);

                let type_arguments: Option<serde_json::Value> =
                    decl.type_arguments().map(|args| {
                        let arr: Vec<serde_json::Value> = args
                            .iter()
                            .map(|arg| match arg {
                                crate::parse::docs::TypeArgument::Named(arr) => {
                                    serde_json::json!(arr)
                                }
                                crate::parse::docs::TypeArgument::Value(v) => v.clone(),
                            })
                            .collect();
                        serde_json::Value::Array(arr)
                    });

                all_declarations.push(Declaration {
                    id: decl_id,
                    module_id,
                    name: decl.title.clone(),
                    kind: decl.kind_str().to_string(),
                    type_signature,
                    type_ast: decl.type_ast().cloned(),
                    data_decl_type: decl.data_decl_type().map(|s| s.to_string()),
                    type_arguments,
                    roles: decl.roles().map(|r| serde_json::to_value(r).unwrap()),
                    superclasses: decl
                        .superclasses()
                        .map(|s| serde_json::Value::Array(s.clone())),
                    fundeps: decl.fundeps().map(|f| serde_json::Value::Array(f.clone())),
                    synonym_type: match &decl.info {
                        crate::parse::docs::DeclarationInfo::TypeSynonym {
                            type_info, ..
                        } => type_info.clone(),
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
                    source_code: None,
                });

                // Process child declarations
                for child in &decl.children {
                    let child_id = id_gen.next_child_id();

                    let type_signature = match &child.info {
                        crate::parse::docs::ChildDeclarationInfo::Instance {
                            type_info, ..
                        } => type_info.as_ref().map(render_type),
                        crate::parse::docs::ChildDeclarationInfo::TypeClassMember {
                            type_info,
                        } => type_info.as_ref().map(render_type),
                        _ => None,
                    };

                    all_children.push(ChildDeclaration {
                        id: child_id,
                        declaration_id: decl_id,
                        name: child.title.clone(),
                        kind: child.kind_str().to_string(),
                        type_signature,
                        type_ast: match &child.info {
                            crate::parse::docs::ChildDeclarationInfo::Instance {
                                type_info,
                                ..
                            } => type_info.clone(),
                            crate::parse::docs::ChildDeclarationInfo::TypeClassMember {
                                type_info,
                            } => type_info.clone(),
                            _ => None,
                        },
                        constructor_args: child
                            .constructor_args()
                            .map(|a| serde_json::Value::Array(a.clone())),
                        instance_chain: None,
                        instance_constraints: child
                            .instance_constraints()
                            .map(|c| serde_json::Value::Array(c.clone())),
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

            all_modules.push(module);
        }
        parse_time += tp0.elapsed();

        // Create namespaces
        let tn0 = Instant::now();
        let mut namespace_map: HashMap<String, i64> = HashMap::new();
        let mut next_ns_id = id_gen.current_namespace_id() + 1;

        for path in &namespace_paths {
            let ns_id = get_or_create_namespace(conn, path, &mut next_ns_id)?;
            namespace_map.insert(path.clone(), ns_id);
        }

        id_gen.set_namespace_counter(next_ns_id - 1);

        // Set namespace IDs on modules
        for module in &mut all_modules {
            module.namespace_id = namespace_map.get(&module.name).copied();
        }
        ns_time += tn0.elapsed();

        // Insert into database
        let ti0 = Instant::now();
        append_modules(conn, &all_modules)?;
        stats.modules_loaded += all_modules.len();

        append_declarations(conn, &all_declarations)?;
        stats.declarations_loaded += all_declarations.len();

        append_child_declarations(conn, &all_children)?;
        stats.child_declarations_loaded += all_children.len();
        insert_time += ti0.elapsed();

        stats.packages_loaded += 1;
    }

    if verbose {
        let total = t1.elapsed();
        eprintln!(
            "    registry insert breakdown: parse={:.1}s, namespaces={:.1}s, db_insert={:.1}s, total={:.1}s",
            parse_time.as_secs_f64(),
            ns_time.as_secs_f64(),
            insert_time.as_secs_f64(),
            total.as_secs_f64()
        );
    }

    Ok(stats)
}

/// Represents an extra (local/git) package to load
#[derive(Debug, Clone)]
pub struct ExtraPackage {
    pub name: String,
    pub version: String,
    pub package_version_id: i64,
}

/// Query the database for extra packages (non-registry, non-workspace) that need modules loaded
pub fn get_extra_packages_to_load(conn: &duckdb::Connection) -> Result<Vec<ExtraPackage>> {
    let mut stmt = conn.prepare(
        "SELECT pv.id, pv.name, pv.version
         FROM package_versions pv
         LEFT JOIN modules m ON m.package_version_id = pv.id
         WHERE pv.source NOT IN ('registry', 'workspace')
         GROUP BY pv.id, pv.name, pv.version
         HAVING COUNT(m.id) = 0",
    )?;

    let packages: Vec<ExtraPackage> = stmt
        .query_map([], |row| {
            Ok(ExtraPackage {
                package_version_id: row.get(0)?,
                name: row.get(1)?,
                version: row.get(2)?,
            })
        })?
        .filter_map(|r| r.ok())
        .collect();

    Ok(packages)
}

/// Load extra (local/git) package modules from the project's output directory.
///
/// These packages have source paths starting with "../" (pointing to sibling repos)
/// rather than ".spago/" (registry deps). We match docs.json modules to packages
/// by checking if the module's source_span path starts with the package's path from spago.lock.
pub fn load_extra_modules_from_output(
    conn: &duckdb::Connection,
    output_dir: &Path,
    extra_packages: &[ExtraPackage],
    package_paths: &HashMap<String, String>,  // package name -> path from spago.lock
    id_gen: &IdGenerator,
    progress: &ProgressReporter,
    verbose: bool,
) -> Result<RegistryStats> {
    let mut stats = RegistryStats::default();

    if extra_packages.is_empty() {
        return Ok(stats);
    }

    // Build package_version_id lookup by name
    let mut pkg_id_map: HashMap<String, i64> = HashMap::new();
    for pkg in extra_packages {
        pkg_id_map.insert(pkg.name.clone(), pkg.package_version_id);
    }

    // Build sorted path -> package_version_id entries (longest path first for specificity)
    let mut path_entries: Vec<(&str, i64)> = Vec::new();
    for pkg in extra_packages {
        if let Some(path) = package_paths.get(&pkg.name) {
            path_entries.push((path.as_str(), pkg.package_version_id));
        }
    }
    path_entries.sort_by(|a, b| b.0.len().cmp(&a.0.len()));

    if path_entries.is_empty() {
        if verbose {
            eprintln!("  No path mappings found for extra packages");
        }
        return Ok(stats);
    }

    // Find all docs.json files
    let docs_files = find_docs_json_files(output_dir)?;

    if verbose {
        eprintln!("  Scanning {} docs.json files for extra package modules...", docs_files.len());
        for (path, _) in &path_entries {
            eprintln!("    Path prefix: {}", path);
        }
    }

    progress.set_message("Loading extra package modules...");
    progress.set_total(docs_files.len() as u64);

    // Group modules by package
    let mut modules_by_package: HashMap<i64, Vec<(DocsJson, PathBuf)>> = HashMap::new();
    let mut matched_packages: HashSet<i64> = HashSet::new();

    for docs_path in &docs_files {
        progress.inc(1);

        let docs = match DocsJson::from_path(docs_path) {
            Ok(d) => d,
            Err(_) => continue,
        };

        // Get source path and try to match against extra package paths
        let source_path = match docs.get_source_path() {
            Some(sp) => sp,
            None => continue,
        };

        // Skip registry deps
        if source_path.starts_with(".spago/") || source_path.starts_with("./.spago/") {
            continue;
        }

        // Match against known extra package paths
        let pkg_id = path_entries
            .iter()
            .find(|(prefix, _)| {
                source_path.starts_with(prefix)
                    && source_path.as_bytes().get(prefix.len()).map_or(true, |&b| b == b'/')
            })
            .map(|(_, id)| *id);

        if let Some(pkg_id) = pkg_id {
            matched_packages.insert(pkg_id);
            modules_by_package
                .entry(pkg_id)
                .or_default()
                .push((docs, docs_path.clone()));
        }
    }

    if verbose {
        eprintln!(
            "  Found {} extra package modules across {} packages",
            modules_by_package.values().map(|v| v.len()).sum::<usize>(),
            matched_packages.len()
        );
    }

    // Insert modules for each package (same logic as registry loading)
    progress.set_message("Inserting extra package modules...");
    progress.set_total(matched_packages.len() as u64);

    let te_start = Instant::now();
    let mut extra_parse_time = std::time::Duration::ZERO;
    let mut extra_insert_time = std::time::Duration::ZERO;

    for (pkg_id, modules_data) in modules_by_package {
        progress.inc(1);

        let tep0 = Instant::now();
        let mut all_modules = Vec::new();
        let mut all_declarations = Vec::new();
        let mut all_children = Vec::new();
        let mut namespace_paths: HashSet<String> = HashSet::new();

        for (docs, docs_path) in modules_data {
            let module_id = id_gen.next_module_id();

            let corefn_path = docs_path.with_file_name("corefn.json");
            let loc = if corefn_path.exists() {
                crate::parse::CoreFn::from_path(&corefn_path)
                    .ok()
                    .and_then(|cf| cf.compute_loc())
                    .or_else(|| docs.compute_loc())
            } else {
                docs.compute_loc()
            };

            namespace_paths.insert(docs.name.clone());

            let module = Module {
                id: module_id,
                package_version_id: pkg_id,
                namespace_id: None,
                name: docs.name.clone(),
                path: docs_path
                    .parent()
                    .and_then(|p| p.file_name())
                    .and_then(|n| n.to_str())
                    .map(|s| s.to_string()),
                comments: docs.comments.clone(),
                loc,
            };

            for decl in &docs.declarations {
                let decl_id = id_gen.next_declaration_id();
                let type_signature = decl.type_ast().map(render_type);

                let type_arguments: Option<serde_json::Value> =
                    decl.type_arguments().map(|args| {
                        let arr: Vec<serde_json::Value> = args
                            .iter()
                            .map(|arg| match arg {
                                crate::parse::docs::TypeArgument::Named(arr) => {
                                    serde_json::json!(arr)
                                }
                                crate::parse::docs::TypeArgument::Value(v) => v.clone(),
                            })
                            .collect();
                        serde_json::Value::Array(arr)
                    });

                all_declarations.push(Declaration {
                    id: decl_id,
                    module_id,
                    name: decl.title.clone(),
                    kind: decl.kind_str().to_string(),
                    type_signature,
                    type_ast: decl.type_ast().cloned(),
                    data_decl_type: decl.data_decl_type().map(|s| s.to_string()),
                    type_arguments,
                    roles: decl.roles().map(|r| serde_json::to_value(r).unwrap()),
                    superclasses: decl
                        .superclasses()
                        .map(|s| serde_json::Value::Array(s.clone())),
                    fundeps: decl.fundeps().map(|f| serde_json::Value::Array(f.clone())),
                    synonym_type: match &decl.info {
                        crate::parse::docs::DeclarationInfo::TypeSynonym {
                            type_info, ..
                        } => type_info.clone(),
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
                    source_code: None,
                });

                for child in &decl.children {
                    let child_id = id_gen.next_child_id();

                    let type_signature = match &child.info {
                        crate::parse::docs::ChildDeclarationInfo::Instance {
                            type_info, ..
                        } => type_info.as_ref().map(render_type),
                        crate::parse::docs::ChildDeclarationInfo::TypeClassMember {
                            type_info,
                        } => type_info.as_ref().map(render_type),
                        _ => None,
                    };

                    all_children.push(ChildDeclaration {
                        id: child_id,
                        declaration_id: decl_id,
                        name: child.title.clone(),
                        kind: child.kind_str().to_string(),
                        type_signature,
                        type_ast: match &child.info {
                            crate::parse::docs::ChildDeclarationInfo::Instance {
                                type_info,
                                ..
                            } => type_info.clone(),
                            crate::parse::docs::ChildDeclarationInfo::TypeClassMember {
                                type_info,
                            } => type_info.clone(),
                            _ => None,
                        },
                        constructor_args: child
                            .constructor_args()
                            .map(|a| serde_json::Value::Array(a.clone())),
                        instance_chain: None,
                        instance_constraints: child
                            .instance_constraints()
                            .map(|c| serde_json::Value::Array(c.clone())),
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

            all_modules.push(module);
        }
        extra_parse_time += tep0.elapsed();

        // Create namespaces
        let mut namespace_map: HashMap<String, i64> = HashMap::new();
        let mut next_ns_id = id_gen.current_namespace_id() + 1;

        for path in &namespace_paths {
            let ns_id = get_or_create_namespace(conn, path, &mut next_ns_id)?;
            namespace_map.insert(path.clone(), ns_id);
        }

        id_gen.set_namespace_counter(next_ns_id - 1);

        for module in &mut all_modules {
            module.namespace_id = namespace_map.get(&module.name).copied();
        }

        if verbose {
            eprintln!(
                "  Extra pkg_id={}: {} modules, {} declarations, {} children",
                pkg_id, all_modules.len(), all_declarations.len(), all_children.len()
            );
        }

        let tei0 = Instant::now();
        append_modules(conn, &all_modules)?;
        stats.modules_loaded += all_modules.len();

        append_declarations(conn, &all_declarations)?;
        stats.declarations_loaded += all_declarations.len();

        append_child_declarations(conn, &all_children)?;
        stats.child_declarations_loaded += all_children.len();
        extra_insert_time += tei0.elapsed();

        stats.packages_loaded += 1;
    }

    if verbose {
        let total = te_start.elapsed();
        eprintln!(
            "    extra insert breakdown: parse={:.1}s, db_insert={:.1}s, total={:.1}s",
            extra_parse_time.as_secs_f64(),
            extra_insert_time.as_secs_f64(),
            total.as_secs_f64()
        );
    }

    Ok(stats)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_spago_path() {
        assert_eq!(
            parse_spago_path(".spago/p/prelude-6.0.1/src/Prelude.purs"),
            Some(("prelude".to_string(), "6.0.1".to_string()))
        );

        assert_eq!(
            parse_spago_path("./.spago/p/arrays-7.3.0/src/Data/Array.purs"),
            Some(("arrays".to_string(), "7.3.0".to_string()))
        );

        assert_eq!(
            parse_spago_path(".spago/p/aff-promise-4.0.0/src/Control/Promise.purs"),
            Some(("aff-promise".to_string(), "4.0.0".to_string()))
        );

        // Local module - should return None
        assert_eq!(
            parse_spago_path("src/Main.purs"),
            None
        );
    }
}
