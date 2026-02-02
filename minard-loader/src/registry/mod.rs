//! Registry package loading from local output directory
//!
//! Instead of downloading from the registry (which only has source, not compiled output),
//! this module loads dependency modules from the project's own output/ directory.
//! When spago builds, it compiles ALL modules (workspace + dependencies) to output/.
//! The source_span in docs.json tells us whether a module is local or from .spago/.

use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};

use crate::db::{
    get_or_create_namespace, insert_child_declarations, insert_declarations, insert_modules,
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

    if verbose {
        eprintln!(
            "  Found {} dependency modules across {} packages",
            modules_by_package.values().map(|v| v.len()).sum::<usize>(),
            matched_packages.len()
        );
    }

    // Now insert modules for each package
    progress.set_message("Inserting dependency modules...");
    progress.set_total(matched_packages.len() as u64);

    for (pkg_id, modules_data) in modules_by_package {
        progress.inc(1);

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

        // Create namespaces
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

        // Insert into database
        insert_modules(conn, &all_modules)?;
        stats.modules_loaded += all_modules.len();

        insert_declarations(conn, &all_declarations)?;
        stats.declarations_loaded += all_declarations.len();

        insert_child_declarations(conn, &all_children)?;
        stats.child_declarations_loaded += all_children.len();

        stats.packages_loaded += 1;
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
