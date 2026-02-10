//! Package cache loading
//!
//! Loads packages from extracted tarballs in a package cache directory.
//! This is used to load packages from the registry that aren't in any project's
//! transitive dependencies.

use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::{Path, PathBuf};

use serde::Deserialize;
use walkdir::WalkDir;

use crate::db::{get_or_create_namespace, insert_declarations, insert_modules, IdGenerator};
use crate::error::Result;
use crate::model::{Declaration, Module};
use crate::progress::ProgressReporter;

/// Statistics from loading packages from cache
#[derive(Debug, Default)]
pub struct CacheLoadStats {
    pub packages_loaded: usize,
    pub packages_skipped: usize,
    pub modules_loaded: usize,
    pub declarations_loaded: usize,
    pub total_loc: usize,
}

impl CacheLoadStats {
    pub fn report(&self) -> String {
        format!(
            "Loaded {} packages ({} skipped): {} modules, {} declarations, {} LOC",
            self.packages_loaded,
            self.packages_skipped,
            self.modules_loaded,
            self.declarations_loaded,
            self.total_loc
        )
    }
}

/// Package set entry (from package-set JSON)
#[derive(Debug, Deserialize)]
pub struct PackageSetInfo {
    pub version: String,
    pub compiler: String,
    pub published: String,
    pub packages: HashMap<String, String>, // name -> version
}

/// Parse package set JSON file
pub fn parse_package_set(path: &Path) -> Result<PackageSetInfo> {
    let content = fs::read_to_string(path)?;
    let info: PackageSetInfo = serde_json::from_str(&content)?;
    Ok(info)
}

/// Find all package directories in a cache directory
/// Returns Vec of (package_name, version, path)
pub fn find_cache_packages(cache_dir: &Path) -> Vec<(String, String, PathBuf)> {
    let mut packages = Vec::new();

    if let Ok(entries) = fs::read_dir(cache_dir) {
        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_dir() {
                // Directory name format: "package-name-version"
                if let Some(name) = path.file_name().and_then(|n| n.to_str()) {
                    if let Some((pkg_name, version)) = parse_package_dir_name(name) {
                        packages.push((pkg_name, version, path));
                    }
                }
            }
        }
    }

    packages.sort_by(|a, b| a.0.cmp(&b.0));
    packages
}

/// Parse package directory name to extract name and version
/// Format: "package-name-1.2.3"
fn parse_package_dir_name(name: &str) -> Option<(String, String)> {
    // Find the last dash followed by a digit (start of version)
    let mut last_version_dash = None;
    for (i, c) in name.char_indices() {
        if c == '-' {
            if let Some(next_char) = name.chars().nth(i + 1) {
                if next_char.is_ascii_digit() {
                    last_version_dash = Some(i);
                }
            }
        }
    }

    let dash_idx = last_version_dash?;
    let pkg_name = &name[..dash_idx];
    let version = &name[dash_idx + 1..];

    Some((pkg_name.to_string(), version.to_string()))
}

/// Load packages from cache that aren't already in the database
pub fn load_cache_packages(
    conn: &duckdb::Connection,
    cache_dir: &Path,
    package_set: Option<&PackageSetInfo>,
    id_gen: &IdGenerator,
    progress: &ProgressReporter,
    verbose: bool,
) -> Result<CacheLoadStats> {
    let mut stats = CacheLoadStats::default();

    // Get packages already in database
    let existing_packages = get_existing_packages(conn)?;

    // Find packages in cache
    let cache_packages = find_cache_packages(cache_dir);

    if verbose {
        eprintln!(
            "Found {} packages in cache, {} already in database",
            cache_packages.len(),
            existing_packages.len()
        );
    }

    progress.set_message("Loading packages from cache...");
    progress.set_total(cache_packages.len() as u64);

    for (pkg_name, version, pkg_path) in cache_packages {
        progress.inc(1);

        // Skip if already loaded
        let key = (pkg_name.clone(), version.clone());
        if existing_packages.contains(&key) {
            stats.packages_skipped += 1;
            continue;
        }

        // Verify version matches package set if provided
        if let Some(ps) = package_set {
            if let Some(expected_version) = ps.packages.get(&pkg_name) {
                if expected_version != &version {
                    if verbose {
                        eprintln!(
                            "  Skipping {}-{}: version mismatch (expected {})",
                            pkg_name, version, expected_version
                        );
                    }
                    stats.packages_skipped += 1;
                    continue;
                }
            }
        }

        // Find the actual package source directory (name-version/name-version/src)
        let inner_dir = pkg_path.join(format!("{}-{}", pkg_name, version));
        let src_dir = if inner_dir.exists() {
            inner_dir.join("src")
        } else {
            pkg_path.join("src")
        };

        if !src_dir.exists() {
            if verbose {
                eprintln!("  Skipping {}: no src directory", pkg_name);
            }
            stats.packages_skipped += 1;
            continue;
        }

        // Load the package
        match load_single_package(conn, &pkg_name, &version, &src_dir, id_gen, verbose) {
            Ok(pkg_stats) => {
                stats.packages_loaded += 1;
                stats.modules_loaded += pkg_stats.modules;
                stats.declarations_loaded += pkg_stats.declarations;
                stats.total_loc += pkg_stats.loc;
            }
            Err(e) => {
                if verbose {
                    eprintln!("  Error loading {}: {}", pkg_name, e);
                }
                stats.packages_skipped += 1;
            }
        }
    }

    Ok(stats)
}

/// Get existing packages from database
fn get_existing_packages(conn: &duckdb::Connection) -> Result<HashSet<(String, String)>> {
    let mut stmt = conn.prepare("SELECT name, version FROM package_versions")?;
    let packages: HashSet<(String, String)> = stmt
        .query_map([], |row| Ok((row.get(0)?, row.get(1)?)))?
        .filter_map(|r| r.ok())
        .collect();
    Ok(packages)
}

/// Stats from loading a single package
struct SinglePackageStats {
    modules: usize,
    declarations: usize,
    loc: usize,
}

/// Load a single package from its src directory
fn load_single_package(
    conn: &duckdb::Connection,
    pkg_name: &str,
    version: &str,
    src_dir: &Path,
    id_gen: &IdGenerator,
    _verbose: bool,
) -> Result<SinglePackageStats> {
    let mut stats = SinglePackageStats {
        modules: 0,
        declarations: 0,
        loc: 0,
    };

    // Create package version entry
    let pkg_id = id_gen.next_package_id();
    conn.execute(
        "INSERT INTO package_versions (id, name, version, source) VALUES (?, ?, ?, 'registry')",
        duckdb::params![pkg_id, pkg_name, version],
    )?;

    // Find all .purs files
    let purs_files: Vec<PathBuf> = WalkDir::new(src_dir)
        .into_iter()
        .filter_map(|e| e.ok())
        .filter(|e| {
            e.path()
                .extension()
                .map(|ext| ext == "purs")
                .unwrap_or(false)
        })
        .map(|e| e.path().to_path_buf())
        .collect();

    let mut all_modules = Vec::new();
    let mut all_declarations = Vec::new();
    let mut namespace_paths: HashSet<String> = HashSet::new();

    for purs_path in purs_files {
        // Parse the PureScript file
        if let Some(parsed) = parse_purs_file(&purs_path, pkg_id, id_gen) {
            namespace_paths.insert(parsed.module.name.clone());
            stats.loc += parsed.module.loc.unwrap_or(0) as usize;
            all_modules.push(parsed.module);
            all_declarations.extend(parsed.declarations);
        }
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
    stats.modules = all_modules.len();

    insert_declarations(conn, &all_declarations)?;
    stats.declarations = all_declarations.len();

    Ok(stats)
}

/// Parsed PureScript file data
struct ParsedPursFile {
    module: Module,
    declarations: Vec<Declaration>,
}

/// Parse a PureScript file to extract module name, LOC, and basic declarations
fn parse_purs_file(path: &Path, package_id: i64, id_gen: &IdGenerator) -> Option<ParsedPursFile> {
    let content = fs::read_to_string(path).ok()?;
    let lines: Vec<&str> = content.lines().collect();
    let loc = lines.len() as i32;

    // Extract module name from "module Foo.Bar where" line
    let module_name = extract_module_name(&content)?;

    let module_id = id_gen.next_module_id();

    let module = Module {
        id: module_id,
        package_version_id: package_id,
        namespace_id: None,
        name: module_name.clone(),
        path: path
            .parent()
            .and_then(|p| p.file_name())
            .and_then(|n| n.to_str())
            .map(|s| s.to_string()),
        comments: None,
        loc: Some(loc),
    };

    // Extract top-level declarations (simplified parsing)
    let declarations = extract_declarations(&content, module_id, id_gen);

    Some(ParsedPursFile {
        module,
        declarations,
    })
}

/// Extract module name from PureScript source
fn extract_module_name(content: &str) -> Option<String> {
    for line in content.lines() {
        let trimmed = line.trim();
        if trimmed.starts_with("module ") {
            // module Foo.Bar where
            // module Foo.Bar (exports) where
            let after_module = &trimmed[7..];
            let name_end = after_module
                .find(" where")
                .or_else(|| after_module.find('('))
                .or_else(|| after_module.find('\n'))
                .unwrap_or(after_module.len());
            let name = after_module[..name_end].trim();
            if !name.is_empty() {
                return Some(name.to_string());
            }
        }
    }
    None
}

/// Extract declarations from PureScript source (simplified)
fn extract_declarations(content: &str, module_id: i64, id_gen: &IdGenerator) -> Vec<Declaration> {
    let mut declarations = Vec::new();

    for line in content.lines() {
        let trimmed = line.trim();

        // Skip empty lines, comments, imports
        if trimmed.is_empty()
            || trimmed.starts_with("--")
            || trimmed.starts_with("import")
            || trimmed.starts_with("module")
        {
            continue;
        }

        // Type signatures: name :: Type
        if let Some(idx) = trimmed.find(" :: ") {
            let name = trimmed[..idx].trim();
            // Skip if name contains special characters (operators, etc.)
            if is_valid_identifier(name) {
                let type_sig = trimmed[idx + 4..].trim().to_string();
                let decl_id = id_gen.next_declaration_id();
                declarations.push(Declaration {
                    id: decl_id,
                    module_id,
                    name: name.to_string(),
                    kind: "value".to_string(),
                    type_signature: Some(type_sig),
                    type_ast: None,
                    data_decl_type: None,
                    type_arguments: None,
                    roles: None,
                    superclasses: None,
                    fundeps: None,
                    synonym_type: None,
                    comments: None,
                    source_span: None,
                    source_code: None,
                });
            }
        }
        // Data declarations: data Foo = ...
        else if trimmed.starts_with("data ") {
            let rest = &trimmed[5..];
            let name_end = rest
                .find(' ')
                .or_else(|| rest.find('='))
                .unwrap_or(rest.len());
            let name = rest[..name_end].trim();
            if is_valid_identifier(name) {
                let decl_id = id_gen.next_declaration_id();
                declarations.push(Declaration {
                    id: decl_id,
                    module_id,
                    name: name.to_string(),
                    kind: "data".to_string(),
                    type_signature: None,
                    type_ast: None,
                    data_decl_type: Some("data".to_string()),
                    type_arguments: None,
                    roles: None,
                    superclasses: None,
                    fundeps: None,
                    synonym_type: None,
                    comments: None,
                    source_span: None,
                    source_code: None,
                });
            }
        }
        // Newtype declarations: newtype Foo = ...
        else if trimmed.starts_with("newtype ") {
            let rest = &trimmed[8..];
            let name_end = rest
                .find(' ')
                .or_else(|| rest.find('='))
                .unwrap_or(rest.len());
            let name = rest[..name_end].trim();
            if is_valid_identifier(name) {
                let decl_id = id_gen.next_declaration_id();
                declarations.push(Declaration {
                    id: decl_id,
                    module_id,
                    name: name.to_string(),
                    kind: "data".to_string(),
                    type_signature: None,
                    type_ast: None,
                    data_decl_type: Some("newtype".to_string()),
                    type_arguments: None,
                    roles: None,
                    superclasses: None,
                    fundeps: None,
                    synonym_type: None,
                    comments: None,
                    source_span: None,
                    source_code: None,
                });
            }
        }
        // Type synonyms: type Foo = ...
        else if trimmed.starts_with("type ") && !trimmed.starts_with("type class") {
            let rest = &trimmed[5..];
            let name_end = rest
                .find(' ')
                .or_else(|| rest.find('='))
                .unwrap_or(rest.len());
            let name = rest[..name_end].trim();
            if is_valid_identifier(name) {
                let decl_id = id_gen.next_declaration_id();
                declarations.push(Declaration {
                    id: decl_id,
                    module_id,
                    name: name.to_string(),
                    kind: "type_synonym".to_string(),
                    type_signature: None,
                    type_ast: None,
                    data_decl_type: None,
                    type_arguments: None,
                    roles: None,
                    superclasses: None,
                    fundeps: None,
                    synonym_type: None,
                    comments: None,
                    source_span: None,
                    source_code: None,
                });
            }
        }
        // Type class: class Foo where
        else if trimmed.starts_with("class ") {
            let rest = &trimmed[6..];
            // Skip constraints before =>
            let after_arrow = rest.find("=>").map(|i| &rest[i + 2..]).unwrap_or(rest);
            let name_end = after_arrow
                .find(' ')
                .or_else(|| after_arrow.find('('))
                .unwrap_or(after_arrow.len());
            let name = after_arrow[..name_end].trim();
            if is_valid_identifier(name) {
                let decl_id = id_gen.next_declaration_id();
                declarations.push(Declaration {
                    id: decl_id,
                    module_id,
                    name: name.to_string(),
                    kind: "type_class".to_string(),
                    type_signature: None,
                    type_ast: None,
                    data_decl_type: None,
                    type_arguments: None,
                    roles: None,
                    superclasses: None,
                    fundeps: None,
                    synonym_type: None,
                    comments: None,
                    source_span: None,
                    source_code: None,
                });
            }
        }
        // Foreign import: foreign import name :: Type
        else if trimmed.starts_with("foreign import ") {
            let rest = &trimmed[15..];
            if let Some(idx) = rest.find(" :: ") {
                let name = rest[..idx].trim();
                if is_valid_identifier(name) {
                    let type_sig = rest[idx + 4..].trim().to_string();
                    let decl_id = id_gen.next_declaration_id();
                    declarations.push(Declaration {
                        id: decl_id,
                        module_id,
                        name: name.to_string(),
                        kind: "value".to_string(),
                        type_signature: Some(type_sig),
                        type_ast: None,
                        data_decl_type: None,
                        type_arguments: None,
                        roles: None,
                        superclasses: None,
                        fundeps: None,
                        synonym_type: None,
                        comments: None,
                        source_span: None,
                        source_code: None,
                    });
                }
            }
        }
    }

    // Deduplicate by name (keep first occurrence, which is usually the type signature)
    let mut seen: HashSet<String> = HashSet::new();
    declarations.retain(|d| seen.insert(d.name.clone()));

    declarations
}

/// Check if a name is a valid PureScript identifier
fn is_valid_identifier(name: &str) -> bool {
    if name.is_empty() {
        return false;
    }

    let first = name.chars().next().unwrap();
    if !first.is_ascii_alphabetic() && first != '_' {
        return false;
    }

    // Allow letters, digits, underscores, and primes
    name.chars()
        .all(|c| c.is_ascii_alphanumeric() || c == '_' || c == '\'')
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_package_dir_name() {
        assert_eq!(
            parse_package_dir_name("prelude-6.0.1"),
            Some(("prelude".to_string(), "6.0.1".to_string()))
        );
        assert_eq!(
            parse_package_dir_name("aff-promise-4.0.0"),
            Some(("aff-promise".to_string(), "4.0.0".to_string()))
        );
        assert_eq!(
            parse_package_dir_name("web-html-4.1.1"),
            Some(("web-html".to_string(), "4.1.1".to_string()))
        );
    }

    #[test]
    fn test_extract_module_name() {
        assert_eq!(
            extract_module_name("module Data.Array where\n"),
            Some("Data.Array".to_string())
        );
        assert_eq!(
            extract_module_name("module Prelude\n  ( class Show\n  ) where\n"),
            Some("Prelude".to_string())
        );
    }

    #[test]
    fn test_is_valid_identifier() {
        assert!(is_valid_identifier("foo"));
        assert!(is_valid_identifier("Foo"));
        assert!(is_valid_identifier("foo'"));
        assert!(is_valid_identifier("_foo"));
        assert!(!is_valid_identifier(""));
        assert!(!is_valid_identifier("123"));
        assert!(!is_valid_identifier("<$>"));
    }
}
