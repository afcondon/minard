use std::path::{Path, PathBuf};
use walkdir::WalkDir;

use super::detect::detect_backend;
use crate::error::{LoaderError, Result};
use crate::model::Backend;

/// Discovered project structure
#[derive(Debug, Clone)]
pub struct ProjectDiscovery {
    pub project_path: PathBuf,
    pub spago_lock_path: PathBuf,
    pub output_dir: PathBuf,
    pub docs_json_files: Vec<PathBuf>,
    /// Relative name when discovered via scan (e.g., "showcases/hypo-punter")
    pub relative_name: Option<String>,
    /// Detected primary backend for the project
    pub primary_backend: Backend,
}

impl ProjectDiscovery {
    /// Discover a PureScript project at the given path
    pub fn discover(project_path: &Path) -> Result<Self> {
        Self::discover_with_name(project_path, None)
    }

    /// Discover a PureScript project with an optional relative name
    pub fn discover_with_name(project_path: &Path, relative_name: Option<String>) -> Result<Self> {
        if !project_path.exists() {
            return Err(LoaderError::ProjectNotFound(project_path.to_path_buf()));
        }

        let spago_lock_path = project_path.join("spago.lock");
        if !spago_lock_path.exists() {
            return Err(LoaderError::SpagoLockNotFound(spago_lock_path));
        }

        let output_dir = project_path.join("output");
        if !output_dir.exists() {
            return Err(LoaderError::OutputDirNotFound(output_dir));
        }

        // Find all docs.json files in output directory
        let docs_json_files = find_docs_json_files(&output_dir);

        // Detect primary backend
        let primary_backend = detect_backend(project_path);

        Ok(Self {
            project_path: project_path.to_path_buf(),
            spago_lock_path,
            output_dir,
            docs_json_files,
            relative_name,
            primary_backend,
        })
    }

    /// Get the number of discovered docs.json files
    pub fn module_count(&self) -> usize {
        self.docs_json_files.len()
    }

    /// Get the project name (relative name if set, otherwise directory name)
    pub fn project_name(&self) -> String {
        self.relative_name.clone().unwrap_or_else(|| {
            self.project_path
                .file_name()
                .and_then(|n| n.to_str())
                .map(|s| s.to_string())
                .unwrap_or_else(|| "unknown".to_string())
        })
    }
}

/// Discover all PureScript projects in a directory tree
pub fn discover_all(root: &Path) -> Vec<ProjectDiscovery> {
    let mut projects = Vec::new();

    // Walk directory tree looking for spago.lock files
    for entry in WalkDir::new(root)
        .into_iter()
        .filter_map(|e| e.ok())
        .filter(|e| e.file_name() == "spago.lock")
    {
        let spago_lock_path = entry.path();
        let project_path = match spago_lock_path.parent() {
            Some(p) => p,
            None => continue,
        };

        // Check if this project has an output directory with docs.json files
        let output_dir = project_path.join("output");
        if !output_dir.exists() {
            continue;
        }

        // Compute relative name from root
        let relative_name = project_path
            .strip_prefix(root)
            .ok()
            .and_then(|p| p.to_str())
            .map(|s| s.to_string())
            .filter(|s| !s.is_empty());

        match ProjectDiscovery::discover_with_name(project_path, relative_name) {
            Ok(discovery) => {
                // Only include projects that have at least one docs.json
                if !discovery.docs_json_files.is_empty() {
                    projects.push(discovery);
                }
            }
            Err(_) => continue,
        }
    }

    // Sort by project name for consistent ordering
    projects.sort_by(|a, b| a.project_name().cmp(&b.project_name()));
    projects
}

/// Find all docs.json files in the output directory
fn find_docs_json_files(output_dir: &Path) -> Vec<PathBuf> {
    let mut files = Vec::new();

    for entry in WalkDir::new(output_dir)
        .min_depth(2)
        .max_depth(2)
        .into_iter()
        .filter_map(|e| e.ok())
    {
        let path = entry.path();
        if path.file_name().map(|n| n == "docs.json").unwrap_or(false) {
            files.push(path.to_path_buf());
        }
    }

    files.sort();
    files
}

/// Extract module name from docs.json path
/// e.g., /path/to/output/Data.Maybe/docs.json -> "Data.Maybe"
pub fn module_name_from_path(docs_path: &Path) -> Option<String> {
    docs_path
        .parent()
        .and_then(|p| p.file_name())
        .and_then(|n| n.to_str())
        .map(|s| s.to_string())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_module_name_from_path() {
        let path = PathBuf::from("/foo/output/Data.Maybe/docs.json");
        assert_eq!(
            module_name_from_path(&path),
            Some("Data.Maybe".to_string())
        );
    }
}
