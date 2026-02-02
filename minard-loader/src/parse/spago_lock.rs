use serde::Deserialize;
use std::collections::HashMap;
use std::fs;
use std::path::Path;

use crate::error::{LoaderError, Result};

/// Represents the top-level spago.lock file structure
#[derive(Debug, Deserialize)]
pub struct SpagoLock {
    pub workspace: Workspace,
    pub packages: HashMap<String, PackageEntry>,
}

#[derive(Debug, Deserialize)]
pub struct Workspace {
    pub packages: HashMap<String, WorkspacePackage>,
    pub package_set: Option<PackageSet>,
    pub extra_packages: Option<HashMap<String, ExtraPackage>>,
}

#[derive(Debug, Deserialize)]
pub struct WorkspacePackage {
    pub path: String,
    pub core: Option<BuildConfig>,
    pub test: Option<BuildConfig>,
}

#[derive(Debug, Deserialize)]
pub struct BuildConfig {
    pub dependencies: Vec<String>,
    pub build_plan: Vec<String>,
}

#[derive(Debug, Deserialize)]
pub struct PackageSet {
    pub address: Option<PackageSetAddress>,
    pub compiler: Option<String>,
    /// Content can be either:
    /// - Registry style: HashMap<String, String> (package -> version)
    /// - Git package set (purerl): HashMap<String, GitPackageInfo> (package -> {repo, version, deps})
    #[serde(default)]
    pub content: PackageSetContent,
}

/// Flexible content field that accepts both registry and git package set formats
#[derive(Debug, Default, Deserialize)]
#[serde(untagged)]
pub enum PackageSetContent {
    #[default]
    Empty,
    /// Registry-style: package name -> version string
    Registry(HashMap<String, String>),
    /// Git package set (purerl): package name -> {repo, version, dependencies}
    GitPackageSet(HashMap<String, GitPackageSetEntry>),
}

/// Entry in a git-based package set (e.g., purerl)
#[derive(Debug, Deserialize)]
pub struct GitPackageSetEntry {
    pub repo: String,
    pub version: String,
    #[serde(default)]
    pub dependencies: Vec<String>,
}

#[derive(Debug, Deserialize)]
pub struct PackageSetAddress {
    pub registry: Option<String>,
}

#[derive(Debug, Deserialize)]
pub struct ExtraPackage {
    pub path: Option<String>,
    pub git: Option<String>,
    pub ref_: Option<String>,
}

/// Package entry in the resolved packages section
#[derive(Debug, Deserialize)]
pub struct PackageEntry {
    #[serde(rename = "type")]
    pub source_type: String,
    pub version: Option<String>,
    pub integrity: Option<String>,
    pub dependencies: Vec<String>,
    pub path: Option<String>,
}

impl SpagoLock {
    /// Parse a spago.lock file from a path
    pub fn from_path(path: &Path) -> Result<Self> {
        let content = fs::read_to_string(path).map_err(|e| {
            if e.kind() == std::io::ErrorKind::NotFound {
                LoaderError::SpagoLockNotFound(path.to_path_buf())
            } else {
                LoaderError::Io(e)
            }
        })?;

        Self::from_str(&content, path)
    }

    /// Parse spago.lock from a string
    pub fn from_str(content: &str, path: &Path) -> Result<Self> {
        serde_json::from_str(content).map_err(|e| LoaderError::JsonParse {
            path: path.to_path_buf(),
            source: e,
        })
    }

    /// Get all packages that need to be loaded (registry + local + workspace)
    pub fn all_packages(&self) -> Vec<PackageInfo> {
        let mut result = Vec::new();

        // Registry and local packages from the resolved packages section
        for (name, entry) in &self.packages {
            result.push(PackageInfo {
                name: name.clone(),
                version: entry.version.clone().unwrap_or_else(|| "0.0.0".to_string()),
                source: entry.source_type.clone(),
                dependencies: entry.dependencies.clone(),
            });
        }

        result
    }

    /// Get workspace package names
    pub fn workspace_packages(&self) -> Vec<String> {
        self.workspace.packages.keys().cloned().collect()
    }
}

/// Simplified package info for loading
#[derive(Debug, Clone)]
pub struct PackageInfo {
    pub name: String,
    pub version: String,
    pub source: String,
    pub dependencies: Vec<String>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_sample() {
        let sample = r#"{
            "workspace": {
                "packages": {
                    "my-app": {
                        "path": ".",
                        "core": {
                            "dependencies": ["prelude", "effect"],
                            "build_plan": ["prelude", "effect"]
                        }
                    }
                }
            },
            "packages": {
                "prelude": {
                    "type": "registry",
                    "version": "6.0.2",
                    "integrity": "sha256-...",
                    "dependencies": []
                },
                "effect": {
                    "type": "registry",
                    "version": "4.0.0",
                    "integrity": "sha256-...",
                    "dependencies": ["prelude"]
                }
            }
        }"#;

        let lock =
            SpagoLock::from_str(sample, Path::new("test.lock")).expect("Failed to parse sample");
        assert_eq!(lock.packages.len(), 2);
        assert!(lock.packages.contains_key("prelude"));
        assert!(lock.packages.contains_key("effect"));
    }
}
