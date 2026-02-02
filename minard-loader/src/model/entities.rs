use serde_json::Value;
use std::fmt;

/// Backend/target language for a PureScript project
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum Backend {
    #[default]
    JavaScript,  // Default PS target
    Erlang,      // purerl
    Python,      // purepy
    Lua,         // pslua
    Rust,        // Rust/WASM (for projects with Cargo.toml)
}

impl Backend {
    pub fn as_str(&self) -> &'static str {
        match self {
            Backend::JavaScript => "js",
            Backend::Erlang => "erlang",
            Backend::Python => "python",
            Backend::Lua => "lua",
            Backend::Rust => "rust",
        }
    }

    pub fn from_str(s: &str) -> Self {
        match s.to_lowercase().as_str() {
            "erlang" | "purerl" | "erl" => Backend::Erlang,
            "python" | "purepy" | "py" => Backend::Python,
            "lua" | "pslua" => Backend::Lua,
            "rust" | "wasm" => Backend::Rust,
            _ => Backend::JavaScript,
        }
    }
}

impl fmt::Display for Backend {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

/// A project - a codebase being analyzed
#[derive(Debug, Clone)]
pub struct Project {
    pub id: i64,
    pub name: String,
    pub repo_path: Option<String>,
    pub description: Option<String>,
    pub primary_backend: Backend,
}

/// A snapshot - a point-in-time analysis of a project
#[derive(Debug, Clone)]
pub struct Snapshot {
    pub id: i64,
    pub project_id: i64,
    pub git_hash: Option<String>,
    pub git_ref: Option<String>,
    pub label: Option<String>,
}

/// Snapshot package association
#[derive(Debug, Clone)]
pub struct SnapshotPackage {
    pub snapshot_id: i64,
    pub package_version_id: i64,
    pub source: String, // "registry" | "workspace" | "local"
    pub is_direct: bool,
}

/// A package version - the core identity in the schema
#[derive(Debug, Clone)]
pub struct PackageVersion {
    pub id: i64,
    pub name: String,
    pub version: String,
    pub description: Option<String>,
    pub license: Option<String>,
    pub repository: Option<String>,
    pub source: String, // "registry" | "local" | "git"
    // FFI statistics (polyglot support)
    pub loc_ffi_js: Option<i32>,
    pub loc_ffi_erlang: Option<i32>,
    pub loc_ffi_python: Option<i32>,
    pub loc_ffi_lua: Option<i32>,
    pub ffi_file_count: Option<i32>,
}

/// A module namespace - hierarchical path like Data.Array.NonEmpty
#[derive(Debug, Clone)]
pub struct ModuleNamespace {
    pub id: i64,
    pub path: String,      // "Data.Array.NonEmpty" (full dotted path)
    pub segment: String,   // "NonEmpty" (just this level)
    pub parent_id: Option<i64>,
    pub depth: i32,
    pub is_leaf: bool,
}

/// A module within a package version
#[derive(Debug, Clone)]
pub struct Module {
    pub id: i64,
    pub package_version_id: i64,
    pub namespace_id: Option<i64>,
    pub name: String,
    pub path: Option<String>,
    pub comments: Option<String>,
    pub loc: Option<i32>,
}

/// A top-level declaration in a module
#[derive(Debug, Clone)]
pub struct Declaration {
    pub id: i64,
    pub module_id: i64,
    pub name: String,
    pub kind: String,
    pub type_signature: Option<String>,
    pub type_ast: Option<Value>,
    pub data_decl_type: Option<String>,
    pub type_arguments: Option<Value>,
    pub roles: Option<Value>,
    pub superclasses: Option<Value>,
    pub fundeps: Option<Value>,
    pub synonym_type: Option<Value>,
    pub comments: Option<String>,
    pub source_span: Option<Value>,
}

/// A child declaration (constructor, instance, class member)
#[derive(Debug, Clone)]
pub struct ChildDeclaration {
    pub id: i64,
    pub declaration_id: i64,
    pub name: String,
    pub kind: String,
    pub type_signature: Option<String>,
    pub type_ast: Option<Value>,
    pub constructor_args: Option<Value>,
    pub instance_chain: Option<Value>,
    pub instance_constraints: Option<Value>,
    pub comments: Option<String>,
    pub source_span: Option<Value>,
}

/// Package dependency relationship
#[derive(Debug, Clone)]
pub struct PackageDependency {
    pub dependent_id: i64,
    pub dependency_name: String,
}

/// Results from parsing a docs.json file
#[derive(Debug)]
pub struct ParsedModule {
    pub module: Module,
    pub declarations: Vec<Declaration>,
    pub child_declarations: Vec<ChildDeclaration>,
}

/// Statistics from a load operation
#[derive(Debug, Default)]
pub struct LoadStats {
    pub project_name: String,
    pub snapshot_label: Option<String>,
    pub packages_loaded: usize,
    pub packages_reused: usize,
    pub modules_loaded: usize,
    pub modules_reused: usize,
    pub declarations_loaded: usize,
    pub child_declarations_loaded: usize,
    pub namespaces_created: usize,
    pub dependencies_loaded: usize,
    pub parse_errors: usize,
    pub elapsed_ms: u64,
    // Post-load metrics
    pub module_imports_loaded: usize,
    pub function_calls_loaded: usize,
    pub commits_loaded: usize,
    pub topo_layers_computed: usize,
    // Registry package metrics
    pub registry_packages_loaded: usize,
    pub registry_modules_loaded: usize,
    pub registry_declarations_loaded: usize,
}

impl LoadStats {
    pub fn report(&self) -> String {
        let label = self
            .snapshot_label
            .as_ref()
            .map(|l| format!(" ({})", l))
            .unwrap_or_default();

        let pkg_info = if self.packages_reused > 0 {
            format!("{} packages ({} new, {} reused)",
                self.packages_loaded + self.packages_reused,
                self.packages_loaded,
                self.packages_reused)
        } else {
            format!("{} packages", self.packages_loaded)
        };

        let mod_info = if self.modules_reused > 0 {
            format!("{} modules ({} new, {} reused)",
                self.modules_loaded + self.modules_reused,
                self.modules_loaded,
                self.modules_reused)
        } else {
            format!("{} modules", self.modules_loaded)
        };

        let mut result = format!(
            "Loaded {}{}: {}, {}, {} declarations, {} children",
            self.project_name,
            label,
            pkg_info,
            mod_info,
            self.declarations_loaded,
            self.child_declarations_loaded,
        );

        // Add registry stats if any
        if self.registry_modules_loaded > 0 {
            result.push_str(&format!(
                " + registry: {} modules from {} packages",
                self.registry_modules_loaded,
                self.registry_packages_loaded
            ));
        }

        // Add post-load stats if any
        let mut extras = Vec::new();
        if self.module_imports_loaded > 0 {
            extras.push(format!("{} imports", self.module_imports_loaded));
        }
        if self.function_calls_loaded > 0 {
            extras.push(format!("{} calls", self.function_calls_loaded));
        }
        if self.commits_loaded > 0 {
            extras.push(format!("{} commits", self.commits_loaded));
        }
        if self.topo_layers_computed > 0 {
            extras.push(format!("{} topo layers", self.topo_layers_computed));
        }

        if !extras.is_empty() {
            result.push_str(&format!(" + {}", extras.join(", ")));
        }

        result.push_str(&format!(" in {}ms", self.elapsed_ms));
        if self.parse_errors > 0 {
            result.push_str(&format!(" ({} parse errors)", self.parse_errors));
        }

        result
    }

    /// Merge stats from multiple loads
    pub fn merge(&mut self, other: &LoadStats) {
        self.packages_loaded += other.packages_loaded;
        self.packages_reused += other.packages_reused;
        self.modules_loaded += other.modules_loaded;
        self.modules_reused += other.modules_reused;
        self.declarations_loaded += other.declarations_loaded;
        self.child_declarations_loaded += other.child_declarations_loaded;
        self.namespaces_created += other.namespaces_created;
        self.dependencies_loaded += other.dependencies_loaded;
        self.parse_errors += other.parse_errors;
        self.module_imports_loaded += other.module_imports_loaded;
        self.function_calls_loaded += other.function_calls_loaded;
        self.commits_loaded += other.commits_loaded;
        self.topo_layers_computed += other.topo_layers_computed;
        self.registry_packages_loaded += other.registry_packages_loaded;
        self.registry_modules_loaded += other.registry_modules_loaded;
        self.registry_declarations_loaded += other.registry_declarations_loaded;
    }
}

/// Aggregate statistics from scanning multiple projects
#[derive(Debug, Default)]
pub struct ScanStats {
    pub projects_loaded: usize,
    pub projects_skipped: usize,
    pub total_packages_new: usize,
    pub total_packages_reused: usize,
    pub total_modules_new: usize,
    pub total_modules_reused: usize,
    pub total_declarations: usize,
    pub total_children: usize,
    pub total_namespaces: usize,
    pub total_parse_errors: usize,
    pub elapsed_ms: u64,
}

impl ScanStats {
    pub fn add(&mut self, stats: &LoadStats) {
        self.projects_loaded += 1;
        self.total_packages_new += stats.packages_loaded;
        self.total_packages_reused += stats.packages_reused;
        self.total_modules_new += stats.modules_loaded;
        self.total_modules_reused += stats.modules_reused;
        self.total_declarations += stats.declarations_loaded;
        self.total_children += stats.child_declarations_loaded;
        self.total_namespaces += stats.namespaces_created;
        self.total_parse_errors += stats.parse_errors;
    }

    pub fn report(&self) -> String {
        format!(
            "Scanned {} projects ({} skipped): {} packages ({} new), {} modules ({} new), {} declarations, {} children in {}ms ({} parse errors)",
            self.projects_loaded,
            self.projects_skipped,
            self.total_packages_new + self.total_packages_reused,
            self.total_packages_new,
            self.total_modules_new + self.total_modules_reused,
            self.total_modules_new,
            self.total_declarations,
            self.total_children,
            self.elapsed_ms,
            self.total_parse_errors
        )
    }
}
