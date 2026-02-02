//! Backend detection and FFI file scanning for polyglot PureScript projects

use std::fs;
use std::io::{BufRead, BufReader};
use std::path::Path;
use walkdir::WalkDir;

use crate::db::insert::FfiStats;
use crate::model::Backend;

/// Detect the primary backend for a PureScript project
///
/// Detection heuristics (in order of priority):
/// 1. spago.yaml `backend` field (explicit declaration)
/// 2. Presence of `output-py/` directory → Python (PurePy)
/// 3. Presence of `lua-ffi/` directory → Lua (PsLua)
/// 4. Presence of `_build/default/lib/` directory → Erlang (Purerl)
/// 5. Presence of Cargo.toml in subdirectory → Rust/WASM
/// 6. Default to JavaScript
pub fn detect_backend(project_path: &Path) -> Backend {
    // First check spago.yaml for explicit backend field
    if let Some(backend) = detect_from_spago_yaml(project_path) {
        return backend;
    }

    // Check for output-py directory (PurePy)
    if project_path.join("output-py").exists() {
        return Backend::Python;
    }

    // Check for lua-ffi directory (PsLua)
    if project_path.join("lua-ffi").exists() {
        return Backend::Lua;
    }

    // Check for rebar3 build directory (Purerl)
    if project_path.join("_build/default/lib").exists() {
        return Backend::Erlang;
    }

    // Check for Cargo.toml in subdirectories (Rust/WASM kernel)
    if has_rust_kernel(project_path) {
        return Backend::Rust;
    }

    // Default to JavaScript
    Backend::JavaScript
}

/// Check if project has a Rust/WASM kernel (Cargo.toml in subdirectory)
fn has_rust_kernel(project_path: &Path) -> bool {
    // Look for Cargo.toml in immediate subdirectories
    // Common patterns: force-kernel/, wasm/, rust/, kernel/
    let candidates = ["force-kernel", "wasm", "rust", "kernel", "wasm-kernel"];
    for dir in &candidates {
        if project_path.join(dir).join("Cargo.toml").exists() {
            return true;
        }
    }

    // Also check for any Cargo.toml in immediate subdirectories
    if let Ok(entries) = std::fs::read_dir(project_path) {
        for entry in entries.filter_map(|e| e.ok()) {
            if entry.path().is_dir() && entry.path().join("Cargo.toml").exists() {
                return true;
            }
        }
    }

    false
}

/// Parse spago.yaml and look for backend field
fn detect_from_spago_yaml(project_path: &Path) -> Option<Backend> {
    let spago_yaml = project_path.join("spago.yaml");
    if !spago_yaml.exists() {
        return None;
    }

    let content = fs::read_to_string(&spago_yaml).ok()?;

    // Simple YAML parsing - look for backend field
    // Can be either:
    //   backend: erlang
    // or:
    //   backend:
    //     cmd: "/path/to/purerl"
    let lines: Vec<&str> = content.lines().collect();
    for (i, line) in lines.iter().enumerate() {
        let trimmed = line.trim();
        if trimmed.starts_with("backend:") {
            let value = trimmed
                .strip_prefix("backend:")
                .map(|s| s.trim().trim_matches('"').trim_matches('\''))
                .unwrap_or("");

            // If value is non-empty, it's a direct backend name
            if !value.is_empty() {
                return Some(Backend::from_str(value));
            }

            // Otherwise, look for cmd: on the next line (nested object)
            if i + 1 < lines.len() {
                let next_line = lines[i + 1].trim();
                if next_line.starts_with("cmd:") {
                    let cmd = next_line
                        .strip_prefix("cmd:")
                        .map(|s| s.trim().trim_matches('"').trim_matches('\''))
                        .unwrap_or("");
                    // Detect backend from command name
                    if cmd.contains("purerl") {
                        return Some(Backend::Erlang);
                    } else if cmd.contains("purepy") || cmd.contains("python") {
                        return Some(Backend::Python);
                    } else if cmd.contains("pslua") || cmd.contains("lua") {
                        return Some(Backend::Lua);
                    }
                }
            }
        }
    }

    None
}

/// FFI file extensions by backend
const FFI_EXTENSIONS: &[(&str, Backend)] = &[
    (".js", Backend::JavaScript),
    (".erl", Backend::Erlang),
    (".py", Backend::Python),
    (".lua", Backend::Lua),
    (".rs", Backend::Rust),
];

/// Scan a directory for FFI files and count lines of code per backend
pub fn scan_ffi_files(project_path: &Path) -> FfiStats {
    let mut stats = FfiStats::default();

    // Directories to scan for FFI files
    let mut ffi_dirs = vec![
        project_path.join("src"),
        project_path.join("lua-ffi"),  // PsLua convention
    ];

    // Also scan Rust project directories (for WASM kernels)
    if let Ok(entries) = std::fs::read_dir(project_path) {
        for entry in entries.filter_map(|e| e.ok()) {
            let path = entry.path();
            if path.is_dir() && path.join("Cargo.toml").exists() {
                ffi_dirs.push(path.join("src"));
            }
        }
    }

    for ffi_dir in &ffi_dirs {
        if !ffi_dir.exists() {
            continue;
        }

        for entry in WalkDir::new(ffi_dir)
            .into_iter()
            .filter_map(|e| e.ok())
            .filter(|e| e.file_type().is_file())
        {
            let path = entry.path();
            let ext = path
                .extension()
                .and_then(|e| e.to_str())
                .map(|e| format!(".{}", e));

            if let Some(ext_str) = ext {
                for (ffi_ext, backend) in FFI_EXTENSIONS {
                    if &ext_str == *ffi_ext {
                        let loc = count_lines(path);
                        match backend {
                            Backend::JavaScript => stats.loc_js += loc,
                            Backend::Erlang => stats.loc_erlang += loc,
                            Backend::Python => stats.loc_python += loc,
                            Backend::Lua => stats.loc_lua += loc,
                            Backend::Rust => stats.loc_rust += loc,
                        }
                        stats.file_count += 1;
                    }
                }
            }
        }
    }

    stats
}

/// Count lines of code in a file (non-empty, non-comment lines)
fn count_lines(path: &Path) -> i32 {
    let file = match fs::File::open(path) {
        Ok(f) => f,
        Err(_) => return 0,
    };

    let reader = BufReader::new(file);
    let mut count = 0;
    let mut in_block_comment = false;

    for line in reader.lines().filter_map(|l| l.ok()) {
        let trimmed = line.trim();

        // Skip empty lines
        if trimmed.is_empty() {
            continue;
        }

        // Handle block comments (/* ... */ or {- ... -})
        if in_block_comment {
            if trimmed.contains("*/") || trimmed.contains("-}") {
                in_block_comment = false;
            }
            continue;
        }

        if trimmed.starts_with("/*") || trimmed.starts_with("{-") {
            if !trimmed.contains("*/") && !trimmed.contains("-}") {
                in_block_comment = true;
            }
            continue;
        }

        // Skip single-line comments
        if trimmed.starts_with("//")
            || trimmed.starts_with("#")
            || trimmed.starts_with("--")
            || trimmed.starts_with("%")
        {
            continue;
        }

        count += 1;
    }

    count
}

/// Scan a package directory for FFI files
/// Returns stats for the given package path
pub fn scan_package_ffi(package_path: &Path) -> FfiStats {
    let mut stats = FfiStats::default();

    // For registry packages, FFI is in src/
    // For local packages, FFI might be in src/ or alongside .purs files
    let src_dir = package_path.join("src");
    let search_dir = if src_dir.exists() {
        src_dir
    } else {
        package_path.to_path_buf()
    };

    for entry in WalkDir::new(&search_dir)
        .into_iter()
        .filter_map(|e| e.ok())
        .filter(|e| e.file_type().is_file())
    {
        let path = entry.path();
        let filename = path
            .file_name()
            .and_then(|n| n.to_str())
            .unwrap_or("");

        // Check for FFI file naming conventions
        // JavaScript: Foo.js or Foo.Bar.js (matching module name)
        // Erlang: foo_bar.erl
        // Python: foo_bar_foreign.py or just foo_bar.py
        // Lua: foo.lua

        for (ext, backend) in FFI_EXTENSIONS {
            if filename.ends_with(ext) {
                // Skip if this is clearly not FFI (e.g., build output)
                if filename.ends_with(".min.js") || filename == "bundle.js" {
                    continue;
                }

                let loc = count_lines(path);
                match backend {
                    Backend::JavaScript => stats.loc_js += loc,
                    Backend::Erlang => stats.loc_erlang += loc,
                    Backend::Python => stats.loc_python += loc,
                    Backend::Lua => stats.loc_lua += loc,
                    Backend::Rust => stats.loc_rust += loc,
                }
                stats.file_count += 1;
            }
        }
    }

    stats
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_backend_from_str() {
        assert_eq!(Backend::from_str("erlang"), Backend::Erlang);
        assert_eq!(Backend::from_str("purerl"), Backend::Erlang);
        assert_eq!(Backend::from_str("python"), Backend::Python);
        assert_eq!(Backend::from_str("purepy"), Backend::Python);
        assert_eq!(Backend::from_str("lua"), Backend::Lua);
        assert_eq!(Backend::from_str("pslua"), Backend::Lua);
        assert_eq!(Backend::from_str("javascript"), Backend::JavaScript);
        assert_eq!(Backend::from_str("js"), Backend::JavaScript);
        assert_eq!(Backend::from_str("unknown"), Backend::JavaScript);
    }

    #[test]
    fn test_backend_as_str() {
        assert_eq!(Backend::JavaScript.as_str(), "js");
        assert_eq!(Backend::Erlang.as_str(), "erlang");
        assert_eq!(Backend::Python.as_str(), "python");
        assert_eq!(Backend::Lua.as_str(), "lua");
    }
}
