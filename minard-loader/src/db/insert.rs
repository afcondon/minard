use duckdb::{params, Connection};
use std::collections::HashMap;

use crate::error::Result;
use crate::model::{
    Backend, ChildDeclaration, Declaration, Module, ModuleNamespace, PackageDependency,
    PackageVersion, Project, Snapshot, SnapshotPackage,
};

/// Insert or get a project, returning its ID
pub fn get_or_create_project(
    conn: &Connection,
    id: i64,
    name: &str,
    repo_path: Option<&str>,
    primary_backend: Backend,
) -> Result<i64> {
    // First try to find existing project
    let existing: std::result::Result<i64, _> = conn.query_row(
        "SELECT id FROM projects WHERE name = ?",
        params![name],
        |row| row.get(0),
    );

    match existing {
        Ok(id) => {
            // Update backend if project already exists
            conn.execute(
                "UPDATE projects SET primary_backend = ? WHERE id = ?",
                params![primary_backend.as_str(), id],
            )?;
            Ok(id)
        }
        Err(duckdb::Error::QueryReturnedNoRows) => {
            // Insert new project
            conn.execute(
                "INSERT INTO projects (id, name, repo_path, primary_backend) VALUES (?, ?, ?, ?)",
                params![id, name, repo_path, primary_backend.as_str()],
            )?;
            Ok(id)
        }
        Err(e) => Err(e.into()),
    }
}

/// Insert a project
pub fn insert_project(conn: &Connection, project: &Project) -> Result<()> {
    conn.execute(
        "INSERT OR IGNORE INTO projects (id, name, repo_path, description, primary_backend)
         VALUES (?, ?, ?, ?, ?)",
        params![
            project.id,
            project.name,
            project.repo_path,
            project.description,
            project.primary_backend.as_str()
        ],
    )?;
    Ok(())
}

/// Insert a snapshot
pub fn insert_snapshot(conn: &Connection, snapshot: &Snapshot) -> Result<()> {
    conn.execute(
        "INSERT OR IGNORE INTO snapshots (id, project_id, git_hash, git_ref, label) VALUES (?, ?, ?, ?, ?)",
        params![
            snapshot.id,
            snapshot.project_id,
            snapshot.git_hash,
            snapshot.git_ref,
            snapshot.label
        ],
    )?;
    Ok(())
}

/// Insert snapshot package associations
pub fn insert_snapshot_packages(conn: &Connection, packages: &[SnapshotPackage]) -> Result<()> {
    let mut stmt = conn.prepare(
        "INSERT OR IGNORE INTO snapshot_packages (snapshot_id, package_version_id, source, is_direct)
         VALUES (?, ?, ?, ?)",
    )?;

    for pkg in packages {
        stmt.execute(params![
            pkg.snapshot_id,
            pkg.package_version_id,
            pkg.source,
            pkg.is_direct
        ])?;
    }

    Ok(())
}

/// Insert a batch of package versions
pub fn insert_package_versions(conn: &Connection, packages: &[PackageVersion]) -> Result<()> {
    let mut stmt = conn.prepare(
        "INSERT OR IGNORE INTO package_versions
         (id, name, version, description, license, repository, source,
          loc_ffi_js, loc_ffi_erlang, loc_ffi_python, loc_ffi_lua, ffi_file_count)
         VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
    )?;

    for pkg in packages {
        stmt.execute(params![
            pkg.id,
            pkg.name,
            pkg.version,
            pkg.description,
            pkg.license,
            pkg.repository,
            pkg.source,
            pkg.loc_ffi_js,
            pkg.loc_ffi_erlang,
            pkg.loc_ffi_python,
            pkg.loc_ffi_lua,
            pkg.ffi_file_count,
        ])?;
    }

    Ok(())
}

/// Insert package versions and return map of (name, version) -> actual ID
/// This handles the case where packages may already exist with different IDs
pub fn insert_package_versions_with_ids(
    conn: &Connection,
    packages: &[PackageVersion],
) -> Result<std::collections::HashMap<(String, String), i64>> {
    use std::collections::HashMap;

    // First insert all packages (ignoring duplicates)
    insert_package_versions(conn, packages)?;

    // Then query back all actual IDs
    let mut result = HashMap::new();
    let mut stmt = conn.prepare(
        "SELECT id, name, version FROM package_versions WHERE name = ? AND version = ?",
    )?;

    for pkg in packages {
        let id: i64 = stmt.query_row(params![&pkg.name, &pkg.version], |row| row.get(0))?;
        result.insert((pkg.name.clone(), pkg.version.clone()), id);
    }

    Ok(result)
}

/// Insert a batch of package dependencies
pub fn insert_package_dependencies(conn: &Connection, deps: &[PackageDependency]) -> Result<()> {
    let mut stmt = conn.prepare(
        "INSERT OR IGNORE INTO package_dependencies (dependent_id, dependency_name)
         VALUES (?, ?)",
    )?;

    for dep in deps {
        stmt.execute(params![dep.dependent_id, dep.dependency_name,])?;
    }

    Ok(())
}

/// Insert a batch of modules
pub fn insert_modules(conn: &Connection, modules: &[Module]) -> Result<()> {
    let mut stmt = conn.prepare(
        "INSERT OR IGNORE INTO modules
         (id, package_version_id, namespace_id, name, path, comments, loc)
         VALUES (?, ?, ?, ?, ?, ?, ?)",
    )?;

    for module in modules {
        stmt.execute(params![
            module.id,
            module.package_version_id,
            module.namespace_id,
            module.name,
            module.path,
            module.comments,
            module.loc,
        ])?;
    }

    Ok(())
}

/// Insert modules and return info about which were new vs reused
/// Returns (new_count, reused_count, module_id_map)
/// module_id_map maps (package_version_id, module_name) -> actual_module_id
pub fn insert_modules_with_dedup(
    conn: &Connection,
    modules: &[Module],
) -> Result<(usize, usize, HashMap<(i64, String), i64>)> {
    let mut new_count = 0usize;
    let mut reused_count = 0usize;
    let mut module_id_map = HashMap::new();

    let mut insert_stmt = conn.prepare(
        "INSERT OR IGNORE INTO modules
         (id, package_version_id, namespace_id, name, path, comments, loc)
         VALUES (?, ?, ?, ?, ?, ?, ?)",
    )?;

    let mut query_stmt = conn.prepare(
        "SELECT id FROM modules WHERE package_version_id = ? AND name = ?",
    )?;

    for module in modules {
        // Try to insert
        let rows_affected = insert_stmt.execute(params![
            module.id,
            module.package_version_id,
            module.namespace_id,
            module.name,
            module.path,
            module.comments,
            module.loc,
        ])?;

        // Query back the actual ID
        let actual_id: i64 =
            query_stmt.query_row(params![module.package_version_id, &module.name], |row| {
                row.get(0)
            })?;

        module_id_map.insert(
            (module.package_version_id, module.name.clone()),
            actual_id,
        );

        if rows_affected > 0 {
            new_count += 1;
        } else {
            reused_count += 1;
        }
    }

    Ok((new_count, reused_count, module_id_map))
}

/// Get or create a namespace, returning its ID
/// Creates parent namespaces as needed
pub fn get_or_create_namespace(conn: &Connection, path: &str, next_id: &mut i64) -> Result<i64> {
    // Check if namespace already exists
    let existing: std::result::Result<i64, _> = conn.query_row(
        "SELECT id FROM module_namespaces WHERE path = ?",
        params![path],
        |row| row.get(0),
    );

    if let Ok(id) = existing {
        return Ok(id);
    }

    // Split path to get parent and segment
    let parts: Vec<&str> = path.split('.').collect();
    let segment = parts.last().unwrap().to_string();
    let depth = (parts.len() - 1) as i32;

    // Get or create parent namespace
    let parent_id = if parts.len() > 1 {
        let parent_path = parts[..parts.len() - 1].join(".");
        Some(get_or_create_namespace(conn, &parent_path, next_id)?)
    } else {
        None
    };

    // Mark parent as non-leaf if it exists
    if let Some(pid) = parent_id {
        conn.execute(
            "UPDATE module_namespaces SET is_leaf = FALSE WHERE id = ?",
            params![pid],
        )?;
    }

    // Insert new namespace
    let id = *next_id;
    *next_id += 1;

    conn.execute(
        "INSERT INTO module_namespaces (id, path, segment, parent_id, depth, is_leaf)
         VALUES (?, ?, ?, ?, ?, TRUE)",
        params![id, path, segment, parent_id, depth],
    )?;

    Ok(id)
}

/// Insert namespaces and return map of path -> ID
pub fn insert_namespaces_with_ids(
    conn: &Connection,
    namespaces: &[ModuleNamespace],
) -> Result<HashMap<String, i64>> {
    let mut result = HashMap::new();

    let mut insert_stmt = conn.prepare(
        "INSERT OR IGNORE INTO module_namespaces (id, path, segment, parent_id, depth, is_leaf)
         VALUES (?, ?, ?, ?, ?, ?)",
    )?;

    let mut query_stmt = conn.prepare("SELECT id FROM module_namespaces WHERE path = ?")?;

    for ns in namespaces {
        insert_stmt.execute(params![
            ns.id,
            &ns.path,
            &ns.segment,
            ns.parent_id,
            ns.depth,
            ns.is_leaf,
        ])?;

        // Query back actual ID
        let actual_id: i64 = query_stmt.query_row(params![&ns.path], |row| row.get(0))?;
        result.insert(ns.path.clone(), actual_id);
    }

    Ok(result)
}

/// Check if a package already has modules loaded
pub fn package_has_modules(conn: &Connection, package_version_id: i64) -> Result<bool> {
    let count: i64 = conn.query_row(
        "SELECT COUNT(*) FROM modules WHERE package_version_id = ?",
        params![package_version_id],
        |row| row.get(0),
    )?;
    Ok(count > 0)
}

/// Insert a batch of declarations
pub fn insert_declarations(conn: &Connection, decls: &[Declaration]) -> Result<()> {
    let mut stmt = conn.prepare(
        "INSERT OR IGNORE INTO declarations
         (id, module_id, name, kind, type_signature, type_ast,
          data_decl_type, type_arguments, roles, superclasses, fundeps,
          synonym_type, comments, source_span)
         VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
    )?;

    for decl in decls {
        let type_ast_json = decl.type_ast.as_ref().map(|v| v.to_string());
        let type_args_json = decl.type_arguments.as_ref().map(|v| v.to_string());
        let roles_json = decl.roles.as_ref().map(|v| v.to_string());
        let superclasses_json = decl.superclasses.as_ref().map(|v| v.to_string());
        let fundeps_json = decl.fundeps.as_ref().map(|v| v.to_string());
        let synonym_json = decl.synonym_type.as_ref().map(|v| v.to_string());
        let source_span_json = decl.source_span.as_ref().map(|v| v.to_string());

        stmt.execute(params![
            decl.id,
            decl.module_id,
            decl.name,
            decl.kind,
            decl.type_signature,
            type_ast_json,
            decl.data_decl_type,
            type_args_json,
            roles_json,
            superclasses_json,
            fundeps_json,
            synonym_json,
            decl.comments,
            source_span_json,
        ])?;
    }

    Ok(())
}

/// Insert a batch of child declarations
pub fn insert_child_declarations(conn: &Connection, children: &[ChildDeclaration]) -> Result<()> {
    let mut stmt = conn.prepare(
        "INSERT OR IGNORE INTO child_declarations
         (id, declaration_id, name, kind, type_signature, type_ast,
          constructor_args, instance_chain, instance_constraints, comments, source_span)
         VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
    )?;

    for child in children {
        let type_ast_json = child.type_ast.as_ref().map(|v| v.to_string());
        let args_json = child.constructor_args.as_ref().map(|v| v.to_string());
        let chain_json = child.instance_chain.as_ref().map(|v| v.to_string());
        let constraints_json = child.instance_constraints.as_ref().map(|v| v.to_string());
        let source_span_json = child.source_span.as_ref().map(|v| v.to_string());

        stmt.execute(params![
            child.id,
            child.declaration_id,
            child.name,
            child.kind,
            child.type_signature,
            type_ast_json,
            args_json,
            chain_json,
            constraints_json,
            child.comments,
            source_span_json,
        ])?;
    }

    Ok(())
}

/// Max IDs from the database
#[derive(Debug, Default)]
pub struct MaxIds {
    pub project: i64,
    pub snapshot: i64,
    pub package: i64,
    pub namespace: i64,
    pub module: i64,
    pub declaration: i64,
    pub child: i64,
}

/// Get max IDs from existing database tables
pub fn get_max_ids(conn: &Connection) -> Result<MaxIds> {
    let max_proj: i64 = conn
        .query_row("SELECT COALESCE(MAX(id), 0) FROM projects", [], |row| {
            row.get(0)
        })
        .unwrap_or(0);

    let max_snap: i64 = conn
        .query_row("SELECT COALESCE(MAX(id), 0) FROM snapshots", [], |row| {
            row.get(0)
        })
        .unwrap_or(0);

    let max_pkg: i64 = conn
        .query_row(
            "SELECT COALESCE(MAX(id), 0) FROM package_versions",
            [],
            |row| row.get(0),
        )
        .unwrap_or(0);

    let max_ns: i64 = conn
        .query_row(
            "SELECT COALESCE(MAX(id), 0) FROM module_namespaces",
            [],
            |row| row.get(0),
        )
        .unwrap_or(0);

    let max_mod: i64 = conn
        .query_row("SELECT COALESCE(MAX(id), 0) FROM modules", [], |row| {
            row.get(0)
        })
        .unwrap_or(0);

    let max_decl: i64 = conn
        .query_row(
            "SELECT COALESCE(MAX(id), 0) FROM declarations",
            [],
            |row| row.get(0),
        )
        .unwrap_or(0);

    let max_child: i64 = conn
        .query_row(
            "SELECT COALESCE(MAX(id), 0) FROM child_declarations",
            [],
            |row| row.get(0),
        )
        .unwrap_or(0);

    Ok(MaxIds {
        project: max_proj,
        snapshot: max_snap,
        package: max_pkg,
        namespace: max_ns,
        module: max_mod,
        declaration: max_decl,
        child: max_child,
    })
}

/// Get existing package ID by name and version
pub fn get_package_id(conn: &Connection, name: &str, version: &str) -> Result<Option<i64>> {
    let result: std::result::Result<i64, _> = conn.query_row(
        "SELECT id FROM package_versions WHERE name = ? AND version = ?",
        params![name, version],
        |row| row.get(0),
    );

    match result {
        Ok(id) => Ok(Some(id)),
        Err(duckdb::Error::QueryReturnedNoRows) => Ok(None),
        Err(e) => Err(e.into()),
    }
}

/// FFI statistics for a package
#[derive(Debug, Default)]
pub struct FfiStats {
    pub loc_js: i32,
    pub loc_erlang: i32,
    pub loc_python: i32,
    pub loc_lua: i32,
    pub loc_rust: i32,
    pub file_count: i32,
}

/// Update FFI statistics for a package
pub fn update_package_ffi_stats(
    conn: &Connection,
    package_version_id: i64,
    stats: &FfiStats,
) -> Result<()> {
    conn.execute(
        "UPDATE package_versions SET
         loc_ffi_js = ?,
         loc_ffi_erlang = ?,
         loc_ffi_python = ?,
         loc_ffi_lua = ?,
         loc_ffi_rust = ?,
         ffi_file_count = ?
         WHERE id = ?",
        params![
            stats.loc_js,
            stats.loc_erlang,
            stats.loc_python,
            stats.loc_lua,
            stats.loc_rust,
            stats.file_count,
            package_version_id
        ],
    )?;
    Ok(())
}
