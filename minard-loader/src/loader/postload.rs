use duckdb::{params, Connection};
use std::collections::{HashMap, HashSet};
use std::path::Path;
use std::process::Command;
use std::time::Instant;

use crate::db::{append_function_calls, append_module_imports};
use crate::error::Result;
use crate::parse::CoreFn;

/// Compute topological layers for all packages in a snapshot
/// Layer 0 = packages with no dependencies
/// Higher layers depend only on lower layers
pub fn compute_topo_layers(conn: &Connection, snapshot_id: i64) -> Result<usize> {
    // Get all packages and their dependencies for this snapshot
    let mut pkg_deps: HashMap<i64, HashSet<String>> = HashMap::new();
    let mut pkg_names: HashMap<String, i64> = HashMap::new();

    // Get packages in this snapshot
    let mut stmt = conn.prepare(
        "SELECT pv.id, pv.name FROM snapshot_packages sp
         JOIN package_versions pv ON sp.package_version_id = pv.id
         WHERE sp.snapshot_id = ?",
    )?;

    let mut rows = stmt.query(params![snapshot_id])?;
    while let Some(row) = rows.next()? {
        let id: i64 = row.get(0)?;
        let name: String = row.get(1)?;
        pkg_deps.insert(id, HashSet::new());
        pkg_names.insert(name, id);
    }

    // Get dependencies for these packages
    let mut dep_stmt = conn.prepare(
        "SELECT pd.dependent_id, pd.dependency_name FROM package_dependencies pd
         JOIN snapshot_packages sp ON pd.dependent_id = sp.package_version_id
         WHERE sp.snapshot_id = ?",
    )?;

    let mut dep_rows = dep_stmt.query(params![snapshot_id])?;
    while let Some(row) = dep_rows.next()? {
        let dependent_id: i64 = row.get(0)?;
        let dep_name: String = row.get(1)?;

        if let Some(deps) = pkg_deps.get_mut(&dependent_id) {
            deps.insert(dep_name);
        }
    }

    // Compute layers using Kahn's algorithm
    let mut layers: HashMap<i64, i32> = HashMap::new();
    let mut updated = 0;

    // Layer 0: packages with no dependencies (or whose deps are outside snapshot)
    for (&pkg_id, deps) in &pkg_deps {
        let has_internal_deps = deps.iter().any(|d| pkg_names.contains_key(d));
        if !has_internal_deps {
            layers.insert(pkg_id, 0);
        }
    }

    // Iteratively assign layers
    let mut changed = true;
    while changed {
        changed = false;
        for (&pkg_id, deps) in &pkg_deps {
            if layers.contains_key(&pkg_id) {
                continue;
            }

            // Check if all deps have layers assigned
            let mut max_dep_layer: Option<i32> = None;
            let mut all_resolved = true;

            for dep_name in deps {
                if let Some(&dep_id) = pkg_names.get(dep_name) {
                    if let Some(&dep_layer) = layers.get(&dep_id) {
                        max_dep_layer = Some(max_dep_layer.map_or(dep_layer, |m| m.max(dep_layer)));
                    } else {
                        all_resolved = false;
                        break;
                    }
                }
                // Dependencies outside snapshot don't count
            }

            if all_resolved {
                let layer = max_dep_layer.map_or(0, |m| m + 1);
                layers.insert(pkg_id, layer);
                changed = true;
            }
        }
    }

    // Update database - store in snapshot_packages
    let mut update_stmt = conn.prepare(
        "UPDATE snapshot_packages SET topo_layer = ?
         WHERE package_version_id = ? AND snapshot_id = ?",
    )?;

    for (&pkg_id, &layer) in &layers {
        update_stmt.execute(params![layer, pkg_id, snapshot_id])?;
        updated += 1;
    }

    Ok(updated)
}

/// Insert module imports from corefn.json files
/// Collects all imports, deduplicates in memory, then bulk-inserts via Appender.
pub fn insert_module_imports(
    conn: &Connection,
    output_dir: &Path,
    package_version_id: i64,
) -> Result<usize> {
    // Get all modules for this package
    let mut modules: HashMap<String, i64> = HashMap::new();
    let mut stmt = conn.prepare("SELECT id, name FROM modules WHERE package_version_id = ?")?;
    let mut rows = stmt.query(params![package_version_id])?;
    while let Some(row) = rows.next()? {
        let id: i64 = row.get(0)?;
        let name: String = row.get(1)?;
        modules.insert(name.clone(), id);
    }

    // Collect all imports, dedup via HashSet
    let mut seen: HashSet<(i64, String)> = HashSet::new();
    let mut all_imports: Vec<(i64, String)> = Vec::new();
    let mut parse_time = std::time::Duration::ZERO;
    let mut files_parsed = 0usize;

    for (module_name, module_id) in &modules {
        let corefn_path = output_dir.join(module_name).join("corefn.json");
        if !corefn_path.exists() {
            continue;
        }

        let tp = Instant::now();
        match CoreFn::from_path(&corefn_path) {
            Ok(corefn) => {
                let imports = corefn.imported_modules();
                parse_time += tp.elapsed();
                files_parsed += 1;

                for imported in imports {
                    let key = (*module_id, imported.clone());
                    if seen.insert(key) {
                        all_imports.push((*module_id, imported));
                    }
                }
            }
            Err(_) => continue,
        }
    }

    let count = all_imports.len();

    // Single bulk insert via Appender
    let ti = Instant::now();
    append_module_imports(conn, &all_imports)?;
    let insert_time = ti.elapsed();

    eprintln!(
        "    module_imports breakdown: {} files, parse={:.1}s, db_insert={:.1}s",
        files_parsed, parse_time.as_secs_f64(), insert_time.as_secs_f64()
    );

    Ok(count)
}

/// Resolve imported_module_id for all module_imports rows where it is NULL.
/// Module names are globally unique within a PureScript package set, so a
/// simple name-based join is unambiguous.
pub fn resolve_import_module_ids(conn: &Connection) -> Result<usize> {
    let updated = conn.execute(
        r#"
        UPDATE module_imports SET imported_module_id = m.id
        FROM modules m
        WHERE m.name = module_imports.imported_module
          AND module_imports.imported_module_id IS NULL
        "#,
        [],
    )?;
    Ok(updated)
}

/// Insert function calls from corefn.json files
/// Collects all calls, deduplicates in memory, then bulk-inserts via Appender.
pub fn insert_function_calls(
    conn: &Connection,
    output_dir: &Path,
    package_version_id: i64,
) -> Result<usize> {
    // Get all modules for this package
    let mut modules: HashMap<String, i64> = HashMap::new();
    let mut stmt = conn.prepare("SELECT id, name FROM modules WHERE package_version_id = ?")?;
    let mut rows = stmt.query(params![package_version_id])?;
    while let Some(row) = rows.next()? {
        let id: i64 = row.get(0)?;
        let name: String = row.get(1)?;
        modules.insert(name.clone(), id);
    }

    // Get max ID for function_calls
    let max_id: i64 = conn
        .query_row("SELECT COALESCE(MAX(id), 0) FROM function_calls", [], |row| row.get(0))
        .unwrap_or(0);
    let mut next_id = max_id + 1;

    // Collect all calls, dedup via HashSet on (caller_module_id, caller_name, callee_module, callee_name)
    let mut seen: HashSet<(i64, String, String, String)> = HashSet::new();
    let mut all_calls: Vec<(i64, i64, String, String, String, bool)> = Vec::new();
    let mut parse_time = std::time::Duration::ZERO;
    let mut files_parsed = 0usize;

    for (module_name, module_id) in &modules {
        let corefn_path = output_dir.join(module_name).join("corefn.json");
        if !corefn_path.exists() {
            continue;
        }

        let tp = Instant::now();
        match CoreFn::from_path(&corefn_path) {
            Ok(corefn) => {
                let calls = corefn.extract_function_calls();
                parse_time += tp.elapsed();
                files_parsed += 1;

                for call in calls {
                    let key = (
                        *module_id,
                        call.caller_name.clone(),
                        call.callee_module.clone(),
                        call.callee_name.clone(),
                    );
                    if seen.insert(key) {
                        all_calls.push((
                            next_id,
                            *module_id,
                            call.caller_name,
                            call.callee_module,
                            call.callee_name,
                            call.is_cross_module,
                        ));
                        next_id += 1;
                    }
                }
            }
            Err(_) => continue,
        }
    }

    let count = all_calls.len();

    // Single bulk insert via Appender
    let ti = Instant::now();
    append_function_calls(conn, &all_calls)?;
    let insert_time = ti.elapsed();

    eprintln!(
        "    function_calls breakdown: {} files, parse={:.1}s, db_insert={:.1}s",
        files_parsed, parse_time.as_secs_f64(), insert_time.as_secs_f64()
    );

    Ok(count)
}

/// Git commit information
#[derive(Debug, Clone)]
pub struct CommitInfo {
    pub hash: String,
    pub timestamp: i64,
    pub date: String,
    pub author: String,
    pub subject: String,
    pub files: Vec<String>,
}

/// Collect git commits and associate them with modules
pub fn collect_git_data(
    conn: &Connection,
    project_path: &Path,
    project_id: i64,
    package_version_id: i64,
) -> Result<(usize, usize)> {
    // Get recent commits (last 100)
    let commits = get_recent_commits(project_path, 100);
    if commits.is_empty() {
        return Ok((0, 0));
    }

    // Get all modules for this package with their source paths
    let mut module_paths: HashMap<String, i64> = HashMap::new();
    let mut stmt = conn.prepare(
        "SELECT m.id, ANY_VALUE(d.source_span::json->>'name') as src_path
         FROM modules m
         JOIN declarations d ON d.module_id = m.id
         WHERE m.package_version_id = ?
         GROUP BY m.id",
    )?;

    let rows_result = stmt.query(params![package_version_id]);
    if let Ok(mut rows) = rows_result {
        while let Ok(Some(row)) = rows.next() {
            let id: i64 = row.get(0)?;
            let src_path: Option<String> = row.get(1)?;
            if let Some(path) = src_path {
                // Normalize path: remove leading "./" if present
                let normalized = path.trim_start_matches("./").to_string();
                module_paths.insert(normalized, id);
            }
        }
    }

    // Get max ID for commits
    let max_commit_id: i64 = conn
        .query_row("SELECT COALESCE(MAX(id), 0) FROM commits", [], |row| row.get(0))
        .unwrap_or(0);
    let mut next_commit_id = max_commit_id + 1;

    // Insert commits - use separate insert and query since DuckDB INSERT OR IGNORE with RETURNING is tricky
    let mut insert_commit_stmt = conn.prepare(
        "INSERT OR IGNORE INTO commits
         (id, project_id, hash, timestamp, date, author, subject)
         VALUES (?, ?, ?, ?, ?, ?, ?)",
    )?;

    let mut get_commit_stmt = conn.prepare(
        "SELECT id FROM commits WHERE project_id = ? AND hash = ?",
    )?;

    let mut link_stmt = conn.prepare(
        "INSERT OR IGNORE INTO module_commits (module_id, commit_id, change_type)
         VALUES (?, ?, ?)",
    )?;

    let mut commits_inserted = 0;
    let mut links_inserted = 0;

    for commit in &commits {
        // Insert commit (ignore if exists)
        let rows_affected = insert_commit_stmt.execute(params![
            next_commit_id,
            project_id,
            &commit.hash,
            commit.timestamp,
            &commit.date,
            &commit.author,
            &commit.subject,
        ])?;

        if rows_affected > 0 {
            commits_inserted += 1;
            next_commit_id += 1;
        }

        // Get the commit ID (whether just inserted or already existed)
        let commit_id: i64 = match get_commit_stmt.query_row(
            params![project_id, &commit.hash],
            |row| row.get(0),
        ) {
            Ok(id) => id,
            Err(_) => continue,
        };

        // Link to modules based on changed files
        for file in &commit.files {
            // Normalize file path
            let normalized = file.trim_start_matches("./").to_string();

            // Check if this file matches any module
            for (mod_path, &mod_id) in &module_paths {
                if normalized.ends_with(mod_path) || mod_path.ends_with(&normalized) {
                    link_stmt.execute(params![mod_id, commit_id, "modified"])?;
                    links_inserted += 1;
                    break;
                }
            }
        }
    }

    Ok((commits_inserted, links_inserted))
}

/// Get recent commits from a git repository
fn get_recent_commits(path: &Path, limit: usize) -> Vec<CommitInfo> {
    let mut commits = Vec::new();

    // Get commit info with files changed
    let output = Command::new("git")
        .args([
            "log",
            &format!("-{}", limit),
            "--pretty=format:%H|%at|%ai|%an|%s",
            "--name-only",
            "--",
            "src/",
        ])
        .current_dir(path)
        .output();

    let output = match output {
        Ok(o) if o.status.success() => o,
        _ => return commits,
    };

    let stdout = String::from_utf8_lossy(&output.stdout);
    let mut current_commit: Option<CommitInfo> = None;

    for line in stdout.lines() {
        if line.is_empty() {
            if let Some(commit) = current_commit.take() {
                commits.push(commit);
            }
        } else if line.contains('|') {
            // This is a commit header line
            if let Some(commit) = current_commit.take() {
                commits.push(commit);
            }

            let parts: Vec<&str> = line.splitn(5, '|').collect();
            if parts.len() >= 5 {
                current_commit = Some(CommitInfo {
                    hash: parts[0].to_string(),
                    timestamp: parts[1].parse().unwrap_or(0),
                    date: parts[2].to_string(),
                    author: parts[3].to_string(),
                    subject: parts[4].to_string(),
                    files: Vec::new(),
                });
            }
        } else if let Some(ref mut commit) = current_commit {
            // This is a file path
            commit.files.push(line.to_string());
        }
    }

    // Don't forget the last commit
    if let Some(commit) = current_commit {
        commits.push(commit);
    }

    commits
}

/// Update module metrics based on git data
pub fn update_module_metrics(conn: &Connection, package_version_id: i64) -> Result<usize> {
    // This updates metrics for modules based on their commit history
    let updated = conn.execute(
        r#"
        INSERT INTO module_metrics (module_id, commit_count, author_count, authors)
        SELECT
            m.id,
            COUNT(DISTINCT c.id),
            COUNT(DISTINCT c.author),
            json_group_array(DISTINCT c.author)
        FROM modules m
        LEFT JOIN module_commits mc ON mc.module_id = m.id
        LEFT JOIN commits c ON c.id = mc.commit_id
        WHERE m.package_version_id = ?
        GROUP BY m.id
        ON CONFLICT (module_id) DO UPDATE SET
            commit_count = excluded.commit_count,
            author_count = excluded.author_count,
            authors = excluded.authors
        "#,
        params![package_version_id],
    )?;

    Ok(updated)
}

/// Compute coupling metrics for modules
pub fn update_coupling_metrics(conn: &Connection, package_version_id: i64) -> Result<usize> {
    // Efferent coupling: number of modules this module depends on
    let efferent = conn.execute(
        r#"
        UPDATE module_metrics SET efferent_coupling = (
            SELECT COUNT(DISTINCT mi.imported_module)
            FROM module_imports mi
            WHERE mi.module_id = module_metrics.module_id
        )
        WHERE module_id IN (SELECT id FROM modules WHERE package_version_id = ?)
        "#,
        params![package_version_id],
    )?;

    // Afferent coupling: number of modules that depend on this module
    conn.execute(
        r#"
        UPDATE module_metrics SET afferent_coupling = (
            SELECT COUNT(DISTINCT mi.module_id)
            FROM module_imports mi
            JOIN modules m ON m.name = mi.imported_module
            WHERE m.id = module_metrics.module_id
        )
        WHERE module_id IN (SELECT id FROM modules WHERE package_version_id = ?)
        "#,
        params![package_version_id],
    )?;

    // Instability = Ce / (Ce + Ca)
    conn.execute(
        r#"
        UPDATE module_metrics SET instability =
            CASE WHEN (efferent_coupling + afferent_coupling) > 0
                 THEN CAST(efferent_coupling AS REAL) / (efferent_coupling + afferent_coupling)
                 ELSE 0.0
            END
        WHERE module_id IN (SELECT id FROM modules WHERE package_version_id = ?)
        "#,
        params![package_version_id],
    )?;

    Ok(efferent)
}
