use duckdb::Connection;

use crate::error::Result;

/// SQL schema for minard-loader (M3.1 - Polyglot Support)
/// v3.1: Added primary_backend to projects, FFI LOC columns to package_versions
const SCHEMA_SQL: &str = r#"
-- =============================================================================
-- REGISTRY LAYER
-- =============================================================================

CREATE TABLE IF NOT EXISTS package_sets (
    id              INTEGER PRIMARY KEY,
    version         VARCHAR NOT NULL UNIQUE,
    compiler        VARCHAR NOT NULL,
    source          VARCHAR,
    published_at    TIMESTAMP,
    package_count   INTEGER,
    created_at      TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE IF NOT EXISTS package_versions (
    id              INTEGER PRIMARY KEY,
    name            VARCHAR NOT NULL,
    version         VARCHAR NOT NULL,
    description     TEXT,
    license         VARCHAR,
    repository      VARCHAR,
    source          VARCHAR DEFAULT 'registry',
    -- FFI statistics (polyglot support v3.2)
    loc_ffi_js      INTEGER DEFAULT 0,
    loc_ffi_erlang  INTEGER DEFAULT 0,
    loc_ffi_python  INTEGER DEFAULT 0,
    loc_ffi_lua     INTEGER DEFAULT 0,
    loc_ffi_rust    INTEGER DEFAULT 0,
    ffi_file_count  INTEGER DEFAULT 0,
    created_at      TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    UNIQUE(name, version)
);

CREATE TABLE IF NOT EXISTS package_set_members (
    package_set_id      INTEGER NOT NULL REFERENCES package_sets(id),
    package_version_id  INTEGER NOT NULL REFERENCES package_versions(id),
    topo_layer          INTEGER,
    PRIMARY KEY (package_set_id, package_version_id)
);

CREATE TABLE IF NOT EXISTS package_dependencies (
    dependent_id    INTEGER NOT NULL REFERENCES package_versions(id),
    dependency_name VARCHAR NOT NULL,
    PRIMARY KEY (dependent_id, dependency_name)
);

-- =============================================================================
-- PROJECT LAYER
-- =============================================================================

CREATE TABLE IF NOT EXISTS projects (
    id              INTEGER PRIMARY KEY,
    name            VARCHAR NOT NULL UNIQUE,
    repo_path       VARCHAR,
    description     TEXT,
    package_set_id  INTEGER REFERENCES package_sets(id),
    primary_backend VARCHAR DEFAULT 'js',  -- 'js' | 'erlang' | 'python' | 'lua'
    created_at      TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE IF NOT EXISTS snapshots (
    id              INTEGER PRIMARY KEY,
    project_id      INTEGER NOT NULL REFERENCES projects(id),
    git_hash        VARCHAR,
    git_ref         VARCHAR,
    label           VARCHAR,
    snapshot_at     TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    UNIQUE(project_id, git_hash)
);

CREATE TABLE IF NOT EXISTS snapshot_packages (
    snapshot_id         INTEGER NOT NULL REFERENCES snapshots(id),
    package_version_id  INTEGER NOT NULL REFERENCES package_versions(id),
    source              VARCHAR NOT NULL,
    is_direct           BOOLEAN DEFAULT FALSE,
    topo_layer          INTEGER,
    PRIMARY KEY (snapshot_id, package_version_id)
);

-- =============================================================================
-- NAMESPACE LAYER
-- =============================================================================

CREATE TABLE IF NOT EXISTS module_namespaces (
    id              INTEGER PRIMARY KEY,
    path            VARCHAR NOT NULL UNIQUE,
    segment         VARCHAR NOT NULL,
    parent_id       INTEGER REFERENCES module_namespaces(id),
    depth           INTEGER NOT NULL,
    is_leaf         BOOLEAN DEFAULT TRUE
);

CREATE INDEX IF NOT EXISTS idx_namespace_parent ON module_namespaces(parent_id);
CREATE INDEX IF NOT EXISTS idx_namespace_depth ON module_namespaces(depth);

-- =============================================================================
-- MODULE LAYER
-- =============================================================================

CREATE TABLE IF NOT EXISTS modules (
    id                  INTEGER PRIMARY KEY,
    package_version_id  INTEGER NOT NULL REFERENCES package_versions(id),
    namespace_id        INTEGER REFERENCES module_namespaces(id),
    name                VARCHAR NOT NULL,
    path                VARCHAR,
    comments            TEXT,
    loc                 INTEGER,
    UNIQUE(package_version_id, name)
);

CREATE INDEX IF NOT EXISTS idx_module_package ON modules(package_version_id);
CREATE INDEX IF NOT EXISTS idx_module_namespace ON modules(namespace_id);
CREATE INDEX IF NOT EXISTS idx_module_name ON modules(name);

CREATE TABLE IF NOT EXISTS module_imports (
    module_id           INTEGER NOT NULL REFERENCES modules(id),
    imported_module     VARCHAR NOT NULL,
    PRIMARY KEY (module_id, imported_module)
);

CREATE TABLE IF NOT EXISTS module_reexports (
    module_id           INTEGER NOT NULL REFERENCES modules(id),
    source_module       VARCHAR NOT NULL,
    declaration_name    VARCHAR NOT NULL,
    PRIMARY KEY (module_id, source_module, declaration_name)
);

-- =============================================================================
-- DECLARATION LAYER
-- =============================================================================

CREATE TABLE IF NOT EXISTS declarations (
    id              INTEGER PRIMARY KEY,
    module_id       INTEGER NOT NULL REFERENCES modules(id),
    name            VARCHAR NOT NULL,
    kind            VARCHAR NOT NULL,
    type_signature  TEXT,
    type_ast        JSON,
    data_decl_type  VARCHAR,
    type_arguments  JSON,
    roles           JSON,
    superclasses    JSON,
    fundeps         JSON,
    synonym_type    JSON,
    comments        TEXT,
    source_span     JSON,
    source_code     TEXT,
    UNIQUE(module_id, name)
);

CREATE INDEX IF NOT EXISTS idx_decl_module ON declarations(module_id);
CREATE INDEX IF NOT EXISTS idx_decl_kind ON declarations(kind);
CREATE INDEX IF NOT EXISTS idx_decl_name ON declarations(name);

CREATE TABLE IF NOT EXISTS child_declarations (
    id                  INTEGER PRIMARY KEY,
    declaration_id      INTEGER NOT NULL REFERENCES declarations(id),
    name                VARCHAR NOT NULL,
    kind                VARCHAR NOT NULL,
    type_signature      TEXT,
    type_ast            JSON,
    constructor_args    JSON,
    instance_chain      JSON,
    instance_constraints JSON,
    comments            TEXT,
    source_span         JSON
);

CREATE INDEX IF NOT EXISTS idx_child_decl_parent ON child_declarations(declaration_id);
CREATE INDEX IF NOT EXISTS idx_child_decl_kind ON child_declarations(kind);

-- =============================================================================
-- CALL GRAPH LAYER
-- =============================================================================

CREATE TABLE IF NOT EXISTS function_calls (
    id                  INTEGER PRIMARY KEY,
    caller_module_id    INTEGER NOT NULL REFERENCES modules(id),
    caller_name         VARCHAR NOT NULL,
    callee_module       VARCHAR NOT NULL,
    callee_name         VARCHAR NOT NULL,
    is_cross_module     BOOLEAN DEFAULT TRUE,
    call_count          INTEGER DEFAULT 1,
    UNIQUE(caller_module_id, caller_name, callee_module, callee_name)
);

CREATE INDEX IF NOT EXISTS idx_calls_caller ON function_calls(caller_module_id, caller_name);
CREATE INDEX IF NOT EXISTS idx_calls_callee ON function_calls(callee_module, callee_name);

-- =============================================================================
-- GIT LAYER
-- =============================================================================

CREATE TABLE IF NOT EXISTS commits (
    id              INTEGER PRIMARY KEY,
    project_id      INTEGER NOT NULL REFERENCES projects(id),
    hash            VARCHAR NOT NULL,
    timestamp       INTEGER,
    date            VARCHAR,
    author          VARCHAR,
    subject         TEXT,
    files_created   JSON,
    files_modified  JSON,
    files_deleted   JSON,
    UNIQUE(project_id, hash)
);

CREATE INDEX IF NOT EXISTS idx_commits_project ON commits(project_id);
CREATE INDEX IF NOT EXISTS idx_commits_timestamp ON commits(timestamp);

CREATE TABLE IF NOT EXISTS module_commits (
    module_id       INTEGER NOT NULL REFERENCES modules(id),
    commit_id       INTEGER NOT NULL REFERENCES commits(id),
    change_type     VARCHAR,
    PRIMARY KEY (module_id, commit_id)
);

-- =============================================================================
-- METRICS LAYER
-- =============================================================================

CREATE TABLE IF NOT EXISTS module_metrics (
    module_id           INTEGER PRIMARY KEY REFERENCES modules(id),
    commit_count        INTEGER DEFAULT 0,
    days_since_modified INTEGER,
    age_in_days         INTEGER,
    author_count        INTEGER DEFAULT 0,
    authors             JSON,
    efferent_coupling   INTEGER DEFAULT 0,
    afferent_coupling   INTEGER DEFAULT 0,
    instability         REAL,
    normalized_commits  REAL,
    normalized_authors  REAL,
    normalized_coupling REAL
);

CREATE TABLE IF NOT EXISTS declaration_metrics (
    declaration_id          INTEGER PRIMARY KEY REFERENCES declarations(id),
    external_call_count     INTEGER DEFAULT 0,
    external_caller_count   INTEGER DEFAULT 0,
    total_coupling          INTEGER DEFAULT 0,
    coupling_intensity      REAL
);

-- =============================================================================
-- METADATA
-- =============================================================================

CREATE TABLE IF NOT EXISTS metadata (
    key     VARCHAR PRIMARY KEY,
    value   VARCHAR
);

INSERT OR REPLACE INTO metadata (key, value) VALUES ('schema_version', '3.1');
INSERT OR REPLACE INTO metadata (key, value) VALUES ('created_at', CURRENT_TIMESTAMP);
"#;

/// Views are created separately (DuckDB has issues with some view syntax in batch)
const VIEWS_SQL: &str = r#"
-- Namespace tree with statistics
CREATE OR REPLACE VIEW namespace_stats AS
WITH RECURSIVE ns_tree AS (
    SELECT id, path, segment, parent_id, depth, is_leaf
    FROM module_namespaces
    WHERE parent_id IS NULL
    UNION ALL
    SELECT n.id, n.path, n.segment, n.parent_id, n.depth, n.is_leaf
    FROM module_namespaces n
    INNER JOIN ns_tree t ON n.parent_id = t.id
)
SELECT
    ns.id,
    ns.path,
    ns.segment,
    ns.depth,
    ns.parent_id,
    COUNT(DISTINCT m.id) AS module_count,
    COUNT(DISTINCT pv.id) AS package_count,
    COUNT(DISTINCT d.id) AS declaration_count,
    COALESCE(SUM(m.loc), 0) AS total_loc
FROM ns_tree ns
LEFT JOIN modules m ON m.namespace_id = ns.id
LEFT JOIN package_versions pv ON m.package_version_id = pv.id
LEFT JOIN declarations d ON d.module_id = m.id
GROUP BY ns.id, ns.path, ns.segment, ns.depth, ns.parent_id;

-- Package summary for a snapshot
CREATE OR REPLACE VIEW snapshot_package_summary AS
SELECT
    sp.snapshot_id,
    pv.id AS package_version_id,
    pv.name,
    pv.version,
    sp.source,
    sp.is_direct,
    pv.license,
    pv.repository,
    COUNT(DISTINCT m.id) AS module_count,
    COUNT(DISTINCT d.id) AS declaration_count,
    COALESCE(SUM(m.loc), 0) AS total_loc
FROM snapshot_packages sp
JOIN package_versions pv ON sp.package_version_id = pv.id
LEFT JOIN modules m ON m.package_version_id = pv.id
LEFT JOIN declarations d ON d.module_id = m.id
GROUP BY sp.snapshot_id, pv.id, pv.name, pv.version, sp.source, sp.is_direct, pv.license, pv.repository;

-- Module with full context
CREATE OR REPLACE VIEW module_details AS
SELECT
    m.id,
    m.name AS module_name,
    m.path,
    m.comments,
    m.loc,
    pv.name AS package_name,
    pv.version AS package_version,
    pv.id AS package_version_id,
    ns.path AS namespace_path,
    ns.depth AS namespace_depth,
    mm.commit_count,
    mm.author_count,
    mm.efferent_coupling,
    mm.afferent_coupling,
    mm.instability
FROM modules m
JOIN package_versions pv ON m.package_version_id = pv.id
LEFT JOIN module_namespaces ns ON m.namespace_id = ns.id
LEFT JOIN module_metrics mm ON mm.module_id = m.id;

-- Declaration with full context
CREATE OR REPLACE VIEW declaration_details AS
SELECT
    d.id,
    d.name AS declaration_name,
    d.kind,
    d.type_signature,
    d.comments,
    m.name AS module_name,
    pv.name AS package_name,
    pv.version AS package_version,
    dm.total_coupling,
    dm.coupling_intensity
FROM declarations d
JOIN modules m ON d.module_id = m.id
JOIN package_versions pv ON m.package_version_id = pv.id
LEFT JOIN declaration_metrics dm ON dm.declaration_id = d.id;

-- Type class instances
CREATE OR REPLACE VIEW type_class_instances AS
SELECT
    cd.id AS instance_id,
    cd.name AS instance_name,
    cd.type_signature AS instance_type,
    d.name AS class_name,
    m.name AS module_name,
    pv.name AS package_name,
    pv.version AS package_version
FROM child_declarations cd
JOIN declarations d ON cd.declaration_id = d.id
JOIN modules m ON d.module_id = m.id
JOIN package_versions pv ON m.package_version_id = pv.id
WHERE cd.kind = 'instance';

-- Polyglot summary view (v3.2)
CREATE OR REPLACE VIEW polyglot_summary AS
SELECT
    p.id AS project_id,
    p.name AS project_name,
    p.primary_backend,
    COUNT(DISTINCT pv.id) AS package_count,
    COALESCE(SUM(m.loc), 0) AS total_loc_purescript,
    COALESCE(SUM(pv.loc_ffi_js), 0) AS total_loc_js,
    COALESCE(SUM(pv.loc_ffi_erlang), 0) AS total_loc_erlang,
    COALESCE(SUM(pv.loc_ffi_python), 0) AS total_loc_python,
    COALESCE(SUM(pv.loc_ffi_lua), 0) AS total_loc_lua,
    COALESCE(SUM(pv.loc_ffi_rust), 0) AS total_loc_rust,
    COALESCE(SUM(pv.ffi_file_count), 0) AS total_ffi_files
FROM projects p
JOIN snapshots s ON s.project_id = p.id
JOIN snapshot_packages sp ON sp.snapshot_id = s.id
JOIN package_versions pv ON sp.package_version_id = pv.id
LEFT JOIN modules m ON m.package_version_id = pv.id
GROUP BY p.id, p.name, p.primary_backend;

-- Backend distribution view (v3.2)
CREATE OR REPLACE VIEW backend_distribution AS
SELECT
    primary_backend,
    COUNT(DISTINCT project_id) AS project_count,
    SUM(total_loc_purescript) AS total_purescript_loc,
    SUM(total_loc_js + total_loc_erlang + total_loc_python + total_loc_lua + total_loc_rust) AS total_ffi_loc
FROM polyglot_summary
GROUP BY primary_backend;
"#;

/// Initialize the database schema
pub fn init_schema(conn: &Connection) -> Result<()> {
    conn.execute_batch(SCHEMA_SQL)?;
    // Views created separately to handle DuckDB quirks
    conn.execute_batch(VIEWS_SQL)?;
    Ok(())
}

/// Drop all tables (for fresh initialization)
pub fn drop_all_tables(conn: &Connection) -> Result<()> {
    conn.execute_batch(
        r#"
        DROP VIEW IF EXISTS type_class_instances;
        DROP VIEW IF EXISTS declaration_details;
        DROP VIEW IF EXISTS module_details;
        DROP VIEW IF EXISTS snapshot_package_summary;
        DROP VIEW IF EXISTS namespace_stats;
        DROP TABLE IF EXISTS declaration_metrics;
        DROP TABLE IF EXISTS module_metrics;
        DROP TABLE IF EXISTS module_commits;
        DROP TABLE IF EXISTS commits;
        DROP TABLE IF EXISTS function_calls;
        DROP TABLE IF EXISTS child_declarations;
        DROP TABLE IF EXISTS declarations;
        DROP TABLE IF EXISTS module_reexports;
        DROP TABLE IF EXISTS module_imports;
        DROP TABLE IF EXISTS modules;
        DROP TABLE IF EXISTS module_namespaces;
        DROP TABLE IF EXISTS snapshot_packages;
        DROP TABLE IF EXISTS snapshots;
        DROP TABLE IF EXISTS projects;
        DROP TABLE IF EXISTS package_dependencies;
        DROP TABLE IF EXISTS package_set_members;
        DROP TABLE IF EXISTS package_versions;
        DROP TABLE IF EXISTS package_sets;
        DROP TABLE IF EXISTS metadata;
    "#,
    )?;
    Ok(())
}

/// Get database statistics
pub fn get_stats(conn: &Connection) -> Result<DbStats> {
    let project_count: i64 = conn
        .query_row("SELECT COUNT(*) FROM projects", [], |row| row.get(0))
        .unwrap_or(0);

    let snapshot_count: i64 = conn
        .query_row("SELECT COUNT(*) FROM snapshots", [], |row| row.get(0))
        .unwrap_or(0);

    let package_count: i64 = conn
        .query_row("SELECT COUNT(*) FROM package_versions", [], |row| row.get(0))
        .unwrap_or(0);

    let module_count: i64 = conn
        .query_row("SELECT COUNT(*) FROM modules", [], |row| row.get(0))
        .unwrap_or(0);

    let declaration_count: i64 = conn
        .query_row("SELECT COUNT(*) FROM declarations", [], |row| row.get(0))
        .unwrap_or(0);

    let child_count: i64 = conn
        .query_row("SELECT COUNT(*) FROM child_declarations", [], |row| {
            row.get(0)
        })
        .unwrap_or(0);

    let dependency_count: i64 = conn
        .query_row("SELECT COUNT(*) FROM package_dependencies", [], |row| {
            row.get(0)
        })
        .unwrap_or(0);

    let namespace_count: i64 = conn
        .query_row("SELECT COUNT(*) FROM module_namespaces", [], |row| {
            row.get(0)
        })
        .unwrap_or(0);

    // Get project details for breakdown
    let project_details = get_project_details(conn).unwrap_or_default();

    Ok(DbStats {
        project_count,
        snapshot_count,
        package_count,
        module_count,
        declaration_count,
        child_count,
        dependency_count,
        namespace_count,
        project_details,
    })
}

/// Get detailed project/snapshot information
fn get_project_details(conn: &Connection) -> Result<Vec<ProjectDetail>> {
    let mut stmt = conn.prepare(
        "SELECT p.name, p.primary_backend, COUNT(s.id) as snapshot_count,
                (SELECT git_ref FROM snapshots WHERE project_id = p.id ORDER BY snapshot_at DESC LIMIT 1) as latest_ref,
                (SELECT SUBSTRING(git_hash, 1, 7) FROM snapshots WHERE project_id = p.id ORDER BY snapshot_at DESC LIMIT 1) as latest_hash
         FROM projects p
         LEFT JOIN snapshots s ON s.project_id = p.id
         GROUP BY p.id, p.name, p.primary_backend
         ORDER BY p.name",
    )?;

    let rows = stmt.query_map([], |row| {
        Ok(ProjectDetail {
            name: row.get(0)?,
            backend: row.get::<_, Option<String>>(1)?.unwrap_or_else(|| "js".to_string()),
            snapshot_count: row.get(2)?,
            latest_ref: row.get(3)?,
            latest_hash: row.get(4)?,
        })
    })?;

    let mut details = Vec::new();
    for row in rows {
        details.push(row?);
    }
    Ok(details)
}

#[derive(Debug, Clone)]
pub struct ProjectDetail {
    pub name: String,
    pub backend: String,
    pub snapshot_count: i64,
    pub latest_ref: Option<String>,
    pub latest_hash: Option<String>,
}

#[derive(Debug)]
pub struct DbStats {
    pub project_count: i64,
    pub snapshot_count: i64,
    pub package_count: i64,
    pub module_count: i64,
    pub declaration_count: i64,
    pub child_count: i64,
    pub dependency_count: i64,
    pub namespace_count: i64,
    pub project_details: Vec<ProjectDetail>,
}

impl DbStats {
    pub fn report(&self) -> String {
        let mut lines = vec![String::from("Database contains:")];

        if self.project_count > 0 {
            lines.push(format!("  {} projects", self.project_count));
            for proj in &self.project_details {
                let latest = match (&proj.latest_ref, &proj.latest_hash) {
                    (Some(ref_name), Some(hash)) => format!("latest: {}@{}", ref_name, hash),
                    (Some(ref_name), None) => format!("latest: {}", ref_name),
                    (None, Some(hash)) => format!("latest: {}", hash),
                    (None, None) => "no snapshots".to_string(),
                };
                let backend_tag = if proj.backend != "js" {
                    format!(" [{}]", proj.backend)
                } else {
                    String::new()
                };
                lines.push(format!(
                    "    {}{}: {} snapshots ({})",
                    proj.name, backend_tag, proj.snapshot_count, latest
                ));
            }
        }

        lines.push(format!("  {} packages", self.package_count));
        lines.push(format!("  {} modules", self.module_count));
        lines.push(format!("  {} declarations", self.declaration_count));
        lines.push(format!("  {} child declarations", self.child_count));
        lines.push(format!("  {} namespaces", self.namespace_count));
        lines.push(format!("  {} dependencies", self.dependency_count));

        lines.join("\n")
    }
}
