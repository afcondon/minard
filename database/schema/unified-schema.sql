-- =============================================================================
-- Code Explorer Unified Schema v3.0
-- =============================================================================
--
-- A unified data model for PureScript project analysis that provides:
--   - Consistent identity across registry and project packages
--   - Full type information for Pursuit-like queries
--   - Module namespace hierarchy independent of packages
--   - Git history integration at module level
--   - Computed metrics for visualization
--
-- Design principles:
--   - Package versions are the core identity (name + version)
--   - spago.lock is the source of truth for dependency resolution
--   - Types are first-class citizens (stored as both text and AST)
--   - Module namespaces form a tree independent of package boundaries
--
-- Data sources:
--   - Registry index (_registry-index/)  → package_sets, package_versions
--   - spago.lock                         → snapshot_packages (resolved deps)
--   - .spago/p/*/purs.json              → package metadata (license, repo)
--   - output/*/docs.json                → modules, declarations, types
--   - output/*/corefn.json              → module_imports, function_calls
--   - git log                           → commits, module_commits
--
-- =============================================================================

-- =============================================================================
-- REGISTRY LAYER
-- What exists in the PureScript ecosystem
-- =============================================================================

-- A point-in-time snapshot of the package registry
-- Corresponds to a specific package set version (e.g., 71.0.0)
CREATE TABLE IF NOT EXISTS package_sets (
    id              INTEGER PRIMARY KEY,
    version         VARCHAR NOT NULL UNIQUE,   -- "71.0.0"
    compiler        VARCHAR NOT NULL,          -- "0.15.15"
    source          VARCHAR,                   -- "registry" | "custom"
    published_at    TIMESTAMP,
    package_count   INTEGER,
    created_at      TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- A specific version of a package - THE core identity
-- "prelude" is not an entity; "prelude@6.0.2" is
CREATE TABLE IF NOT EXISTS package_versions (
    id              INTEGER PRIMARY KEY,
    name            VARCHAR NOT NULL,          -- "prelude"
    version         VARCHAR NOT NULL,          -- "6.0.2"

    -- Metadata (from purs.json / registry manifest)
    description     TEXT,
    license         VARCHAR,                   -- "MIT", "BSD-3-Clause"
    repository      VARCHAR,                   -- "purescript/purescript-prelude"

    -- For tracking where this package came from
    source          VARCHAR DEFAULT 'registry', -- "registry" | "local" | "git"

    created_at      TIMESTAMP DEFAULT CURRENT_TIMESTAMP,

    UNIQUE(name, version)
);

-- Which package versions are in which package sets
-- Same package version can appear in multiple sets
CREATE TABLE IF NOT EXISTS package_set_members (
    package_set_id      INTEGER NOT NULL REFERENCES package_sets(id),
    package_version_id  INTEGER NOT NULL REFERENCES package_versions(id),
    topo_layer          INTEGER,               -- Dependency depth (0 = no deps)

    PRIMARY KEY (package_set_id, package_version_id)
);

-- Package-level dependencies (from registry manifests)
-- Note: version constraints are resolved by the package set context
CREATE TABLE IF NOT EXISTS package_dependencies (
    dependent_id    INTEGER NOT NULL REFERENCES package_versions(id),
    dependency_name VARCHAR NOT NULL,          -- Just the name, not version

    PRIMARY KEY (dependent_id, dependency_name)
);

-- =============================================================================
-- PROJECT LAYER
-- A specific codebase being analyzed
-- =============================================================================

-- A project is a codebase we're analyzing
-- Links to a package set as its "universe" of available packages
CREATE TABLE IF NOT EXISTS projects (
    id              INTEGER PRIMARY KEY,
    name            VARCHAR NOT NULL UNIQUE,   -- "ce2-website"
    repo_path       VARCHAR,                   -- Filesystem path
    description     TEXT,
    package_set_id  INTEGER REFERENCES package_sets(id),
    created_at      TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- A point-in-time analysis of a project
-- Each snapshot captures the state at a specific git commit
CREATE TABLE IF NOT EXISTS snapshots (
    id              INTEGER PRIMARY KEY,
    project_id      INTEGER NOT NULL REFERENCES projects(id),
    git_hash        VARCHAR,                   -- Full SHA
    git_ref         VARCHAR,                   -- "main", "v1.0.0", "feature/foo"
    label           VARCHAR,                   -- Human-readable label
    snapshot_at     TIMESTAMP DEFAULT CURRENT_TIMESTAMP,

    UNIQUE(project_id, git_hash)
);

-- Which package versions are used by a snapshot
-- This comes from spago.lock's build_plan - THE resolved dependency set
CREATE TABLE IF NOT EXISTS snapshot_packages (
    snapshot_id         INTEGER NOT NULL REFERENCES snapshots(id),
    package_version_id  INTEGER NOT NULL REFERENCES package_versions(id),

    -- How this package relates to the project
    source              VARCHAR NOT NULL,      -- 'registry' | 'workspace' | 'extra'
    is_direct           BOOLEAN DEFAULT FALSE, -- Direct dependency vs transitive

    PRIMARY KEY (snapshot_id, package_version_id)
);

-- =============================================================================
-- NAMESPACE LAYER
-- Module hierarchy independent of packages
-- =============================================================================

-- The implicit tree structure of module names
-- "Data.Array.NonEmpty" creates nodes: Data, Data.Array, Data.Array.NonEmpty
CREATE TABLE IF NOT EXISTS module_namespaces (
    id              INTEGER PRIMARY KEY,
    path            VARCHAR NOT NULL UNIQUE,   -- "Data.Array" (full dotted path)
    segment         VARCHAR NOT NULL,          -- "Array" (just this level)
    parent_id       INTEGER REFERENCES module_namespaces(id),
    depth           INTEGER NOT NULL,          -- 0 for top-level (Data, Control, etc.)

    -- Denormalized for convenience
    is_leaf         BOOLEAN DEFAULT TRUE       -- Has actual modules, not just children
);

-- Index for tree traversal
CREATE INDEX IF NOT EXISTS idx_namespace_parent ON module_namespaces(parent_id);
CREATE INDEX IF NOT EXISTS idx_namespace_depth ON module_namespaces(depth);

-- =============================================================================
-- MODULE LAYER
-- Code structure within packages
-- =============================================================================

-- A module belongs to a specific package version
-- Same module name can exist in different versions (Data.Array in arrays@1.0 vs @2.0)
CREATE TABLE IF NOT EXISTS modules (
    id                  INTEGER PRIMARY KEY,
    package_version_id  INTEGER NOT NULL REFERENCES package_versions(id),
    namespace_id        INTEGER REFERENCES module_namespaces(id),

    name                VARCHAR NOT NULL,      -- "Data.Array.NonEmpty"
    path                VARCHAR,               -- Source file path relative to package

    -- Documentation
    comments            TEXT,                  -- Module-level documentation

    -- Metrics (optional, computed)
    loc                 INTEGER,               -- Lines of code

    UNIQUE(package_version_id, name)
);

-- Index for common queries
CREATE INDEX IF NOT EXISTS idx_module_package ON modules(package_version_id);
CREATE INDEX IF NOT EXISTS idx_module_namespace ON modules(namespace_id);
CREATE INDEX IF NOT EXISTS idx_module_name ON modules(name);

-- Module imports (from corefn.json)
-- Records which modules this module imports
CREATE TABLE IF NOT EXISTS module_imports (
    module_id           INTEGER NOT NULL REFERENCES modules(id),
    imported_module     VARCHAR NOT NULL,      -- FQN of imported module

    PRIMARY KEY (module_id, imported_module)
);

-- Module re-exports
-- When a module re-exports declarations from another module
CREATE TABLE IF NOT EXISTS module_reexports (
    module_id           INTEGER NOT NULL REFERENCES modules(id),
    source_module       VARCHAR NOT NULL,      -- Where the decl originally comes from
    declaration_name    VARCHAR NOT NULL,

    PRIMARY KEY (module_id, source_module, declaration_name)
);

-- =============================================================================
-- DECLARATION LAYER
-- Types, values, classes - the heart of PureScript
-- =============================================================================

-- Top-level declarations in a module
CREATE TABLE IF NOT EXISTS declarations (
    id              INTEGER PRIMARY KEY,
    module_id       INTEGER NOT NULL REFERENCES modules(id),

    -- Identity
    name            VARCHAR NOT NULL,          -- "filter", "Maybe", "Functor"
    kind            VARCHAR NOT NULL,          -- See declaration_kinds below

    -- Type information (THE key fields for a PureScript tool)
    type_signature  TEXT,                      -- Rendered: "forall a. (a -> Boolean) -> Array a -> Array a"
    type_ast        JSON,                      -- Structured AST from docs.json (enables type search)

    -- For data/newtype declarations
    data_decl_type  VARCHAR,                   -- 'data' | 'newtype' | null
    type_arguments  JSON,                      -- [["a", kind], ["b", kind]]
    roles           JSON,                      -- ["Representational", "Phantom"]

    -- For type class declarations
    superclasses    JSON,                      -- Array of constraint ASTs
    fundeps         JSON,                      -- Functional dependencies

    -- For type synonyms
    synonym_type    JSON,                      -- The expanded type AST

    -- Documentation and source
    comments        TEXT,                      -- Documentation comments
    source_span     JSON,                      -- {start: {line, col}, end: {line, col}}
    source_code     TEXT,                      -- Actual source if available

    UNIQUE(module_id, name)
);

-- Declaration kinds (for reference):
--   'value'         - Functions, constants (has type_signature)
--   'data'          - Data type (has type_arguments, constructors as children)
--   'newtype'       - Newtype wrapper (has type_arguments, single constructor)
--   'type_synonym'  - Type alias (has type_arguments, synonym_type)
--   'type_class'    - Type class (has type_arguments, superclasses, fundeps, members as children)
--   'foreign'       - Foreign import (has type_signature)

-- Index for searching
CREATE INDEX IF NOT EXISTS idx_decl_module ON declarations(module_id);
CREATE INDEX IF NOT EXISTS idx_decl_kind ON declarations(kind);
CREATE INDEX IF NOT EXISTS idx_decl_name ON declarations(name);

-- Child declarations: constructors, instances, class members
-- These belong to a parent declaration
CREATE TABLE IF NOT EXISTS child_declarations (
    id                  INTEGER PRIMARY KEY,
    declaration_id      INTEGER NOT NULL REFERENCES declarations(id),

    name                VARCHAR NOT NULL,      -- "Just", "Nothing", "map"
    kind                VARCHAR NOT NULL,      -- 'constructor' | 'instance' | 'class_member'

    -- Type information
    type_signature      TEXT,                  -- Rendered type
    type_ast            JSON,                  -- Structured AST

    -- For constructors
    constructor_args    JSON,                  -- Array of argument type ASTs

    -- For instances
    instance_chain      JSON,                  -- Instance chain info
    instance_constraints JSON,                 -- Required constraints

    -- Documentation
    comments            TEXT,
    source_span         JSON
);

CREATE INDEX IF NOT EXISTS idx_child_decl_parent ON child_declarations(declaration_id);
CREATE INDEX IF NOT EXISTS idx_child_decl_kind ON child_declarations(kind);

-- =============================================================================
-- CALL GRAPH LAYER
-- Function-level dependencies
-- =============================================================================

-- Function call relationships (from corefn.json analysis)
CREATE TABLE IF NOT EXISTS function_calls (
    id                  INTEGER PRIMARY KEY,
    caller_module_id    INTEGER NOT NULL REFERENCES modules(id),
    caller_name         VARCHAR NOT NULL,      -- Function making the call

    callee_module       VARCHAR NOT NULL,      -- Target module (may be external)
    callee_name         VARCHAR NOT NULL,      -- Target function

    is_cross_module     BOOLEAN DEFAULT TRUE,
    call_count          INTEGER DEFAULT 1      -- How many times called
);

CREATE INDEX IF NOT EXISTS idx_calls_caller ON function_calls(caller_module_id, caller_name);
CREATE INDEX IF NOT EXISTS idx_calls_callee ON function_calls(callee_module, callee_name);

-- =============================================================================
-- GIT LAYER
-- History and churn metrics
-- =============================================================================

-- Git commits for a project (not per-snapshot, project-wide)
CREATE TABLE IF NOT EXISTS commits (
    id              INTEGER PRIMARY KEY,
    project_id      INTEGER NOT NULL REFERENCES projects(id),

    hash            VARCHAR NOT NULL,          -- Full SHA
    timestamp       INTEGER,                   -- Unix timestamp
    date            VARCHAR,                   -- ISO date string
    author          VARCHAR,
    subject         TEXT,                      -- Commit message first line

    -- File changes (for quick access without parsing)
    files_created   JSON,                      -- ["src/Foo.purs", ...]
    files_modified  JSON,
    files_deleted   JSON,

    UNIQUE(project_id, hash)
);

CREATE INDEX IF NOT EXISTS idx_commits_project ON commits(project_id);
CREATE INDEX IF NOT EXISTS idx_commits_timestamp ON commits(timestamp);

-- Link modules to commits that touched them
CREATE TABLE IF NOT EXISTS module_commits (
    module_id       INTEGER NOT NULL REFERENCES modules(id),
    commit_id       INTEGER NOT NULL REFERENCES commits(id),
    change_type     VARCHAR,                   -- 'created' | 'modified' | 'deleted'

    PRIMARY KEY (module_id, commit_id)
);

-- =============================================================================
-- METRICS LAYER
-- Computed/derived metrics for visualization
-- =============================================================================

-- Module-level metrics (computed, can be refreshed)
CREATE TABLE IF NOT EXISTS module_metrics (
    module_id           INTEGER PRIMARY KEY REFERENCES modules(id),

    -- Git metrics
    commit_count        INTEGER DEFAULT 0,
    days_since_modified INTEGER,
    age_in_days         INTEGER,
    author_count        INTEGER DEFAULT 0,
    authors             JSON,                  -- ["alice", "bob"]

    -- Coupling metrics
    efferent_coupling   INTEGER DEFAULT 0,     -- Modules this depends on
    afferent_coupling   INTEGER DEFAULT 0,     -- Modules that depend on this
    instability         REAL,                  -- Ce / (Ce + Ca), 0=stable, 1=unstable

    -- Normalized for visualization (0-1 scale)
    normalized_commits  REAL,
    normalized_authors  REAL,
    normalized_coupling REAL
);

-- Declaration-level coupling metrics
CREATE TABLE IF NOT EXISTS declaration_metrics (
    declaration_id          INTEGER PRIMARY KEY REFERENCES declarations(id),

    external_call_count     INTEGER DEFAULT 0, -- Functions this calls (external)
    external_caller_count   INTEGER DEFAULT 0, -- Functions that call this (external)
    total_coupling          INTEGER DEFAULT 0,
    coupling_intensity      REAL               -- Normalized 0-1
);

-- =============================================================================
-- CONVENIENCE VIEWS
-- Pre-computed queries for common access patterns
-- =============================================================================

-- Namespace tree with statistics
CREATE VIEW IF NOT EXISTS namespace_stats AS
WITH RECURSIVE ns_tree AS (
    -- Start with root namespaces
    SELECT id, path, segment, parent_id, depth, is_leaf
    FROM module_namespaces
    WHERE parent_id IS NULL

    UNION ALL

    -- Recursively add children
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
CREATE VIEW IF NOT EXISTS snapshot_package_summary AS
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
CREATE VIEW IF NOT EXISTS module_details AS
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

-- Declaration with full context (for search results)
CREATE VIEW IF NOT EXISTS declaration_details AS
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

-- Type class instances across the codebase
CREATE VIEW IF NOT EXISTS type_class_instances AS
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

-- =============================================================================
-- METADATA
-- Schema version and configuration
-- =============================================================================

CREATE TABLE IF NOT EXISTS metadata (
    key     VARCHAR PRIMARY KEY,
    value   VARCHAR
);

-- Insert schema version
INSERT OR REPLACE INTO metadata (key, value) VALUES ('schema_version', '3.0');
INSERT OR REPLACE INTO metadata (key, value) VALUES ('created_at', CURRENT_TIMESTAMP);
