# minard-loader M2: Project & Snapshot Support

## Overview

Extend minard-loader to support multiple projects and snapshots, enabling version switching in the frontend.

## Current State (M1)

M1 loads a single project into a flat schema:
- `package_versions` - packages from spago.lock
- `modules` - from docs.json files
- `declarations` / `child_declarations` - types, functions, classes

Missing: project identity and point-in-time snapshots.

## M2 Requirements

### 1. Schema Additions

Add to `src/db/schema.rs`:

```sql
-- A codebase being analyzed
CREATE TABLE IF NOT EXISTS projects (
    id              INTEGER PRIMARY KEY,
    name            VARCHAR NOT NULL UNIQUE,
    repo_path       VARCHAR,
    description     TEXT,
    created_at      TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- A point-in-time analysis of a project
CREATE TABLE IF NOT EXISTS snapshots (
    id              INTEGER PRIMARY KEY,
    project_id      INTEGER NOT NULL REFERENCES projects(id),
    git_hash        VARCHAR,
    git_ref         VARCHAR,
    label           VARCHAR,
    snapshot_at     TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    UNIQUE(project_id, git_hash)
);

-- Which package versions are used by a snapshot
CREATE TABLE IF NOT EXISTS snapshot_packages (
    snapshot_id         INTEGER NOT NULL REFERENCES snapshots(id),
    package_version_id  INTEGER NOT NULL REFERENCES package_versions(id),
    source              VARCHAR NOT NULL,  -- 'registry' | 'workspace' | 'local'
    is_direct           BOOLEAN DEFAULT FALSE,
    PRIMARY KEY (snapshot_id, package_version_id)
);
```

### 2. CLI Changes

Update `src/main.rs` and `src/config.rs`:

```
minard-loader load [OPTIONS] <PROJECT_PATH>

Options:
  -d, --database <DATABASE>    Path to database file [default: minard.duckdb]
  -p, --project <NAME>         Project name (defaults to directory name)
  -l, --label <LABEL>          Snapshot label (defaults to git ref or "manual")
  --scan                       Scan for all spago.lock files in subdirectories
```

**New behavior with `--scan`:**
- Walk PROJECT_PATH looking for `spago.lock` files
- Each found project becomes a separate project record
- Project name derived from relative path (e.g., `showcases/hypo-punter`)

### 3. Discovery Changes

Update `src/loader/discovery.rs`:

```rust
/// Discover all PureScript projects in a directory tree
pub fn discover_all(root: &Path) -> Result<Vec<ProjectDiscovery>> {
    // Walk directory tree
    // For each spago.lock found:
    //   - Check for output/ directory with docs.json files
    //   - Create ProjectDiscovery with relative path as name
    // Return all valid projects
}
```

### 4. Pipeline Changes

Update `src/loader/pipeline.rs`:

```rust
pub fn load_project(
    conn: &Connection,
    discovery: &ProjectDiscovery,
    project_name: &str,
    snapshot_label: Option<&str>,
) -> Result<LoadStats> {
    // 1. Get or create project record
    // 2. Get git info (hash, ref) if in a git repo
    // 3. Create snapshot record
    // 4. Load packages (link to snapshot via snapshot_packages)
    // 5. Load modules and declarations (as before)
}
```

### 5. Git Integration

Add `src/git.rs`:

```rust
/// Get current git info for a path
pub fn get_git_info(path: &Path) -> Option<GitInfo> {
    // Run: git rev-parse HEAD
    // Run: git symbolic-ref --short HEAD (or git describe --tags)
    // Return GitInfo { hash, ref_name }
}
```

Use `std::process::Command` to shell out to git (simpler than libgit2).

### 6. Stats Update

Update stats command to show project/snapshot breakdown:

```
Database contains:
  2 projects
    demo-website: 3 snapshots (latest: main@abc123)
    hypo-punter: 1 snapshot (latest: manual)
  91 packages
  722 modules
  8023 declarations
```

## Testing

Test with PSD3-Repos monorepo:

```bash
# Scan entire monorepo
./target/release/minard-loader load --scan -d test.duckdb /Users/afc/work/afc-work/PSD3-Repos

# Should find and load:
# - site/website
# - showcases/hypo-punter
# - showcases/hylograph-app
# - visualisation libraries/* (those with output/)
# etc.
```

## Compatibility

- M1 databases can be migrated by running schema additions (tables are IF NOT EXISTS)
- Existing package_versions/modules/declarations remain valid
- Frontend API unchanged for basic queries, new endpoints for project/snapshot listing

## Out of Scope (M3+)

- Module imports (from corefn.json)
- Function call graphs
- Git history / commit timeline
- Diff between snapshots
