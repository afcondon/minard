# Minard Loader Specification

**Status**: Planned
**Target Language**: Rust or Go
**Purpose**: Fast, reliable data ingestion for the Minard database

## Overview

The loader is responsible for extracting information from PureScript (and eventually Haskell) codebases and populating the Minard DuckDB database. It must be fast enough to run on every compile without causing friction.

## Current State

The existing loader (`database/loader/ce-loader.js`, ~1400 lines Node.js) works but has issues:

| Issue | Impact |
|-------|--------|
| ~30s load time for 1000 modules | Discourages frequent updates |
| Single-threaded JSON parsing | CPU bottleneck |
| Full reload only | No incremental support |
| Brittle error handling | Fails silently on malformed input |

## Requirements

### Functional Requirements

**FR1: Project Discovery**
- Accept path to PureScript project root
- Locate `spago.yaml`, `spago.lock`, `output/` directory
- Support both workspace (monorepo) and single-package projects

**FR2: Data Extraction**

| Source | Data Extracted | Tables Populated |
|--------|----------------|------------------|
| `spago.lock` | Package names, versions, dependencies | `package_versions`, `package_dependencies` |
| `output/*/docs.json` | Module names, exports, declarations, type signatures | `modules`, `declarations`, `child_declarations` |
| `output/*/corefn.json` | Function calls, import references | `function_calls`, `module_imports` |
| `src/**/*.purs` | Lines of code, source spans | `modules.loc`, `declarations.source_code` |
| `git log` | Commits, authors, timestamps | `commits`, `module_commits` |
| `git diff` (optional) | Changed files since last snapshot | Incremental loading |

**FR3: Graph Construction**
- Build module import graph (adjacency list)
- Build function call graph
- Compute transitive closures for reachability analysis
- Identify orphaned modules (not reachable from entry points)

**FR4: Metrics Computation**
- Module LOC (lines of code)
- Declaration count per module
- Coupling metrics (external calls in/out)
- Git churn (commits, authors, recency)

**FR5: Database Operations**
- Create snapshot record
- Insert all extracted data within transaction
- Support `--incremental` mode (only changed modules)
- Support `--replace` mode (delete previous snapshot first)

**FR6: Multi-Project Support**
- Track multiple projects in same database
- Each project has independent snapshots
- Cross-project queries remain possible

### Non-Functional Requirements

**NFR1: Performance**
- Target: <5 seconds for 1000 modules (full load)
- Target: <1 second for incremental load (10 changed modules)
- Parallel file I/O and JSON parsing

**NFR2: Reliability**
- Graceful handling of malformed JSON
- Clear error messages with file paths
- Transaction rollback on failure
- No partial/corrupt database states

**NFR3: Portability**
- Single static binary (no runtime dependencies)
- macOS (ARM + x64), Linux (x64)
- Windows nice-to-have

**NFR4: Observability**
- Progress output (files processed, time elapsed)
- `--verbose` mode with detailed logging
- `--quiet` mode for CI integration
- JSON output mode for programmatic use

## Command-Line Interface

```
minard-loader [OPTIONS] <COMMAND>

Commands:
  load        Load a project into the database
  snapshot    Create a new snapshot of an existing project
  incremental Update database with only changed files
  orphans     Report orphaned modules (no database write)
  stats       Show database statistics

Options:
  -d, --database <PATH>   Path to DuckDB file [default: ./minard.duckdb]
  -v, --verbose           Verbose output
  -q, --quiet             Suppress non-error output
  --json                  Output as JSON (for tooling)

load:
  minard-loader load <PROJECT_PATH> [OPTIONS]

  Options:
    -n, --name <NAME>       Project name [default: directory name]
    -l, --label <LABEL>     Snapshot label [default: timestamp]
    --no-git                Skip git history extraction
    --no-calls              Skip function call extraction (faster)

incremental:
  minard-loader incremental <PROJECT_PATH> [OPTIONS]

  Options:
    --since <COMMIT>        Only files changed since this commit
    --since-last            Only files changed since last snapshot

orphans:
  minard-loader orphans <PROJECT_PATH> [OPTIONS]

  Options:
    --entry <MODULE>        Entry point module [default: Main]
    --format <FMT>          Output format: text, json, csv
```

## Data Structures

### Input Parsing

```rust
// From docs.json
struct DocsJson {
    name: String,           // "Data.Array"
    comments: Option<String>,
    declarations: Vec<Declaration>,
    reExports: Vec<ReExport>,
}

struct Declaration {
    title: String,          // "map"
    comments: Option<String>,
    info: DeclarationInfo,
    sourceSpan: Option<SourceSpan>,
    children: Vec<ChildDeclaration>,
}

enum DeclarationInfo {
    ValueDeclaration { type_: Type },
    DataDeclaration { dataDeclType: String, typeArguments: Vec<...>, constructors: Vec<...> },
    TypeSynonymDeclaration { ... },
    TypeClassDeclaration { ... },
    // ...
}

// From corefn.json
struct CorefnJson {
    moduleName: Vec<String>,  // ["Data", "Array"]
    imports: Vec<Import>,
    exports: Vec<String>,
    decls: Vec<Decl>,
}

struct Decl {
    // Contains Expr trees with function applications
    // Parse to extract call sites
}

// From spago.lock
struct SpagoLock {
    packages: HashMap<String, PackageInfo>,
}

struct PackageInfo {
    version: String,
    dependencies: Vec<String>,
}
```

### Internal Representation

```rust
struct Project {
    name: String,
    root_path: PathBuf,
    packages: Vec<Package>,
    modules: Vec<Module>,
}

struct Module {
    id: ModuleId,
    name: String,           // "Data.Array"
    package: PackageId,
    path: Option<PathBuf>,  // src/Data/Array.purs
    loc: u32,
    imports: Vec<ModuleId>,
    declarations: Vec<Declaration>,
}

struct CallGraph {
    // caller (module.decl) -> [(callee_module, callee_decl)]
    edges: HashMap<(ModuleId, DeclId), Vec<(ModuleId, String)>>,
}
```

## Algorithm Outline

```
1. DISCOVER
   - Find spago.yaml, determine project type (workspace vs single)
   - Find spago.lock, parse package versions
   - Find output/ directory, enumerate module directories
   - Find src/ directory, enumerate source files

2. PARSE (parallel)
   - For each output/<Module>/docs.json: parse declarations
   - For each output/<Module>/corefn.json: parse imports, calls
   - For each src/**/*.purs: count LOC, extract source spans
   - Parse git log for commit history

3. BUILD GRAPHS
   - Build module import graph
   - Build function call graph
   - Compute package dependencies from module imports

4. COMPUTE METRICS
   - Module LOC, declaration counts
   - Coupling: external calls in/out per declaration
   - Git: commit count, author count, days since modified

5. GENERATE SQL
   - Create snapshot record
   - Batch INSERT statements (1000 rows per statement)
   - Use prepared statements for type safety

6. EXECUTE
   - Begin transaction
   - Execute all INSERTs
   - Commit (or rollback on error)

7. REPORT
   - Print summary: modules loaded, declarations, time elapsed
   - Return exit code 0 on success
```

## Incremental Loading

For `--incremental` mode:

1. Query database for last snapshot's file timestamps
2. Compare with current file mtimes
3. Parse only changed files
4. Delete old records for changed modules
5. Insert new records
6. Update snapshot metadata

Key insight: Module-level granularity is sufficient. If any file in a module changes, reload the entire module.

## Future Extensions

### HTTPurple Route Extraction

Parse server code to extract API routes:

```purescript
-- Pattern to detect
route :: RouteDuplex' Route
route = root $ sum
  { "GetUser": path "api/users" (int segment)
  , "CreateUser": path "api/users" noArgs
  , "Health": path "health" noArgs
  }
```

Extract:
- Route name → URL pattern mapping
- HTTP method (from handler inspection)
- Request/response types (from type signatures)

Populate new tables:
- `api_routes (id, module_id, name, method, url_pattern)`
- `api_route_types (route_id, request_type, response_type)`

### WebSocket Endpoint Extraction

Similar pattern matching for WebSocket handlers.

### Cross-Reference with Frontend

Link frontend API calls to backend routes:
- Frontend: `fetch "/api/users"` or `Affjax.get "/api/users"`
- Backend: `"GetUser": path "api/users" ...`
- Result: `api_calls (frontend_module_id, backend_route_id)`

This enables:
- Dead API detection (defined but never called)
- Missing API detection (called but not defined)
- Full-stack dependency graphs

### Haskell Support

The loader architecture should support Haskell projects:
- Parse `.cabal` or `package.yaml` instead of `spago.yaml`
- Parse Haddock output instead of `docs.json`
- Parse `.hi` files or GHC output for call graphs

### Registry Integration

For PureScript:
- Fetch package metadata from registry API
- Populate `package_versions.description`, `license`, `repository`
- Track package release history

For Haskell:
- Fetch from Hackage/Stackage APIs

## Testing Strategy

**Unit Tests:**
- JSON parsing (docs.json, corefn.json, spago.lock)
- Graph algorithms (transitive closure, orphan detection)
- SQL generation

**Integration Tests:**
- Load a known test project
- Verify expected tables populated
- Verify query results match expected

**Performance Tests:**
- Benchmark on large project (1000+ modules)
- Compare against Node.js baseline
- Track regression

**Test Projects:**
- `test-fixtures/minimal/` - 3 modules, basic structure
- `test-fixtures/workspace/` - Monorepo with multiple packages
- `test-fixtures/large/` - Generated 1000 modules (for perf)

## Implementation Notes

### Why Rust or Go?

| Criterion | Rust | Go |
|-----------|------|-----|
| Performance | Excellent | Very good |
| JSON parsing | serde (excellent) | encoding/json (good) |
| SQLite/DuckDB | rusqlite, duckdb-rs | go-duckdb |
| Binary size | ~5MB | ~10MB |
| Build time | Slow | Fast |
| Learning curve | Steep | Gentle |

**Recommendation**: Go for faster iteration, Rust if performance is critical. Either is fine.

### DuckDB Integration

Both languages have DuckDB bindings:
- Rust: `duckdb-rs` crate
- Go: `go-duckdb` package

Use prepared statements and batch inserts for performance.

### Parallelism

- File I/O: Parallel directory traversal
- JSON parsing: Worker pool (num_cpus threads)
- SQL execution: Single-threaded (DuckDB limitation)

### Error Handling

- Parse errors: Log warning, skip file, continue
- Missing files: Log warning, continue
- Database errors: Rollback, exit with error code
- All errors include file path and line number where applicable

## Milestones

1. **M1: Basic Loading** - Parse docs.json, spago.lock, populate core tables
2. **M2: Call Graph** - Parse corefn.json, extract function calls
3. **M3: Git Integration** - Extract commit history, compute metrics
4. **M4: Incremental** - Detect changes, partial reload
5. **M5: Performance** - Parallel parsing, batch inserts, <5s target
6. **M6: API Routes** - HTTPurple route extraction (future)

## Open Questions

1. Should we support loading from tarball (for CI without git)?
2. Should we embed DuckDB or require external installation?
3. How to handle private/unpublished packages in spago.lock?
4. Should orphan detection be in loader or separate tool?

---

## Appendix A: DuckDB Schema

The complete schema is at `database/schema/unified-schema.sql`. Key tables:

```sql
-- =============================================================================
-- REGISTRY LAYER - What exists in the PureScript ecosystem
-- =============================================================================

CREATE TABLE IF NOT EXISTS package_sets (
    id              INTEGER PRIMARY KEY,
    version         VARCHAR NOT NULL UNIQUE,   -- "71.0.0"
    compiler        VARCHAR NOT NULL,          -- "0.15.15"
    source          VARCHAR,                   -- "registry" | "custom"
    published_at    TIMESTAMP,
    package_count   INTEGER,
    created_at      TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE IF NOT EXISTS package_versions (
    id              INTEGER PRIMARY KEY,
    name            VARCHAR NOT NULL,          -- "prelude"
    version         VARCHAR NOT NULL,          -- "6.0.2"
    description     TEXT,
    license         VARCHAR,                   -- "MIT", "BSD-3-Clause"
    repository      VARCHAR,                   -- "purescript/purescript-prelude"
    source          VARCHAR DEFAULT 'registry', -- "registry" | "local" | "git"
    created_at      TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    UNIQUE(name, version)
);

CREATE TABLE IF NOT EXISTS package_dependencies (
    dependent_id    INTEGER NOT NULL REFERENCES package_versions(id),
    dependency_name VARCHAR NOT NULL,          -- Just the name, not version
    PRIMARY KEY (dependent_id, dependency_name)
);

-- =============================================================================
-- PROJECT LAYER - A specific codebase being analyzed
-- =============================================================================

CREATE TABLE IF NOT EXISTS projects (
    id              INTEGER PRIMARY KEY,
    name            VARCHAR NOT NULL UNIQUE,   -- "ce2-website"
    repo_path       VARCHAR,                   -- Filesystem path
    description     TEXT,
    package_set_id  INTEGER REFERENCES package_sets(id),
    created_at      TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE IF NOT EXISTS snapshots (
    id              INTEGER PRIMARY KEY,
    project_id      INTEGER NOT NULL REFERENCES projects(id),
    git_hash        VARCHAR,                   -- Full SHA
    git_ref         VARCHAR,                   -- "main", "v1.0.0"
    label           VARCHAR,                   -- Human-readable label
    snapshot_at     TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    UNIQUE(project_id, git_hash)
);

CREATE TABLE IF NOT EXISTS snapshot_packages (
    snapshot_id         INTEGER NOT NULL REFERENCES snapshots(id),
    package_version_id  INTEGER NOT NULL REFERENCES package_versions(id),
    source              VARCHAR NOT NULL,      -- 'registry' | 'workspace' | 'extra'
    is_direct           BOOLEAN DEFAULT FALSE,
    PRIMARY KEY (snapshot_id, package_version_id)
);

-- =============================================================================
-- MODULE LAYER - Code structure within packages
-- =============================================================================

CREATE TABLE IF NOT EXISTS modules (
    id                  INTEGER PRIMARY KEY,
    package_version_id  INTEGER NOT NULL REFERENCES package_versions(id),
    namespace_id        INTEGER REFERENCES module_namespaces(id),
    name                VARCHAR NOT NULL,      -- "Data.Array.NonEmpty"
    path                VARCHAR,               -- Source file path
    comments            TEXT,
    loc                 INTEGER,
    UNIQUE(package_version_id, name)
);

CREATE TABLE IF NOT EXISTS module_imports (
    module_id           INTEGER NOT NULL REFERENCES modules(id),
    imported_module     VARCHAR NOT NULL,
    PRIMARY KEY (module_id, imported_module)
);

-- =============================================================================
-- DECLARATION LAYER - Types, values, classes
-- =============================================================================

CREATE TABLE IF NOT EXISTS declarations (
    id              INTEGER PRIMARY KEY,
    module_id       INTEGER NOT NULL REFERENCES modules(id),
    name            VARCHAR NOT NULL,          -- "filter", "Maybe", "Functor"
    kind            VARCHAR NOT NULL,          -- 'value' | 'data' | 'newtype' | 'type_synonym' | 'type_class' | 'foreign'
    type_signature  TEXT,                      -- "forall a. (a -> Boolean) -> Array a -> Array a"
    type_ast        JSON,                      -- Structured AST from docs.json
    data_decl_type  VARCHAR,                   -- 'data' | 'newtype' | null
    type_arguments  JSON,                      -- [["a", kind], ["b", kind]]
    comments        TEXT,
    source_span     JSON,                      -- {start: {line, col}, end: {line, col}}
    source_code     TEXT,
    UNIQUE(module_id, name)
);

CREATE TABLE IF NOT EXISTS child_declarations (
    id                  INTEGER PRIMARY KEY,
    declaration_id      INTEGER NOT NULL REFERENCES declarations(id),
    name                VARCHAR NOT NULL,      -- "Just", "Nothing", "map"
    kind                VARCHAR NOT NULL,      -- 'constructor' | 'instance' | 'class_member'
    type_signature      TEXT,
    type_ast            JSON,
    constructor_args    JSON,
    comments            TEXT,
    source_span         JSON
);

-- =============================================================================
-- CALL GRAPH LAYER - Function-level dependencies
-- =============================================================================

CREATE TABLE IF NOT EXISTS function_calls (
    id                  INTEGER PRIMARY KEY,
    caller_module_id    INTEGER NOT NULL REFERENCES modules(id),
    caller_name         VARCHAR NOT NULL,
    callee_module       VARCHAR NOT NULL,
    callee_name         VARCHAR NOT NULL,
    is_cross_module     BOOLEAN DEFAULT TRUE,
    call_count          INTEGER DEFAULT 1
);

-- =============================================================================
-- GIT LAYER - History and churn metrics
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

CREATE TABLE IF NOT EXISTS module_commits (
    module_id       INTEGER NOT NULL REFERENCES modules(id),
    commit_id       INTEGER NOT NULL REFERENCES commits(id),
    change_type     VARCHAR,                   -- 'created' | 'modified' | 'deleted'
    PRIMARY KEY (module_id, commit_id)
);

-- =============================================================================
-- METRICS LAYER - Computed for visualization
-- =============================================================================

CREATE TABLE IF NOT EXISTS module_metrics (
    module_id           INTEGER PRIMARY KEY REFERENCES modules(id),
    commit_count        INTEGER DEFAULT 0,
    days_since_modified INTEGER,
    age_in_days         INTEGER,
    author_count        INTEGER DEFAULT 0,
    authors             JSON,
    efferent_coupling   INTEGER DEFAULT 0,     -- Modules this depends on
    afferent_coupling   INTEGER DEFAULT 0,     -- Modules that depend on this
    instability         REAL                   -- Ce / (Ce + Ca)
);

CREATE TABLE IF NOT EXISTS declaration_metrics (
    declaration_id          INTEGER PRIMARY KEY REFERENCES declarations(id),
    external_call_count     INTEGER DEFAULT 0,
    external_caller_count   INTEGER DEFAULT 0,
    total_coupling          INTEGER DEFAULT 0,
    coupling_intensity      REAL
);
```

---

## Appendix B: Sample Input Files

### docs.json (from `output/Data.Unit/docs.json`)

```json
{
  "name": "Data.Unit",
  "comments": null,
  "declarations": [
    {
      "title": "Unit",
      "comments": "The `Unit` type has a single inhabitant...",
      "info": {
        "declType": "data",
        "dataDeclType": "data",
        "typeArguments": [],
        "roles": []
      },
      "sourceSpan": {
        "start": [11, 1],
        "end": [11, 33],
        "name": ".spago/p/prelude-6.0.2/src/Data/Unit.purs"
      },
      "children": [],
      "kind": null
    },
    {
      "title": "unit",
      "comments": "`unit` is the sole inhabitant of the `Unit` type.",
      "info": {
        "declType": "value",
        "type": {
          "tag": "TypeConstructor",
          "contents": [["Data", "Unit"], "Unit"],
          "annotation": []
        }
      },
      "sourceSpan": {
        "start": [14, 1],
        "end": [14, 28],
        "name": ".spago/p/prelude-6.0.2/src/Data/Unit.purs"
      },
      "children": [],
      "kind": null
    }
  ],
  "reExports": []
}
```

**Key fields:**
- `info.declType`: One of `"value"`, `"data"`, `"newtype"`, `"typeSynonym"`, `"typeClass"`, `"alias"`
- `info.type`: Type AST (for values)
- `info.dataDeclType`: `"data"` or `"newtype"` (for data declarations)
- `sourceSpan.name`: Path to source file (use to compute LOC)

### corefn.json (from `output/Data.Unit/corefn.json`)

```json
{
  "builtWith": "0.15.15",
  "moduleName": ["Data", "Unit"],
  "modulePath": ".spago/p/prelude-6.0.2/src/Data/Unit.purs",
  "imports": [
    {
      "annotation": {
        "meta": null,
        "sourceSpan": {"start": [1, 1], "end": [14, 28]}
      },
      "moduleName": ["Prim"]
    }
  ],
  "exports": ["unit"],
  "foreign": ["unit"],
  "decls": [],
  "reExports": {},
  "comments": [],
  "sourceSpan": {"start": [1, 1], "end": [14, 28]}
}
```

**Key fields:**
- `moduleName`: Array of segments (join with "." for full name)
- `imports[].moduleName`: Modules this depends on
- `exports`: Public declarations
- `foreign`: FFI declarations (have no `decls` entry)
- `decls`: Function bodies (parse for call graph)

### spago.lock (YAML, top-level keys)

```yaml
workspace:
  packages:
    my-project:
      path: .
      dependencies:
        - prelude
        - effect

  package_set:
    address:
      registry: 67.0.1

  extra_packages: {}

packages:
  prelude:
    type: registry
    version: 6.0.2
    integrity: sha256-...
    dependencies: []

  effect:
    type: registry
    version: 4.0.0
    integrity: sha256-...
    dependencies:
      - prelude
```

**Key fields:**
- `packages`: Resolved dependency versions (the build plan)
- `workspace.packages`: Local packages in monorepo
- `workspace.extra_packages`: Non-registry dependencies

---

## Appendix C: Existing Loader Reference

The current Node.js implementation is at `database/loader/ce-loader.js` (~1400 lines).

**CLI interface:**
```bash
ce-loader init [--fresh] [--db <path>]
ce-loader add-project --name <name> --repo <path>
ce-loader snapshot --project <name> [--label <label>] [--ref <git-ref>]
ce-loader load --name <name> --repo <path> [--label <label>]
ce-loader list-projects
ce-loader list-snapshots --project <name>
```

**Key functions to study:**
- `loadDocsJson()` - Parses docs.json, extracts declarations
- `loadCorefnJson()` - Parses corefn.json, extracts imports
- `extractCallGraph()` - Walks corefn AST for function calls
- `computeMetrics()` - Calculates coupling, churn
- `insertBatch()` - Bulk database inserts

**Known issues in existing loader:**
1. No parallelism - processes files sequentially
2. Full JSON parse even for incremental - no streaming
3. Git log spawns external process per commit
4. No progress reporting during long loads
5. Hardcoded paths in several places

**Files in `database/loader/`:**
- `ce-loader.js` - Main loader CLI
- `init-schema.js` - Schema initialization
- `load-from-json.js` - Legacy JSON import
- `populate-source-code.js` - Extracts source snippets
- `site-explorer-loader.js` - Route analysis loader
- `hs-loader.js` - Haskell loader (incomplete)

---

## Appendix D: Directory Structure

After running `spago build`, a PureScript project has:

```
project/
├── spago.yaml              # Package config
├── spago.lock              # Resolved dependencies (PARSE THIS)
├── src/                    # Source files
│   └── Main.purs
├── output/                 # Compiled output
│   ├── Main/
│   │   ├── docs.json       # Documentation (PARSE THIS)
│   │   ├── corefn.json     # Core functional (PARSE THIS)
│   │   ├── externs.cbor    # Compiler externs (ignore)
│   │   └── index.js        # Generated JS (ignore)
│   ├── Data.Array/
│   │   ├── docs.json
│   │   └── corefn.json
│   └── ...
└── .spago/                 # Downloaded dependencies
    └── p/
        └── prelude-6.0.2/
            ├── purs.json   # Package metadata
            └── src/
```

**Key insight:** The `output/` directory contains one subdirectory per module, named by the full module name (e.g., `Data.Array.NonEmpty`). Each has its own `docs.json` and `corefn.json`.

---

*This spec is intended to be complete enough for implementation by someone unfamiliar with the codebase. Questions welcome.*
