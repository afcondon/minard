# Code Explorer Database Schema

## Overview

The unified schema (v3.0) provides a consistent data model for PureScript project analysis. It supports:

- **Package set browsing**: Explore the entire PureScript registry
- **Project dependency analysis**: Understand what your project depends on
- **Type-based search**: Find functions by signature (Pursuit-style)
- **Namespace navigation**: Browse the `Data.*`, `Control.*` hierarchy
- **Code metrics**: Coupling, churn, authorship

## Design Principles

### 1. Package Versions as Core Identity

A "package" without a version is meaningless. The entity is `prelude@6.0.2`, not `prelude`.

```sql
-- Correct: version is part of identity
SELECT * FROM package_versions WHERE name = 'prelude' AND version = '6.0.2';

-- The same package name can have many versions
SELECT version FROM package_versions WHERE name = 'prelude';
-- → 6.0.0, 6.0.1, 6.0.2, ...
```

### 2. spago.lock is the Source of Truth

For dependency resolution, we trust `spago.lock`'s `build_plan`, not derived analysis.

```
spago.yaml          → User's intent (version ranges)
spago.lock          → Resolved reality (exact versions) ← WE USE THIS
module imports      → Runtime dependencies (subset of build_plan)
```

### 3. Types are First-Class

Type signatures are stored both as rendered text (for display) and as AST JSON (for search):

```sql
SELECT name, type_signature FROM declarations
WHERE type_signature LIKE '%Array a -> Maybe a%';

-- For sophisticated type search, query the AST:
SELECT name, type_ast FROM declarations
WHERE json_extract(type_ast, '$.tag') = 'TypeApp';
```

### 4. Namespaces are Independent of Packages

The module `Data.Array` exists as a namespace node regardless of which package defines it:

```sql
-- Find all packages that contribute to Data.*
SELECT DISTINCT pv.name, COUNT(*) as modules
FROM modules m
JOIN module_namespaces ns ON m.namespace_id = ns.id
JOIN package_versions pv ON m.package_version_id = pv.id
WHERE ns.path LIKE 'Data.%'
GROUP BY pv.name;
```

## Entity Relationships

```
PackageSet (71.0.0)
    │
    ├──contains──▶ PackageVersion (prelude@6.0.2)
    │                     │
    │                     ├──contains──▶ Module (Prelude)
    │                     │                   │
    │                     │                   ├──contains──▶ Declaration (id)
    │                     │                   │                   │
    │                     │                   │                   └──has──▶ ChildDeclaration
    │                     │                   │
    │                     │                   └──imports──▶ Module
    │                     │
    │                     └──depends──▶ PackageVersion
    │
Project (ce2-website)
    │
    └──has──▶ Snapshot (abc123)
                  │
                  └──uses──▶ PackageVersion (via snapshot_packages)
```

## Data Sources

| Table | Primary Source | Secondary Sources |
|-------|---------------|-------------------|
| `package_sets` | Registry index | - |
| `package_versions` | Registry index | purs.json (metadata) |
| `package_set_members` | Registry index | - |
| `package_dependencies` | Registry manifests | - |
| `projects` | User input | - |
| `snapshots` | Git | - |
| `snapshot_packages` | **spago.lock** | - |
| `module_namespaces` | Derived from module names | - |
| `modules` | docs.json, corefn.json | spago graph modules |
| `module_imports` | corefn.json | - |
| `declarations` | docs.json | - |
| `child_declarations` | docs.json | - |
| `function_calls` | corefn.json analysis | - |
| `commits` | git log | - |
| `module_metrics` | Computed from commits, imports | - |

## Key Tables

### package_versions

The core identity table. Every package version gets a unique ID.

```sql
CREATE TABLE package_versions (
    id          INTEGER PRIMARY KEY,
    name        VARCHAR NOT NULL,      -- "prelude"
    version     VARCHAR NOT NULL,      -- "6.0.2"
    description TEXT,
    license     VARCHAR,
    repository  VARCHAR,
    source      VARCHAR,               -- "registry" | "local" | "git"
    UNIQUE(name, version)
);
```

### snapshot_packages

Links a project snapshot to its resolved dependencies. The `source` field distinguishes:

- `registry`: Package from the package set
- `workspace`: Local package in the spago workspace
- `extra`: Package from extraPackages (not in base set)

```sql
CREATE TABLE snapshot_packages (
    snapshot_id         INTEGER REFERENCES snapshots(id),
    package_version_id  INTEGER REFERENCES package_versions(id),
    source              VARCHAR NOT NULL,  -- 'registry' | 'workspace' | 'extra'
    is_direct           BOOLEAN,
    PRIMARY KEY (snapshot_id, package_version_id)
);
```

### declarations

Stores all top-level declarations with full type information.

```sql
CREATE TABLE declarations (
    id              INTEGER PRIMARY KEY,
    module_id       INTEGER REFERENCES modules(id),
    name            VARCHAR NOT NULL,
    kind            VARCHAR NOT NULL,      -- 'value' | 'data' | 'newtype' | 'type_class' | ...

    -- THE key fields for PureScript tooling
    type_signature  TEXT,                  -- "forall a. (a -> Boolean) -> Array a -> Array a"
    type_ast        JSON,                  -- Structured AST for type search

    -- Kind-specific fields
    type_arguments  JSON,                  -- For data/class
    superclasses    JSON,                  -- For type classes
    fundeps         JSON,                  -- Functional dependencies

    comments        TEXT,
    source_span     JSON,
    UNIQUE(module_id, name)
);
```

### module_namespaces

The implicit hierarchy of module names, independent of packages.

```sql
-- "Data.Array.NonEmpty" creates three nodes:
-- 1. Data (depth=0, parent=NULL)
-- 2. Data.Array (depth=1, parent=1)
-- 3. Data.Array.NonEmpty (depth=2, parent=2)

CREATE TABLE module_namespaces (
    id          INTEGER PRIMARY KEY,
    path        VARCHAR NOT NULL UNIQUE,   -- "Data.Array"
    segment     VARCHAR NOT NULL,          -- "Array"
    parent_id   INTEGER REFERENCES module_namespaces(id),
    depth       INTEGER NOT NULL
);
```

## Useful Views

### namespace_stats

Aggregate statistics per namespace node:

```sql
SELECT path, module_count, package_count, declaration_count
FROM namespace_stats
WHERE path LIKE 'Data.%'
ORDER BY module_count DESC;
```

### snapshot_package_summary

Package information for a specific snapshot:

```sql
SELECT name, version, source, module_count, declaration_count
FROM snapshot_package_summary
WHERE snapshot_id = 1
ORDER BY source, name;
```

### declaration_details

Full context for search results:

```sql
SELECT declaration_name, type_signature, module_name, package_name
FROM declaration_details
WHERE type_signature LIKE '%Functor%';
```

### type_class_instances

Find all instances of a type class:

```sql
SELECT instance_type, module_name, package_name
FROM type_class_instances
WHERE class_name = 'Functor';
```

## Schema Version History

| Version | Date | Changes |
|---------|------|---------|
| 3.0 | 2026-01-22 | Unified schema with namespaces, full type AST |
| 2.0 | 2026-01-15 | Multi-project support, snapshots |
| 1.0 | 2025-12-01 | Initial schema |

## Migration

Version 3.0 is a breaking change. To migrate:

```bash
# Backup existing data if needed
cp ce-data.duckdb ce-data.duckdb.backup

# Initialize fresh database
ce-loader init --fresh

# Reload package set
ce-loader package-set load --version 71.0.0

# Re-snapshot projects
ce-loader snapshot create --project myproject
```
