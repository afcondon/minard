# minard-loader M3: Full Schema Compatibility

## Problem Statement

M1/M2 created a simplified schema that is NOT compatible with the existing `unified-schema.sql`. This breaks the existing API and prevents A/B testing between old and new loaders.

Additionally, the current implementation may not be properly deduplicating shared packages across projects, resulting in a 426MB database vs 83MB for similar data.

## Requirements

### 1. Schema Compatibility

The Rust loader MUST use the exact schema from `apps/minard/database/schema/unified-schema.sql`.

**Tables that must exist:**
- `package_sets` - registry snapshots
- `package_versions` - unique (name, version) pairs
- `package_set_members` - links packages to sets
- `package_dependencies` - package-level deps
- `projects` - codebases being analyzed
- `snapshots` - point-in-time analysis
- `snapshot_packages` - links snapshots to packages
- `module_namespaces` - hierarchical namespace tree
- `modules` - with namespace_id foreign key
- `declarations` - full column set
- `child_declarations` - full column set

**Read the existing schema file** at `apps/minard/database/schema/unified-schema.sql` and implement it exactly.

### 2. Deduplication Strategy

The database must properly deduplicate shared data:

**Package versions**:
- `prelude@6.0.1` exists ONCE regardless of how many projects use it
- Use `INSERT OR IGNORE` with `UNIQUE(name, version)` constraint
- After insert, query back actual ID for foreign key references

**Modules**:
- `Data.Array` in `arrays@1.0.0` exists ONCE
- Unique constraint: `UNIQUE(package_version_id, name)`
- Critical: all projects using `arrays@1.0.0` must reference the SAME package_version_id

**Declarations**:
- Each declaration exists once per module
- Unique constraint: `UNIQUE(module_id, name)`

**Snapshot linkage**:
- `snapshot_packages` links each snapshot to the shared package_versions
- This is how we know "project X uses prelude@6.0.1" without duplicating prelude's data

### 3. Deduplication Verification

Before considering M3 complete, verify deduplication with these checks:

```sql
-- Package deduplication: should have ~100-200 unique packages, not 1700+
SELECT COUNT(*) as total, COUNT(DISTINCT name || '@' || version) as unique_pairs
FROM package_versions;

-- Module deduplication: modules per package should be reasonable
SELECT pv.name, pv.version, COUNT(m.id) as module_count
FROM package_versions pv
JOIN modules m ON m.package_version_id = pv.id
GROUP BY pv.id
ORDER BY module_count DESC
LIMIT 10;

-- No duplicate modules within same package
SELECT package_version_id, name, COUNT(*) as copies
FROM modules
GROUP BY package_version_id, name
HAVING COUNT(*) > 1;
-- Should return 0 rows
```

### 4. Module Namespaces

Implement the `module_namespaces` table:

```sql
CREATE TABLE module_namespaces (
    id        INTEGER PRIMARY KEY,
    path      VARCHAR NOT NULL UNIQUE,  -- "Data.Array"
    segment   VARCHAR NOT NULL,          -- "Array"
    parent_id INTEGER REFERENCES module_namespaces(id),
    depth     INTEGER NOT NULL,
    is_leaf   BOOLEAN DEFAULT TRUE
);
```

When inserting a module `Data.Array.NonEmpty`:
1. Ensure namespace `Data` exists (depth 0, no parent)
2. Ensure namespace `Data.Array` exists (depth 1, parent = Data)
3. Ensure namespace `Data.Array.NonEmpty` exists (depth 2, parent = Data.Array)
4. Link module to `Data.Array.NonEmpty` namespace

### 5. Database Size Target

With proper deduplication, loading 21 projects that share most dependencies should result in a database of ~80-120MB, NOT 400MB+.

The bulk of the data is shared packages (prelude, arrays, maybe, etc.). Only project-specific local packages and the linkage tables (snapshot_packages) should grow with more projects.

### 6. Testing

1. Load single project (hypo-punter), note size
2. Load second project sharing same deps, size should barely increase
3. Load all 21 projects, size should be ~2-3x single project (not 21x)

### 7. API Compatibility

After loading, these existing API endpoints must work unchanged:
- `GET /api/v2/packages` - list packages
- `GET /api/v2/modules` - list modules
- `GET /api/v2/namespaces` - list namespace tree
- `GET /api/projects` - list projects
- `GET /api/snapshots/:id` - get snapshot details

Test by running the existing server against the new database.

## Out of Scope

- Performance optimization (M4)
- Incremental loading (M4)
- Import graphs / function calls (M4+)

## Deliverables

1. Updated `src/db/schema.rs` matching unified-schema.sql exactly
2. Namespace tree population in pipeline
3. Deduplication verification queries passing
4. Database size ~100MB for full monorepo scan
5. Existing API endpoints working
