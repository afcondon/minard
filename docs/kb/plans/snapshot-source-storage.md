# Snapshot Source Storage — Enabling Full Comparison Diagrams

## Goal

Store workspace module source code in the database at load time so that the
CompareModuleViz "before" column can render all 4 diagrams (Layers, Arcs,
Declarations, **Concerns**) — not just the first 3.

Currently the Concerns diagram (case-branch call graph) requires reading
the `.purs` source file from disk. For historical snapshots the source isn't
on disk, so the "before" column shows "No case expressions to analyze".
This plan fixes that.

## Context

The multi-snapshot comparison infrastructure is complete (Phases 1-3 of
`multi-snapshot-compare.md`). Two snapshots exist in the DB:

| Snapshot | ID  | Git Hash  | Label                           | Modules |
|----------|-----|-----------|---------------------------------|---------|
| Current  | 213 | `8a95408` | feature/structural-decomp-viz   | 875     |
| Before   | 271 | `6ce60e0` | main-pre-refactor               | 862     |

The comparison page works end-to-end but the "before" column's Concerns
panel is blank because `computeColumnFromSnapshot` skips source analysis.

### What Changed Between Snapshots (the refactoring we want to visualize)

- `SceneCoordinator.purs`: `canonicalStateCode` and `themeForScene`
  extracted to `SceneCoordinator/Pure.purs` (commit `07619a7`)
- `Loader.purs`: pure model transformations extracted to
  `Loader/Transform.purs` (commit `8a95408`)
- These are visible in the Concerns diagram as removed/reduced clusters

### Key Insight

The Rust loader **already reads** the full source file into memory
(`pipeline.rs:748-756`) for every workspace module. It just doesn't store
it. The fix is to carry it through to the DB.

## Implementation Plan

### Step 1: Schema — Add `source_text` column to `modules`

**File**: `minard-loader/src/db/unified-schema.sql` (reference only —
schema is created in Rust)

**Change**: Add `source_text TEXT` column to the `modules` table.

The loader creates the schema via `CREATE TABLE IF NOT EXISTS`. Since the
table likely already exists, we need an `ALTER TABLE` migration. Add this
to the loader's schema setup:

```sql
ALTER TABLE modules ADD COLUMN IF NOT EXISTS source_text TEXT;
```

DuckDB supports `IF NOT EXISTS` on ALTER TABLE ADD COLUMN.

**Where to add it**: In `insert.rs`, find the `ensure_schema` or table
creation function. Add the ALTER after the CREATE TABLE.

### Step 2: Rust Model — Add `source_text` to `Module` struct

**File**: `minard-loader/src/model/entities.rs:104-112`

```rust
pub struct Module {
    pub id: i64,
    pub package_version_id: i64,
    pub namespace_id: Option<i64>,
    pub name: String,
    pub path: Option<String>,
    pub comments: Option<String>,
    pub loc: Option<i32>,
    pub source_text: Option<String>,  // NEW: full .purs source
}
```

**Impact**: Every place that constructs a `Module` must provide `source_text`.
Search for `Module {` in `pipeline.rs` to find all construction sites.

### Step 3: Loader Pipeline — Capture source text

**File**: `minard-loader/src/loader/pipeline.rs`

The source is already read at line 748-756 as `source_lines: Option<Vec<String>>`.
We need to join it back and attach it to the `Module` in the `ParsedModule`.

Find where `ParsedModule` is constructed (search `ParsedModule {` in
pipeline.rs). The `module` field is built from `Module { ... }`. Add:

```rust
source_text: source_lines.as_ref().map(|lines| lines.join("\n")),
```

**Important**: Only store source for workspace modules. Registry/dependency
modules don't need source stored (they'd bloat the DB). The pipeline already
distinguishes these — workspace modules go through the `docs_json_files`
parsing path. Check the `source` field: only set `source_text` when the
package source is `"workspace"` or `"local"`.

### Step 4: DB Insert — Include `source_text` in appender

**File**: `minard-loader/src/db/insert.rs:663-681`

Update `append_modules` to include the new column:

```rust
appender.append_row(params![
    module.id,
    module.package_version_id,
    module.namespace_id,
    module.name,
    module.path,
    module.comments,
    module.loc,
    module.source_text,  // NEW
])?;
```

### Step 5: Build and test the loader

```bash
cd minard-loader && cargo build --release
```

Fix any compilation errors from the new `source_text` field (every `Module {}`
constructor needs it — use `source_text: None` for registry/dependency modules).

### Step 6: Reload BOTH snapshots

**This is the critical multi-step sequence. Follow it exactly.**

The DB currently has snapshots loaded WITHOUT source_text. We need to
reload both to populate the new column.

#### 6a. Copy the new loader binary

```bash
cp minard-loader/target/release/minard-loader /tmp/minard-loader-with-source
```

#### 6b. Stop the API server

```bash
lsof -ti :3000 | xargs kill
```

#### 6c. Run ALTER TABLE migration

```bash
python3 -c "
import duckdb
db = duckdb.connect('database/ce-unified.duckdb')
db.execute('ALTER TABLE modules ADD COLUMN IF NOT EXISTS source_text TEXT')
db.close()
"
```

#### 6d. Delete the two minard snapshots (they'll be reloaded)

```bash
python3 -c "
import duckdb
db = duckdb.connect('database/ce-unified.duckdb')
for sid in [213, 271]:
    db.execute('DELETE FROM snapshot_packages WHERE snapshot_id = ?', [sid])
    db.execute('DELETE FROM snapshots WHERE id = ?', [sid])
    print(f'Deleted snapshot {sid}')
db.close()
"
```

#### 6e. Reload the pre-refactor snapshot from main

```bash
# Commit any uncommitted work first!
git stash  # if needed

git checkout main

# Clean stale output from feature branch modules
rm -rf output/CE2.Data.Loader.Transform
rm -rf output/CE2.Component.SceneCoordinator.Pure

spago build -p minard-frontend

/tmp/minard-loader-with-source load \
  --database /Users/afc/work/afc-work/CodeExplorer/minard/database/ce-unified.duckdb \
  --scan /Users/afc/work/afc-work \
  --label "main-pre-refactor" -v
```

#### 6f. Reload the feature branch snapshot

```bash
git checkout feature/structural-decomp-viz
git stash pop  # if stashed

spago build -p minard-frontend

/tmp/minard-loader-with-source load \
  --database /Users/afc/work/afc-work/CodeExplorer/minard/database/ce-unified.duckdb \
  --scan /Users/afc/work/afc-work \
  --label "feature-branch" -v
```

#### 6g. Verify source_text was stored

```bash
python3 -c "
import duckdb
db = duckdb.connect('database/ce-unified.duckdb', read_only=True)
for r in db.execute('''
  SELECT s.id, s.label, m.name, LENGTH(m.source_text) as src_len
  FROM snapshots s
  JOIN snapshot_packages sp ON sp.snapshot_id = s.id
  JOIN modules m ON m.package_version_id = sp.package_version_id
  WHERE s.project_id = (SELECT MIN(id) FROM projects)
    AND m.name = 'CE2.Component.SceneCoordinator'
  ORDER BY s.id
''').fetchall():
    print(f'Snapshot #{r[0]} ({r[1]}): {r[2]} source_text={r[3]} bytes')
db.close()
"
```

Expected: both snapshots have non-null source_text for SceneCoordinator.

### Step 7: Server API — Snapshot-aware source endpoint

**Files**: `server/src/API/Unified.purs`, `server/src/API/Unified.js`,
`server/src/Main.purs`

#### 7a. Add route

In `Main.purs`, the existing `V2GetModuleSource` route reads from disk.
Add a new pattern: when `?snapshot=N` is present, read from DB instead.

In the router's `V2GetModuleSource` case:

```purescript
V2GetModuleSource ->
  case Object.lookup "module" query of
    Just moduleName -> case mSnapshot of
      Just snapId -> Unified.getModuleSourceFromSnapshot db moduleName snapId
      Nothing -> Unified.getModuleSource db moduleName
    Nothing -> ok "{ \"error\": \"module query param required\" }"
```

#### 7b. Add `getModuleSourceFromSnapshot` function

In `Unified.purs`:

```purescript
getModuleSourceFromSnapshot :: Database -> String -> Int -> Aff Response
getModuleSourceFromSnapshot db moduleName snapshotId = do
  rows <- queryAllParams db """
    SELECT m.source_text, m.path
    FROM modules m
    JOIN snapshot_packages sp ON sp.package_version_id = m.package_version_id
    WHERE m.name = ? AND sp.snapshot_id = ?
      AND m.source_text IS NOT NULL
    LIMIT 1
  """ [unsafeToForeign moduleName, unsafeToForeign snapshotId]
  case firstRow rows of
    Just row -> do
      let json = buildSnapshotSourceJson row
      ok' jsonHeaders json
    Nothing -> notFound
```

In `Unified.js`:

```javascript
export const buildSnapshotSourceJson = (row) => {
  return JSON.stringify({
    source: row.source_text || '',
    path: row.path || ''
  });
};
```

### Step 8: Frontend — Snapshot-aware source fetching

**File**: `frontend/src/Data/Loader.purs`

Add:

```purescript
fetchModuleSourceForSnapshot :: String -> Int -> Aff (Either String ModuleSource)
fetchModuleSourceForSnapshot moduleName snapshotId = do
  result <- fetchJson (apiBaseUrl <> "/api/v2/module-source?module="
    <> moduleName <> "&snapshot=" <> show snapshotId)
  pure $ result >>= \json -> decodeJson json # mapLeft printJsonDecodeError
```

Export it from the module.

### Step 9: Frontend — Enable concern analysis for "before" column

**File**: `frontend/src/Component/CompareModuleViz.purs`

In `computeColumnFromSnapshot` (around line 502-519), replace the skip:

```purescript
-- Currently:
-- Skip concern analysis for before-snapshot (source not on disk)
pure { ..., subDeclAnalysis: Nothing, subDeclGraph: Nothing }

-- Replace with:
{ mAnalysis, mSubDeclGraph } <- do
  result <- liftAff $ Loader.fetchModuleSourceForSnapshot modName snapshotId
  case result of
    Left _ -> pure { mAnalysis: Nothing, mSubDeclGraph: Nothing }
    Right src -> do
      let analysis = SDA.analyzeModuleSource src.source
      let { declarations: subDecls, internalCalls: subCalls } = SDA.branchesToDeclGraph analysis.allBranches
      -- ... same graph construction as computeColumn ...
      pure { mAnalysis: Just analysis, mSubDeclGraph: Just subGraph }

pure { ..., subDeclAnalysis: mAnalysis, subDeclGraph: mSubDeclGraph }
```

Copy the graph-building logic from `computeColumn` lines 418-427.

### Step 10: Build, bundle, restart, test

```bash
# Server
spago build -p minard-server

# Frontend
spago build -p minard-frontend
spago bundle -p minard-frontend

# Bump cache buster in frontend/public/index.html

# Restart
lsof -ti :3000 | xargs kill
cd minard && node server/run.js &

# Test
# Navigate to SceneCoordinator signature map → "Compare snapshots"
# BEFORE column should now show Concerns diagram with canonicalStateCode
# and themeForScene clusters
# AFTER column should show those clusters absent (extracted to Pure)
```

## Risk Mitigation

### DB size
Workspace module source is typically 50-200 KB per module, with ~70
workspace modules per project. Total: ~5-15 MB per snapshot. Acceptable.

### Column ordering in DuckDB appender
DuckDB appenders are column-order sensitive. After ALTER TABLE ADD COLUMN,
the new column is last. Ensure `append_modules` puts `source_text` last
in the params list.

### Registry modules
Do NOT store source for registry modules (~800 per project). Set
`source_text: None` for non-workspace packages. The loader already
distinguishes these.

### Stale output/ directories
When checking out main, remove output directories for modules that
don't exist on main (e.g., `CE2.Data.Loader.Transform`,
`CE2.Component.SceneCoordinator.Pure`). These are left over from
incremental compilation.

## Files Modified (Summary)

| File | Change |
|------|--------|
| `minard-loader/src/model/entities.rs` | Add `source_text: Option<String>` to `Module` |
| `minard-loader/src/loader/pipeline.rs` | Capture joined source lines into Module |
| `minard-loader/src/db/insert.rs` | Add column to appender, add ALTER TABLE migration |
| `server/src/API/Unified.purs` | Add `getModuleSourceFromSnapshot` |
| `server/src/API/Unified.js` | Add `buildSnapshotSourceJson` |
| `server/src/Main.purs` | Route snapshot param to new function |
| `frontend/src/Data/Loader.purs` | Add `fetchModuleSourceForSnapshot` |
| `frontend/src/Component/CompareModuleViz.purs` | Enable concern analysis for before column |

## Verification Checklist

- [ ] `cargo build --release` succeeds
- [ ] Loader populates `source_text` for workspace modules only
- [ ] Both snapshots have source_text for SceneCoordinator and Loader
- [ ] `GET /api/v2/module-source?module=CE2.Component.SceneCoordinator&snapshot=271` returns source
- [ ] `spago build -p minard-server` and `spago build -p minard-frontend` succeed
- [ ] Compare view shows Concerns diagram for BOTH columns
- [ ] SceneCoordinator before shows canonicalStateCode/themeForScene clusters
- [ ] SceneCoordinator after shows those clusters absent
