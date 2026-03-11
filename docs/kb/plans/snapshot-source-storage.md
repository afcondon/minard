# Worktree-Based Snapshot Comparison — Enabling Full Comparison Diagrams

## Goal

Enable CompareModuleViz to render all 4 diagrams (Layers, Arcs, Declarations,
**Concerns**) for both "before" and "after" columns by keeping source code on
disk via git worktrees rather than storing it in the database.

Currently the Concerns diagram (case-branch call graph) requires reading the
`.purs` source file from disk. For historical snapshots the source isn't on
disk, so the "before" column shows "No case expressions to analyze".

## Approach: Git Worktrees Instead of DB Source Storage

~~Previous plan~~ (superseded): store `source_text` in the `modules` table.
That required Rust loader changes, schema migration, and re-loading snapshots.

**New approach**: use `git worktree add` to maintain multiple checkouts
simultaneously on disk. Each worktree has its own `output/` with `docs.json`
files AND the original `.purs` source files. The loader already discovers
projects by path and stores `repo_path` in the `projects` table. The existing
`getModuleSource` endpoint already resolves source files via
`projects.repo_path`. We just need to make it snapshot-aware.

### Why this is better

1. **No Rust/loader/schema changes** — zero changes to the loader or DB schema
2. **Source is always available** — worktrees are real checkouts with real files
3. **Generalizes naturally** — the tool already supports multiple projects; worktrees
   are just more projects at different paths, each loaded with a label
4. **No DB bloat** — source stays on disk where it belongs
5. **Simpler mental model** — a "snapshot" is a loaded worktree, not an abstract
   DB-only construct

## Context

The multi-snapshot comparison infrastructure is complete (Phases 1-3 of
`multi-snapshot-compare.md`). Two snapshots currently exist in the DB:

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

## How the Existing Plumbing Works

Understanding these three mechanisms is key to seeing why the worktree
approach requires minimal code changes:

### 1. Loader project discovery (`discovery.rs:65-104`)

The loader's `--scan` flag walks the directory tree looking for
`spago.lock` + `output/docs.json`. The **project name** is derived from
the path relative to the scan root:

```
scan root:    /Users/afc/work/afc-work
project path: /Users/afc/work/afc-work/CodeExplorer/minard
→ project name: "CodeExplorer/minard"
```

A worktree at a different path gets a **different project name** automatically:

```
scan root:    /Users/afc/work/afc-work
worktree:     /Users/afc/work/afc-work/CodeExplorer/minard-main
→ project name: "CodeExplorer/minard-main"
```

### 2. Project `repo_path` storage (`insert.rs:36-39`, `schema.rs:59-67`)

```sql
CREATE TABLE projects (
    id INTEGER PRIMARY KEY,
    name VARCHAR NOT NULL UNIQUE,
    repo_path VARCHAR,
    ...
);
```

The `repo_path` is the absolute path to the project root on disk. The
`getModuleSource` endpoint uses this to find source files.

### 3. Source file resolution (`Unified.js:821-856`)

`buildModuleSourceJson` resolves a source file path via:

```
fullPath = resolve(projectRoot, row.repo_path, sourceSpan.name)
```

Where `sourceSpan.name` comes from declarations in the DB and `repo_path`
comes from the projects table. For a worktree, `repo_path` will point to
the worktree directory, and the source files will be there.

**The chain**: `snapshot → project → repo_path` + `declaration → source_span`
→ full file path → `readFileSync`.

## Implementation Plan

### Step 1: Create git worktree for `main`

```bash
cd /Users/afc/work/afc-work/CodeExplorer/minard

# Commit any uncommitted work first
git add -A && git commit -m "WIP"

# Create worktree — this creates a full checkout of main at a sibling path
git worktree add ../minard-main main
```

This gives us:
- `CodeExplorer/minard/` — feature branch (current)
- `CodeExplorer/minard-main/` — main branch

Both directories have their own `.purs` source files and can have their
own `output/` directory after building.

### Step 2: Build PureScript in the worktree

```bash
cd /Users/afc/work/afc-work/CodeExplorer/minard-main
spago build -p minard-frontend
```

This populates `minard-main/output/` with `docs.json` files. The `.purs`
source files are already present from the checkout.

### Step 3: Delete the existing pre-refactor snapshot

The current snapshot #271 was loaded from the same project path (same
project name). We need to delete it so the worktree load creates a new
project entry with the correct `repo_path`.

```bash
cd /Users/afc/work/afc-work/CodeExplorer/minard

# Stop API server (DuckDB exclusive lock)
lsof -ti :3000 | xargs kill

python3 -c "
import duckdb
db = duckdb.connect('database/ce-unified.duckdb')
# Find and delete the pre-refactor snapshot
for r in db.execute('''
    SELECT id, label FROM snapshots
    WHERE label LIKE '%pre-refactor%' OR label LIKE '%main%'
''').fetchall():
    sid = r[0]
    db.execute('DELETE FROM snapshot_packages WHERE snapshot_id = ?', [sid])
    db.execute('DELETE FROM snapshots WHERE id = ?', [sid])
    print(f'Deleted snapshot {sid} ({r[1]})')
db.close()
"
```

### Step 4: Load the worktree as a separate project

```bash
cd /Users/afc/work/afc-work/CodeExplorer/minard

# The loader scans from the afc-work root, so it will discover:
# - CodeExplorer/minard       (existing project, feature branch)
# - CodeExplorer/minard-main  (new project, main branch)

minard-loader/target/release/minard-loader load \
  --database /Users/afc/work/afc-work/CodeExplorer/minard/database/ce-unified.duckdb \
  --scan /Users/afc/work/afc-work \
  --label "main-pre-refactor" -v
```

**What happens**: The loader discovers `minard-main/` as a separate project
(name: `CodeExplorer/minard-main`). It creates a new `projects` row with
`repo_path` pointing to the worktree. Modules are loaded under a new
`package_version_id` tied to the new project's snapshot.

**Important**: This will also reload the feature branch project. That's fine —
the loader handles re-loads via `delete_snapshot_for_commit`.

### Step 5: Verify the load

```bash
python3 -c "
import duckdb
db = duckdb.connect('database/ce-unified.duckdb', read_only=True)
print('=== Projects ===')
for r in db.execute('SELECT id, name, repo_path FROM projects').fetchall():
    print(f'  #{r[0]}: {r[1]} → {r[2]}')
print()
print('=== Snapshots ===')
for r in db.execute('''
    SELECT s.id, s.project_id, s.label, s.git_hash,
           COUNT(DISTINCT sp.package_version_id) as pkg_count
    FROM snapshots s
    LEFT JOIN snapshot_packages sp ON sp.snapshot_id = s.id
    GROUP BY s.id, s.project_id, s.label, s.git_hash
    ORDER BY s.id
''').fetchall():
    print(f'  #{r[0]} (project {r[1]}): {r[2]} [{r[3][:7] if r[3] else \"?\"}] — {r[4]} packages')
db.close()
"
```

Expected: two projects, two snapshots, each with ~30+ packages and
different `repo_path` values.

### Step 6: Server — Make `getModuleSource` snapshot-aware

**File**: `server/src/API/Unified.purs` (lines 1114-1135)

The current SQL doesn't filter by snapshot, so it picks an arbitrary
declaration row. Add a snapshot parameter:

```purescript
-- EXISTING (unchanged — used when no snapshot param)
getModuleSource :: Database -> String -> Aff Response

-- NEW: snapshot-specific version
getModuleSourceFromSnapshot :: Database -> String -> Int -> Aff Response
getModuleSourceFromSnapshot db moduleName snapshotId = do
  rows <- queryAllParams db """
    SELECT d.source_span, pr.repo_path
    FROM declarations d
    JOIN modules m ON d.module_id = m.id
    JOIN package_versions pv ON m.package_version_id = pv.id
    JOIN snapshot_packages sp ON sp.package_version_id = pv.id
    JOIN snapshots s ON s.id = sp.snapshot_id
    JOIN projects pr ON pr.id = s.project_id
    WHERE m.name = ? AND sp.snapshot_id = ?
      AND d.source_span IS NOT NULL
    LIMIT 1
  """ [unsafeToForeign moduleName, unsafeToForeign snapshotId]
  case firstRow rows of
    Nothing -> notFound
    Just row -> do
      json <- liftEffect $ buildModuleSourceJson row
      case toMaybe json of
        Nothing -> notFound
        Just j -> ok' jsonHeaders j
```

The key change is `AND sp.snapshot_id = ?` — this ensures we resolve
the source file from the **correct project's** `repo_path` (i.e., the
worktree directory for the "before" snapshot, the main directory for
the "after" snapshot).

**File**: `server/src/Main.purs` (lines 247-250)

Route the snapshot param:

```purescript
V2GetModuleSource ->
  case Object.lookup "module" query of
    Just moduleName -> case Object.lookup "snapshot" query >>= Int.fromString of
      Just snapId -> Unified.getModuleSourceFromSnapshot db moduleName snapId
      Nothing -> Unified.getModuleSource db moduleName
    Nothing -> ok "{ \"error\": \"module query param required\" }"
```

Note: we use the **explicit** `snapshot` query param here, NOT the
`mSnapshot` default. The default snapshot is for listing endpoints;
source fetching should be explicit about which snapshot it wants.

### Step 7: Frontend — Snapshot-aware source fetching

**File**: `frontend/src/Data/Loader.purs` (near line 1584)

Add alongside existing `fetchModuleSource`:

```purescript
-- | Fetch module source from a specific snapshot (worktree on disk)
fetchModuleSourceForSnapshot :: String -> Int -> Aff (Either String ModuleSource)
fetchModuleSourceForSnapshot moduleName snapshotId = do
  result <- fetchJson (apiBaseUrl <> "/api/v2/module-source?module="
    <> moduleName <> "&snapshot=" <> show snapshotId)
  pure $ result >>= \json -> decodeJson json # mapLeft printJsonDecodeError
```

Export it from the module.

### Step 8: Frontend — Enable concern analysis for "before" column

**File**: `frontend/src/Component/CompareModuleViz.purs` (lines 449-515)

In `computeColumnFromSnapshot`, replace the skip at the end:

```purescript
  -- REMOVE: Skip concern analysis for before-snapshot (source not on disk)
  -- REPLACE WITH: Fetch source from snapshot-specific worktree path

  { subDeclAnalysis: mAnalysis, subDeclGraph: mSubGraph } <- do
    srcResult <- liftAff $ Loader.fetchModuleSourceForSnapshot modName snapshotId
    case srcResult of
      Left _ -> pure { subDeclAnalysis: Nothing, subDeclGraph: Nothing }
      Right src -> do
        let analysis = SDA.analyzeModuleSource src.source
        let { declarations: subDecls, internalCalls: subCalls } =
              SDA.branchesToDeclGraph analysis.allBranches
        let subNodes = Array.fromFoldable $ foldl
              (\acc d -> Set.insert d.name acc) Set.empty subDecls
        let subEdges = foldl (\acc c ->
              Map.alter (Just <<< Set.insert c.calleeName <<< fromMaybe Set.empty)
                c.callerName acc) Map.empty subCalls
        pure
          { subDeclAnalysis: Just analysis
          , subDeclGraph: Just { nodes: subNodes, edges: subEdges }
          }

  pure
    { moduleName: modName
    , packageName: pkgName
    , decls
    , calls: functionCalls
    , layerLayout: mLayerLayout
    , arcLayout: mArcLayout
    , declGraph: Just graph
    , declDecomp: decomp
    , subDeclAnalysis: mAnalysis
    , subDeclGraph: mSubGraph
    }
```

The graph-building logic mirrors `computeColumn` (lines 418-427).

### Step 9: Build, bundle, restart, test

```bash
cd /Users/afc/work/afc-work/CodeExplorer/minard

# Server
spago build -p minard-server

# Frontend
spago build -p minard-frontend
spago bundle -p minard-frontend

# Bump cache buster in frontend/public/index.html

# Restart API
lsof -ti :3000 | xargs kill
node server/run.js &

# Test
# Navigate to SceneCoordinator signature map → "Compare snapshots"
# BEFORE column should now show Concerns diagram with canonicalStateCode
# and themeForScene clusters
# AFTER column should show those clusters absent (extracted to Pure)
```

### Step 10: Clean up

- Remove debug logging from `CompareModuleViz.purs` (added during Phase 4
  debugging)
- Optionally remove the git worktree when no longer needed:
  `git worktree remove ../minard-main`

## Snapshot Selection UX

The compare flow currently picks the snapshot with the fewest modules as
"before" (`SceneCoordinator.purs`). With worktrees this heuristic still
works, but we should verify that the snapshot list endpoint returns
snapshots across projects (not just the current project). The compare-with
dropdown should show labels like "main-pre-refactor" to make it clear.

If the `listSnapshots` endpoint is project-scoped (it takes `?project=N`),
we may need to either:
- Call it without a project filter when populating the compare dropdown, or
- Add a `listAllSnapshots` variant

Check this during Step 9 testing.

## Future: Multiple Projects and Worktrees

This approach generalizes naturally:

- **Multiple projects**: The loader already handles this — different scan
  paths produce different project names. The tool can compare modules
  across entirely different codebases.
- **Multiple worktrees of the same project**: Each worktree at a different
  path becomes a separate project in the DB with its own `repo_path`.
  Labels distinguish them ("main", "feature-X", "v2.0-release").
- **Temporal comparison**: Create worktrees at tagged releases to compare
  how a module evolved across versions.

The key invariant: **every snapshot has source on disk** via its project's
`repo_path`. No special source-storage mechanism needed.

## Files Modified (Summary)

| File | Change |
|------|--------|
| `server/src/API/Unified.purs` | Add `getModuleSourceFromSnapshot` |
| `server/src/Main.purs` | Route explicit `snapshot` param to new function |
| `frontend/src/Data/Loader.purs` | Add `fetchModuleSourceForSnapshot` |
| `frontend/src/Component/CompareModuleViz.purs` | Enable concern analysis in `computeColumnFromSnapshot` |

No Rust/loader/schema changes required.

## Verification Checklist

- [ ] Git worktree created at `../minard-main` with main checkout
- [ ] `spago build -p minard-frontend` succeeds in worktree
- [ ] Loader discovers both `minard/` and `minard-main/` as separate projects
- [ ] DB has two projects with different `repo_path` values
- [ ] DB has two snapshots, one per project
- [ ] `GET /api/v2/module-source?module=CE2.Component.SceneCoordinator&snapshot=<before-id>` returns source from worktree
- [ ] `GET /api/v2/module-source?module=CE2.Component.SceneCoordinator&snapshot=<after-id>` returns source from feature branch
- [ ] `spago build -p minard-server` and `spago build -p minard-frontend` succeed
- [ ] Compare view shows Concerns diagram for BOTH columns
- [ ] SceneCoordinator before shows canonicalStateCode/themeForScene clusters
- [ ] SceneCoordinator after shows those clusters absent
