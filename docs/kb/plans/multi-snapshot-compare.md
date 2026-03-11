# Multi-Snapshot Module Comparison

Compare the same module across two snapshots (e.g. before/after refactoring),
showing all 4 diagram types (Layers, Arcs, Declarations, Concerns) side by side.

## Current State

- `CompareModuleViz.purs` exists with a 2×4 grid layout — this is reusable
- `CompareModules` scene variant wired through Scene.purs and SceneCoordinator
- DB schema already supports multiple snapshots per project (snapshots table)
- BUT the Rust loader deletes old snapshots on every load
- AND workspace packages all get version "0.0.0", so old module data is destroyed

## The Problem (3 blockers)

### Blocker 1: Loader deletes old snapshots
`minard-loader/src/db/insert.rs:513-536` — `delete_old_snapshots()` removes all
previous snapshots for a project before inserting the new one. Called from
`pipeline.rs:141-151`.

**Fix**: Comment out the `delete_old_snapshots` call, or add a `--keep N` flag.

### Blocker 2: Workspace package version identity
Workspace packages all get version `"0.0.0"` with source `"workspace"`. When the
loader runs again, `delete_package_module_data` wipes and re-inserts modules for
that package_version. Old module/declaration data is destroyed.

**Fix**: Incorporate git hash into workspace package version strings, e.g.
`"0.0.0-abc1234"` instead of `"0.0.0"`. This gives each commit's workspace code
a distinct `package_version` row, so old modules survive.

### Blocker 3: API endpoints don't filter by snapshot
All `/api/v2/*` endpoints implicitly use "latest snapshot" or no snapshot filter
at all. With multiple snapshots, they'd return the union of all versions.

**Fix**: Add optional `?snapshot=N` query parameter. When absent, use latest
snapshot (backward compatible). When present, scope to that snapshot's packages.

## Implementation Plan

### Phase 1: Rust Loader (keep multiple snapshots)

**Files**: `minard-loader/src/loader/pipeline.rs`, `minard-loader/src/db/insert.rs`

1. Comment out or gate `delete_old_snapshots()` call in `pipeline.rs`
2. Change workspace package version from `"0.0.0"` to `"0.0.0-{git_hash_short}"`
   - Find where workspace package versions are set (likely in the spago.lock parser)
   - The git hash is already available in the pipeline (it's stored in the snapshot)
3. Handle the `UNIQUE(project_id, git_hash)` constraint — different commits are
   fine; same commit re-load should upsert or skip
4. `cargo build --release`

### Phase 2: Server API (snapshot-scoped queries)

**Files**: `server/src/API/Unified.purs`, `server/src/API/Unified.js`, `server/src/Main.purs`

New endpoint:
- `GET /api/v2/projects/:id/snapshots` — list snapshots with labels/dates/stats

Modified endpoints (add optional `?snapshot=N`):
- `GET /api/v2/packages` — filter snapshot_packages by snapshot_id
- `GET /api/v2/modules` — same
- `GET /api/v2/all-imports` — scope to snapshot's modules
- `GET /api/v2/all-calls` — scope to snapshot's modules
- `GET /api/v2/module-declarations/:id` — already by module ID, OK if module IDs
  are snapshot-distinct (they will be with git-hash versioning)
- `GET /api/v2/module-calls/:id` — same

Key insight: with git-hash-versioned workspace packages, each snapshot's modules
have different IDs. So module-by-ID endpoints are already snapshot-scoped. The
bulk listing endpoints (`/modules`, `/packages`, `/all-imports`, `/all-calls`)
need the snapshot filter.

### Phase 3: Frontend (snapshot picker + compare wiring)

**Files**: `Loader.purs`, `CompareModuleViz.purs`, `SceneCoordinator.purs`,
`ModuleSignatureMapViz.purs`

1. Add `fetchSnapshots :: Int -> Aff (Either String (Array Snapshot))` to Loader
2. Add snapshot-aware fetch variants for modules/declarations/calls
3. Replace "Structure" button in ModuleSignatureMap nav with "Compare" button
4. Compare button opens a snapshot picker (dropdown of available snapshots)
5. Selecting a snapshot navigates to CompareModules scene
6. CompareModuleViz loads "before" data from the selected snapshot's endpoints
   and "after" data from the current (latest) snapshot

### Phase 4: Load the pre-refactor snapshot

**Sequence** (after phases 1-3 are committed):

1. Current DB already has the "after" state (latest load)
2. Find the pre-refactor commit: the commit before `8a95408` (Loader.Transform
   extraction). Use `git log --oneline` to identify it.
3. `git checkout <pre-refactor-hash>` (detached HEAD)
4. `cd frontend && spago build` — regenerate output/ with old docs.json
5. Run loader: `minard-loader load -q . -d <db-path> -l "pre-refactor"`
   - This creates a second snapshot with label "pre-refactor"
   - Old workspace packages get version "0.0.0-<old-hash>" so they coexist
6. `git checkout main` — back to current

### Phase 5: Stretch goal — extracted module tracking

When module A is split into A + A.Pure, the compare view could offer A.Pure as
an alternative "after" column. Detection: modules in the "after" snapshot that
share a name prefix with the "before" module and didn't exist in the "before"
snapshot.

## Key Files

| File | Role |
|------|------|
| `minard-loader/src/db/insert.rs` | delete_old_snapshots (line 513), delete_package_module_data |
| `minard-loader/src/loader/pipeline.rs` | Load orchestration, calls delete_old_snapshots (line 141) |
| `server/src/API/Unified.purs` + `.js` | All /api/v2/* endpoints |
| `server/src/Main.purs` | Router |
| `frontend/src/Data/Loader.purs` | All fetch functions (lines 1094+) |
| `frontend/src/Component/CompareModuleViz.purs` | 2×4 grid (already built) |
| `frontend/src/Component/ModuleSignatureMapViz.purs` | Entry point for compare button |

## Risks

- **DB size**: keeping all snapshots grows the DB. Mitigated by the existing
  `cleanup_orphaned_package_versions` which GCs unreferenced package_versions.
- **Module source fetch**: `fetchModuleSource` reads from disk, not DB. The
  "before" source won't be on disk after checkout. May need to store source in
  DB or skip the Concerns diagram for the "before" column.
- **spago build at old commit**: the old commit needs compatible dependencies.
  Should work if spago.lock is committed.
