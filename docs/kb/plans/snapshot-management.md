# Snapshot Management — Frontend-Driven Worktree Lifecycle

## Goal

Let users create and clean up snapshots from the UI. A snapshot is a git
worktree checked out at a specific ref, loaded into DuckDB via the Rust loader.
The same view handles both creation ("snapshot this ref") and cleanup ("delete
these worktrees + their DB data").

## Current State

- Worktrees are created manually (`git worktree add`)
- Loading is done via ProjectManagementViz's "Add Project" flow (enter path, validate, load)
- Each worktree becomes a separate "project" in the DB with its own `repo_path`
- CompareSnapshots scene already consumes cross-project snapshot data
- Git FFI exists: `getGitHeadHash`, `getGitBranchName`, `getGitStatusJson`
- No API for listing refs, creating worktrees, or removing worktrees

## Design

### Scene

Add a `SnapshotManagement` scene (or extend `ProjectManagement`). The view has
two sections:

```
┌──────────────────────────────────────────────────────────────┐
│  Snapshots                                                   │
│                                                              │
│  ┌─ Create from commit ───────────────────────────────────┐  │
│  │                                                         │  │
│  │  ● 28a4808  Convert curried FFI to Fn/EffectFn …  2h   │  │
│  │  ○ e7e02c2  Worktree-based snapshot comparison …  5h   │  │
│  │  ○ cd3fc54  Default snapshot from git HEAD …      1d   │  │
│  │  ○ a93fe87  Multi-snapshot module comparison …    1d   │  │
│  │  ○ 8a95408  Extract Loader.Transform: pure …     2d   │  │
│  │  ○ 07619a7  Extract SceneCoordinator.Pure: …     2d   │  │
│  │  ○ 06dee49  4-tab module view: Layers, Arcs …    3d   │  │
│  │  ○ 0196d89  Structural decomposition, sub- …    4d   │  │
│  │  ○ ─── main ─── ──────────────────────────────         │  │
│  │  ○ 6ce60e0  Merge pull request #12 …             7d   │  │
│  │  ○ f3a1b2c  Fix treemap label overlap …          8d   │  │
│  │       [ Show more ]                                     │  │
│  │                                                         │  │
│  │  Label: [ optional label_______ ]                       │  │
│  │  [ Create Snapshot ]                                    │  │
│  │                                                         │  │
│  │  Status: ████████░░ Loading modules... (12s)            │  │
│  └─────────────────────────────────────────────────────────┘  │
│                                                              │
│  ┌─ Existing Snapshots ───────────────────────────────────┐  │
│  │  ☐ Select All  │  [ Delete Selected ] [ Cancel ]       │  │
│  │                                                         │  │
│  │  ☐  #238  main (6ce60e0)     841 modules   3.2s        │  │
│  │       minard-main/  ← worktree path                    │  │
│  │  ☐  #213  feature/structural-decomp (8a95408)          │  │
│  │       (current checkout — cannot delete)  875 mod      │  │
│  │  ☐  #205  v0.9.0 (a1b2c3d)  790 modules   2.8s        │  │
│  │       minard-v0.9.0/                                    │  │
│  └─────────────────────────────────────────────────────────┘  │
└──────────────────────────────────────────────────────────────┘
```

The commit list is the primary selection mechanism — commit messages are the
most human-readable way to find the right point in history. Branch heads and
tags are shown inline as visual markers (like gitk/tig) rather than as a
separate dropdown. Radio-button selection, click a commit to pick it.

### Interaction

**Create:**
1. User scrolls the commit log and clicks a commit (radio select)
2. Optionally adds a label (defaults to first line of commit message)
3. Clicks "Create Snapshot"
4. Backend: creates worktree → builds (spago) → runs loader → returns result
5. Frontend shows progress, then snapshot appears in the list

**Cleanup:**
1. Checkboxes on each snapshot row
2. "Select All" / "Select None" toggle
3. Current checkout is marked and cannot be selected
4. "Delete Selected" shows confirmation, then:
   - Backend removes worktree (`git worktree remove`)
   - Backend cascade-deletes project + snapshot data from DB
   - Frontend refreshes list

## Implementation Plan

### Phase 1: Backend — Git Commit Log

New endpoint in `API/Snapshots.purs`:

**`GET /api/v2/git/log?count=30&offset=0`**
Returns recent commits with branch/tag decorations.

```json
{
  "commits": [
    {
      "hash": "28a4808",
      "message": "Convert curried FFI to Fn/EffectFn and remove deprecated export",
      "author": "Andrew Condon",
      "date": "2026-03-11T14:30:00Z",
      "relativeDate": "2 hours ago",
      "refs": ["HEAD", "main", "feature/structural-decomp-viz"]
    },
    {
      "hash": "e7e02c2",
      "message": "Worktree-based snapshot comparison with full concern diagrams",
      "author": "Andrew Condon",
      "date": "2026-03-11T09:15:00Z",
      "relativeDate": "5 hours ago",
      "refs": []
    }
  ],
  "hasMore": true
}
```

FFI: `git log --format='%H%x00%s%x00%an%x00%aI%x00%ar%x00%D' -30 --skip=0`
(NUL-delimited fields, one commit per line). Parse in JS, return as JSON.

Commits that already have a snapshot loaded are marked so the UI can show
them differently (greyed radio, or a checkmark instead).

### Phase 2: Backend — Worktree Lifecycle

**`POST /api/v2/snapshots/create`**
```json
{ "ref": "main", "label": "optional label" }
```

Backend steps:
1. Resolve ref to commit hash (`git rev-parse <ref>`)
2. Check if a snapshot with this hash already exists → return early if so
3. Create worktree: `git worktree add ../minard-<sanitized-ref> <ref>`
4. Ensure compiled output exists in worktree:
   - Check for `output/` dir; if missing, run `spago build` in worktree
5. Run Rust loader against worktree path with label
6. Return snapshot info + load stats

Returns:
```json
{
  "success": true,
  "snapshotId": 271,
  "projectId": 98,
  "worktreePath": "../minard-main",
  "stats": { "packages": 42, "modules": 841, "declarations": 9200 },
  "elapsedMs": 3200
}
```

**`POST /api/v2/snapshots/delete`**
```json
{ "snapshotIds": [238, 205] }
```

Backend steps (per snapshot):
1. Look up project by snapshot ID → get `repo_path`
2. Refuse if `repo_path` is the current working directory
3. Remove worktree: `git worktree remove <path>`
4. Cascade-delete project data from DB (reuse `deleteProjectSql`)
5. Return results per snapshot

Returns:
```json
{
  "results": [
    { "snapshotId": 238, "deleted": true },
    { "snapshotId": 205, "deleted": true, "warning": "worktree already removed" }
  ]
}
```

**`GET /api/v2/snapshots/details`**
Enhanced snapshot listing that includes worktree paths and deletion eligibility:

```json
{
  "snapshots": [
    {
      "id": 238, "projectId": 98, "gitHash": "6ce60e0", "gitRef": "main",
      "label": "main-pre-refactor", "moduleCount": 841,
      "worktreePath": "../minard-main",
      "isCurrentCheckout": false,
      "canDelete": true
    }
  ]
}
```

### Phase 3: Backend — Async Execution

The loader currently blocks the event loop via `execSync`. For a good UX we
need progress feedback. Two options:

**Option A: Child process + polling (simpler)**
- `POST /api/v2/snapshots/create` spawns `child_process.spawn` and returns
  immediately with a `jobId`
- `GET /api/v2/snapshots/job/:id` returns status (`pending`, `building`,
  `loading`, `done`, `error`)
- Frontend polls every 2-3 seconds

**Option B: Keep synchronous (pragmatic)**
- The loader typically completes in 2-5 seconds
- `spago build` in the worktree could take longer (30-60s) but only needed
  if no `output/` exists
- Keep `execSync` but add a loading spinner in the frontend
- If `spago build` is needed, warn the user it'll take longer

**Recommendation**: Start with Option B. The blocking is only a UX issue (no
other users hitting the server). Move to Option A later if build times
become painful.

### Phase 4: Frontend — Snapshot Management Component

New `SnapshotManagementViz.purs` component (or section within
ProjectManagementViz).

**State:**
```purescript
type State =
  { snapshots :: Array SnapshotDetail    -- From enhanced listing
  , commitLog :: Array GitCommit         -- From git log endpoint
  , commitLogOffset :: Int               -- For "show more" pagination
  , selectedHash :: Maybe String         -- Radio-selected commit hash
  , label :: String                      -- Optional label (defaults to commit msg)
  , createPhase :: CreatePhase           -- Idle | Creating | Success | Error
  , selectedForDelete :: Set Int         -- Checked snapshot IDs
  , deletePhase :: DeletePhase           -- Idle | Confirming | Deleting | Done
  }

data CreatePhase = Idle | Creating | Success SnapshotResult | CreateError String
data DeletePhase = Idle | Confirming | Deleting | Done
```

**Rendering:**
- Commit log as radio-button list, branch/tag refs shown as inline badges
- Commits with existing snapshots shown with indicator (no radio, already loaded)
- Snapshot list with checkboxes, module counts, git info
- Current checkout row disabled (greyed, no checkbox)
- Bulk selection: "Select All" toggles all deletable snapshots
- Delete confirmation: "Delete N snapshots? This removes worktrees and all
  associated data."

### Phase 5: Scene Integration

Add `SnapshotManagement` to the `Scene` ADT (or fold into `ProjectManagement`).
Wire into SceneCoordinator. Add navigation link from ProjectManagement and/or
a toolbar button.

Consider: a "Compare" button on each snapshot row that navigates directly to
`CompareSnapshots` with that snapshot as the "before".

## Key Decisions

1. **Worktree naming**: `minard-<sanitized-ref>` (e.g., `minard-main`,
   `minard-v0.9.0`). Sanitize: replace `/` with `-`, strip special chars.

2. **Worktree location**: Sibling to the main checkout (e.g.,
   `CodeExplorer/minard-main/`). The loader already handles arbitrary paths.

3. **`spago build` requirement**: Worktrees need compiled output for the
   loader to read `docs.json`. Options:
   - Require the user to have built the project at that ref (fail with message)
   - Auto-run `spago build` in the worktree (slower but fully automated)
   - **Recommendation**: Auto-build with warning about timing. Most refs will
     have compatible dependencies so `spago build` is fast.

4. **One project per worktree**: Each worktree creates a new "project" in
   the DB. This is the existing model and works well — `repo_path` on the
   project points to the worktree, source resolution just works.

5. **Snapshot vs. Project confusion**: To the user, these are "snapshots of
   my project at different points". To the DB, they're separate projects.
   The UI should hide this — show snapshot info (ref, hash, label) not
   project IDs.

## File Changes

| File | Change |
|------|--------|
| `server/src/API/Snapshots.js` | New: git ref listing, worktree create/remove FFI |
| `server/src/API/Snapshots.purs` | New: snapshot management endpoints |
| `server/src/Main.purs` | Route new endpoints |
| `frontend/src/Data/Loader.purs` | Add snapshot management API calls + types |
| `frontend/src/Component/SnapshotManagementViz.purs` | New: snapshot management UI |
| `frontend/src/Scene.purs` | Add SnapshotManagement scene (or extend ProjectManagement) |
| `frontend/src/Component/SceneCoordinator.purs` | Wire new scene |
| `frontend/src/Component/ProjectManagementViz.purs` | Add "Manage Snapshots" link |

## Not In Scope (Future)

- **Filter commits by module** — `git log -- src/Foo/Bar.purs` to show only
  commits that touched a specific module. Very useful when you know what you
  want to compare, but the git path mapping (module name → file path) adds
  complexity. Good candidate for a v2 enhancement.
- **Change heatmap in commit picker** — show which areas of the codebase each
  commit affected (like the existing "C" view). Would make the commit list
  much more informative but is a significant visualization effort.
- Scheduled/automatic snapshots (e.g., on every commit)
- Diff view between snapshots (beyond the existing CompareModules)
- Snapshot annotations/notes
- Snapshot sharing/export
