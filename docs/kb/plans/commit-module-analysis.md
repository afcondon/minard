# Commit-Module Analysis: Three Tiers

*From the commit-module grid to a rotating co-change cube.*

## Current State

Working 2D dot matrix (`CommitModuleGridViz.purs`): commits × modules as CSS grid, data from `GET /api/v2/git/commit-files`. Alphabetical module ordering, monochrome dots, no summaries.

## The Key Insight

The commit-module incidence matrix M is a bipartite graph. `M^T * M` gives the module-module co-change graph — coupling that's invisible in the import graph. Stacking per-commit slices gives a 3D tensor: the full temporal story of how modules co-evolve.

---

## Tier 1: Grid View Enhancements

Pure HTML/CSS, all in `CommitModuleGridViz.purs`.

### 1A. Module Column Reordering

Three modes, toggled from a control bar:

| Mode | Algorithm | Reveals |
|------|-----------|---------|
| **Alphabetical** | Current default | Nothing special |
| **Frequency** | Sort by commit count, descending | Which modules churn most |
| **Co-change** | Greedy nearest-neighbor seriation on Jaccard similarity | Change cohorts — modules that move together |

The co-change seriation: compute `C[i][j] = commits touching both i and j`, start with highest-frequency module, repeatedly append the unplaced module with highest co-change to the last-placed one. O(n²), trivial for 10-60 modules.

New ADT: `OrderMode = Alphabetical | ByFrequency | ByCosimilarity`
New pure function: `reorderModules :: OrderMode -> Array CommitFileEntry -> Array String -> Array String`

### 1B. Frequency Summary Bars

- **Column skyline**: bar above each module header, height ∝ commit count. Same `#0E4C8A` blue.
- **Row breadth**: cell at end of each row showing module count. Number or tiny bar.

Computed from `state.commits` at render time. Adds one extra column to the CSS grid template.

### 1C. Color Encoding

Change server to `git log --name-status` (instead of `--name-only`) to capture A/M/D/R per file.

| Status | Color | Meaning |
|--------|-------|---------|
| A (add) | `#27ae60` green | New module appeared |
| M (modify) | `#0E4C8A` blue | Existing module changed |
| D (delete) | `#e74c3c` red | Module removed |

Extend `CommitFileEntry.modules` to carry status info.

### 1D. Toggle UI

```
[ Alphabetical | Frequency | Co-change ]    [ Bars ]    [ Color: uniform | by operation ]
```

Small text, monospace, Swiss-clean, above the grid.

---

## Tier 2: Cross-Pollination with Treemap

Feed git co-change data back into the existing `ModuleTreemapEnriched`.

### 2A. Change-Frequency Heat Coloring

New `ColorMode` variant: `ChangeFrequency`. Cool (pale blue) → hot (deep orange) by commit count. Same pattern as the 6 existing color modes. HSL interpolation: H 210→25, S 40→95, L 85→45.

### 2B. Co-Change Cluster Coloring

Build co-change graph (modules as nodes, edges where co-change count ≥ 3), run `labelPropagation` from `Data.Graph.Algorithms` (already used for import-graph clusters). New `ColorMode`: `CoChangeCluster`.

**This produces a genuinely different signal from the import graph.** Modules that never import each other but always change together become visible.

### 2C. Change-Frequency Sizing

Toggle between LOC sizing (current) and change-frequency sizing. One-line change in `computeModulePositions`: `value: toNumber $ fromMaybe 1 $ Map.lookup m.name frequencyMap`. Makes heavily-churned utility modules visually dominant.

### 2D. Time-Range Slider

Filter commits to "last N days" or "last N commits." The treemap updates live. Slide from "all time" to "last week" and watch which modules light up.

---

## Tier 3: The Co-Change Cube

Module × Module × Commit as a rotating 3D visualization.

### The Tensor

```
T[c][i][j] = 1  iff modules i and j both changed in commit c
```

For 30 modules × 200 commits: ~5,000 lit voxels out of 180,000 potential. Trivially renderable.

### Faces of the Cube

| Face | Axes | Shows |
|------|------|-------|
| **Front** | module × module | Co-change matrix at one commit slice |
| **Side** | module × commit | Temporal signal of one module pair |
| **Top** | commit × module | The 2D grid we started with |

### Technology: Three.js

- `InstancedMesh` with small cube geometry, one instance per lit voxel
- Color: warm (frequent pair) → cool (rare pair)
- `OrbitControls` for rotate/zoom/pan
- No WASM needed (hylograph WASM kernel is a force simulation engine, not a renderer)
- ~150KB gzipped, acceptable alongside existing D3 bundle

### Interactions

- **Slice plane**: translucent plane sweeping through commit axis, controlled by slider
- **Module-pair highlight**: hover a voxel → highlight all voxels for that pair across time
- **Time-range clip**: same slider as Tier 2D, clips the cube
- **Face projections**: shadow projections on cube faces showing aggregated views

### Scene Integration

New `Scene` constructor: `CoChangeCube String` (package name). Halogen component `CoChangeCubeViz.purs` wraps a JS `CoChangeCube.js` FFI module. Same pattern as the D3 FFI components.

### Data Module

New `CE2.Data.CoChange`:
```purescript
type CoChangeTensor =
  { moduleNames :: Array String
  , commitHashes :: Array String
  , tensor :: Array Boolean         -- c * n * n flattened
  , aggregateMatrix :: Array Number -- n * n, M^T * M
  , moduleFrequency :: Array Int    -- n
  , commitBreadth :: Array Int      -- c
  }

buildCoChangeTensor :: Array CommitFileEntry -> Array String -> CoChangeTensor
reorderModules :: OrderMode -> Array CommitFileEntry -> Array String -> Array String
jaccardSimilarity :: Array CommitFileEntry -> String -> String -> Number
```

Pure, testable, reusable across all three tiers.

---

## Sequencing

### Phase 1: Tier 1 (2-3 sessions)
1. **Co-change data module** (`CE2.Data.CoChange`) — foundation for everything
2. **Module reordering** (1A) — highest information density improvement
3. **Frequency bars** (1B) — visual summary
4. **Color by operation** (1C) — server-side `--name-status`, then rendering
5. **Toggle UI** (1D) — wire up controls

### Phase 2: Tier 2 (2-3 sessions)
1. **Change-frequency color mode** (2A)
2. **Change-frequency sizing** (2C)
3. **Co-change cluster coloring** (2B)
4. **Time-range slider** (2D)

### Phase 3: Tier 3 (3-5 sessions)
1. Add Three.js, build JS FFI module
2. Basic voxel rendering + orbit controls
3. Halogen wrapper + scene wiring
4. Slice plane interaction
5. Face projections + polish

---

## The Vision

The 2D grid is a direct readout of "what changed when." Tier 1 makes it analytically useful. Tier 2 connects it to the spatial layout we already have. Tier 3 reveals the full temporal structure of module coupling — something no existing code visualization tool does.

Together with the existing import-graph analysis, this creates a **two-lens system**: the static lens (what depends on what) and the temporal lens (what changes with what). When they agree, you have well-structured code. When they disagree, you have a finding.
