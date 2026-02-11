# Navigation & Interaction Design

## Design Goals

- All navigation via direct interaction with the visualization (no back arrows, minimal chrome)
- Allow level-jumping wherever possible (don't force users through intermediate screens)
- No surprises: going "back" returns to the same view and state you came from
- Browser history buttons work; no application-level back button
- Breadcrumb-style navigator for outward/upward jumps (since there's no obvious in-viz equivalent)

## Current Architecture

### Scene ADT (9 scenes, 3 depth levels)

| Scene | Level | Theme | Description |
|-------|-------|-------|-------------|
| `GalaxyTreemap` | 0 - Galaxy | Blueprint | Full registry as squarified treemap |
| `GalaxyBeeswarm` | 0 - Galaxy | Blueprint | Force-directed beeswarm, topo-layered |
| `TypeClassGrid` | 0 - Galaxy | Blueprint | Type class cards with method/instance donuts |
| `SolarSwarm` | 1 - Solar | Beige | Project packages as bubble-pack beeswarm |
| `OverlayChordMatrix` | 1 - Solar | Beige | Placeholder for dependency overlay |
| `PkgTreemap(pkg)` | 2 - Module | Paperwhite | Module treemap with declaration circles |
| `PkgModuleBeeswarm(pkg)` | 2 - Module | Paperwhite | Module beeswarm/bubble-pack overlay |
| `ModuleOverview(pkg, mod)` | 2 - Module | Paperwhite | Bubble pack + declaration listing |
| `DeclarationDetail(pkg, mod, decl)` | 2 - Module | Paperwhite | Single declaration detail view |

### ViewMode sub-views

At SolarSwarm and PkgTreemap, a footer toggle switches between:
- **PrimaryView** (default visualization)
- **ChordView** (dependency chord diagram)
- **MatrixView** (adjacency matrix)

### Current navigation graph

```
                    ┌─────────────────┐
                    │  GalaxyTreemap  │  Level 0 (Blueprint)
                    │  ─────────────  │
                    │  circle → SolarSwarm(focal)
                    │  rect   → PkgTreemap(pkg)     ← level jump
                    └────────┬────────┘
                     forward ↓  ↑ back
                    ┌────────┴────────┐
                    │ GalaxyBeeswarm  │  Level 0
                    │ ──────────────  │
                    │ circle → SolarSwarm(focal)
                    │ label  → PkgTreemap(pkg)      ← level jump
                    └────────┬────────┘
                     forward ↓  ↑ back
         ┌──────────┬────────┴────────┬──────────┐
         │Chord/Mtx │  SolarSwarm     │          │  Level 1 (Beige)
         │  toggle  │  ───────────    │          │
         │          │  circle → set focal (stay)  │
         │          │  label  → PkgTreemap(pkg)   │
         │          └────────┬────────┘          │
         └───────────────────┼───────────────────┘
                     forward ↓  ↑ back
         ┌──────────┬────────┴────────┬──────────┐
         │Chord/Mtx │  PkgTreemap     │Beeswarm  │  Level 2 (Paper)
         │  toggle  │  ──────────     │ overlay   │
         │          │  module → ModuleOverview     │
         │          │  decl   → DeclDetail  ← jump│
         │          └────────┬────────┘          │
         └───────────────────┼───────────────────┘
                             ↓  ↑ back
                    ┌────────┴────────┐
                    │ ModuleOverview   │  Level 2
                    │ ──────────────   │
                    │ decl → DeclDetail│
                    └────────┬────────┘
                             ↓  ↑ back
                    ┌────────┴────────┐
                    │DeclarationDetail│  Level 2
                    │ peer decl click │
                    │  → same scene   │
                    └─────────────────┘
```

TypeClassGrid is accessible via a "TC" header button (outside the main flow).

### parentScene DAG (back navigation)

```
GalaxyTreemap         → GalaxyTreemap (root)
GalaxyBeeswarm        → GalaxyTreemap
SolarSwarm            → GalaxyBeeswarm
PkgTreemap(pkg)       → SolarSwarm
PkgModuleBeeswarm(pkg)→ PkgTreemap(pkg)
ModuleOverview(p,m)   → PkgTreemap(p)
DeclarationDetail(p,m,d) → ModuleOverview(p,m)
OverlayChordMatrix    → SolarSwarm
TypeClassGrid         → GalaxyTreemap
```

### Browser history

- `pushHistoryState` on every NavigateTo/NavigateBack/NavigateForward
- `popstate` listener deserializes scene string, dispatches `HandlePopstate`
- Scene serialization: `"PkgTreemap(halogen)"`, `"ModuleOverview(halogen,Halogen.HTML)"`, etc.
- **ViewMode is NOT serialized** — browser back doesn't restore chord/matrix state

## Issues

### 1. Inconsistent click semantics

The same gesture (click) does fundamentally different things depending on what visual element is hit:

- **GalaxyTreemap**: circle click → SolarSwarm; rect click → PkgTreemap (different levels)
- **GalaxyBeeswarm**: circle click → SolarSwarm; label click → PkgTreemap
- **SolarSwarm**: circle click → set focal (stays); label click → PkgTreemap

There's no consistent rule a user can learn about what "click" means.

### 2. Back navigation ignores actual path

`parentScene` defines a fixed DAG. If you jumped from Galaxy directly to PkgTreemap (via rect click), back takes you to SolarSwarm — which you never visited. This violates the "no surprises" principle.

### 3. No outward level-jumping

You can jump *in* (Galaxy → PkgTreemap) but can't jump *out* (PkgTreemap → Galaxy) in one step. Back always walks the parentScene chain.

### 4. ViewMode invisible in history

Chord/matrix toggle is stored in component state but not in the URL. Browser back/forward doesn't restore it.

## Design Decisions

### D1. Registry level: Treemap is primary, Beeswarm is secondary

The package-set treemap is the entry point and primary view at the Registry level. The beeswarm is visually appealing but is demoted to a secondary/alternate view.

### D2. Galaxy Treemap click semantics (reversed from current)

Two distinct click targets in the treemap, with semantics matched to visual context:

| Target | Hover behavior | Click behavior | Rationale |
|--------|---------------|----------------|-----------|
| **Circle** (package data) | Tooltip with package info (no dependency highlight) | Navigate INTO that package → PkgTreemap | You clicked the entity, you go deeper into it |
| **Rect** (treemap cell background) | Dependency highlighting + arrows/links | Navigate to dependency topograph/beeswarm | You were looking at dependencies, you get the dependency view |

This reverses the current behavior (circle → SolarSwarm, rect → PkgTreemap) and is more semantically coherent: clicking the data representation of an entity takes you into that entity.

Dependency visualization on hover should include **arrows/links** between cells, not just color highlighting.

### D3. Click = go deeper, modifier+click = filter/alternate

Plain click always means "drill into this entity's primary view." This is the universal grammar.

| Gesture | Meaning |
|---------|---------|
| **Click** on data | Go deeper into that entity's primary view |
| **Modifier+click** | Contextual: set focal filter, switch viz mode, etc. |
| **Hover** | Preview/highlight only (no navigation) |

At SolarSwarm level: click on a package circle navigates to PkgTreemap (not sets focal). Modifier+click sets focal filter (neighborhood view). Click target is the circle (data), not the label.

### D4. ViewMode (Chord/Matrix) included in URL state

Chord and matrix views are serialized in history state so browser back/forward restores them. No surprises.

### D5. Breadcrumbs replace depth tabs

The 3-band color strip is too subtle. Replace with a breadcrumb bar derived from the current view state strings:

```
Registry  ›  halogen  ›  Halogen.HTML  ›  text
```

Each segment is clickable — jumps directly to that entity's primary view. Remove the back arrow button; breadcrumbs provide the upward/outward navigation.

### D6. Rendering inconsistencies deferred

Visual consistency issues (e.g., detail disappearing when navigating to module level) are deferred until after the navigation model is implemented.

## Target Model

### Entity hierarchy with primary and alternate views

```
Registry  →  Package  →  Module  →  Declaration
```

| Entity | Primary View | Alternates |
|--------|-------------|-----------|
| Registry | Treemap (GalaxyTreemap) | Beeswarm, TypeClassGrid |
| Package set | SolarSwarm (bubble-pack) | Chord, Matrix |
| Single package | Enriched Treemap | Chord, Matrix, Beeswarm overlay |
| Module | ModuleOverview | (none yet) |
| Declaration | DeclarationDetail | (none yet) |

### Target navigation graph

```
┌──────────────────────────────────────────────────────────┐
│  REGISTRY (Blueprint theme)                              │
│                                                          │
│  Primary: GalaxyTreemap                                  │
│    circle click  → PkgTreemap(pkg)   [drill into pkg]   │
│    rect click    → DependencyBeeswarm(pkg)  [dep view]  │
│    circle hover  → tooltip (pkg info)                    │
│    rect hover    → dependency arrows/links               │
│                                                          │
│  Alt: GalaxyBeeswarm (modifier or breadcrumb switch)     │
│  Alt: TypeClassGrid  (modifier or breadcrumb switch)     │
└──────────────────────┬───────────────────────────────────┘
                       │ click circle
                       ▼
┌──────────────────────────────────────────────────────────┐
│  PACKAGE SET (Beige theme)  — SolarSwarm                 │
│                                                          │
│  Primary: BubblePackBeeswarm                             │
│    circle click     → PkgTreemap(pkg)  [drill into pkg]  │
│    mod+circle click → set focal filter (neighborhood)    │
│                                                          │
│  Alt: Chord diagram                                      │
│  Alt: Adjacency matrix                                   │
└──────────────────────┬───────────────────────────────────┘
                       │ click circle
                       ▼
┌──────────────────────────────────────────────────────────┐
│  SINGLE PACKAGE (Paperwhite theme)  — PkgTreemap(pkg)    │
│                                                          │
│  Primary: Enriched Treemap                               │
│    module click → ModuleOverview(pkg, mod)               │
│    decl click   → DeclarationDetail(pkg, mod, decl)      │
│                                                          │
│  Alt: Chord diagram                                      │
│  Alt: Adjacency matrix                                   │
│  Alt: Beeswarm overlay                                   │
└──────────────────────┬───────────────────────────────────┘
                       │ click module or decl
                       ▼
┌──────────────────────────────────────────────────────────┐
│  MODULE (Paperwhite theme)  — ModuleOverview(pkg, mod)   │
│                                                          │
│  Bubble pack + declaration listing                       │
│    decl click → DeclarationDetail(pkg, mod, decl)        │
└──────────────────────┬───────────────────────────────────┘
                       │ click decl
                       ▼
┌──────────────────────────────────────────────────────────┐
│  DECLARATION (Paperwhite theme)                          │
│                                                          │
│  DeclarationDetail(pkg, mod, decl)                       │
│    peer decl click → DeclarationDetail (same level)      │
└──────────────────────────────────────────────────────────┘

Breadcrumb bar (always visible):
  Registry  ›  halogen  ›  Halogen.HTML  ›  text
  ^^^^^^^^     ^^^^^^^^     ^^^^^^^^^^^^^     ^^^^
  clickable    clickable    clickable         current
```

### URL serialization (target)

Include visualization mode in history state:

```
"GalaxyTreemap"
"GalaxyTreemap:beeswarm"
"GalaxyTreemap:typeclasses"
"SolarSwarm"
"SolarSwarm:chord"
"PkgTreemap(halogen)"
"PkgTreemap(halogen):matrix"
"ModuleOverview(halogen,Halogen.HTML)"
"DeclarationDetail(halogen,Halogen.HTML,text)"
```

### What gets removed

- **Back arrow button** — replaced by breadcrumb
- **Forward arrow button** — replaced by click-to-drill
- **3-band depth tabs** — replaced by breadcrumb
- **parentScene DAG** — no longer needed; browser history handles back, breadcrumb handles upward jumps
- **NavigateBack / NavigateForward actions** — replaced by NavigateTo only
- **Label-click as distinct navigation** — click is on data (circles), labels are decorative
