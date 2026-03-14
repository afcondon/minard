# View Transition Matrix

## View → Scene mapping

|                   | Anatomy                      | Special            | Treemap        | Text            | Git     |
| ----------------- | ---------------------------- | ------------------ | -------------- | --------------- | ------- |
| Package set level | Anatomy(P), Anatomy(M)       | Bubblepack Swarm   | Galaxy Treemap | Packages Report | —       |
| Package level     | Package Anatomy              | Chord/Matrix       | Solar Treemap  | Modules Report  | Commits |
| Module level      | Module Anatomy               | ModuleSignatureMap |                |                 |         |
| Declarations      |                              | Code / editor      |                |                 |         |

| View Name          | Scene(s)                                | Status                | Notes                                 |
| ------------------ | --------------------------------------- | --------------------- | ------------------------------------- |
| Anatomy(P)         | `ProjectAnatomy` (Packages toggle)      | exists                |                                       |
| Anatomy(M)         | `ProjectAnatomy` (Modules toggle)       | exists                | same scene, toggle between P/M        |
| Bubblepack Swarm   | `SolarSwarm`                            | exists (needs polish) | see note                              |
| Galaxy Treemap     | `GalaxyTreemap`                         | exists                |                                       |
| Packages Report    | `PackageReport`                         | exists                |                                       |
| Chord/Matrix       | `PkgTreemap` + `MatrixView`/`ChordView` | exists                | ViewMode toggle on same scene         |
| Package Anatomy    | `PackageAnatomy` (was `StructuralDecomp`) | exists              | renamed from Structure                |
| Module Anatomy     | `ModuleAnatomy` (was `ModuleStructure`)   | exists              | renamed from Structure(M)             |
| Solar Treemap      | `PkgTreemap` (`PrimaryView`)            | exists                |                                       |
| Modules Report     | `AnnotationReport`                      | exists                |                                       |
| Commits            | `CommitModuleGrid` → `CoChangeCube`     | exists                | package level only                    |
| ModuleSignatureMap | `ModuleSignatureMap`                    | exists                | incorporates module-level text/report |
| Code / editor      | `DeclarationDetail`                     | exists                |                                       |

Notes:
- Bubblepack Swarm is midway between Anatomy and Treemap — packages as bubblepacks showing modules inside, topo layers, hover deps

**Deferred (not MVP):** TypeClassGrid, NamespaceTree

**Overlays (orthogonal):** purity, reachability, etc. — not tracked here

---

## Drill-Down Transitions (click entity to go deeper)

### Package Set → Package (click a package)

| From             | To             | Exists? | Trigger                                             | Validated   |
| ---------------- | -------------- | ------- | --------------------------------------------------- | ----------- |
| Anatomy(P)       | Package Anatomy| ✅      | `PackageClicked` → `PackageAnatomy pkg`             | ✅          |
| Anatomy(P)       | Anatomy(M)     | ✅      | toggle on page                                      | ✅          |
| Anatomy(P)       | Chord/Matrix   | ○       |                                                     |             |
| Anatomy(P)       | Modules Report | ○       | should exist, add to page/nav                       |             |
| Anatomy(P)       | Commits        | ○       | should exist, add to page/nav                       |             |
| Bubblepack Swarm | Solar Treemap  | ✅      | click package bubble                                | ✅          |
| Bubblepack Swarm | Anatomy(M)     | ○       | should exist, add to nav                            |             |
| Bubblepack Swarm | Chord/Matrix   | ○       | it's possible these could work together             |             |
| Bubblepack Swarm | Package Anatomy| ○       | probably valid if we find affordance                |             |
| Bubblepack Swarm | Modules Report | ○       | not direct, via top nav to Report(P)                |             |
| Bubblepack Swarm | Commits        | ○       | probably not useful                                 |             |
| Galaxy Treemap   | Solar Treemap  | ✅      | click treemap cell                                  | ✅          |
| Galaxy Treemap   | Anatomy(M)     | ○       | should exist                                        |             |
| Galaxy Treemap   | Chord/Matrix   | ○       | not meaningful unless Chord/Matrix at Package level |             |
| Galaxy Treemap   | Package Anatomy| ○       | not directly                                        |             |
| Galaxy Treemap   | Modules Report | ○       | not directly                                        |             |
| Galaxy Treemap   | Commits        | ○       | not meaningful                                      |             |
| Packages Report  | Solar Treemap  | ✅      | `PackageClicked`                                    |             |
| Packages Report  | Modules Report | ✅      | button marked "detail", too hidden, but works       | ✅ see note |
| Packages Report  | Anatomy(M)     | ○       | via top level nav thru Anatomy(P)                   | ✅          |
| Packages Report  | Chord/Matrix   | ○       |                                                     |             |
| Packages Report  | Package Anatomy| ○       | we could add this                                   |             |
| Packages Report  | Commits        | ○       | we could add this                                   |             |

Notes:
- Packages Report to Modules Report is via a somewhat obscure button at the bottom "Details". All the UI on this page needs revising, it's pretty but totally confusing and not self-explanatory.

### Package → Module (click a module)

| From           | To                 | Exists? | Trigger                            |     |
| -------------- | ------------------ | ------- | ---------------------------------- | --- |
| Anatomy(M)     | Module Anatomy     | ✅      | click module circle in beeswarm    | ✅  |
| Chord/Matrix   | ModuleSignatureMap | ○       | click on arc or square (TODO)      |     |
| Package Anatomy | Module Anatomy    | ✅      | click module name in layer card or block list | ✅ |
| Solar Treemap  | ModuleSignatureMap | ✅      | click treemap cell                 | ✅  |
| Modules Report | ModuleSignatureMap | ✅      | `NavigateToModule`                 | ✅  |
| Commits        | ModuleSignatureMap | ○       | click on module name               | ✅  |

### Module → Declaration (click a declaration)

| From               | To            | Exists? | Trigger           | Validated |
| ------------------ | ------------- | ------- | ----------------- | --------- |
| ModuleSignatureMap | Code / editor | ✅      | click declaration | ✅        |
|                    |               |         |                   |           |

---

## Same-Level Transitions (switch view type)

### Within Package Set Level

| From             | To               | Exists? | Mechanism                                | Validated |
| ---------------- | ---------------- | ------- | ---------------------------------------- | --------- |
| Anatomy(P)       | Galaxy Treemap   | ✅      | `NavigateToGalaxy` button (below fold)   | ✅        |
| Anatomy(P)       | Bubblepack Swarm | ○       | Could use modifier + click here          |           |
| Anatomy(P)       | Packages Report  | ○       | Top nav works here                       | ✅        |
| Galaxy Treemap   | Anatomy(P)       | ○       | Top nav works here                       | ✅        |
| Galaxy Treemap   | Bubblepack Swarm | ✅      | clicking on any circle in the bubblepack | ✅        |
| Galaxy Treemap   | Packages Report  | ○       | Top nav works here                       | ✅        |
| Bubblepack Swarm | Galaxy Treemap   | ✅      | parent breadcrumb                        |           |
| Bubblepack Swarm | Anatomy(P)       | ○       | top nav works here                       | ✅        |
| Bubblepack Swarm | Packages Report  | ○       | top nav works here                       | ✅        |
| Packages Report  | Galaxy Treemap   | ○       | Currently breadcrumb, confusingly        | ✅        |
| Packages Report  | Anatomy(P)       | ○       | top nav works here                       | ✅        |
| Packages Report  | Bubblepack Swarm | ○       | see note                                 |           |

Notes:
- ~~Galaxy Treemap to Bubblepack swarm has two paths, the button in the sub-nav and clicking on any circle in the bubblepack. We want to lose that button in the sub-nav.~~ **DONE**: layout toggle removed from sub-nav. Click-circle is the only path in. Return via browser history and parent breadcrumb both valid.
- Packages Report to Galaxy Treemap now reachable via top nav "Maps" button.
- Packages Report to Bubblepack Swarm. We should add the bubblepack group mini-viz to each package report section, exactly as we do for the Modules Report. Having done that, a click on that bubblepack - which is identical to the bubblepack in Galaxy Treemap, will take you to Bubblepack Swarm, exactly as it does in GalaxyTreemap

### Within Package Level

| From           | To             | Exists? | Mechanism                         | Validated |
| -------------- | -------------- | ------- | --------------------------------- | --------- |
| ~~Solar Treemap~~  | ~~Chord/Matrix~~   | ~~✅~~      | ~~`ViewMode` toggle~~ **REMOVED**    |        |
| ~~Chord/Matrix~~   | ~~Solar Treemap~~  | ~~✅~~      | ~~`ViewMode` toggle~~ **REMOVED**   |         |
| Solar Treemap  | Package Anatomy | ○      | top nav Anatomy → click package   |           |
| Solar Treemap  | Modules Report | ○       | top nav Reports                   |           |
| Commits        | Solar Treemap  | ✅      | parent breadcrumb                 |           |
| Commits        | CoChangeCube   | ✅      | drill deeper (same Git column)    |           |

Notes:
- Solar Treemap to Chord/Matrix works but the whole Chord/Matrix feature needs rethink
- ~~Structure has two levels, Package and Module and is really a continuation of the Anatomy views~~ **DONE**: renamed to Package Anatomy (`PackageAnatomy`) and Module Anatomy (`ModuleAnatomy`)
- ~~breadcrumbs for Modules Report say Galaxy > Report Modules should be Report > Modules~~ **DONE**: breadcrumbs now show `Reports > Modules`

### Transitions now unreachable (removed sub-nav scene links)

These were removed intentionally. Need to find new homes for the useful ones:

- **Solar Treemap → Commits**: was sub-nav "Commits" button. Needs a new path — perhaps a contextual link within the treemap, or a Git section in the breadcrumb area.
- **Solar Treemap → CoChangeCube**: was sub-nav "Cube" button. Only reachable through Commits → Cube drill-down now.
- **PkgModuleBeeswarm**: no entry point at all — layout toggle was the only way in. Needs a new affordance (e.g. click gesture on treemap, or a view toggle within the Maps family).
- **Module-level → Commits/Cube**: were sub-nav links from ModuleSignatureMap, ModuleOverview, ModuleAnatomy, DeclarationDetail. These were arguably wrong (package-level views linked from module level). If needed, reachable by breadcrumb-up to package then navigating to Commits.
- **Chord/Matrix views**: ViewMode toggle removed from SolarSwarm and PkgTreemap sub-nav. These views are now unreachable. The underlying rendering code still exists but has no UI entry point. Needs rethink as separate scenes or a different interaction model.

---

## Drill-Up Transitions (back/parent navigation)

| From               | To                 | Exists? | Mechanism                    | Validated |
| ------------------ | ------------------ | ------- | ---------------------------- | --------- |
| Solar Treemap      | Bubblepack Swarm   | ✅      | `parentScene` → `SolarSwarm` |           |
| Chord/Matrix       | Solar Treemap      | ✅      | same scene, mode switch      |           |
| Package Anatomy    | Anatomy(P)         | ✅      | `parentScene` → `ProjectAnatomy` | ✅    |
| Module Anatomy     | Package Anatomy    | ✅      | `parentScene` → `PackageAnatomy pkg` | ✅ |
| Modules Report     | Packages Report    | ✅      | `parentScene`                |           |
| Commits            | Solar Treemap      | ✅      | `parentScene` → `PkgTreemap` |           |
| ModuleSignatureMap | Solar Treemap      | ✅      | `parentScene` → `PkgTreemap` |           |
| Code / editor      | ModuleSignatureMap | ✅      | `parentScene`                |           |
| CoChangeCube       | Commits            | ✅      | `parentScene`                |           |
| Any package-level  | Landing            | ✅      | Minard logo breadcrumb       | ✅        |
|                    |                    |         |                              |           |

Notes: breadcrumbs and/or browser history are the preferred mechanism for drill-up, i'll review all these when the lateral and drill down transitions are all nailed and the breadcrumbs are fixed
---

## Landing Page Transitions

| From    | To              | Exists? |
| ------- | --------------- | ------- |
| Landing | Galaxy Treemap  | ✅      |
| Landing | Packages Report | ✅      |
| Landing | Anatomy(P)      | ✅      |
| Landing | Projects (CRUD) | ✅      |

All good here for now.

---

## Review Summary (from validation pass)

### Navigation infrastructure (DONE)

- **Top nav**: now Maps | Reports | Anatomy | Projects | Sync. Each highlights correctly for its scene family.
- **Breadcrumbs**: overhauled. Maps family uses Powers of Ten metaphor (Maps > Galaxy > SolarSystem {pkg} > Planet {mod}). Anatomy, Reports, Projects each have their own root segment. No more fabricated Galaxy-rooted paths for non-Maps views.
- **Sub-nav (Row 2)**: cleaned up. Only view-transforming controls remain (color overlays, peek, view mode toggles). All scene links removed — cross-family navigation handled by top nav and breadcrumbs.
- **Landing page**: buttons match top nav (Maps, Reports, Anatomy, Projects).

### Missing drill-downs (high value)

- ~~**Anatomy(M) → ModuleSignatureMap**~~ **DONE**: click module circle → ModuleAnatomy (stays in Anatomy family)
- **Commits → ModuleSignatureMap** — click module name. Already works in UI (validated) even though not wired as explicit transition.
- **Chord/Matrix → ModuleSignatureMap** — click arc or matrix square. TODO.
- ~~**Package Anatomy → ModuleSignatureMap**~~ **DONE**: click module name in layer card or block list → ModuleAnatomy (stays in Anatomy family). HATS graph nodes and matrix cells not yet clickable (needs HATS onClick behaviors).

### Packages Report UI

- Drill-down to Modules Report is via an obscure "Details" button at the bottom — too hidden
- The whole page needs UI revision: "pretty but totally confusing and not self-explanatory"
- Adding bubblepack mini-vizs to package report cards (as Modules Report already has) would create a natural path to Bubblepack Swarm via click, mirroring how it works in Galaxy Treemap

### Structure / Anatomy merge (DONE)

- Renamed `StructuralDecomp` → `PackageAnatomy`, `ModuleStructure` → `ModuleAnatomy`
- Renamed component files: `StructuralDecompViz.purs` → `PackageAnatomyViz.purs`, `ModuleStructureViz.purs` → `ModuleAnatomyViz.purs`
- Updated all scene labels, breadcrumbs, nav buttons from "Structure" to "Anatomy"
- Updated top table to show Anatomy as a full drill-down column: Project → Package → Module

### Bubblepack Swarm cleanup (DONE)

- ~~Galaxy Treemap → Bubblepack Swarm has two redundant paths (sub-nav button + click circle). Remove sub-nav button, keep click-circle.~~ Layout toggle removed.
- Return via browser history and parent breadcrumb both valid.
- PkgModuleBeeswarm now has no entry point — layout toggle was it. Needs new affordance.
