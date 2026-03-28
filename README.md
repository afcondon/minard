# Minard

Code cartography for PureScript projects.

When development is as accelerated as it is now with LLMs, and when LLMs have a context window that is constantly emptying, it's vital that both parties have a source of truth that they share. Minard aims to be that source of truth and a locus of discussion between human and AI developers.

Named for [Charles Joseph Minard](https://en.wikipedia.org/wiki/Charles_Joseph_Minard), whose 1869 visualization of Napoleon's Russian campaign remains the best statistical graphic ever drawn.

## What It Does

Point Minard at a PureScript workspace. It scans your source, parses every module, resolves the full dependency graph from your lock file, and loads everything into a DuckDB database. Then you explore.

The frontend is a single-page Halogen app with interconnected views across four lenses (Maps, Reports, Anatomy, Git) at three depth levels (Project, Package, Module). Navigation follows a "Powers of Ten" pattern — start at the full package universe, drill into a package, into a module, into individual declarations. Each level answers different questions.

### Views

| Level | Lens | View | What you see |
|-------|------|------|--------------|
| Project | Maps | Galaxy Treemap | Every package as a rectangle with module bubbles inside — two levels of structure, each clickable |
| Project | Reports | Package Report | AI-generated quality observations for each module, with human review workflow |
| Project | Anatomy | Project Anatomy | Your dependency universe as a force-directed beeswarm — workspace, direct deps, transitive deps |
| Project | Git | Git Overview | Commit activity across all modules, showing where development energy is concentrated |
| Package | Maps | Module Treemap | Modules as rectangles, declarations as bubbles — hover for tooltips, click to drill in. Toggle overlays for git status, reachability, purity, clustering, coupling, change frequency, co-change, and cross-module call links |
| Package | Reports | Annotation Report | AI and human annotations across all modules, filterable by status, kind, and thread |
| Package | Anatomy | Package Anatomy | Biconnected component decomposition revealing structural blocks and bridges |
| Package | Git | Commit Grid | Per-module commit history as a dot-matrix heatmap, showing co-change patterns |
| Module | All | Module Planet | Unified module view combining signatures, dependencies, layers, concerns, cutpoints, annotations, blame, and sparkline — all as togglable panels with keyboard shortcuts |

### Module Planet

The integrated module page (Module Planet) merges what were previously separate views into a single panel-based layout:

- **Signatures (S)** — type signatures with blame-age coloring, click to focus
- **Dependencies (D)** — cross-module call graph for the focused declaration
- **Layers (L)** — call hierarchy organized into dependency layers
- **Cutpoints (X)** — articulation points and bridges in the module's call graph
- **Concerns (C)** — declarations clustered by shared calling patterns
- **Annotations (A)** — AI and human observations with threaded discussion

Each panel toggles independently via button or hotkey. The blame ribbon and commit sparkline are always visible.

### Interaction

- **Click** any package, module, or declaration to drill down
- **Hover** module bubbles in the Galaxy Treemap for tooltips showing module name and LOC
- **U** on the Module Treemap to toggle cross-module dependency links
- **R** for reachability overlay — which modules are actually used from the entry point
- **P** for purity overlay — blue for pure, amber for effectful
- **G** for git status, **H** for change frequency, **K** for clusters, **C** for coupling, **X** for co-change
- **O** for source overlay — registry vs local vs workspace packages
- **Search** with typeahead across declarations, modules, and packages

## Current State

**Pre-release / technology preview. Works locally. PureScript only.**

The tool indexes its own codebase (437 packages in the dependency universe, 81 workspace modules, 34k LOC) and navigates fluidly between all views. The annotation system supports AI-generated module summaries with human review and threaded discussion. All 81 workspace modules have AI-generated summary annotations.

What's not done: no hosted demo, no install story beyond "clone and build," no support for languages other than PureScript. The database schema is stable but not documented for external use.

## Architecture

```
minard/
├── frontend/        34k LOC PureScript — Halogen app, Hylograph visualizations
├── server/           3k LOC PureScript/JS — HTTPurple REST API over DuckDB
├── minard-loader/    7k LOC Rust — scans PureScript workspaces, loads DuckDB
├── database/         DuckDB file
├── vscode-extension/ Jump between visualization and source
├── site-explorer/    Route analysis for Halogen SPAs
└── docs/skills/      Claude Code skills (annotate)
```

### Frontend (PureScript + Halogen + Hylograph)

81 modules using [Hylograph](https://github.com/afcondon/hylograph) — a PureScript visualization library built on D3 with a declarative AST (HATS) for bindings, selections, transitions, and force simulations. Type signatures rendered by [sigil](https://github.com/afcondon/purescript-sigil).

### Server (PureScript + HTTPurple)

REST API serving package, module, declaration, import, and annotation data from DuckDB. Runs as a Node.js process.

Key endpoints:
- `/api/v2/stats` — project-wide statistics (packages, modules, declarations, calls)
- `/api/v2/packages` — packages with stats, dependencies, topological layers
- `/api/v2/modules` — modules with LOC, declaration counts, package info
- `/api/v2/all-imports`, `/api/v2/all-calls` — bulk graphs for cross-package analysis
- `/api/v2/annotations` — AI/human annotations with threading and review status
- `/api/v2/module-source` — read module source files (for AI annotation)
- `/api/v2/git/blame`, `/api/v2/git/commit-files` — git history and blame data
- `/api/v2/report` — generated markdown codebase report
- `/api/v2/projects/*` — project management (list, validate, load, delete)

### Loader (Rust)

Scans a PureScript workspace: parses `spago.yaml` and `spago.lock`, reads compiler output (`docs.json`, `externs.json`), resolves the registry snapshot, computes topological layers, extracts function calls, and bulk-loads everything into DuckDB via the Appender API. Full scan of a 437-package workspace runs in ~3 seconds.

### Database (DuckDB)

Columnar analytics database. Schema supports multiple projects and snapshots. Tables for packages, modules, declarations, imports, function calls, type signatures, annotations, git metrics, and route definitions. Content hashing on modules enables stale-annotation detection across reloads.

## Running Locally

Prerequisites: PureScript toolchain (spago, purs), Node.js, Rust toolchain (for the loader).

```bash
make bootstrap    # check prereqs, build everything, self-scan
make start        # start server (port 3000) + frontend (port 3001)
```

Or manually:

```bash
# Build the loader
cd minard-loader && cargo build --release

# Scan a workspace
./minard-loader/target/release/minard-loader \
  load --database database/ce-unified.duckdb \
  --scan /path/to/your/purescript/workspace

# Build and start the server (must run from minard/)
spago build -p minard-server
node server/run.js          # port 3000

# Build and start the frontend
spago build -p minard-frontend
spago bundle -p minard-frontend
cd frontend && npx serve public -p 3001
```

Open http://localhost:3001.

## AI Collaboration

Minard's annotation system is designed for dialogue between AI and human developers. AI agents read source code via the API and write structured annotations — module summaries, quality observations, architecture notes, coupling analysis. Humans review, confirm, dispute, or extend with their own context. Each annotation is threaded — replies form a conversation, and disagreements surface where code structure doesn't match architectural intent.

The full structural database is available via REST API. An AI agent can query module dependencies, find articulation points, trace call graphs, read prior annotations, and get module summaries — the same data the visualizations use — without reading a single source file.

A Claude Code skill (`/annotate`) is included for generating and reviewing annotations from the CLI. See `docs/skills/annotate.md`.

## Design Principles

1. **Database-first.** Expensive analysis at load time. Queries are fast.
2. **CLI + Viz.** The AI needs queryable data. The human needs pictures. Same database, different interfaces.
3. **Declarative.** Visualizations describe what, not how. Hylograph's HATS AST handles bindings and transitions.
4. **Multi-scale.** No single view suffices. Fluid navigation across levels is the core interaction.
5. **AI as participant.** Cached structural analysis in the database means AI agents don't re-derive understanding from source every conversation.

## License

MIT
