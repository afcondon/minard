# Minard

Code cartography for PureScript projects.

When development is as accelerated as it is now with LLMs, and when LLMs have a context window that is constantly emptying, it's vital that both parties have a source of truth that they share. Minard aims to be that source of truth and a locus of discussion between human and AI developers.

Named for [Charles Joseph Minard](https://en.wikipedia.org/wiki/Charles_Joseph_Minard), whose 1869 visualization of Napoleon's Russian campaign remains the best statistical graphic ever drawn.

## What It Does

Point Minard at a PureScript workspace. It scans your source, parses every module, resolves the full dependency graph from your lock file, and loads everything into a DuckDB database. Then you explore.

The frontend is a single-page Halogen app with 13 interconnected views. Navigation follows a "Powers of Ten" pattern — start at the full package universe, drill into a package, into a module, into a declaration, down to individual call sites. Each level answers different questions.

### Views

| Level | View | What you see |
|-------|------|--------------|
| Universe | Galaxy Treemap | Every package in your dependency set, sized by LOC, colored by namespace or cluster |
| Universe | Galaxy Beeswarm | Same packages arranged by topological layer, filterable by scope |
| Universe | Project Anatomy | Your workspace vs. direct vs. transitive deps, with colored spago.yaml blocks |
| Neighborhood | Solar Swarm | Bubble-packed modules within packages |
| Package | Package Treemap | Modules as cells in a treemap, with enriched circle-packed declarations inside |
| Package | Module Beeswarm | Modules arranged by internal dependency depth |
| Module | Module Signature Map | Every exported declaration with full rendered type signatures (via [sigil](https://github.com/afcondon/purescript-sigil)) |
| Module | Module Overview | Bubble pack + declaration listing |
| Declaration | Declaration Detail | Arc diagram showing call relationships, purity coloring |
| Cross-cutting | Type Class Grid | All type classes with method counts, instance counts, inheritance |
| Cross-cutting | Annotation Report | AI and human annotations on every module, threaded discussion |
| Cross-cutting | Project Management | Onboard new projects, validate prerequisites, trigger loader |

### Interaction

- **Click** any package, module, or declaration to drill down
- **Hold R** on any treemap for reachability overlay — see which modules are actually used
- **Hold P** for purity overlay — blue for pure, amber for effectful
- **Color modes** on treemaps: namespace, topological layer, community cluster
- **Search** with typeahead across declarations

## Current State

**Pre-release. Works locally. PureScript only.**

The tool indexes its own codebase (117 packages, 864 modules) and navigates fluidly between all 13 views. The annotation system supports AI-generated module summaries with human review and threaded discussion. Project onboarding works end-to-end from the browser.

What's not done: no hosted demo, no install story beyond "clone and build," no support for projects outside the local filesystem. The database schema is stable but not documented for external use.

## Architecture

```
minard/
├── frontend/        23k LOC PureScript — Halogen app, Hylograph visualizations
├── server/           3k LOC PureScript/JS — HTTPurple REST API over DuckDB
├── minard-loader/    7k LOC Rust — scans PureScript workspaces, loads DuckDB
├── database/         DuckDB file (schema v3.4)
├── vscode-extension/ Jump between visualization and source
├── site-explorer/    Route analysis for Halogen SPAs
└── tools/            CLI utilities (minard-reach, minard-annotate)
```

### Frontend (PureScript + Halogen + Hylograph)

21 visualization modules using [Hylograph](https://github.com/afcondon/hylograph) — a PureScript visualization library built on D3 with a declarative AST (HATS) for bindings, selections, transitions, and force simulations. Type signatures rendered by [sigil](https://github.com/afcondon/purescript-sigil).

### Server (PureScript + HTTPurple)

REST API serving package, module, declaration, import, and annotation data from DuckDB. Runs as a Node.js process.

Key endpoints:
- `/api/v2/packages` — packages with stats, dependencies, topological layers
- `/api/v2/modules/:pkg` — modules with import counts, LOC, content hashes
- `/api/v2/declarations/:pkg/:module` — declarations with type signatures, call data
- `/api/v2/all-imports`, `/api/v2/all-calls` — bulk graphs for cross-package analysis
- `/api/v2/annotations` — AI/human annotations with threading
- `/api/v2/projects/*` — project management (list, validate, load, delete)

### Loader (Rust)

Scans a PureScript workspace: parses `spago.yaml` and `spago.lock`, reads compiler output (`docs.json`, `externs.json`), resolves the registry snapshot, computes topological layers, extracts function calls, and bulk-loads everything into DuckDB via the Appender API. Full scan of a 117-package workspace runs in ~3 seconds.

### VS Code Extension

Bridges the visualization and the editor. Jump from a declaration view to the source line, open a package folder, or open a module file. Planned: navigation from editor to visualization (select a symbol, see it in context).

### Database (DuckDB)

Columnar analytics database. Schema supports multiple projects and snapshots. Tables for packages, modules, declarations, imports, function calls, type signatures, annotations, git metrics, and route definitions. Content hashing on modules enables stale-annotation detection across reloads.

## Running Locally

Prerequisites: PureScript toolchain (spago, purs), Node.js, Rust toolchain (for the loader).

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

Alternatively, skip the loader and use the browser's project management page to onboard a project after starting the server with an empty database.

## AI Collaboration

Minard's annotation system is designed for dialogue between AI and human developers. AI agents read source code and write structured annotations (module summaries, quality observations, architecture notes). Humans review, confirm, dispute, or extend with their own context. Each annotation is threaded — replies form a conversation, and disagreements surface where code structure doesn't match architectural intent.

The annotation API is REST, so any tool that can `curl` can participate. A Claude Code skill (`/annotate`) is included for generating and reviewing annotations from the CLI.

## Design Principles

1. **Database-first.** Expensive analysis at load time. Queries are fast.
2. **CLI + Viz.** The AI needs queryable data. The human needs pictures. Same database, different interfaces.
3. **Declarative.** Visualizations describe what, not how. Hylograph's HATS AST handles bindings and transitions.
4. **Multi-scale.** No single view suffices. Fluid navigation across levels is the core interaction.

## License

MIT
