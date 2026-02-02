# Minard

**Overcome the Fog of War in AI-led development.**

Code cartography from package sets to call sites. Named for [Charles Joseph Minard](https://en.wikipedia.org/wiki/Charles_Joseph_Minard), whose 1869 visualization of Napoleon's Russian campaign remains "the best statistical graphic ever drawn" - showing army size, location, direction, temperature, and time in a single view.

Like Minard's chart tracked the Grande Armée's losses from 422,000 to 10,000, Minard (the tool) tracks your codebase: dead modules, orphaned exports, unreachable routes, coupling hotspots. At a glance, on a laptop screen.

## Core Conceits

### Powers of Ten

Navigate code at every scale:
- **Package Set** → What's in my dependency universe?
- **Package** → What modules comprise this library?
- **Module** → What does this module export and import?
- **Declaration** → What calls this function? What does it call?
- **Call Site** → Show me exactly where

Each level answers different questions. The tool lets you zoom fluidly between them.

### Fog of War

Modern development with LLM assistance creates a novel visibility problem:

| Actor | Problem | Without Minard |
|-------|---------|----------------|
| **Human** | Changes are voluminous, hard to track details | "I think we deleted that... or did we?" |
| **LLM** | Context window amnesia, loses history between sessions | "Let me grep for that again..." |
| **Both** | Drift between intent and implementation | Ship bugs, accumulate debt |

**Minard's solution**: CLI for the AI (fast, queryable, authoritative), visualization for the human (patterns, anomalies, at-a-glance). Same database, complementary interfaces.

## Components

```
minard/
├── frontend/          # Halogen visualization app
├── server/            # Node.js API over DuckDB
├── database/          # DuckDB schema + loader scripts
├── site-explorer/     # Route analysis + spidering
├── vscode-extension/  # Editor integration
└── loader/            # (planned) Fast Rust/Go data ingestion
```

### Frontend (Halogen + Hylograph)

Interactive visualization for exploring codebases:
- Force-directed module dependency graphs
- Treemaps by package, LOC, declaration count
- Beeswarm plots for topological layers
- Bubble packs for namespace hierarchy
- Chord diagrams for coupling

### Server (PureScript + HTTPurple)

REST API over the database:
- `/api/v2/packages` - List packages with stats
- `/api/v2/modules` - List modules with imports
- `/api/v2/all-imports` - Bulk import graph
- `/api/v2/declarations/search/:query` - Type-aware search

### Database (DuckDB)

Rich schema supporting:
- Multiple projects, multiple snapshots per project
- Package versions as first-class identity
- Namespace hierarchy independent of packages
- Type signatures as rendered text AND queryable AST
- Module imports, function calls, git metrics
- Route definitions and spider results

### Site Explorer

Dual-mode route analysis for Halogen SPAs:
- **Static**: Parse route ADT, extract component mappings
- **Dynamic**: Puppeteer spidering, discover actual navigation
- **Comparison**: Find unreachable routes, extra routes, orphaned modules

### VS Code Extension

Jump from visualization to source code.

## Quick Start

```bash
# Build everything
cd apps/minard
npm install
spago build

# Load a project
node database/loader/ce-loader.js load --project my-app --path /path/to/project

# Start the server
node server/run.js

# Start the frontend (dev)
cd frontend && npm run serve
```

## Status

- **Frontend**: Production-ready, deployed
- **Server**: Production-ready, deployed
- **Database**: Stable schema (v3), well-documented
- **Site Explorer**: Functional, needs integration
- **Loader**: Works, but slow. Rewrite planned (Rust/Go)
- **VS Code Extension**: Proof of concept

## Roadmap

### Near-term
- [ ] Incremental loading (currently ~30s for full reload)
- [ ] Orphan visualization (highlight in force graph)
- [ ] Cross-project analysis (library usage across consumers)
- [ ] CLI expansion: `unused-exports`, `coupling-report`, `churn-report`

### Medium-term
- [ ] MCP server for direct LLM queries
- [ ] Haskell project support
- [ ] Loader rewrite in Rust/Go

### Future
- [ ] Demo with the original Minard dataset (stdlib-js has it!)
- [ ] Other language support (TypeScript, Rust)

## Philosophy

Minard is opinionated:

1. **Database-first**: Expensive computations happen at load time. Queries are fast.
2. **CLI + Viz**: Not either/or. The AI needs CLI, the human needs visualization. Same truth, different views.
3. **Declarative**: Uses Hylograph (HATS) for visualization. Describes *what*, not *how*.
4. **Multi-scale**: No single view is enough. Powers of Ten navigation is core.

## Etymology

Charles Joseph Minard (1781-1870) was a French civil engineer who pioneered information graphics. His 1869 flow map of Napoleon's 1812 Russian campaign shows:

- The army's size (width of line)
- Geographic position (map)
- Direction of travel (color: tan=advance, black=retreat)
- Temperature during retreat (bottom graph)
- Key events (city labels)

All in one image. This is what we aspire to for code.

## License

MIT

---

*"At least your death march will look good."*
