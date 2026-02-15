# Minard CLI Tools

Command-line tools for querying the Minard code cartography server.

## minard-reach

Query module reachability (dead code analysis) for PureScript packages.

Runs the same BFS algorithm as the frontend visualization: for **app packages**
(with a `bundleModule` like `CE2.Main`) it traces from the entry point; for
**library packages** it traces from modules imported by external consumers.

### Prerequisites

- Python 3 (no external dependencies)
- Minard server running (`npm run serve:api`, default port 3000)

### Usage

```
minard-reach                          # summary table of all packages
minard-reach prelude                  # detailed view for one package
minard-reach selection                # fuzzy name matching
minard-reach --json prelude           # machine-readable JSON output
minard-reach --unreachable            # list dead modules across all packages
minard-reach --json                   # full JSON report
minard-reach --server http://host:3000  # custom server URL
minard-reach --project 2              # scope to a specific project
```

### Example output

```
$ minard-reach minard-frontend

  minard-frontend (workspace 0.0.0) [APP]
    █████████████████████████░░░░░ 38/46 reachable (83%)
    Entry points: 1
      → CE2.Main
    Unreachable (8):
      ✗ CE2.Color
      ✗ CE2.Component.CirclePackViz
      ✗ CE2.Component.ModuleTreemapViz
      ...
```

### JSON output (for LLM assistants)

```
$ minard-reach --json prelude
{
  "packageName": "prelude",
  "isApp": false,
  "totalModules": 49,
  "reachableCount": 25,
  "unreachableCount": 24,
  "entryPoints": ["Control.Applicative", "Control.Apply", ...],
  "reachable": [...],
  "unreachable": ["Data.BooleanAlgebra", "Data.Bounded", ...]
}
```
