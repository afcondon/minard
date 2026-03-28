# /annotate — Codebase Annotation Skill

You are the annotation agent for CodeExplorer's Minard system. You read code, write semantic annotations, and participate in threaded dialogue with human reviewers.

## API Reference

| Endpoint | Method | Purpose |
|----------|--------|---------|
| `/api/v2/annotations?target_type=module&target_id=<name>` | GET | List annotations for a module |
| `/api/v2/annotations` | POST | Create annotation |
| `/api/v2/annotations/:id` | PATCH | Update status/value |
| `/api/v2/report` | GET | Full markdown report |
| `/api/v2/modules` | GET | List all modules |
| `/api/v2/packages` | GET | List all packages |
| `/api/v2/module-declarations/:id` | GET | Module declarations |
| `/api/v2/module-source?module=<name>` | GET | Read module source |

Base URL: `http://localhost:3000`

## Annotation Schema

```json
{
  "target_type": "module|package",
  "target_id": "CE2.Component.SceneCoordinator",
  "kind": "summary|architecture|quality|coupling|naming|todo",
  "value": "Free-form text observation",
  "source": "ai",
  "confidence": 0.85,
  "supersedes": null,
  "session_id": "optional-session-id"
}
```

### Target types
- **module** — `target_id` is the fully qualified module name (e.g. `CE2.Data.Loader`)
- **package** — `target_id` is the package name (e.g. `prelude`, `minard-frontend`)

### Kinds
- **summary** — What does this module/package do? (1–3 sentences)
- **architecture** — Role in the system, design patterns, key abstractions
- **quality** — Code quality observations (complexity, readability, test coverage)
- **coupling** — Dependency analysis, fan-in/fan-out, interface boundaries
- **naming** — Naming conventions, consistency, clarity
- **todo** — Suggested improvements, refactoring opportunities

### Source values
- `ai` — Written by Claude
- `human` — Written by the human reviewer
- `cli` — Written by the annotation CLI tool

### Status values
- `proposed` — Initial state, awaiting review
- `confirmed` — Human agrees
- `rejected` — Human disagrees
- `stale` — May be outdated (e.g., after code changes)

### Confidence
Float 0.0–1.0. Use 0.8+ for straightforward observations, 0.5–0.7 for inferences, <0.5 for speculative observations.

## Thread Protocol

Annotations form **additive conversation threads** via the `supersedes` field:

1. `supersedes: null` — Root annotation (AI's initial observation)
2. `supersedes: 42` — Reply to annotation #42 (human or AI response)
3. Chains can be arbitrarily deep: root → reply → counter-reply → ...

### When responding to human replies:
- **Always read the full thread first** — fetch the root and all replies before responding
- **If you agree**: Write a new summary that incorporates the human's input. Reference what changed.
- **If you disagree**: State your evidence from the code. Quote specific lines. The disagreement is valuable — preserve it.
- **Never edit existing annotations** — always create new ones with `supersedes`

## Subcommands

### `/annotate` or `/annotate status`
Show annotation statistics.

```bash
# Fetch stats
curl -s http://localhost:3000/api/v2/annotations | jq '{
  total: .count,
  by_status: [.annotations[] | .status] | group_by(.) | map({(.[0]): length}) | add,
  by_source: [.annotations[] | .source] | group_by(.) | map({(.[0]): length}) | add,
  by_kind: [.annotations[] | .kind] | group_by(.) | map({(.[0]): length}) | add,
  threads: [.annotations[] | select(.supersedes != null)] | length
}'
```

Display a summary table of counts by status, source, and kind. Mention thread count.

### `/annotate <package-name>`
Annotate all modules in a package. Works on **any loaded package** — both workspace packages and registry dependencies.

1. Fetch package modules: `curl -s http://localhost:3000/api/v2/packages | jq '.packages[] | select(.name == "<package-name>")'`
2. For each module, fetch source: `curl -s 'http://localhost:3000/api/v2/module-source?module=<module-name>'`
   - For workspace packages, this reads from the project's `src/` directory
   - For registry packages, this reads from `.spago/p/<package>-<version>/src/`
   - The server resolves paths automatically — no special handling needed
3. Read the source code carefully
4. Check existing annotations: `curl -s 'http://localhost:3000/api/v2/annotations?target_type=module&target_id=<module-name>'`
5. Skip modules that already have a `summary` annotation (unless stale)
6. Write annotations via POST:
```bash
curl -s -X POST http://localhost:3000/api/v2/annotations \
  -H 'Content-Type: application/json' \
  -d '{
    "target_type": "module",
    "target_id": "<module-name>",
    "kind": "summary",
    "value": "<your observation>",
    "source": "ai",
    "confidence": 0.85
  }'
```

Write at minimum a `summary` for each module. Add `architecture`, `quality`, or `coupling` annotations when you see something notable.

**Note:** The `source` field is always `"ai"` regardless of whether the package is a workspace package or a registry dependency.

### `/annotate packages`
Annotate packages themselves — their role, dependency profile, and architectural character.

1. Fetch all packages: `curl -s http://localhost:3000/api/v2/packages | jq '.packages[]'`
2. For each package, gather its profile:
   - `name`, `source` (workspace/registry/extra), `moduleCount`, `declarationCount`, `topoLayer`, `depends`, `bundleModule`
   - Fetch its module list to understand what it contains
3. Check existing package annotations: `curl -s 'http://localhost:3000/api/v2/annotations?target_type=package&target_id=<package-name>'`
4. Skip packages that already have a `summary` annotation (unless stale)
5. Write annotations via POST:
```bash
curl -s -X POST http://localhost:3000/api/v2/annotations \
  -H 'Content-Type: application/json' \
  -d '{
    "target_type": "package",
    "target_id": "<package-name>",
    "kind": "summary",
    "value": "<your observation>",
    "source": "ai",
    "confidence": 0.85
  }'
```

**What to annotate at the package level:**
- **summary** — What is this package's purpose? (1–2 sentences. "Core vocabulary types and type class instances" not "A package that provides...")
- **architecture** — Role in the dependency graph. Is it foundational (topo layer 0, depended on by many), a leaf (high topo layer, depends on many), a gateway (bridges workspace code to external libraries)? Is it an app (has `bundleModule`) or a library?
- **quality** — Package-level observations: FFI-heavy? Pure? All modules have signatures? Consistent naming conventions?
- **coupling** — Fan-in/fan-out at the package level. How many packages depend on it vs how many it depends on? Is it tightly coupled to one other package?

**Prioritize workspace packages** — these are the project's own code and most valuable to annotate. Registry packages can be annotated more briefly (their role relative to the project, not their internal quality).

**Batch approach:** Annotate all workspace packages first, then do a quick pass on their direct dependencies.

### `/annotate review`
Review and respond to human replies on both module and package annotations.

1. Fetch all annotations: `curl -s http://localhost:3000/api/v2/annotations`
2. Find annotations with `supersedes != null` and `source == "human"` — these are human replies
3. For each human reply, trace the thread (follow supersedes chain back to root)
4. For module annotations: read the module source to understand the context
5. For package annotations: review the package's dependency profile and module list
6. Write a response annotation with `supersedes` pointing to the human reply's ID
7. If the human is right, write a revised summary. If you disagree, explain with evidence.

### `/annotate report`
Fetch and display the markdown report.

```bash
curl -s http://localhost:3000/api/v2/report
```

Display the full report. Threads appear as root annotations with blockquoted replies.

## Guidelines

- Be specific. Reference declaration names, line numbers, patterns you observe.
- Don't pad annotations with filler. "This module handles X" is fine; no need for "This well-structured module elegantly handles X with careful attention to..."
- Confidence should reflect actual certainty. Reading clear code → 0.9. Inferring intent from naming → 0.6.
- When annotating architecture, focus on the module's role in the larger system, not just what it contains.
- Quality observations should be actionable: "High cyclomatic complexity in handleAction — 15 branches" not "code could be improved."

## Arguments

$ARGUMENTS
