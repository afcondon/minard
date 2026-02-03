# Type Class Visualization Plan

**Created**: 2026-02-03
**Status**: active
**Category**: plan

## Vision

A single-screen view of all type classes in a package set, revealing the type system structure at a glance. This could open up a whole new way to explore codebases - complementing the current module/package views with a type-centric perspective.

## Current State

### Data Available
- **39 unique type classes** in workspace (after deduplication)
- **686 type class declarations** before dedup (same classes across 21 project snapshots)
- Superclass relationships captured in `declarations.superclasses` JSON column
- Method counts via `child_declarations` (kind = 'class_member')
- Instance counts via `type_class_instances` table (extracted from `instance_type` field)

### API Endpoint
`GET /api/v2/type-class-stats` returns:
```json
{
  "typeClasses": [
    { "id": 859, "name": "ToAttributeValue", "moduleName": "...", "packageName": "...",
      "methodCount": 1, "instanceCount": 288 }
  ],
  "count": 39,
  "summary": { "totalMethods": 157, "totalInstances": 1838, ... }
}
```

### Known Limitations
1. **Workspace only**: Rust loader only parses local project sources, not registry dependencies
2. **Duplication**: Multi-snapshot schema causes same packages to appear multiple times
3. **Instance extraction**: Class name parsed from `instance_type` field (e.g., "Eq MyType" → "Eq")

## Proposed Visualizations

### 1. Type Class Grid (Priority)
A grid of all type classes, each cell showing:
- Class name
- Small donut/ring: arcs = methods
- Center number: instance count
- Color coding by category (mathematical vs DSL vs Prim)

Sort options:
- By instance count (most implemented first)
- By method count (largest algebras first)
- Alphabetically
- By module/package

Click to expand: show all methods, list instances

### 2. Type Class Hierarchy Tree (Backlog)
For finally-tagless DSL classes that form grammar trees:
- Root = base expression class
- Children = specialized expression classes
- Visualize as collapsible tree or sunburst

Data available via `superclasses` JSON:
```json
{"constraintClass": [["Hylograph","TreeDSL"], "TreeDSL"], ...}
```

Example hierarchies discovered:
- `TreeDSL` → `ShapeTreeDSL`
- `DataDSL` → `TrigDSL`
- `Monad` → `SelectionM`, `TransitionM`

### 3. Instance Network (Future)
- Nodes = types with instances
- Edges = shared type class implementations
- Clusters reveal "type families" that implement similar interfaces

## Implementation Steps

### Phase 1: Basic Grid
1. Fetch type class stats from API
2. Render as CSS grid of cards
3. Each card: name, method count badge, instance count badge
4. Sort by instance count descending

### Phase 2: Visual Encoding
1. Add mini donut chart per card (methods as arcs)
2. Color by category:
   - Blue: Prim.* (compiler builtins)
   - Green: mathematical (Functor, Monad patterns)
   - Orange: DSL/grammar classes
3. Size cards by instance count

### Phase 3: Interaction
1. Hover: show method names
2. Click: expand to show full method signatures + instance list
3. Filter by package/module

### Phase 4: Hierarchy View
1. Parse superclass relationships
2. Build tree structure
3. Render as collapsible tree or radial layout
4. Toggle between grid and tree views

## Data Model Improvements Needed

### For Package Set Coverage
- Update Rust loader to parse registry package sources (not just workspace)
- Or: import type class data from pursuit/registry metadata

### For Better Deduplication
- Single-snapshot model: analyze package set + local projects as one unit
- Deduplicate at load time, not query time

## Questions to Explore

1. What's the distribution of method counts? (Most classes have 1-3 methods?)
2. Which classes have the most instances? (Eq, Show, Functor?)
3. Do DSL classes cluster together in the hierarchy?
4. Can we detect "orphan instances" (instance defined away from type and class)?

## Related Work

- Hoogle: search by type signature
- Pursuit: documentation browser
- This: structural overview of type system
