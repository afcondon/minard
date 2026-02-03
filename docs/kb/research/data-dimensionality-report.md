# Minard Database Dimensionality Report

**Generated**: 2026-02-03
**Status**: active
**Category**: research

## Executive Summary

Analysis of the PureScript ecosystem as captured in the Minard database: **38,461 declarations** across **1,381 modules** in **159 packages**. This report provides the actual dimensions and distributions to inform visualization design.

## Scale at a Glance

| Metric | Count |
|--------|-------|
| Packages | 159 |
| Modules | 1,381 |
| **Declarations** | **38,461** |
| Function calls | 19,210 |
| Child declarations | 14,852 |

---

## Declarations by Kind

| Kind | Count | % | Description |
|------|-------|---|-------------|
| **value** | 28,049 | 73% | Functions and values |
| **type_synonym** | 5,357 | 14% | Type aliases |
| **data** | 3,093 | 8% | Algebraic data types |
| **alias** | 736 | 2% | Operator aliases (`+.`, `>:`, etc.) |
| **type_class** | 686 | 2% | Type classes |
| **unknown** | 540 | 1% | Primitives (Function, Array, Number, etc.) |

**Key insight**: Functions dominate (73%), but the type system is substantial: **8,450 type definitions** (data + synonyms) and **686 type classes**.

---

## Type System Deep Dive

### Type Definitions

| Category | Count |
|----------|-------|
| Data types (ADTs) | 3,093 |
| Type synonyms | 5,357 |
| **Total types** | **8,450** |

### Type Classes

| Metric | Value |
|--------|-------|
| Type classes | 686 |
| Total instances | 4,691 |
| Total class members (methods) | 2,762 |
| **Avg instances per class** | **6.8** |
| **Avg methods per class** | **4.0** |

**Implication**: 686 type classes is surprisingly large - an order of magnitude more than casual intuition suggests. This is a graphable number. A grid view showing all type classes with their method/instance counts would be revealing.

### Constructors

| Metric | Value |
|--------|-------|
| Total constructors | 7,399 |
| Data types | 3,093 |
| **Avg constructors per type** | **2.4** |

Most data types are small (1-3 constructors). Large sum types exist but are exceptional.

---

## Child Declarations

Children are constructors, instances, and class members attached to parent declarations.

| Kind | Count | Attached to |
|------|-------|-------------|
| **constructor** | 7,399 | data types |
| **instance** | 4,691 | type classes |
| **class_member** | 2,762 | type classes |
| **Total** | **14,852** | |

---

## Module Structure

### Modules per Package

| Statistic | Value |
|-----------|-------|
| Min | 1 |
| Max | 197 (web-html) |
| Median | 3 |
| Mean | ~8 |

**Distribution**:
- 43 packages (27%) have exactly 1 module
- 59 packages (37%) have 2-5 modules
- 33 packages (21%) have 6-10 modules
- 24 packages (15%) have 11+ modules

### Declarations per Module

| Statistic | Value |
|-----------|-------|
| Min | 1 |
| Max | 265 |
| Median | 6 |
| Mean | 11 |

**Distribution**:
- 629 modules (46%) have 1-5 declarations
- 285 modules (21%) have 6-10 declarations
- 234 modules (17%) have 11-20 declarations
- 233 modules (17%) have 21+ declarations

### LOC per Module

| Statistic | Value |
|-----------|-------|
| Min | 5 |
| Max | 5,740 |
| Median | 66 |
| Mean | 139 |

**Distribution**:
- 515 modules (37%) have 1-50 LOC
- 314 modules (23%) have 51-100 LOC
- 277 modules (20%) have 101-200 LOC
- 275 modules (20%) have 201+ LOC

---

## Module Naming Hierarchy

### Depth Distribution

| Depth | Example | Count | % |
|-------|---------|-------|---|
| 1 | `Prelude` | 33 | 2% |
| 2 | `Data.Array` | 348 | 25% |
| 3 | `Data.Array.ST` | 647 | 47% |
| 4 | `Web.HTML.HTMLElement` | 295 | 21% |
| 5+ | `Web.HTML.HTMLElement.Attributes` | 58 | 4% |

### Top-Level Namespaces

| Namespace | Modules |
|-----------|---------|
| Web | 294 |
| Data | 275 |
| Hylograph | 119 |
| Test | 101 |
| Control | 73 |
| Halogen | 52 |
| Node | 43 |
| CSS | 38 |
| Tidal | 37 |
| DOM | 32 |

**Implication**: Namespaces span packages. `Data.*` includes modules from prelude, arrays, maybe, either, and many more. A namespace-based view would reveal cross-package structure.

---

## Function Calls (Dependencies)

| Statistic | Value |
|-----------|-------|
| Modules with recorded calls | 330 (24%) |
| Total call edges | 19,210 |
| Average calls per module | 13 |
| Max calls from one module | 353 |

**Note**: Call data is primarily from workspace packages. Registry packages have sparser call information.

---

## What's Typical vs. Exceptional

| Dimension | Typical | Exceptional |
|-----------|---------|-------------|
| Package size | 1-10 modules | 50+ modules |
| Module size | 5-20 declarations | 100+ declarations |
| Data type | 1-3 constructors | 20+ constructors |
| Type class | 2-5 methods | 15+ methods |
| Instance count | 3-10 per class | 50+ per class |
| Module LOC | 50-200 | 1000+ |
| Module depth | 2-3 levels | 5+ levels |

---

## Visualization Opportunities

### Already Built
- Package treemap (sized by LOC)
- Module circle packing (colored by kind)
- Dependency links between declarations

### Proposed Views

1. **Type Class Grid**
   - 686 type classes in a grid
   - Donut chart per class: arcs = methods
   - Center number: instance count
   - Sort by instance count to reveal most-implemented classes

2. **ADT Sunburst**
   - Visual signature for each data type
   - Center = type name
   - Rays = constructors
   - Could be used as glyphs in treemap

3. **Namespace Treemap**
   - Group by Data.*, Control.*, Web.* instead of package
   - Shows cross-package structure
   - Reveals which namespaces are "owned" by one package vs. distributed

4. **Export/Internal Split**
   - Two-column view per module
   - Left: exported declarations
   - Right: internal declarations
   - Colored dots by kind

5. **Outlier Badges**
   - Flag modules/packages exceeding typical dimensions
   - Large module badge (50+ decls)
   - Complex type badge (10+ constructors)
   - Heavy dependency badge

6. **Linting Views**
   - Exported FFI (potential security/maintenance concern)
   - Orphan instances (instances defined away from type and class)
   - Dead code detection

---

## Key Ratios (Quick Reference)

| Ratio | Value |
|-------|-------|
| Declarations per module | 28 avg, 6 median |
| Modules per package | 8 avg, 3 median |
| Constructors per data type | 2.4 avg |
| Instances per type class | 6.8 avg |
| Methods per type class | 4.0 avg |
| Functions : Types : Classes | 73% : 25% : 2% |

---

## Data Sources

Queried directly from DuckDB (`ce-unified.duckdb`) on 2026-02-03:

```sql
SELECT kind, COUNT(*) FROM declarations GROUP BY kind;
SELECT kind, COUNT(*) FROM child_declarations GROUP BY kind;
```

API endpoints used for module/package structure:
- `/api/v2/modules`
- `/api/v2/packages`
- `/api/v2/all-calls`
