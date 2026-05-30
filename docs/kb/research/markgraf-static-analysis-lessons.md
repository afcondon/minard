# Static-analysis lessons from the first external codebase (markgraf)

Field report, 2026-05-30. Running the Minard suite on a friend's polyglot
repo (markgraf — PureScript core compiled to JS + Go, plus a TS embed)
surfaced four bugs in the suite's own tooling. Each is graded here by
whether static analysis *could* have caught it, and which suite component
is the natural home. Recorded as grist for the Minard mill.

## 1. Embedded-SQL migration ordering — `MissingTargetColumn`

**The bug.** The server's annotations migration (`server/src/Main.purs`)
ran, in one batch:

```sql
CREATE TABLE IF NOT EXISTS annotations (... project_id INTEGER ...);
CREATE INDEX IF NOT EXISTS idx_annotations_project ON annotations(project_id);
-- (later, separate batch:)
ALTER TABLE annotations ADD COLUMN IF NOT EXISTS project_id INTEGER;
```

On a database where `annotations` already existed *without* `project_id`
(every fresh loader-created DB), the `CREATE TABLE IF NOT EXISTS` is a
**no-op**, so `project_id` is absent when the `CREATE INDEX` references it →
binder error, server crashes on boot. The `ALTER` that would have fixed it
ran too late.

**Catchable? In principle yes — minard-db has the right *shape* but not
this case.** I checked the actual code (`MinardDB.Migration`,
`Migration.SQL`, `Migration.Safety`). `Safety` does scan for
`MissingTargetColumn` — but `checkSchemaRI` only walks `table.foreignKeys`,
so today it catches column-reference integrity for **foreign keys only,
not indexes**. Our bug was a `CREATE INDEX` on a missing column. Four
concrete, ordered gaps stand between the current suite and catching it —
this is a real work item, not a wiring-up:

1. **Parse indexes.** `Migration.SQL` *deliberately* skips indexes (see its
   module header: "What we do NOT handle (deliberately): … indexes …"). It
   must learn `CREATE INDEX [IF NOT EXISTS] name ON table(cols)`.
2. **Model indexes.** The `Migration` ADT (`CreateTable`, `DropTable`,
   `AddColumn`, `DropColumn`, `AddForeignKey`, `DropForeignKey`) has no
   index constructor. Add `CreateIndex { table, columns, ifNotExists }`.
3. **Model `IF NOT EXISTS` no-op semantics — the load-bearing part.** The
   parser currently *parses `IF NOT EXISTS` and throws it away*
   (`Migration.SQL` line ~429 yields a bare `CreateTable`), and
   `applyMigration (CreateTable t)` on an existing table returns
   `Left "already exists"`. Neither models what actually bit us: a
   `CREATE TABLE IF NOT EXISTS` that **silently no-ops on a pre-existing,
   divergent table, leaving the OLD columns** (no `project_id`). Without an
   `ifNotExists` flag and that no-op-keeps-divergent-table rule, the
   analyzer can't even see that `project_id` is absent — it's right there
   in the CREATE text.
4. **Extend the RI check to index columns.** Generalize `checkOneFK`'s
   column-presence logic so `CreateIndex` referencing an absent column
   raises `MissingTargetColumn` too.

With all four, feeding the real annotations migration would report exactly:
*"CREATE INDEX idx_annotations_project references annotations.project_id,
absent because the CREATE TABLE IF NOT EXISTS no-op'd on the pre-existing
table and the ALTER that adds it runs later."* That is the bug.

**Ingestion.** Separately, the migrations live as `DB.exec db """..."""`
string literals in `server/src/Main.purs` (and the loader's
`database/schema/*.sql`). An extractor must lift that embedded SQL into
`Migration.SQL.parseSql`. Minard already parses these source files; the
string-literal SQL is in the AST.

## 2. Divergent multi-site schema definitions

**Root cause behind #1.** The `annotations` table is defined in **two
places** that drifted apart: the loader's schema SQL (no `project_id`) and
the server's boot migration (with `project_id`). Neither is wrong alone;
together they're inconsistent, and the inconsistency is what made the
no-op'd CREATE dangerous.

**Catchable? Yes — cross-artifact, polyglot.** Minard ingests both the
loader (Rust + its `*.sql`) and the server (PureScript + embedded SQL) into
one database. A check could ask: *for each logical table, how many
definition sites exist, and do their column sets agree?* "Table `X` is
CREATEd in N places with differing columns" is a high-signal warning and a
natural Minard-DB ✕ Portolan feature — it only exists because the suite sees
the whole polyglot system, not one language. Generalises beyond SQL:
the same shape catches a type/record defined divergently across an FFI
boundary.

## 3. External-format deserializer totality

**The bug.** The loader's `spago.lock` parser (`spago_lock.rs`) crashed on
an extra-package pinned as a bare version string (`"yoga-json": "5.2.0"`);
its `ExtraPackage` struct only accepted the git/path *object* form. The
format permits both shapes; the deserializer covered one.

**Catchable? Partially — a lint heuristic.** When code deserializes a
*documented external format*, a check can flag fields whose accepted shape
is narrower than the format allows (here: a struct where the schema is
`string | object`). This is the parser-side dual of #2: incomplete coverage
of a known set of input variants. Home: Minard-Rust / a loader lint. Weaker
than #1–#2 (needs a spec of the format's variants to compare against), but
the *smell* — an untagged-enum or struct that handles fewer cases than a
sibling field — is detectable.

## 4. Config-handling inconsistency

**The bug.** The server read its port from `PORT`/`MINARD_API_PORT` (env)
but hard-coded the database path as a source constant. Two sibling
configuration values, handled inconsistently — the hard-coded one is the
one a new user can't override without editing source.

**Catchable? Yes — a cheap heuristic.** "Sibling config-like values (ports,
paths, hosts, URLs) where some are env/config-sourced and a peer is a
literal" is a detectable inconsistency. Minard already distinguishes
effectful FFI (the env reads) from pure constants; a pass over
config-shaped literals adjacent to env lookups would have flagged the
hard-coded `dbPath` sitting next to `getPortFromEnv`.

## Not statically catchable (recorded for honesty)

- **serde_json recursion limit** on deeply-nested generated `docs.json` —
  an input-robustness limit, not a source defect. Fix was
  `disable_recursion_limit()`, not a code-shape change.
- **Incremental build-cache corruption** producing a 404-everything server
  (a *partial* `output/` clean wasn't enough; `rm -rf output` was). A build
  hygiene / environment issue.
- **SDI port collisions** during testing — operational. (Lesson lives in
  agent memory: test local servers on unregistered ports.)

## Priority for the mill

1 and 2 are the prize: both are squarely in the suite's stated ambitions
(minard-db migration safety; polyglot cross-artifact view), and together
they'd have caught the only true clone-and-run blocker before it reached an
external user. #1 is not free — it's the four-step extension above
(parse + model indexes, model `IF NOT EXISTS` no-op semantics, extend the
column-RI check) plus an embedded-SQL ingestion path — but minard-db's
existing `MissingTargetColumn` machinery and Alloy temporal model give it a
running start: the column-reference integrity check and the trace-based
"introduced / resolved / standing" framing are exactly the substrate this
needs. The IF-NOT-EXISTS no-op rule (step 3) is the genuinely new modelling
work and the most interesting, since it's where declared schema and
effective schema diverge.
