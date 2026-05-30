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

**Catchable? Yes — and minard-db already has the checker.**
`MinardDB.Migration.Safety` scans a `Migration` sequence for exactly
`MissingTargetColumn` (a step references a column not yet present). This bug
is a textbook instance. Two gaps stop it from firing today:

- **No ingestion path from embedded SQL.** minard-db operates on the
  abstract `Migration` ADT; the bug lived in `DB.exec db """..."""` string
  literals in a PureScript file. We need a DDL extractor that lifts embedded
  migration SQL (and the loader's `database/schema/*.sql`) into the
  `Migration` model so `Safety` can run on the *real* migrations, not just
  hand-written fixtures. Minard already parses these source files — the
  string-literal SQL is sitting right there in the AST.
- **`IF NOT EXISTS` no-op semantics aren't modelled.** A naive analyzer sees
  `project_id` in the `CREATE TABLE` and concludes it exists — missing the
  bug. The model needs an `ifNotExists` flag on `CreateTable`/`AddColumn`
  and the rule that *on a pre-existing, divergent table the columns
  declared in a skipped CREATE are NOT guaranteed.* This is the subtle,
  load-bearing part: the bug only exists because of the gap between the
  declared schema and what a no-op'd CREATE actually leaves behind.

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
(minard-db migration safety; polyglot cross-artifact view), the checker for
#1 *already exists* and just needs to be pointed at real ingested SQL, and
together they'd have caught the only true clone-and-run blocker before it
ever reached an external user.
