#!/usr/bin/env node
// Turn the PRIVATE per-commit export (git-activity/collect.sh, run on each machine) into
// the PUBLIC daily totals the Git view reads.
//
//     node marginalia/build-git-activity.mjs
//     node marginalia/build-git-activity.mjs --in git-activity/private --no-api
//
// Inputs (all read-only):
//   git-activity/private/*.tsv   one export per machine (repo paths, subjects, file paths).
//   tree.json                    the map, for umbrellas (as build-public-timeline.mjs).
//   Marginalia                   GET /api/projects and each project's sourcePath, to put a
//                                repo under a map umbrella. Optional.
//   overrides.json               "gitUmbrella": repo path prefix -> umbrella id, for repos
//                                Marginalia's tree doesn't place (see overrides.example.json).
//
// Outputs:
//   public/git-activity.json          the viewer's data contract (below).
//   git-activity/private/report.md    what was counted, what was left out and why. Local only.
//
// PRIVACY: a published day carries ONLY its date, commit counts, line counts and the same
// split by umbrella id. No repo, path, subject, hash or co-author name leaves this script;
// `publicDay` below is the whitelist.
//
// WHAT COUNTS AS A LINE: lines of hand-maintained source (by extension) and of prose
// (Markdown), never generated output, lockfiles, data, bundles or built sites. A commit is
// counted once even when it sits in several clones (MBP and Mini, worktrees, archived
// copies), keyed by author date + subject, since copies of a repo can rewrite hashes.
// A commit that changes more than BULK_LINES of source (or of prose) in one go is an
// import or a reorganisation, not a day's writing: its commit counts, its lines do not.
// Machine-made commits (the ~/.claude backup, "auto:" snapshots) are not counted at all.

import fs from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';

const HERE = path.dirname(fileURLToPath(import.meta.url));
const args = process.argv.slice(2);
const flag = (name, dflt) => {
  const i = args.indexOf(name);
  return i >= 0 && args[i + 1] ? args[i + 1] : dflt;
};
const IN = flag('--in', path.join(HERE, 'git-activity/private'));
const TREE = flag('--tree', path.resolve(HERE, '../../../cloudflare-sites/andrewcondon/map/tree.json'));
const OUT = flag('--out', path.join(HERE, 'public/git-activity.json'));
const REPORT = flag('--report', path.join(IN, 'report.md'));
const OVERRIDES = flag('--overrides', path.join(HERE, 'overrides.json'));
const API = (process.env.MARGINALIA_API || 'http://andrews-mac-mini:3100').replace(/\/$/, '');
const USE_API = !args.includes('--no-api');

// Same year zero as the timeline (build-public-timeline.mjs).
const YEAR_ZERO = '2025-10-14';
const BULK_LINES = 10000; // source lines changed in one commit
const BULK_FILE_LINES = 4000; // source lines changed in one file by one commit

const SOURCE = new Set(
  'purs erl hrl rs py go hs jl cc cpp c h hpp ts tsx mjs cjs js css sh sql lua dhall nix swift el scd sc tidal lean tla rb java kt zig'.split(' '),
);
const SOURCE_NAMES = new Set(['Makefile']);
const PROSE = new Set(['md', 'adoc']);
// Built or vendored: compiler output (output/, output-erl/, output-py/...), bundles,
// minified files, lockfiles, dependency trees, built sites.
const GENERATED =
  /(^|\/)(output[\w-]*|dist|build|_build|node_modules|vendor|vendored|third_party|\.spago|public|generated|target|site-built|deps|static)\/|bundle|\.min\.|-lock\.|(^|\/)package-lock\.json$/;
// Whole repos that hold built artefacts or other people's packages.
const NOT_WRITTEN_HERE = /(^|\/)(cloudflare-sites|registry-index)(\/|$)/;
// Commits made by a hook or a timer, not by a session: the claude-state backup repo
// (a snapshot of ~/.claude after every turn) and anything whose subject says "auto:".
const MACHINE_MADE = (repo, subject) => /(^|\/)claude-state$/.test(repo) || /^auto:/i.test(subject);

// ---------------------------------------------------------------------------
// Read the exports

const files = fs.readdirSync(IN).filter((f) => f.endsWith('.tsv')).sort();
if (!files.length) throw new Error(`no .tsv exports in ${IN} (run git-activity/collect.sh)`);

const commits = new Map(); // date+subject -> commit
let copies = 0;
for (const f of files) {
  let cur = null;
  for (const line of fs.readFileSync(path.join(IN, f), 'utf8').split('\n')) {
    if (line.startsWith('@@')) {
      const [repo, hash, date, coauthors = '', subject = ''] = line.slice(2).split('\t');
      const key = `${date}\t${subject}`;
      if (commits.has(key)) {
        copies++;
        cur = null;
        continue;
      }
      cur = { repo, hash, date, claude: /anthropic|claude/i.test(coauthors), files: [], machine: f.replace(/\.tsv$/, ''), auto: MACHINE_MADE(repo, subject) };
      commits.set(key, cur);
    } else if (line && cur) {
      const [a, d, ...rest] = line.split('\t');
      if (a === '-') continue; // binary
      cur.files.push({ added: Number(a), deleted: Number(d), path: renamedTo(rest.join('\t')) });
    }
  }
}

// numstat writes a rename as "dir/{old => new}/f" or "old => new"; keep the new path.
function renamedTo(p) {
  if (!p.includes('=>')) return p;
  return p.replace(/\{[^{}]*? => ([^{}]*)\}/g, '$1').replace(/^.* => /, '').replace(/\/\//g, '/');
}

function classify(repo, file) {
  if (NOT_WRITTEN_HERE.test(repo) || GENERATED.test(file.path)) return 'generated';
  const base = file.path.split('/').pop();
  const ext = base.includes('.') ? base.split('.').pop() : base;
  if (SOURCE.has(ext) || SOURCE_NAMES.has(base)) {
    return file.added + file.deleted > BULK_FILE_LINES ? 'bulk' : 'code';
  }
  if (PROSE.has(ext)) return 'prose';
  return 'other';
}

// ---------------------------------------------------------------------------
// Umbrellas: repo -> Marginalia project (by sourcePath) -> nearest map project -> umbrella

const tree = JSON.parse(fs.readFileSync(TREE, 'utf8'));
const umbrellas = tree.nodes.children.map((u) => ({ id: u.id, name: u.name }));
const onMap = new Map();
for (const u of tree.nodes.children) {
  (function walk(n) {
    onMap.set(n.id, u.id);
    n.children.forEach(walk);
  })(u);
}

let projects = new Map(); // id -> { id, name, parentId, sourcePath }
let apiNote = 'not used (--no-api)';
if (USE_API) {
  try {
    const get = async (p) => {
      const res = await fetch(`${API}${p}`, { signal: AbortSignal.timeout(8000) });
      if (!res.ok) throw new Error(`HTTP ${res.status} for ${p}`);
      return res.json();
    };
    const list = (await get('/api/projects')).projects;
    const detail = [];
    for (let i = 0; i < list.length; i += 8) {
      detail.push(...(await Promise.all(list.slice(i, i + 8).map((p) => get(`/api/projects/${p.id}`)))));
    }
    projects = new Map(
      detail.map((d) => {
        const p = d.project || d;
        return [p.id, { id: p.id, name: p.name, parentId: p.parentId, sourcePath: workspacePath(p.sourcePath) }];
      }),
    );
    apiNote = `${projects.size} projects from ${API}`;
  } catch (e) {
    apiNote = `unreachable (${e.message}); no umbrella split`;
  }
}

// A sourcePath relative to ~/work, the way collect.sh names repos ("afc-work/music/reef").
function workspacePath(p) {
  if (!p) return null;
  const s = String(p)
    .replace(/^\/Users\/[^/]+\/work\//, '')
    .replace(/^~\/work\//, '')
    .replace(/\/$/, '');
  return s.startsWith('afc-work/') || s.startsWith('/') ? s : `afc-work/${s}`;
}

function umbrellaOfProject(id) {
  const seen = new Set();
  let q = projects.get(id);
  while (q && !seen.has(q.id)) {
    seen.add(q.id);
    if (onMap.has(q.id)) return onMap.get(q.id);
    q = q.parentId != null ? projects.get(q.parentId) : null;
  }
  return null;
}

// The project whose sourcePath is the repo, or the nearest folder above it; failing that,
// the projects that live inside the repo, if they agree on one umbrella.
const gitUmbrella = Object.fromEntries(
  Object.entries((fs.existsSync(OVERRIDES) ? JSON.parse(fs.readFileSync(OVERRIDES, 'utf8')) : {}).gitUmbrella || {})
    .filter(([k]) => !k.startsWith('_')),
);
for (const [k, u] of Object.entries(gitUmbrella)) {
  if (!umbrellas.some((x) => x.id === u)) throw new Error(`overrides.gitUmbrella["${k}"] = ${u}, which is not an umbrella on the map`);
}
const repoUmbrella = new Map();
function umbrellaOfRepo(repo) {
  if (repoUmbrella.has(repo)) return repoUmbrella.get(repo);
  let best = null;
  for (const p of projects.values()) {
    const s = p.sourcePath;
    if (!s) continue;
    if ((repo === s || repo.startsWith(s + '/')) && (!best || s.length > best.sourcePath.length)) best = p;
  }
  let u = best ? umbrellaOfProject(best.id) : null;
  if (u == null) {
    const prefix = Object.keys(gitUmbrella)
      .filter((k) => repo === k || repo.startsWith(k))
      .sort((a, b) => b.length - a.length)[0];
    if (prefix) u = gitUmbrella[prefix];
  }
  if (u == null) {
    const inside = new Set(
      [...projects.values()]
        .filter((p) => p.sourcePath && p.sourcePath.startsWith(repo + '/'))
        .map((p) => umbrellaOfProject(p.id))
        .filter((x) => x != null),
    );
    if (inside.size === 1) u = [...inside][0];
  }
  repoUmbrella.set(repo, u);
  return u;
}

// ---------------------------------------------------------------------------
// Count

const today = new Date().toISOString().slice(0, 10);
const blank = () => ({ commits: 0, claude: 0, codeAdded: 0, codeDeleted: 0, proseAdded: 0, proseDeleted: 0 });
const days = new Map(); // date -> { ...counts, byUmbrella: Map }
const left = { generated: 0, bulkFiles: 0, other: 0, bulkCommits: [], machineMade: 0 };
const repoTotals = new Map();

for (const c of commits.values()) {
  const date = c.date.slice(0, 10); // the author's own local date
  if (date < YEAR_ZERO || date > today) continue;
  if (c.auto) {
    left.machineMade++;
    continue;
  }
  const t = { codeAdded: 0, codeDeleted: 0, proseAdded: 0, proseDeleted: 0 };
  for (const f of c.files) {
    switch (classify(c.repo, f)) {
      case 'code':
        t.codeAdded += f.added;
        t.codeDeleted += f.deleted;
        break;
      case 'prose':
        t.proseAdded += f.added;
        t.proseDeleted += f.deleted;
        break;
      case 'bulk':
        left.bulkFiles += f.added + f.deleted;
        break;
      case 'generated':
        left.generated += f.added + f.deleted;
        break;
      default:
        left.other += f.added + f.deleted;
    }
  }
  const code = t.codeAdded + t.codeDeleted;
  const prose = t.proseAdded + t.proseDeleted;
  if (code > BULK_LINES || prose > BULK_LINES) {
    left.bulkCommits.push({ c, date, lines: code, prose });
    if (code > BULK_LINES) t.codeAdded = t.codeDeleted = 0;
    if (prose > BULK_LINES) t.proseAdded = t.proseDeleted = 0;
  }
  const u = umbrellaOfRepo(c.repo) ?? 0;
  if (!days.has(date)) days.set(date, { ...blank(), byUmbrella: new Map() });
  const day = days.get(date);
  if (!day.byUmbrella.has(u)) day.byUmbrella.set(u, blank());
  for (const into of [day, day.byUmbrella.get(u)]) {
    into.commits++;
    if (c.claude) into.claude++;
    for (const k of Object.keys(t)) into[k] += t[k];
  }
  const r = repoTotals.get(c.repo) || { commits: 0, code: 0, umbrella: u };
  r.commits++;
  r.code += t.codeAdded + t.codeDeleted;
  repoTotals.set(c.repo, r);
}

// ---------------------------------------------------------------------------
// Public fields

const counts = (x) => [x.commits, x.claude, x.codeAdded, x.codeDeleted, x.proseAdded, x.proseDeleted];
const publicDay = (date, d) => ({
  date,
  // [commits, with a Claude co-author, code +, code -, prose +, prose -]
  total: counts(d),
  // the same per umbrella id; 0 = not under an umbrella on the map
  byUmbrella: [...d.byUmbrella.entries()].sort((a, b) => a[0] - b[0]).map(([u, x]) => [u, ...counts(x)]),
});

const published = [...days.keys()].sort().map((d) => publicDay(d, days.get(d)));
const ALLOWED = new Set(['date', 'total', 'byUmbrella']);
for (const p of published) {
  for (const k of Object.keys(p)) if (!ALLOWED.has(k)) throw new Error(`refusing to publish field "${k}"`);
}

const sum = (k) => published.reduce((s, p) => s + p.total[k], 0);
const out = {
  period: { from: YEAR_ZERO, to: today },
  generatedAt: today,
  umbrellas,
  totals: {
    commits: sum(0),
    claude: sum(1),
    codeAdded: sum(2),
    codeDeleted: sum(3),
    proseAdded: sum(4),
    proseDeleted: sum(5),
    bulkCommits: left.bulkCommits.length,
    // worktrees and clones of one repo count once
    repos: new Set([...repoTotals.keys()].map((r) => r.replace(/\/\.worktrees\/.*$/, '').split('/').pop())).size,
  },
  days: published,
};
fs.mkdirSync(path.dirname(OUT), { recursive: true });
fs.writeFileSync(OUT, JSON.stringify(out) + '\n');

// ---------------------------------------------------------------------------
// report.md (private)

const n = (x) => x.toLocaleString('en-GB');
const umbrellaName = new Map([[0, '(none)'], ...umbrellas.map((u) => [u.id, u.name])]);
const L = [];
L.push('# Git activity: what was counted');
L.push('');
L.push(`Generated ${today} by \`build-git-activity.mjs\`. **Local only; never publish it** (it names repos and commits).`);
L.push('');
L.push(`- Exports: ${files.join(', ')}; ${n(commits.size)} distinct commits, ${n(copies)} copies dropped, ${n(left.machineMade)} machine-made commits not counted.`);
L.push(`- Marginalia: ${apiNote}`);
L.push(`- Published: ${n(out.totals.commits)} commits on ${published.length} days since ${YEAR_ZERO}, ${n(out.totals.claude)} with a Claude co-author.`);
L.push(`- Code +${n(out.totals.codeAdded)} −${n(out.totals.codeDeleted)}; prose +${n(out.totals.proseAdded)} −${n(out.totals.proseDeleted)}.`);
L.push(`- Left out: generated/built ${n(left.generated)} lines, other file types (data, JSON, HTML, images) ${n(left.other)}, single files over ${n(BULK_FILE_LINES)} lines ${n(left.bulkFiles)}, and ${left.bulkCommits.length} bulk commits (over ${n(BULK_LINES)} source lines).`);
L.push('');
L.push('## Bulk commits (commit counted, lines not)');
L.push('');
L.push('| Date | Repo | Source lines | Prose lines | Hash |');
L.push('|---|---|---|---|---|');
for (const b of left.bulkCommits.sort((a, b) => b.lines + b.prose - a.lines - a.prose)) L.push(`| ${b.date} | ${b.c.repo} | ${n(b.lines)} | ${n(b.prose)} | ${b.c.hash.slice(0, 9)} (${b.c.machine}) |`);
L.push('');
L.push('## Repos by source lines counted');
L.push('');
L.push('| Repo | Umbrella | Commits | Source lines |');
L.push('|---|---|---|---|');
for (const [r, t] of [...repoTotals.entries()].sort((a, b) => b[1].code - a[1].code || b[1].commits - a[1].commits)) {
  L.push(`| ${r} | ${umbrellaName.get(t.umbrella)} | ${t.commits} | ${n(t.code)} |`);
}
fs.writeFileSync(REPORT, L.join('\n') + '\n');

console.log(`git-activity.json: ${n(out.totals.commits)} commits on ${published.length} days; code +${n(out.totals.codeAdded)} −${n(out.totals.codeDeleted)}`);
console.log(`report.md: ${left.bulkCommits.length} bulk commits left out; Marginalia: ${apiNote}`);
