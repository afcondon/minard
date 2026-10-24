#!/usr/bin/env node
// Turn the PRIVATE timeline reconstruction into the PUBLIC data the Timeline view reads.
//
//     node marginalia/build-public-timeline.mjs
//     node marginalia/build-public-timeline.mjs --timeline <timeline.json> --tree <tree.json> --no-api
//
// Inputs (all read-only):
//   timeline.json   the reconstruction (~/Downloads/timeline-reconstruction/timeline.json).
//                   Built from private worklogs and prompt histories.
//   tree.json       the map's own data (cloudflare-sites/andrewcondon/map/tree.json). It
//                   defines what is public: a thread is published only if it joins a
//                   project on the map.
//   Marginalia      GET /api/projects (one call, read-only) for the parent chain of a
//                   project that is newer than tree.json or sits below the map's depth.
//                   Optional; without it, joins use tree.json alone.
//   overrides.json  hand-kept, beside this script (see overrides.example.json).
//
// Outputs:
//   public/timeline-public.json   the viewer's data contract (below).
//   excluded.md                   every thread left out and why, plus a privacy review
//                                 list of included threads, for promotion by hand.
//
// PRIVACY IS LOAD-BEARING. A published thread carries ONLY: name, what (one line), kind,
// fate, first, last, days[], umbrella, and the map project it joined (already public in
// tree.json), plus a successor NAME for superseded threads when that successor is itself
// published. Never evidence, pickup, prompts, prompt or commit counts, confidence, aliases,
// or Marginalia status. `publicThread` below is the whitelist: add a field there or it
// does not leave this machine.

import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import { fileURLToPath } from 'node:url';

const HERE = path.dirname(fileURLToPath(import.meta.url));
const args = process.argv.slice(2);
const flag = (name, dflt) => {
  const i = args.indexOf(name);
  return i >= 0 && args[i + 1] ? args[i + 1] : dflt;
};

const TIMELINE = flag('--timeline', path.join(os.homedir(), 'Downloads/timeline-reconstruction/timeline.json'));
const TREE = flag('--tree', path.resolve(HERE, '../../../cloudflare-sites/andrewcondon/map/tree.json'));
const OVERRIDES = flag('--overrides', path.join(HERE, 'overrides.json'));
const OUT = flag('--out', path.join(HERE, 'public/timeline-public.json'));
const EXCLUDED = flag('--excluded', path.join(HERE, 'excluded.md'));
const API = (process.env.MARGINALIA_API || 'http://andrews-mac-mini:3100').replace(/\/$/, '');
const USE_API = !args.includes('--no-api');

const FATES = ['shipped', 'active', 'parked', 'half-done', 'abandoned', 'superseded', 'unknown'];
const KINDS = ['repo', 'feature', 'idea', 'infra', 'doc'];

const readJSON = (p) => JSON.parse(fs.readFileSync(p, 'utf8'));

// ---------------------------------------------------------------------------
// Inputs

const timeline = readJSON(TIMELINE);
const tree = readJSON(TREE);
const overrides = fs.existsSync(OVERRIDES)
  ? readJSON(OVERRIDES)
  : {};
const ov = {
  include: overrides.include || {}, // name -> map project id (any depth)
  exclude: new Set(overrides.exclude || []),
  rename: overrides.rename || {}, // name -> public name
  fate: overrides.fate || {}, // name -> fate
  what: overrides.what || {}, // name -> public one-liner
  successor: overrides.successor || {}, // name -> successor thread name (or null)
};
for (const [n, f] of Object.entries(ov.fate)) {
  if (!FATES.includes(f)) throw new Error(`overrides.fate["${n}"] = "${f}" is not one of ${FATES.join(', ')}`);
}

// The map: umbrella = depth-1 node; every node below it maps to its umbrella.
const umbrellas = tree.nodes.children.map((u) => ({ id: u.id, name: u.name }));
const onMap = new Map(); // project id -> { id, name, umbrella }
for (const u of tree.nodes.children) {
  (function walk(n) {
    onMap.set(n.id, { id: n.id, name: n.name, umbrella: u.id });
    n.children.forEach(walk);
  })(u);
}

let marginalia = new Map(); // id -> { id, name, parentId }
let apiNote = 'not used (--no-api)';
if (USE_API) {
  try {
    const res = await fetch(`${API}/api/projects`, { signal: AbortSignal.timeout(8000) });
    if (!res.ok) throw new Error(`HTTP ${res.status}`);
    const { projects } = await res.json();
    marginalia = new Map(projects.map((p) => [p.id, { id: p.id, name: p.name, parentId: p.parentId }]));
    apiNote = `${projects.length} projects from ${API}`;
  } catch (e) {
    apiNote = `unreachable (${e.message}); joins use tree.json only`;
  }
}
const marginaliaByName = new Map([...marginalia.values()].map((p) => [p.name.toLowerCase(), p]));

// ---------------------------------------------------------------------------
// Joining a thread to the map

// The map project a Marginalia project falls under: itself if on the map, else its nearest
// ancestor that is. Returns { hit, chain } where chain is the Marginalia path walked.
function projectJoin(id) {
  const chain = [];
  const seen = new Set();
  let q = marginalia.get(id) || (onMap.has(id) ? { id, name: onMap.get(id).name } : null);
  while (q && !seen.has(q.id)) {
    seen.add(q.id);
    chain.push(q);
    if (onMap.has(q.id)) return { hit: onMap.get(q.id), chain };
    q = q.parentId != null ? marginalia.get(q.parentId) : null;
  }
  return { hit: null, chain };
}

const byName = new Map(timeline.threads.map((t) => [t.name, t]));

// A thread joins via (1) its own Marginalia id, else (2) its parent: a parent that is a
// thread joins recursively; a parent that is not a thread but names a Marginalia project
// exactly (case-insensitive) joins through that project.
function join(t, seen = new Set()) {
  if (seen.has(t.name)) return { hit: null, why: 'parent cycle' };
  seen.add(t.name);
  let ownMiss = null;
  if (t.marginalia) {
    const { hit, chain } = projectJoin(t.marginalia.id);
    if (hit) return { hit, via: 'marginalia' };
    ownMiss = chain;
  }
  if (t.parent) {
    const pt = byName.get(t.parent);
    if (pt) {
      const r = join(pt, seen);
      if (r.hit) return { hit: r.hit, via: 'parent' };
    } else {
      const pp = marginaliaByName.get(t.parent.toLowerCase());
      if (pp) {
        const { hit } = projectJoin(pp.id);
        if (hit) return { hit, via: 'parent-name' };
      }
    }
  }
  return { hit: null, ownMiss };
}

// The top of a thread's Marginalia ancestry (or its parent's), to say WHERE an excluded
// thread lives, so promoting a whole family is one decision.
function homeOf(t, seen = new Set()) {
  if (seen.has(t.name)) return null;
  seen.add(t.name);
  if (t.marginalia) {
    const { chain } = projectJoin(t.marginalia.id);
    if (chain.length) return chain.map((p) => `${p.name} (${p.id})`).reverse().join(' › ');
    return `${t.marginalia.name} (${t.marginalia.id})`;
  }
  if (t.parent) {
    const pt = byName.get(t.parent);
    if (pt) return homeOf(pt, seen);
    const pp = marginaliaByName.get(t.parent.toLowerCase());
    if (pp) return projectJoin(pp.id).chain.map((p) => `${p.name} (${p.id})`).reverse().join(' › ');
    return `${t.parent} (not a thread, not a Marginalia project)`;
  }
  return null;
}

// ---------------------------------------------------------------------------
// Selection

const included = [];
const excluded = [];
for (const t of timeline.threads) {
  if (ov.exclude.has(t.name)) {
    excluded.push({ t, reason: 'excluded by overrides.json' });
    continue;
  }
  if (ov.include[t.name] != null) {
    const pid = Number(ov.include[t.name]);
    const hit = onMap.get(pid);
    if (!hit) throw new Error(`overrides.include["${t.name}"] = ${pid}, which is not a project on the map`);
    included.push({ t, hit, via: 'override' });
    continue;
  }
  const r = join(t);
  if (r.hit) {
    included.push({ t, hit: r.hit, via: r.via });
  } else {
    const home = homeOf(t);
    const reason = t.marginalia
      ? `Marginalia ${t.marginalia.id} is not on the map; lives under ${home}`
      : t.parent
        ? `no Marginalia link; parent chain ends at ${home}`
        : 'no Marginalia link and no parent';
    excluded.push({ t, reason, home });
  }
}

// ---------------------------------------------------------------------------
// Public fields

const publicName = (t) => ov.rename[t.name] || t.name;
const oneLine = (s) => String(s || '').split(/\r?\n/)[0].replace(/\s+/g, ' ').trim();
const fateOf = (t) => ov.fate[t.name] || (FATES.includes(t.fate) ? t.fate : 'unknown');

// Successor of a superseded thread: an override, else the published thread that Marginalia's
// evolvedInto names, else a published thread NAMED in the evidence just after a cue verb.
// Only the successor's public name leaves this script; the evidence text never does.
const publishedNames = new Set(included.map(({ t }) => t.name));
const byMarginaliaId = new Map();
for (const { t } of included) if (t.marginalia) byMarginaliaId.set(t.marginalia.id, t.name);
const CUE = /(superseded by|replaced by|became|moved into|folded into|absorbed into|rebuilt as|in favour of|passed to|picked up again as|realised|superseded)\s/gi;
const successorGuesses = [];

function successorOf(t) {
  if (Object.prototype.hasOwnProperty.call(ov.successor, t.name)) return ov.successor[t.name];
  const evo = t.marginalia && t.marginalia.evolvedInto;
  if (evo != null) {
    const evoId = typeof evo === 'object' ? evo.id : evo;
    const n = byMarginaliaId.get(evoId);
    if (n && n !== t.name) return n;
  }
  const ev = String(t.evidence || '');
  let best = null;
  for (const m of ev.matchAll(CUE)) {
    const window = ev.slice(m.index + m[0].length, m.index + m[0].length + 48);
    for (const name of publishedNames) {
      if (name === t.name || name.length < 3) continue;
      const at = window.indexOf(name);
      if (at >= 0 && at <= 12 && (!best || name.length > best.length)) best = name;
    }
    if (best) break;
  }
  if (best) successorGuesses.push({ from: t.name, to: best });
  return best;
}

function publicThread({ t, hit }) {
  const fate = fateOf(t);
  const succ = fate === 'superseded' ? successorOf(t) : null;
  const kind = KINDS.includes(t.kind) ? t.kind : 'feature';
  return {
    name: publicName(t),
    what: oneLine(ov.what[t.name] || t.what),
    kind,
    fate,
    first: t.first,
    last: t.last,
    days: [...new Set(t.days)].sort(),
    umbrella: hit.umbrella,
    project: hit.id === hit.umbrella ? null : { id: hit.id, name: hit.name },
    successor: succ && publishedNames.has(succ) ? publicName(byName.get(succ)) : null,
  };
}

const published = included
  .map(publicThread)
  .sort((a, b) => a.first.localeCompare(b.first) || a.name.localeCompare(b.name));

// Belt and braces: the output must contain only whitelisted keys.
const ALLOWED = new Set(['name', 'what', 'kind', 'fate', 'first', 'last', 'days', 'umbrella', 'project', 'successor']);
for (const p of published) {
  for (const k of Object.keys(p)) if (!ALLOWED.has(k)) throw new Error(`refusing to publish field "${k}"`);
}
const dupes = published.map((p) => p.name).filter((n, i, a) => a.indexOf(n) !== i);
if (dupes.length) throw new Error(`duplicate public names after renames: ${dupes.join(', ')}`);

const out = {
  period: { from: timeline.period[0], to: timeline.period[1] },
  generatedAt: new Date().toISOString().slice(0, 10),
  umbrellas,
  threads: published,
};
fs.mkdirSync(path.dirname(OUT), { recursive: true });
fs.writeFileSync(OUT, JSON.stringify(out, null, 1) + '\n');

// ---------------------------------------------------------------------------
// excluded.md

// Words that suggest a line is personal rather than project work. A hit is not a verdict,
// just a line for Andrew to read before the page goes out.
const SENSITIVE = [
  [/\b[\w.+-]+@[\w-]+\.[\w.]+\b/, 'email address'],
  [/\/Users\/|~\//, 'local path'],
  [/\b(Paul|Phil|wife|husband|son|daughter|friend|medical|illness)\b/i, 'a person or personal matter'],
  [/[£$€]\s?\d|\b(price|payment|payments|invoice|salary|sell|selling|revenue|subscription)\b/i, 'money'],
  [/\b(password|token|secret|credential|api key|ssh key)\b/i, 'credentials'],
  [/\b(private|personal|confidential|NDA)\b/i, '"private" or "personal"'],
  [/\b(house|garden|woodworking|kitchen)\b/i, 'home life'],
];
const flagged = [];
for (const p of published) {
  for (const [re, why] of SENSITIVE) {
    if (re.test(p.name) || re.test(p.what)) {
      flagged.push({ p, why });
      break;
    }
  }
}

const FATE_ORDER = ['half-done', 'abandoned', 'parked', 'superseded', 'unknown', 'active', 'shipped'];
const count = (xs, f) => xs.reduce((m, x) => ((m[f(x)] = (m[f(x)] || 0) + 1), m), {});
const fmtCounts = (m) => FATE_ORDER.filter((f) => m[f]).map((f) => `${m[f]} ${f}`).join(', ');
const md = (s) => String(s).replace(/\|/g, '\\|');

const umbrellaName = new Map(umbrellas.map((u) => [u.id, u.name]));
const viaCount = count(included, (x) => x.via);
const lines = [];
lines.push('# Timeline: threads left out of the public page');
lines.push('');
lines.push(`Generated ${out.generatedAt} by \`build-public-timeline.mjs\`. **Local review file; never publish it.**`);
lines.push('');
lines.push(`- Source: \`${TIMELINE}\` (${timeline.threads.length} threads)`);
lines.push(`- Map: \`${TREE}\` (generated ${tree.generatedAt}; umbrellas ${umbrellas.map((u) => `${u.name} ${u.id}`).join(', ')})`);
lines.push(`- Marginalia: ${apiNote}`);
lines.push(`- **Included: ${published.length}** (${fmtCounts(count(published, (p) => p.fate))})`);
lines.push(`  - joined by own Marginalia id ${viaCount.marginalia || 0}, by parent thread ${viaCount.parent || 0}, by parent naming a Marginalia project ${viaCount['parent-name'] || 0}, by override ${viaCount.override || 0}`);
lines.push(`  - per umbrella: ${umbrellas.map((u) => `${u.name} ${published.filter((p) => p.umbrella === u.id).length}`).join(', ')}`);
lines.push(`- **Excluded: ${excluded.length}** (${fmtCounts(count(excluded, (x) => fateOf(x.t)))})`);
lines.push('');
lines.push('To promote a thread, add it to `overrides.json` under `include` with the id of a project on the map');
lines.push('(any depth; it lands in that project\'s umbrella). `exclude`, `rename`, `fate`, `what` and `successor`');
lines.push('are keyed by the thread\'s ORIGINAL name. See `overrides.example.json`. Then re-run the script.');
lines.push('');

lines.push('## Included threads to read before publishing');
lines.push('');
lines.push('A keyword screen over the published name and one-liner. Each hit needs a human yes/no; no hit is not a clearance.');
lines.push('');
if (flagged.length) {
  lines.push('| Thread | Umbrella | Why flagged | What (as published) |');
  lines.push('|---|---|---|---|');
  for (const { p, why } of flagged) lines.push(`| ${md(p.name)} | ${md(umbrellaName.get(p.umbrella))} | ${why} | ${md(p.what)} |`);
} else lines.push('_None._');
lines.push('');

lines.push('## Successors guessed from the evidence');
lines.push('');
lines.push('Superseded threads whose successor was read off the private evidence (only the name is published). Correct any with `successor` in overrides.json.');
lines.push('');
if (successorGuesses.length) for (const g of successorGuesses) lines.push(`- ${md(g.from)} → ${md(g.to)}`);
else lines.push('_None._');
lines.push('');

// Group excluded threads by where they live, largest group first: promoting a family
// (e.g. everything under one Marginalia project) is then one decision.
const groupKey = (x) => {
  if (x.reason.startsWith('excluded by')) return 'Excluded by overrides.json';
  if (!x.home) return 'No Marginalia link and no parent';
  return x.home.split(' › ')[0];
};
const groups = new Map();
for (const x of excluded) {
  const k = groupKey(x);
  if (!groups.has(k)) groups.set(k, []);
  groups.get(k).push(x);
}
lines.push('## Excluded, grouped by where they live');
lines.push('');
const sorted = [...groups.entries()].sort((a, b) => b[1].length - a[1].length || a[0].localeCompare(b[0]));
for (const [k, xs] of sorted) {
  lines.push(`### ${md(k)} (${xs.length}: ${fmtCounts(count(xs, (x) => fateOf(x.t)))})`);
  lines.push('');
  lines.push('| Thread | Fate | Dates | Reason |');
  lines.push('|---|---|---|---|');
  xs.sort((a, b) => FATE_ORDER.indexOf(fateOf(a.t)) - FATE_ORDER.indexOf(fateOf(b.t)) || a.t.first.localeCompare(b.t.first));
  for (const x of xs) lines.push(`| ${md(x.t.name)} | ${fateOf(x.t)} | ${x.t.first} → ${x.t.last} | ${md(x.reason)} |`);
  lines.push('');
}
fs.writeFileSync(EXCLUDED, lines.join('\n'));

console.log(`timeline-public.json: ${published.length} threads (${fmtCounts(count(published, (p) => p.fate))})`);
console.log(`excluded.md: ${excluded.length} excluded, ${flagged.length} flagged for review, ${successorGuesses.length} successors guessed`);
console.log(`Marginalia: ${apiNote}`);
