#!/usr/bin/env node
// The last pass before anything leaves this machine: rewrite every string in the
// public files by the ordered rules in overrides.json "redact" (people's names to
// initials, hosts and paths removed). The rules name the very things being hidden,
// so they live only in the gitignored overrides.json, never in this file.
//
//     node marginalia/redact-public.mjs <file.json|file.html> ...
//
// A rule is [pattern, replacement]; the pattern is a case-sensitive regular
// expression, applied globally, rules in order. In JSON only string values are
// rewritten, and a value that redacts to nothing becomes null (so an optional
// field such as repoUrl simply goes missing). HTML is rewritten as text. A file
// is written back only if something changed.

import fs from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';

const HERE = path.dirname(fileURLToPath(import.meta.url));

export function loadRules(overridesPath = path.join(HERE, 'overrides.json')) {
  if (!fs.existsSync(overridesPath)) return [];
  const rules = JSON.parse(fs.readFileSync(overridesPath, 'utf8')).redact || [];
  return rules
    .filter((r) => Array.isArray(r))
    .map(([pattern, replacement]) => ({ re: new RegExp(pattern, 'g'), replacement }));
}

export const redactText = (rules, s) => rules.reduce((acc, r) => acc.replace(r.re, r.replacement), s);

export function redactJson(rules, v) {
  if (typeof v === 'string') {
    const out = redactText(rules, v);
    if (out === v) return v;
    const tidy = out.replace(/ {2,}/g, ' ').replace(/ ([,.;:)])/g, '$1').trim();
    return tidy === '' ? null : tidy;
  }
  if (Array.isArray(v)) return v.map((x) => redactJson(rules, x));
  if (v && typeof v === 'object') return Object.fromEntries(Object.entries(v).map(([k, x]) => [k, redactJson(rules, x)]));
  return v;
}

if (process.argv[1] === fileURLToPath(import.meta.url)) {
  const rules = loadRules();
  if (!rules.length) {
    console.error('redact-public: no "redact" rules in overrides.json; refusing to call anything clean');
    process.exit(1);
  }
  for (const file of process.argv.slice(2)) {
    const before = fs.readFileSync(file, 'utf8');
    let after = redactText(rules, before);
    if (file.endsWith('.json')) {
      const parsed = JSON.parse(before);
      const clean = redactJson(rules, parsed);
      after = JSON.stringify(clean) === JSON.stringify(parsed) ? before : JSON.stringify(clean) + '\n';
    }
    if (after !== before) {
      fs.writeFileSync(file, after);
      console.log(`redacted ${file}`);
    }
  }
}
