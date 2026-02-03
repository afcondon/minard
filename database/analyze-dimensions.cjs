// Dimensionality analysis of the minard database
const Database = require('better-sqlite3');
const db = new Database('./code-explorer.db', { readonly: true });

function percentile(arr, p) {
  const sorted = [...arr].sort((a, b) => a - b);
  const idx = Math.floor(sorted.length * p);
  return sorted[idx];
}

function stats(arr, name) {
  if (arr.length === 0) return { name, count: 0 };
  const sum = arr.reduce((a, b) => a + b, 0);
  const mean = sum / arr.length;
  const sorted = [...arr].sort((a, b) => a - b);
  return {
    name,
    count: arr.length,
    min: sorted[0],
    max: sorted[sorted.length - 1],
    mean: mean.toFixed(2),
    median: sorted[Math.floor(sorted.length / 2)],
    p90: percentile(arr, 0.9),
    p99: percentile(arr, 0.99)
  };
}

function histogram(arr, buckets = 10) {
  if (arr.length === 0) return [];
  const min = Math.min(...arr);
  const max = Math.max(...arr);
  const range = max - min || 1;
  const bucketSize = range / buckets;
  const hist = new Array(buckets).fill(0);
  arr.forEach(v => {
    const idx = Math.min(Math.floor((v - min) / bucketSize), buckets - 1);
    hist[idx]++;
  });
  return hist.map((count, i) => ({
    range: `${(min + i * bucketSize).toFixed(0)}-${(min + (i + 1) * bucketSize).toFixed(0)}`,
    count,
    pct: (count / arr.length * 100).toFixed(1) + '%'
  }));
}

console.log("=".repeat(80));
console.log("MINARD DATABASE DIMENSIONALITY ANALYSIS");
console.log("=".repeat(80));

// Basic counts
console.log("\n## BASIC COUNTS\n");
const tables = ['packages', 'modules', 'declarations', 'function_calls', 'module_imports'];
tables.forEach(t => {
  try {
    const count = db.prepare(`SELECT COUNT(*) as c FROM ${t}`).get().c;
    console.log(`${t}: ${count}`);
  } catch (e) {
    console.log(`${t}: (table not found)`);
  }
});

// Declarations by kind
console.log("\n## DECLARATIONS BY KIND\n");
const byKind = db.prepare(`SELECT kind, COUNT(*) as c FROM declarations GROUP BY kind ORDER BY c DESC`).all();
byKind.forEach(r => console.log(`  ${r.kind}: ${r.c}`));

// Modules per package
console.log("\n## MODULES PER PACKAGE\n");
const modulesPerPkg = db.prepare(`SELECT package_id, COUNT(*) as c FROM modules GROUP BY package_id`).all().map(r => r.c);
const mppStats = stats(modulesPerPkg, "modules_per_package");
console.log(JSON.stringify(mppStats, null, 2));

// Declarations per module
console.log("\n## DECLARATIONS PER MODULE\n");
const declsPerModule = db.prepare(`SELECT module_id, COUNT(*) as c FROM declarations GROUP BY module_id`).all().map(r => r.c);
const dpmStats = stats(declsPerModule, "declarations_per_module");
console.log(JSON.stringify(dpmStats, null, 2));
console.log("\nHistogram:");
histogram(declsPerModule, 8).forEach(b => console.log(`  ${b.range}: ${b.count} (${b.pct})`));

// Declarations per module BY KIND
console.log("\n## DECLARATIONS PER MODULE BY KIND\n");
['value', 'data', 'newtype', 'type_class', 'type_synonym', 'foreign'].forEach(kind => {
  const counts = db.prepare(`SELECT module_id, COUNT(*) as c FROM declarations WHERE kind = ? GROUP BY module_id`).all(kind).map(r => r.c);
  if (counts.length > 0) {
    const s = stats(counts, kind);
    console.log(`${kind}: mean=${s.mean}, median=${s.median}, max=${s.max}, p90=${s.p90}`);
  }
});

// Children per declaration (constructors, methods, instances)
console.log("\n## CHILDREN PER DECLARATION (constructors, methods, instances)\n");
try {
  const childrenPerDecl = db.prepare(`SELECT declaration_id, COUNT(*) as c FROM child_declarations GROUP BY declaration_id`).all().map(r => r.c);
  const cpdStats = stats(childrenPerDecl, "children_per_declaration");
  console.log(JSON.stringify(cpdStats, null, 2));
} catch (e) {
  // Try alternate approach - count from declarations table if children stored differently
  const declsWithChildren = db.prepare(`SELECT id, (SELECT COUNT(*) FROM child_declarations WHERE declaration_id = declarations.id) as c FROM declarations`).all();
  console.log("(child_declarations table structure may differ)");
}

// LOC per module
console.log("\n## LOC PER MODULE\n");
const locPerModule = db.prepare(`SELECT loc FROM modules WHERE loc IS NOT NULL AND loc > 0`).all().map(r => r.loc);
const locStats = stats(locPerModule, "loc_per_module");
console.log(JSON.stringify(locStats, null, 2));
console.log("\nHistogram:");
histogram(locPerModule, 10).forEach(b => console.log(`  ${b.range}: ${b.count} (${b.pct})`));

// Function calls - in/out degree
console.log("\n## FUNCTION CALL GRAPH DEGREE\n");
const outDegree = db.prepare(`SELECT caller_module_id, COUNT(*) as c FROM function_calls GROUP BY caller_module_id`).all().map(r => r.c);
const inDegree = db.prepare(`SELECT callee_module, COUNT(*) as c FROM function_calls GROUP BY callee_module`).all().map(r => r.c);
console.log("Out-degree (calls made):", JSON.stringify(stats(outDegree, "out_degree")));
console.log("In-degree (calls received):", JSON.stringify(stats(inDegree, "in_degree")));

// Module naming depth
console.log("\n## MODULE NAMING DEPTH\n");
const depths = db.prepare(`SELECT name FROM modules`).all().map(r => r.name.split('.').length);
const depthStats = stats(depths, "naming_depth");
console.log(JSON.stringify(depthStats, null, 2));
console.log("\nDistribution:");
const depthDist = {};
depths.forEach(d => depthDist[d] = (depthDist[d] || 0) + 1);
Object.entries(depthDist).sort((a,b) => a[0] - b[0]).forEach(([d, c]) => console.log(`  depth ${d}: ${c} modules`));

// Top-level namespaces
console.log("\n## TOP-LEVEL NAMESPACES\n");
const namespaces = db.prepare(`SELECT name FROM modules`).all().map(r => r.name.split('.')[0]);
const nsDist = {};
namespaces.forEach(ns => nsDist[ns] = (nsDist[ns] || 0) + 1);
Object.entries(nsDist).sort((a,b) => b[1] - a[1]).slice(0, 15).forEach(([ns, c]) => console.log(`  ${ns}: ${c}`));

// Imports per module
console.log("\n## IMPORTS PER MODULE\n");
const importsPerModule = db.prepare(`SELECT module_id, COUNT(*) as c FROM module_imports GROUP BY module_id`).all().map(r => r.c);
const ipmStats = stats(importsPerModule, "imports_per_module");
console.log(JSON.stringify(ipmStats, null, 2));

// Type signature presence and length
console.log("\n## TYPE SIGNATURES\n");
const withSig = db.prepare(`SELECT COUNT(*) as c FROM declarations WHERE type_signature IS NOT NULL AND type_signature != ''`).get().c;
const withoutSig = db.prepare(`SELECT COUNT(*) as c FROM declarations WHERE type_signature IS NULL OR type_signature = ''`).get().c;
console.log(`With signature: ${withSig}`);
console.log(`Without signature: ${withoutSig}`);
const sigLengths = db.prepare(`SELECT LENGTH(type_signature) as len FROM declarations WHERE type_signature IS NOT NULL AND type_signature != ''`).all().map(r => r.len);
console.log("Signature length:", JSON.stringify(stats(sigLengths, "sig_length")));

// Packages by source
console.log("\n## PACKAGES BY SOURCE\n");
const bySrc = db.prepare(`SELECT source, COUNT(*) as c FROM packages GROUP BY source`).all();
bySrc.forEach(r => console.log(`  ${r.source}: ${r.c}`));

console.log("\n" + "=".repeat(80));
console.log("END OF ANALYSIS");
console.log("=".repeat(80));

db.close();
