const duckdb = require('duckdb');
const db = new duckdb.Database('./ce-unified.duckdb', { access_mode: 'READ_ONLY' });

// Full kind breakdown
db.all(`SELECT kind, COUNT(*) as count FROM declarations GROUP BY kind ORDER BY count DESC`, (err, rows) => {
  console.log("\n## DECLARATIONS BY KIND\n");
  rows.forEach(r => console.log(`  ${r.kind}: ${r.count}`));
  const total = rows.reduce((a, r) => a + Number(r.count), 0);
  console.log(`\n  TOTAL: ${total}`);
});

// Children by kind
db.all(`SELECT kind, COUNT(*) as count FROM child_declarations GROUP BY kind ORDER BY count DESC`, (err, rows) => {
  if (err) { console.log("\n## CHILD DECLARATIONS\n  (table not found or different schema)"); return; }
  console.log("\n## CHILD DECLARATIONS (constructors, methods, instances)\n");
  rows.forEach(r => console.log(`  ${r.kind}: ${r.count}`));
  const total = rows.reduce((a, r) => a + Number(r.count), 0);
  console.log(`\n  TOTAL: ${total}`);
});

// Types breakdown (data + newtype + type_synonym)
db.all(`SELECT 
  SUM(CASE WHEN kind = 'data' THEN 1 ELSE 0 END) as data_types,
  SUM(CASE WHEN kind = 'newtype' THEN 1 ELSE 0 END) as newtypes,
  SUM(CASE WHEN kind = 'type_synonym' THEN 1 ELSE 0 END) as type_synonyms,
  SUM(CASE WHEN kind = 'type_class' THEN 1 ELSE 0 END) as type_classes,
  SUM(CASE WHEN kind IN ('data', 'newtype', 'type_synonym') THEN 1 ELSE 0 END) as total_types
FROM declarations`, (err, rows) => {
  console.log("\n## TYPE DEFINITIONS SUMMARY\n");
  const r = rows[0];
  console.log(`  Data types: ${r.data_types}`);
  console.log(`  Newtypes: ${r.newtypes}`);
  console.log(`  Type synonyms: ${r.type_synonyms}`);
  console.log(`  Type classes: ${r.type_classes}`);
  console.log(`  ─────────────────`);
  console.log(`  Total types: ${r.total_types}`);
  
  setTimeout(() => db.close(), 500);
});
