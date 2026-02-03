const duckdb = require('duckdb');
const db = new duckdb.Database('./ce-unified.duckdb', { access_mode: 'READ_ONLY' });

db.all(`SELECT kind, COUNT(*) as count FROM declarations GROUP BY kind ORDER BY count DESC`, (err, rows) => {
  if (err) { console.error(err); process.exit(1); }
  console.log("\n## DECLARATIONS BY KIND (full database)\n");
  rows.forEach(r => console.log(`  ${r.kind}: ${r.count}`));
  const total = rows.reduce((a, r) => a + Number(r.count), 0);
  console.log(`\n  TOTAL: ${total}`);
  db.close();
});
