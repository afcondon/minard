const duckdb = require('duckdb');
const db = new duckdb.Database('./ce-unified.duckdb', { access_mode: 'READ_ONLY' });

// Sample alias declarations
db.all(`SELECT name, kind FROM declarations WHERE kind = 'alias' LIMIT 10`, (err, rows) => {
  console.log("\n## SAMPLE 'alias' DECLARATIONS\n");
  rows.forEach(r => console.log(`  ${r.name}`));
});

// Sample unknown declarations
db.all(`SELECT name, kind FROM declarations WHERE kind = 'unknown' LIMIT 10`, (err, rows) => {
  console.log("\n## SAMPLE 'unknown' DECLARATIONS\n");
  rows.forEach(r => console.log(`  ${r.name}`));
});

// Check if newtype exists under different name
db.all(`SELECT DISTINCT kind FROM declarations`, (err, rows) => {
  console.log("\n## ALL DECLARATION KINDS\n");
  rows.forEach(r => console.log(`  ${r.kind}`));
  setTimeout(() => db.close(), 300);
});
