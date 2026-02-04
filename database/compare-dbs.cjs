const duckdb = require('duckdb');

// Query ce-unified.duckdb
const db1 = new duckdb.Database('./ce-unified.duckdb', { access_mode: 'READ_ONLY' });
db1.all(`SELECT COUNT(*) as count FROM declarations WHERE kind = 'type_class'`, (err, rows) => {
  console.log("ce-unified.duckdb type_class count:", rows[0].count);
  db1.close();
});

// Query code-explorer.db (what server uses)
const db2 = new duckdb.Database('./code-explorer.db', { access_mode: 'READ_ONLY' });
db2.all(`SELECT COUNT(*) as count FROM declarations WHERE kind = 'type_class'`, (err, rows) => {
  console.log("code-explorer.db type_class count:", rows[0].count);
  db2.close();
});
