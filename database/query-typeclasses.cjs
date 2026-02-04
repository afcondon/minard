const duckdb = require('duckdb');
const db = new duckdb.Database('./ce-unified.duckdb', { access_mode: 'READ_ONLY' });

// Get all type classes with their method and instance counts
db.all(`
  SELECT 
    d.id,
    d.name,
    d.module_id,
    m.name as module_name,
    p.name as package_name,
    COUNT(CASE WHEN c.kind = 'class_member' THEN 1 END) as method_count,
    COUNT(CASE WHEN c.kind = 'instance' THEN 1 END) as instance_count
  FROM declarations d
  JOIN modules m ON d.module_id = m.id
  JOIN packages p ON m.package_id = p.id
  LEFT JOIN child_declarations c ON c.declaration_id = d.id
  WHERE d.kind = 'type_class'
  GROUP BY d.id, d.name, d.module_id, m.name, p.name
  ORDER BY instance_count DESC
  LIMIT 30
`, (err, rows) => {
  if (err) { console.error(err); process.exit(1); }
  
  console.log("\n## TOP 30 TYPE CLASSES BY INSTANCE COUNT\n");
  console.log("| Class | Package | Methods | Instances |");
  console.log("|-------|---------|---------|-----------|");
  rows.forEach(r => {
    console.log(`| ${r.name} | ${r.package_name} | ${r.method_count} | ${r.instance_count} |`);
  });
  
  db.close();
});
