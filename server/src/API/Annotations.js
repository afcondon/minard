// FFI for API.Annotations
// JSON builders and helpers for annotation CRUD API

// =============================================================================
// JSON Builders
// =============================================================================

export const buildAnnotationsJson = (rows) => {
  const annotations = (rows || []).map(formatAnnotation);
  return JSON.stringify({ annotations, count: annotations.length });
};

export const buildAnnotationJson = (row) => {
  return JSON.stringify(formatAnnotation(row));
};

function formatAnnotation(row) {
  return {
    id: Number(row.id),
    targetType: row.target_type,
    targetId: row.target_id,
    targetId2: row.target_id_2 || null,
    kind: row.kind,
    value: row.value,
    source: row.source,
    confidence: row.confidence != null ? Number(row.confidence) : 1.0,
    status: row.status || 'proposed',
    supersedes: row.supersedes != null ? Number(row.supersedes) : null,
    sessionId: row.session_id || null,
    createdAt: row.created_at ? String(row.created_at) : null
  };
}

// =============================================================================
// Body Parsing
// =============================================================================

// Parse a JSON body string. Returns the parsed object or null on failure.
export const parseAnnotationBody = (bodyStr) => {
  try {
    const obj = JSON.parse(bodyStr);
    if (!obj || typeof obj !== 'object') return null;
    return obj;
  } catch (e) {
    return null;
  }
};

// =============================================================================
// Validation
// =============================================================================

// Validate required fields for annotation creation.
// Returns a normalized object with all fields, or null if validation fails.
export const validateCreateFields = (body) => {
  if (!body.target_type || !body.target_id || !body.kind || !body.value || !body.source) {
    return null;
  }
  return {
    target_type: String(body.target_type),
    target_id: String(body.target_id),
    target_id_2: body.target_id_2 ? String(body.target_id_2) : null,
    kind: String(body.kind),
    value: String(body.value),
    source: String(body.source),
    confidence: body.confidence != null ? Number(body.confidence) : 1.0,
    session_id: body.session_id ? String(body.session_id) : null
  };
};

// =============================================================================
// Report Builder
// =============================================================================

// Build a markdown codebase report from annotation rows and module stats rows.
// annotationRows: array of { target_type, target_id, kind, value, status, source, confidence, package_name, module_name }
// moduleStatsRows: array of { module_name, package_name, decl_count, loc }
export const buildReportMarkdown = (annotationRows) => (moduleStatsRows) => {
  const today = new Date().toISOString().split('T')[0];
  const lines = [];
  lines.push('# Codebase Report');
  lines.push(`*Generated ${today}*`);
  lines.push('');

  // Build module stats lookup: package -> module -> { decl_count, loc }
  const moduleStats = {};
  for (const row of moduleStatsRows || []) {
    const pkg = row.package_name || 'unknown';
    const mod = row.module_name || 'unknown';
    if (!moduleStats[pkg]) moduleStats[pkg] = {};
    moduleStats[pkg][mod] = {
      declCount: Number(row.decl_count) || 0,
      loc: Number(row.loc) || 0
    };
  }

  // Group annotations: package -> module -> kind -> [annotations]
  const tree = {};
  for (const row of annotationRows || []) {
    const pkg = row.package_name || 'unknown';
    const mod = row.target_id || 'unknown';
    const kind = row.kind || 'uncategorized';
    if (!tree[pkg]) tree[pkg] = {};
    if (!tree[pkg][mod]) tree[pkg][mod] = {};
    if (!tree[pkg][mod][kind]) tree[pkg][mod][kind] = [];
    tree[pkg][mod][kind].push({
      value: row.value,
      status: row.status || 'proposed',
      source: row.source || '',
      confidence: row.confidence != null ? Number(row.confidence) : 1.0
    });
  }

  const statusIcon = (s) => {
    switch (s) {
      case 'confirmed': return '\u2713';
      case 'rejected': return '\u2717';
      case 'stale': return '\u26a0';
      default: return '\u2022';
    }
  };

  const packages = Object.keys(tree).sort();
  for (const pkg of packages) {
    lines.push(`## Package: ${pkg}`);
    lines.push('');

    const modules = Object.keys(tree[pkg]).sort();
    for (const mod of modules) {
      const stats = (moduleStats[pkg] && moduleStats[pkg][mod]) || {};
      const declStr = stats.declCount != null ? `${stats.declCount} declarations` : '';
      const locStr = stats.loc ? `${stats.loc} LOC` : '';
      const statsStr = [declStr, locStr].filter(Boolean).join(', ');
      const header = statsStr ? `### ${mod} (${statsStr})` : `### ${mod}`;
      lines.push(header);
      lines.push('');

      const kinds = Object.keys(tree[pkg][mod]).sort();
      for (const kind of kinds) {
        lines.push(`**${kind}**`);
        for (const ann of tree[pkg][mod][kind]) {
          const icon = statusIcon(ann.status);
          const tag = ann.status !== 'proposed' ? ` [${ann.status}]` : '';
          lines.push(`- ${icon} ${ann.value}${tag}`);
        }
        lines.push('');
      }
    }
  }

  // Summary stats
  const total = (annotationRows || []).length;
  const byStatus = {};
  for (const row of annotationRows || []) {
    const s = row.status || 'proposed';
    byStatus[s] = (byStatus[s] || 0) + 1;
  }
  lines.push('---');
  lines.push('');
  lines.push(`**Total annotations:** ${total}`);
  const statusParts = Object.entries(byStatus).map(([s, c]) => `${c} ${s}`);
  if (statusParts.length > 0) {
    lines.push(`**By status:** ${statusParts.join(', ')}`);
  }

  return lines.join('\n');
};

// =============================================================================
// Update SQL Builder
// =============================================================================

// Build SET clause components from a parsed body object.
// Returns { setCols: ["status = ?", ...], setParams: ["confirmed", ...] }
// or null if nothing to update.
export const buildUpdateParts = (body) => {
  const setCols = [];
  const setParams = [];

  if (body.status !== undefined) {
    setCols.push("status = ?");
    setParams.push(String(body.status));
  }
  if (body.confidence !== undefined) {
    setCols.push("confidence = ?");
    setParams.push(Number(body.confidence));
  }
  if (body.value !== undefined) {
    setCols.push("value = ?");
    setParams.push(String(body.value));
  }
  if (body.kind !== undefined) {
    setCols.push("kind = ?");
    setParams.push(String(body.kind));
  }

  if (setCols.length === 0) return null;

  return {
    setClause: "SET " + setCols.join(", "),
    setParams
  };
};
