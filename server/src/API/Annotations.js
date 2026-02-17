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
