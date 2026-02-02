// Site Explorer API FFI - JSON builders

export const buildRoutesJson = (rows) => JSON.stringify(
  rows.map(row => ({
    id: row.id,
    routeName: row.route_name,
    urlPattern: row.url_pattern,
    moduleName: row.module_name,
    isArchived: row.is_archived,
    isReachable: row.is_reachable,
    depth: row.discovery_depth,
    foundFrom: row.found_from,
    classification: row.classification,
    note: row.note,
    actionHint: row.action_hint
  }))
);

export const buildAnnotationsJson = (rows) => JSON.stringify(
  rows.map(row => ({
    id: row.id,
    targetType: row.target_type,
    targetId: row.target_id,
    classification: row.classification,
    note: row.note,
    actionHint: row.action_hint,
    createdAt: row.created_at
  }))
);

export const buildReportJson = (rows) => JSON.stringify(
  rows.map(row => ({
    targetType: row.target_type,
    classification: row.classification,
    actionHint: row.action_hint,
    note: row.note,
    targetDescription: row.target_description,
    createdAt: row.created_at
  }))
);

// Parse annotation JSON from request body
// Returns null if parsing fails, otherwise returns the parsed object
export const parseAnnotationJsonImpl = (jsonStr) => {
  try {
    const parsed = JSON.parse(jsonStr);
    // Validate required fields
    if (typeof parsed.targetType !== 'string' ||
        typeof parsed.targetId !== 'number' ||
        typeof parsed.classification !== 'string') {
      return null;
    }
    return {
      targetType: parsed.targetType,
      targetId: parsed.targetId,
      classification: parsed.classification,
      note: parsed.note || '',
      actionHint: parsed.actionHint || ''
    };
  } catch (e) {
    return null;
  }
};

// Extract max_id from a row
export const getMaxId = (row) => row.max_id;

// Build JSON response for created annotation
export const buildCreatedJson = (id) => (input) => JSON.stringify({
  id: id,
  targetType: input.targetType,
  targetId: input.targetId,
  classification: input.classification,
  note: input.note,
  actionHint: input.actionHint
});
