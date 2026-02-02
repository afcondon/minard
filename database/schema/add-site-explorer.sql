-- =============================================================================
-- Site Explorer Schema Extension v1.0
-- =============================================================================
--
-- Extends the Code Explorer schema with web/routing analysis:
--   - Route definitions (from static analysis)
--   - Discovered pages (from spidering)
--   - Route → Module dependencies
--   - User annotations/classifications
--
-- Design principles:
--   - Routes link to modules (the component that renders them)
--   - Spidering captures runtime reachability
--   - Annotations are polymorphic (work on routes, modules, declarations)
--   - Classifications feed LLM-assisted refactoring
--
-- Data sources:
--   - halogen-spider analyze  → routes, route_modules
--   - halogen-spider spider   → discovered_pages
--   - halogen-spider compare  → route reachability flags
--   - Site Explorer UI        → annotations
--
-- =============================================================================

-- =============================================================================
-- ROUTE LAYER
-- Static route definitions from source code analysis
-- =============================================================================

-- A route definition extracted from source code
-- e.g., "HowtoTransitions" → "/howto/transitions" → Component.Howto.Transitions
CREATE TABLE IF NOT EXISTS routes (
    id              INTEGER PRIMARY KEY,
    snapshot_id     INTEGER NOT NULL REFERENCES snapshots(id),

    -- Route identity
    route_name      VARCHAR NOT NULL,          -- "HowtoTransitions" (PureScript constructor)
    url_pattern     VARCHAR NOT NULL,          -- "/howto/transitions" (URL path)

    -- Link to implementing module
    module_id       INTEGER REFERENCES modules(id),
    module_name     VARCHAR,                   -- "Component.Howto.Transitions" (for display)

    -- Analysis results
    is_archived     BOOLEAN DEFAULT FALSE,     -- Marked as ARCHIVED in source

    created_at      TIMESTAMP DEFAULT CURRENT_TIMESTAMP,

    UNIQUE(snapshot_id, route_name)
);

CREATE INDEX IF NOT EXISTS idx_routes_snapshot ON routes(snapshot_id);
CREATE INDEX IF NOT EXISTS idx_routes_module ON routes(module_id);

-- =============================================================================
-- SPIDER LAYER
-- Runtime reachability from spidering a live site
-- =============================================================================

-- A spider run against a specific snapshot/deployment
CREATE TABLE IF NOT EXISTS spider_runs (
    id              INTEGER PRIMARY KEY,
    snapshot_id     INTEGER NOT NULL REFERENCES snapshots(id),

    base_url        VARCHAR NOT NULL,          -- "http://localhost:8080"
    started_at      TIMESTAMP NOT NULL,
    completed_at    TIMESTAMP,

    -- Summary stats
    pages_visited   INTEGER,
    max_depth       INTEGER,
    errors          INTEGER DEFAULT 0,

    -- Configuration used
    config_json     TEXT                       -- SpiderConfig as JSON for reproducibility
);

-- A page discovered by spidering
CREATE TABLE IF NOT EXISTS discovered_pages (
    id              INTEGER PRIMARY KEY,
    spider_run_id   INTEGER NOT NULL REFERENCES spider_runs(id),

    url_path        VARCHAR NOT NULL,          -- "/howto/transitions"
    depth           INTEGER NOT NULL,          -- How many clicks from start
    found_from      VARCHAR,                   -- Parent page URL

    -- Link back to route definition (if matched)
    route_id        INTEGER REFERENCES routes(id),

    UNIQUE(spider_run_id, url_path)
);

CREATE INDEX IF NOT EXISTS idx_discovered_spider ON discovered_pages(spider_run_id);
CREATE INDEX IF NOT EXISTS idx_discovered_route ON discovered_pages(route_id);

-- =============================================================================
-- REACHABILITY VIEW
-- Combines static routes with spider results
-- =============================================================================

CREATE VIEW IF NOT EXISTS route_reachability AS
SELECT
    r.id AS route_id,
    r.snapshot_id,
    r.route_name,
    r.url_pattern,
    r.module_name,
    r.is_archived,
    dp.id IS NOT NULL AS is_reachable,
    dp.depth AS discovery_depth,
    dp.found_from,
    sr.base_url,
    sr.started_at AS spider_date
FROM routes r
LEFT JOIN spider_runs sr ON sr.snapshot_id = r.snapshot_id
LEFT JOIN discovered_pages dp ON dp.route_id = r.id AND dp.spider_run_id = sr.id;

-- =============================================================================
-- ANNOTATION LAYER
-- User classifications that feed LLM-assisted actions
-- =============================================================================

-- Polymorphic annotations - work on any entity type
-- This is the key table for the "classification → LLM action" workflow
CREATE TABLE IF NOT EXISTS annotations (
    id              INTEGER PRIMARY KEY,

    -- What is being annotated
    target_type     VARCHAR NOT NULL,          -- 'route', 'module', 'declaration', 'page'
    target_id       INTEGER NOT NULL,          -- ID in the target table

    -- The classification
    classification  VARCHAR,                   -- 'keep', 'obsolete', 'deprecated', 'replace', 'review'

    -- Free-form notes for LLM context
    note            TEXT,

    -- Action hints (optional structured data for LLM)
    action_hint     VARCHAR,                   -- 'delete', 'refactor', 'merge_into:X', 'update_to:Y'

    -- Audit trail
    created_at      TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    created_by      VARCHAR,                   -- User identifier (future multi-user)

    -- Prevent duplicate annotations on same target
    UNIQUE(target_type, target_id)
);

CREATE INDEX IF NOT EXISTS idx_annotations_target ON annotations(target_type, target_id);
CREATE INDEX IF NOT EXISTS idx_annotations_classification ON annotations(classification);

-- =============================================================================
-- ANNOTATION VIEWS
-- Convenient access to annotations by entity type
-- =============================================================================

CREATE VIEW IF NOT EXISTS route_annotations AS
SELECT
    r.*,
    a.classification,
    a.note,
    a.action_hint,
    a.created_at AS annotated_at
FROM routes r
LEFT JOIN annotations a ON a.target_type = 'route' AND a.target_id = r.id;

CREATE VIEW IF NOT EXISTS module_annotations AS
SELECT
    m.id,
    m.name,
    a.classification,
    a.note,
    a.action_hint,
    a.created_at AS annotated_at
FROM modules m
LEFT JOIN annotations a ON a.target_type = 'module' AND a.target_id = m.id;

-- =============================================================================
-- REPORT GENERATION
-- Export classifications for LLM consumption
-- =============================================================================

-- View for generating LLM action reports
CREATE VIEW IF NOT EXISTS classification_report AS
SELECT
    a.target_type,
    a.classification,
    a.action_hint,
    a.note,
    CASE a.target_type
        WHEN 'route' THEN (SELECT route_name || ' → ' || url_pattern FROM routes WHERE id = a.target_id)
        WHEN 'module' THEN (SELECT name FROM modules WHERE id = a.target_id)
        WHEN 'declaration' THEN (SELECT name FROM declarations WHERE id = a.target_id)
        ELSE 'unknown'
    END AS target_description,
    a.created_at
FROM annotations a
WHERE a.classification IS NOT NULL
ORDER BY a.target_type, a.classification, a.created_at;
