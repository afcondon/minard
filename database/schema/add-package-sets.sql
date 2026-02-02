-- Package Sets Schema
-- Stores PureScript registry package sets separately from project analysis
-- Enables "Powers of Ten" package set view with full registry context

-- =============================================================================
-- Package Sets (Registry Snapshots)
-- =============================================================================

-- Package sets: Point-in-time captures of the PureScript registry
-- Each set is a version like "71.0.0" from the registry package-sets
CREATE TABLE IF NOT EXISTS package_sets (
  id INTEGER PRIMARY KEY,
  name VARCHAR NOT NULL UNIQUE,       -- "71.0.0", "70.1.0"
  compiler_version VARCHAR,           -- "0.15.15"
  source VARCHAR DEFAULT 'registry',  -- "registry", "manual"
  published_at TIMESTAMP,             -- When the package set was published
  package_count INTEGER DEFAULT 0,
  created_at TIMESTAMP DEFAULT current_timestamp
);

-- Packages within a package set (from registry-index manifests)
CREATE TABLE IF NOT EXISTS package_set_packages (
  id INTEGER PRIMARY KEY,
  package_set_id INTEGER NOT NULL,
  name VARCHAR NOT NULL,              -- "halogen", "prelude"
  version VARCHAR,                    -- "7.0.0"
  description TEXT,
  license VARCHAR,
  repository_owner VARCHAR,           -- GitHub owner
  repository_name VARCHAR,            -- GitHub repo name
  depends JSON,                       -- ["prelude", "effect", ...]
  module_count INTEGER,               -- If known from analysis
  topo_layer INTEGER DEFAULT 0,       -- Computed: 0=leaf (no deps), higher=more deps
  created_at TIMESTAMP DEFAULT current_timestamp,
  FOREIGN KEY (package_set_id) REFERENCES package_sets(id),
  UNIQUE (package_set_id, name)
);

CREATE INDEX IF NOT EXISTS idx_psp_package_set ON package_set_packages(package_set_id);
CREATE INDEX IF NOT EXISTS idx_psp_topo_layer ON package_set_packages(package_set_id, topo_layer);

-- =============================================================================
-- Link Projects to Package Sets
-- =============================================================================

-- Add optional package_set reference to projects
-- This allows showing "our usage" overlaid on the full registry
-- Note: DuckDB doesn't support ADD COLUMN with FK constraints, so we add without constraint
ALTER TABLE projects ADD COLUMN IF NOT EXISTS package_set_id INTEGER;

-- =============================================================================
-- Useful Views
-- =============================================================================

-- Package set summary
CREATE OR REPLACE VIEW package_set_summary AS
SELECT
  ps.id,
  ps.name,
  ps.compiler_version,
  ps.published_at,
  ps.package_count,
  MAX(psp.topo_layer) as max_topo_layer
FROM package_sets ps
LEFT JOIN package_set_packages psp ON ps.id = psp.package_set_id
GROUP BY ps.id, ps.name, ps.compiler_version, ps.published_at, ps.package_count;

-- Packages by topo layer (for visualization)
CREATE OR REPLACE VIEW packages_by_layer AS
SELECT
  psp.package_set_id,
  psp.topo_layer,
  COUNT(*) as package_count,
  LIST(psp.name ORDER BY psp.name) as packages
FROM package_set_packages psp
GROUP BY psp.package_set_id, psp.topo_layer
ORDER BY psp.topo_layer;
