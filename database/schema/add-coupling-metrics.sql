-- Coupling Metrics for Static Analysis Visualization
-- Computes cross-module dependency metrics per declaration
--
-- These metrics help visualize the "refactoring cost" of each declaration:
-- - external_call_count: how many external functions this calls (dependencies it brings)
-- - external_caller_count: how many external functions call this (things that break if moved)

-- =============================================================================
-- Materialized Table for Declaration Coupling (pre-computed)
-- =============================================================================

-- Drop existing table if present (for re-running)
DROP TABLE IF EXISTS declaration_coupling;

-- Create table for coupling metrics
CREATE TABLE declaration_coupling (
  id INTEGER PRIMARY KEY,
  snapshot_id INTEGER NOT NULL,
  module VARCHAR NOT NULL,
  name VARCHAR NOT NULL,
  external_call_count INTEGER DEFAULT 0,
  external_caller_count INTEGER DEFAULT 0,
  total_coupling INTEGER DEFAULT 0,
  coupling_intensity DOUBLE DEFAULT 0.0,
  FOREIGN KEY (snapshot_id) REFERENCES snapshots(id),
  UNIQUE (snapshot_id, module, name)
);

CREATE INDEX IF NOT EXISTS idx_decl_coupling_snapshot ON declaration_coupling(snapshot_id);
CREATE INDEX IF NOT EXISTS idx_decl_coupling_module ON declaration_coupling(snapshot_id, module);

-- =============================================================================
-- Module-Level Coupling Table (pre-computed)
-- =============================================================================

DROP TABLE IF EXISTS module_coupling;

CREATE TABLE module_coupling (
  id INTEGER PRIMARY KEY,
  snapshot_id INTEGER NOT NULL,
  module VARCHAR NOT NULL,
  efferent_coupling INTEGER DEFAULT 0,  -- modules this depends on
  afferent_coupling INTEGER DEFAULT 0,  -- modules that depend on this
  instability DOUBLE DEFAULT 0.0,       -- Ce / (Ca + Ce)
  FOREIGN KEY (snapshot_id) REFERENCES snapshots(id),
  UNIQUE (snapshot_id, module)
);

CREATE INDEX IF NOT EXISTS idx_mod_coupling_snapshot ON module_coupling(snapshot_id);
