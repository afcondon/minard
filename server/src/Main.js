// Server.Main FFI — environment variable helpers

// Read PORT env var, defaulting to 3000
export const getPortFromEnv = () => {
  const port = parseInt(process.env.PORT || process.env.MINARD_API_PORT || "3000", 10);
  return isNaN(port) ? 3000 : port;
};

// Read MINARD_DB env var (path to the DuckDB file), defaulting to the
// bundled ce-unified database. Lets users point Minard at their own DB
// without editing source — `MINARD_DB=database/myproject.duckdb node server/run.js`.
export const getDbPathFromEnv = () => {
  return process.env.MINARD_DB || "./database/ce-unified.duckdb";
};
