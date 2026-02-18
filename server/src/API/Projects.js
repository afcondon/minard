// FFI for API.Projects module
// Project management: list, validate path, run loader, delete

import { existsSync, readdirSync } from 'node:fs';
import { execSync } from 'node:child_process';
import { resolve, basename } from 'node:path';

// =============================================================================
// V2 Project Management API
// =============================================================================

// Build JSON response from project rows (joined with snapshot/package stats)
export const listProjectsJson = (rows) => {
  const projects = (rows || []).map(row => ({
    id: Number(row.id),
    name: row.name,
    repoPath: row.repo_path,
    primaryBackend: row.primary_backend || 'js',
    createdAt: row.created_at ? String(row.created_at) : null,
    stats: {
      packageCount: Number(row.package_count) || 0,
      moduleCount: Number(row.module_count) || 0,
      declarationCount: Number(row.declaration_count) || 0
    }
  }));
  return JSON.stringify({ projects, count: projects.length });
};

// Validate a project path: check directory, spago.lock, output/, docs.json, loader binary
// Returns Effect String (thunk)
export const validatePathJson = (pathStr) => () => {
  const checks = {
    directoryExists: false,
    spagoLockExists: false,
    outputDirExists: false,
    docsJsonCount: 0,
    loaderBinaryExists: false
  };
  const issues = [];

  // 1. Directory exists
  checks.directoryExists = existsSync(pathStr);
  if (!checks.directoryExists) {
    issues.push({ severity: 'error', check: 'directoryExists', message: `Directory does not exist: ${pathStr}` });
    return JSON.stringify({ valid: false, path: pathStr, projectName: basename(pathStr), checks, issues });
  }

  // 2. spago.lock exists
  checks.spagoLockExists = existsSync(resolve(pathStr, 'spago.lock'));
  if (!checks.spagoLockExists) {
    issues.push({ severity: 'error', check: 'spagoLockExists', message: 'No spago.lock found. Run `spago build` in the project first.' });
  }

  // 3. output/ directory exists
  const outputDir = resolve(pathStr, 'output');
  checks.outputDirExists = existsSync(outputDir);
  if (!checks.outputDirExists) {
    issues.push({ severity: 'error', check: 'outputDirExists', message: 'No output/ directory. Run `spago build` to compile.' });
  }

  // 4. Count docs.json files in output/
  if (checks.outputDirExists) {
    try {
      const outputDirs = readdirSync(outputDir, { withFileTypes: true })
        .filter(d => d.isDirectory());
      let docsCount = 0;
      for (const d of outputDirs) {
        if (existsSync(resolve(outputDir, d.name, 'docs.json'))) {
          docsCount++;
        }
      }
      checks.docsJsonCount = docsCount;
      if (docsCount === 0) {
        issues.push({ severity: 'warning', check: 'docsJsonCount', message: 'No docs.json files found in output/. The loader will still work but may miss some metadata.' });
      }
    } catch (_e) {
      // If we can't read output dir, leave count at 0
    }
  }

  // 5. Loader binary exists
  const loaderPath = resolveLoaderPath();
  checks.loaderBinaryExists = existsSync(loaderPath);
  if (!checks.loaderBinaryExists) {
    issues.push({ severity: 'error', check: 'loaderBinaryExists', message: `Loader binary not found at ${loaderPath}. Run \`cargo build --release\` in minard-loader/.` });
  }

  const hasErrors = issues.some(i => i.severity === 'error');
  const projectName = basename(pathStr);

  return JSON.stringify({
    valid: !hasErrors,
    path: pathStr,
    projectName,
    checks,
    issues
  });
};

// Run the Rust loader synchronously. Blocks the Node event loop (by design —
// prevents concurrent DuckDB access). Returns Effect String (thunk).
export const runLoaderSync = (projectPath) => (dbPath) => (projectName) => (snapshotLabel) => () => {
  const loaderPath = resolveLoaderPath();
  const start = Date.now();

  try {
    let cmd = `"${loaderPath}" -q "${projectPath}" "${dbPath}"`;
    if (projectName) {
      cmd += ` --name "${projectName}"`;
    }
    if (snapshotLabel) {
      cmd += ` --label "${snapshotLabel}"`;
    }

    const output = execSync(cmd, {
      encoding: 'utf8',
      timeout: 120000,
      stdio: ['pipe', 'pipe', 'pipe']
    });

    const elapsedMs = Date.now() - start;

    // Try to parse loader output for stats (last JSON line)
    let stats = null;
    try {
      const lines = output.trim().split('\n').filter(l => l.length > 0);
      const lastLine = lines[lines.length - 1];
      if (lastLine && lastLine.startsWith('{')) {
        stats = JSON.parse(lastLine);
      }
    } catch (_) {
      // Stats parsing is best-effort
    }

    return JSON.stringify({ success: true, stats, error: null, elapsedMs });
  } catch (err) {
    const elapsedMs = Date.now() - start;
    return JSON.stringify({
      success: false,
      stats: null,
      error: err.stderr ? err.stderr.toString() : err.message,
      elapsedMs
    });
  }
};

// Returns array of SQL statements for cascade delete of a project and its data
export const deleteProjectSql = (projectId) => {
  return [
    `DELETE FROM function_calls WHERE caller_module_id IN (
       SELECT m.id FROM modules m
       JOIN package_versions pv ON m.package_version_id = pv.id
       JOIN snapshot_packages sp ON sp.package_version_id = pv.id
       JOIN snapshots s ON sp.snapshot_id = s.id
       WHERE s.project_id = ${projectId}
     )`,
    `DELETE FROM imports WHERE module_id IN (
       SELECT m.id FROM modules m
       JOIN package_versions pv ON m.package_version_id = pv.id
       JOIN snapshot_packages sp ON sp.package_version_id = pv.id
       JOIN snapshots s ON sp.snapshot_id = s.id
       WHERE s.project_id = ${projectId}
     )`,
    `DELETE FROM child_declarations WHERE declaration_id IN (
       SELECT d.id FROM declarations d
       JOIN modules m ON d.module_id = m.id
       JOIN package_versions pv ON m.package_version_id = pv.id
       JOIN snapshot_packages sp ON sp.package_version_id = pv.id
       JOIN snapshots s ON sp.snapshot_id = s.id
       WHERE s.project_id = ${projectId}
     )`,
    `DELETE FROM declarations WHERE module_id IN (
       SELECT m.id FROM modules m
       JOIN package_versions pv ON m.package_version_id = pv.id
       JOIN snapshot_packages sp ON sp.package_version_id = pv.id
       JOIN snapshots s ON sp.snapshot_id = s.id
       WHERE s.project_id = ${projectId}
     )`,
    `DELETE FROM modules WHERE package_version_id IN (
       SELECT pv.id FROM package_versions pv
       JOIN snapshot_packages sp ON sp.package_version_id = pv.id
       JOIN snapshots s ON sp.snapshot_id = s.id
       WHERE s.project_id = ${projectId}
     )`,
    `DELETE FROM package_dependencies WHERE snapshot_package_id IN (
       SELECT sp.id FROM snapshot_packages sp
       JOIN snapshots s ON sp.snapshot_id = s.id
       WHERE s.project_id = ${projectId}
     )`,
    `DELETE FROM snapshot_packages WHERE snapshot_id IN (
       SELECT id FROM snapshots WHERE project_id = ${projectId}
     )`,
    `DELETE FROM snapshots WHERE project_id = ${projectId}`,
    `DELETE FROM projects WHERE id = ${projectId}`
  ];
};

// =============================================================================
// Body Parsing (same pattern as Annotations)
// =============================================================================

export const parseProjectBody = (bodyStr) => {
  try {
    const obj = JSON.parse(bodyStr);
    if (!obj || typeof obj !== 'object') return null;
    return obj;
  } catch (_e) {
    return null;
  }
};

export const validateLoadFields = (body) => {
  if (!body.path || typeof body.path !== 'string') return null;
  return {
    path: body.path.trim(),
    name: body.name ? String(body.name).trim() : null,
    label: body.label ? String(body.label).trim() : null
  };
};

export const validatePathFields = (body) => {
  if (!body.path || typeof body.path !== 'string') return null;
  return { path: body.path.trim() };
};

// =============================================================================
// Helpers
// =============================================================================

function resolveLoaderPath() {
  if (process.env.MINARD_LOADER_PATH) {
    return process.env.MINARD_LOADER_PATH;
  }
  // Default: minard-loader directory under the minard/ workspace
  return resolve(process.cwd(), 'minard-loader', 'target', 'release', 'minard-loader');
}
