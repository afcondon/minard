// FFI for API.Snapshots module
// Git commit log, worktree lifecycle, snapshot management

import { execSync } from 'node:child_process';
import { existsSync } from 'node:fs';
import { resolve } from 'node:path';

// =============================================================================
// Git Commit Log
// =============================================================================

// Get recent git commits with branch/tag decorations, annotated with
// which commits already have snapshots loaded.
// getGitLogWithSnapshots :: EffectFn3 Int Int (Array Foreign) String
export const getGitLogWithSnapshots = (count, offset, snapshotRows) => {
  // Build set of hashes that have snapshots
  const snapshotHashes = new Set();
  for (const row of (snapshotRows || [])) {
    if (row.git_hash) snapshotHashes.add(row.git_hash);
  }

  try {
    // NUL-delimited fields: hash, subject, author, ISO date, relative date, decorations
    const format = '%H%x00%s%x00%an%x00%aI%x00%ar%x00%D';
    const output = execSync(
      `git log --format='${format}' -${count} --skip=${offset}`,
      { encoding: 'utf8', timeout: 5000 }
    );

    // Check if there are more commits beyond this page
    let hasMore = false;
    try {
      const nextOutput = execSync(
        `git log --format='%H' -1 --skip=${offset + count}`,
        { encoding: 'utf8', timeout: 2000 }
      ).trim();
      hasMore = nextOutput.length > 0;
    } catch (_) {
      // No more commits
    }

    const commits = output.trim().split('\n')
      .filter(line => line.length > 0)
      .map(line => {
        const [hash, message, author, date, relativeDate, refsRaw] = line.split('\0');
        // Parse decoration string: "HEAD -> main, tag: v1.0, origin/main"
        const refs = refsRaw
          ? refsRaw.split(',').map(r => r.trim()).filter(r => r.length > 0)
              // Keep HEAD markers and clean up ref names
              .map(r => {
                if (r.startsWith('HEAD -> ')) return r.substring(8);
                if (r.startsWith('tag: ')) return r.substring(5);
                // Skip origin/ refs (redundant with local branches)
                if (r.startsWith('origin/')) return null;
                return r;
              })
              .filter(r => r !== null)
          : [];
        return {
          hash,
          shortHash: hash.substring(0, 7),
          message,
          author,
          date,
          relativeDate,
          refs,
          hasSnapshot: snapshotHashes.has(hash)
        };
      });

    return JSON.stringify({ commits, hasMore });
  } catch (error) {
    return JSON.stringify({ commits: [], hasMore: false, error: error.message });
  }
};

// =============================================================================
// Worktree Lifecycle
// =============================================================================

// Sanitize a git ref into a safe directory name
function sanitizeRefName(ref) {
  return ref.replace(/\//g, '-').replace(/[^a-zA-Z0-9._-]/g, '');
}

// Resolve ref to full commit hash
// resolveRef :: String -> Effect String
export const resolveRef = (ref) => () => {
  try {
    return execSync(`git rev-parse "${ref}"`, { encoding: 'utf8', timeout: 5000 }).trim();
  } catch (_) {
    return '';
  }
};

// Create a git worktree. Returns JSON result.
// createWorktree :: EffectFn2 String String String
export const createWorktree = (hash, projectRoot) => {
  const shortHash = hash.substring(0, 7);
  const worktreeName = `minard-${shortHash}`;
  const worktreePath = resolve(projectRoot, '..', worktreeName);

  if (existsSync(worktreePath)) {
    return JSON.stringify({
      success: true,
      worktreePath,
      alreadyExisted: true
    });
  }

  try {
    // Detached HEAD at specific commit — avoids branch conflicts
    execSync(`git worktree add --detach "${worktreePath}" "${hash}"`, {
      encoding: 'utf8',
      timeout: 30000,
      cwd: projectRoot
    });
    return JSON.stringify({
      success: true,
      worktreePath,
      alreadyExisted: false
    });
  } catch (error) {
    return JSON.stringify({
      success: false,
      worktreePath,
      error: error.stderr ? error.stderr.toString() : error.message
    });
  }
};

// Check if compiled output exists in a worktree
// hasCompiledOutput :: String -> Effect Boolean
export const hasCompiledOutput = (path) => () => {
  return existsSync(resolve(path, 'output'));
};

// Run spago build in a worktree. Returns JSON result.
// buildInWorktree :: String -> Effect String
export const buildInWorktree = (worktreePath) => () => {
  const start = Date.now();
  try {
    execSync('spago build', {
      cwd: worktreePath,
      encoding: 'utf8',
      timeout: 300000,
      stdio: ['pipe', 'pipe', 'pipe']
    });
    return JSON.stringify({ success: true, elapsedMs: Date.now() - start });
  } catch (error) {
    return JSON.stringify({
      success: false,
      error: error.stderr ? error.stderr.toString().substring(0, 2000) : error.message,
      elapsedMs: Date.now() - start
    });
  }
};

// Remove a git worktree. Returns JSON result.
// removeWorktree :: EffectFn2 String String String
export const removeWorktree = (worktreePath, projectRoot) => {
  try {
    execSync(`git worktree remove "${worktreePath}" --force`, {
      encoding: 'utf8',
      timeout: 10000,
      cwd: projectRoot
    });
    return JSON.stringify({ success: true });
  } catch (_error) {
    try {
      execSync('git worktree prune', {
        encoding: 'utf8',
        timeout: 5000,
        cwd: projectRoot
      });
      return JSON.stringify({ success: true, warning: 'worktree already removed, pruned' });
    } catch (pruneErr) {
      return JSON.stringify({
        success: false,
        error: pruneErr.stderr ? pruneErr.stderr.toString() : pruneErr.message
      });
    }
  }
};

// =============================================================================
// Snapshot Details (enhanced listing)
// =============================================================================

export const buildSnapshotDetailsJson = (rows) => {
  // Determine current working directory for "is this the current checkout?" check
  const cwd = process.cwd().replace(/\/server$/, '');
  const snapshots = (rows || []).map(row => {
    const repoPath = row.repo_path || '.';
    const resolvedPath = resolve(repoPath);
    const isCurrent = resolvedPath === cwd || repoPath === '.';
    return {
      id: Number(row.id),
      projectId: Number(row.project_id),
      gitHash: row.git_hash || null,
      gitRef: row.git_ref || null,
      label: row.label || null,
      repoPath,
      projectName: row.project_name || null,
      packageCount: Number(row.package_count) || 0,
      moduleCount: Number(row.module_count) || 0,
      workspacePackageCount: Number(row.workspace_package_count) || 0,
      isCurrentCheckout: isCurrent,
      canDelete: !isCurrent
    };
  });
  return JSON.stringify({ snapshots, count: snapshots.length });
};

// =============================================================================
// Delete Results
// =============================================================================

export const buildDeleteResultsJson = (results) => {
  return JSON.stringify({ results });
};

// =============================================================================
// Body Parsing
// =============================================================================

export const parseBody = (bodyStr) => {
  try {
    const obj = JSON.parse(bodyStr);
    if (!obj || typeof obj !== 'object') return null;
    return obj;
  } catch (_e) {
    return null;
  }
};

export const getBodyRef = (body) => {
  if (!body.ref || typeof body.ref !== 'string') return null;
  return body.ref.trim();
};

export const getBodyLabel = (body) => {
  if (!body.label || typeof body.label !== 'string') return null;
  return body.label.trim();
};

export const getBodySnapshotIds = (body) => {
  if (!Array.isArray(body.snapshotIds)) return null;
  return body.snapshotIds.filter(id => typeof id === 'number' && Number.isInteger(id));
};

// =============================================================================
// JSON Result Parsing (for reading results from other FFI calls)
// =============================================================================

export const isJsonSuccess = (jsonStr) => {
  try {
    return JSON.parse(jsonStr).success === true;
  } catch (_) {
    return false;
  }
};

export const getJsonField = (jsonStr, field) => {
  try {
    const obj = JSON.parse(jsonStr);
    return obj[field] != null ? String(obj[field]) : null;
  } catch (_) {
    return null;
  }
};

// =============================================================================
// Commit-Module Grid (git log --name-only per commit)
// =============================================================================

// getCommitFilesImpl :: EffectFn3 Int Int (Array String) String
export const getCommitFilesImpl = (count, offset, knownModuleNames) => {
  // Build reverse lookup: for each known module name, compute all plausible
  // file paths that git might report. Module names don't always match file
  // paths (e.g. CE2.Main lives at frontend/src/Main.purs), so we try
  // stripping common prefixes from the module name.
  const filePathToModule = new Map();
  for (const modName of knownModuleNames) {
    const modPath = modName.replace(/\./g, '/') + '.purs';
    // Try common source directory prefixes
    const srcPrefixes = ['frontend/src/', 'server/src/', 'src/', 'test/', 'database/src/', ''];
    for (const prefix of srcPrefixes) {
      filePathToModule.set(prefix + modPath, modName);
    }
    // Also try stripping the first segment of the module name as a namespace prefix
    // e.g. CE2.Component.Foo -> Component/Foo.purs (with prefixes)
    const dotIdx = modName.indexOf('.');
    if (dotIdx > 0) {
      const stripped = modName.substring(dotIdx + 1).replace(/\./g, '/') + '.purs';
      for (const prefix of srcPrefixes) {
        filePathToModule.set(prefix + stripped, modName);
      }
    }
  }

  try {
    // --name-status lists changed files with status letter (A/M/D/R/C)
    // NUL-delimited header: hash, subject, relative date
    const format = '%H%x00%s%x00%ar';
    const output = execSync(
      `git log --name-status --format='${format}' -${count} --skip=${offset}`,
      { encoding: 'utf8', timeout: 10000 }
    );

    const allModulesSet = new Set();
    const commits = [];
    let currentCommit = null;

    for (const line of output.split('\n')) {
      if (line === '') {
        // Blank line separates header from file list, or between commits
        continue;
      }

      if (line.includes('\0')) {
        // This is a commit header line
        if (currentCommit) commits.push(currentCommit);
        const [hash, message, relativeDate] = line.split('\0');
        currentCommit = {
          hash,
          shortHash: hash.substring(0, 7),
          message,
          relativeDate,
          modules: [],
          moduleStatuses: {}
        };
      } else if (currentCommit) {
        // --name-status format: "M\tpath/to/file" or "R100\told\tnew"
        const parts = line.split('\t');
        if (parts.length >= 2) {
          const statusLetter = parts[0].charAt(0); // A, M, D, R, C
          const filePath = parts.length >= 3 ? parts[2] : parts[1]; // For renames, use new path
          const mod = filePathToModule.get(filePath);
          if (mod) {
            currentCommit.modules.push(mod);
            currentCommit.moduleStatuses[mod] = statusLetter;
            allModulesSet.add(mod);
          }
        }
      }
    }
    if (currentCommit) commits.push(currentCommit);

    // Sort allModules alphabetically for stable column ordering
    const allModules = Array.from(allModulesSet).sort();

    return JSON.stringify({ commits, allModules, count: commits.length });
  } catch (error) {
    return JSON.stringify({ commits: [], allModules: [], count: 0, error: error.message });
  }
};

// =============================================================================
// Row Field Access (DuckDB query result objects)
// =============================================================================

export const getRowString = (row) => (field) => {
  const v = row[field];
  return v != null ? String(v) : '';
};

export const getRowInt = (row) => (field) => {
  return Number(row[field]) || 0;
};
