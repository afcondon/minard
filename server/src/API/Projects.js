// FFI for API.Projects module

// Build JSON array of projects
export const buildProjectsListJson = (rows) => {
  const projects = rows.map(row => ({
    id: row.id,
    name: row.name,
    repoPath: row.repo_path,
    description: row.description,
    createdAt: row.created_at,
    snapshotCount: Number(row.snapshot_count),
    latestSnapshotAt: row.latest_snapshot_at
  }));
  return JSON.stringify({ projects });
};

// Build JSON for a single project with its snapshots
export const buildProjectWithSnapshotsJson = (project) => (snapshots) => {
  return JSON.stringify({
    project: {
      id: project.id,
      name: project.name,
      repoPath: project.repo_path,
      description: project.description,
      createdAt: project.created_at
    },
    snapshots: snapshots.map(s => ({
      id: s.id,
      gitHash: s.git_hash,
      gitRef: s.git_ref,
      label: s.label,
      snapshotAt: s.snapshot_at,
      createdAt: s.created_at,
      moduleCount: Number(s.module_count),
      packageCount: Number(s.package_count),
      declarationCount: Number(s.declaration_count)
    }))
  });
};

// Build JSON array of snapshots
export const buildSnapshotsListJson = (rows) => {
  const snapshots = rows.map(s => ({
    id: s.id,
    projectId: s.project_id,
    gitHash: s.git_hash,
    gitRef: s.git_ref,
    label: s.label,
    snapshotAt: s.snapshot_at,
    createdAt: s.created_at,
    moduleCount: Number(s.module_count),
    packageCount: Number(s.package_count),
    declarationCount: Number(s.declaration_count)
  }));
  return JSON.stringify({ snapshots });
};

// Build JSON for a single snapshot
export const buildSnapshotJson = (snapshot) => {
  return JSON.stringify({
    id: snapshot.id,
    projectId: snapshot.project_id,
    projectName: snapshot.project_name,
    gitHash: snapshot.git_hash,
    gitRef: snapshot.git_ref,
    label: snapshot.label,
    snapshotAt: snapshot.snapshot_at,
    createdAt: snapshot.created_at,
    moduleCount: Number(snapshot.module_count),
    packageCount: Number(snapshot.package_count),
    declarationCount: Number(snapshot.declaration_count)
  });
};

// Extract id from a row, returning Nullable Int
export const getIdFromRow_ = (row) => {
  if (row && row.id !== undefined && row.id !== null) {
    return row.id;
  }
  return null;
};

// Build JSON array of package sets
export const buildPackageSetsListJson = (rows) => {
  const packageSets = rows.map(row => ({
    id: row.id,
    name: row.name,
    compilerVersion: row.compiler_version,
    source: row.source,
    publishedAt: row.published_at,
    packageCount: Number(row.package_count),
    createdAt: row.created_at
  }));
  return JSON.stringify({ packageSets });
};

// Build JSON for a single package set with its packages
export const buildPackageSetWithPackagesJson = (packageSet) => (packages) => {
  // Parse depends JSON string for each package, include module counts from unified schema
  const parsedPackages = packages.map(p => ({
    id: p.id,
    name: p.name,
    version: p.version,
    description: p.description,
    license: p.license,
    repositoryOwner: p.repository_owner,
    repositoryName: p.repository_name,
    depends: typeof p.depends === 'string' ? JSON.parse(p.depends) : (p.depends || []),
    topoLayer: Number(p.topo_layer),
    publishedAt: p.published_at,
    releaseNumber: Number(p.release_number) || 1,
    moduleCount: Number(p.module_count) || 0,
    totalLoc: Number(p.total_loc) || 0
  }));

  return JSON.stringify({
    packageSet: {
      id: packageSet.id,
      name: packageSet.name,
      compilerVersion: packageSet.compiler_version,
      source: packageSet.source,
      publishedAt: packageSet.published_at,
      packageCount: Number(packageSet.package_count),
      createdAt: packageSet.created_at
    },
    packages: parsedPackages
  });
};
