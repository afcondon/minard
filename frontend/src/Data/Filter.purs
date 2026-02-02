-- | Data Filtering for Scope-Based Views
-- |
-- | Centralized filtering logic for BeeswarmScope and ModuleScope.
-- | Extracted from visualization components to avoid duplication.
module CE2.Data.Filter
  ( -- Package filtering (for PackageSetPackage)
    filterPackagesByScope
  , computeTransitiveDeps
    -- Node filtering (for SimNode)
  , filterNodesByScope
  , filterNodesByFocalPackage
    -- Module filtering (for V2ModuleListItem)
  , filterModulesByScope
  , ModuleScope(..)
  ) where

import Prelude

import Data.Array as Array
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))

import CE2.Data.Loader (PackageSetPackage, V2ModuleListItem)
import CE2.Types (SimNode, Package, BeeswarmScope(..), projectPackages)

-- =============================================================================
-- Module Scope Type
-- =============================================================================

-- | Scope for module filtering (different from package BeeswarmScope)
data ModuleScope
  = AllModules
  | WorkspaceOnly      -- source == "workspace"
  | WorkspaceAndLocal  -- source == "workspace" || source == "extra"

derive instance eqModuleScope :: Eq ModuleScope

instance showModuleScope :: Show ModuleScope where
  show AllModules = "AllModules"
  show WorkspaceOnly = "WorkspaceOnly"
  show WorkspaceAndLocal = "WorkspaceAndLocal"

-- =============================================================================
-- Package Filtering (PackageSetPackage)
-- =============================================================================

-- | Filter packages based on scope
-- | Used by GalaxyBeeswarmViz for registry-level filtering
filterPackagesByScope :: BeeswarmScope -> Array PackageSetPackage -> Array PackageSetPackage
filterPackagesByScope scope packages = case scope of
  AllPackages -> packages
  ProjectOnly ->
    Array.filter (\p -> Set.member p.name projectSet) packages
  ProjectWithDeps ->
    Array.filter (\p -> Set.member p.name projectWithDepsSet) packages
  ProjectWithTransitive ->
    Array.filter (\p -> Set.member p.name transitiveSet) packages
  where
  projectSet = Set.fromFoldable projectPackages

  projectWithDepsSet = Set.union projectSet directDeps
    where
    projectPkgs = Array.filter (\p -> Set.member p.name projectSet) packages
    directDeps = Set.fromFoldable $ Array.concatMap _.depends projectPkgs

  transitiveSet = computeTransitiveDeps projectSet packages

-- | Compute transitive dependencies from a set of root packages
computeTransitiveDeps :: Set String -> Array PackageSetPackage -> Set String
computeTransitiveDeps roots packages =
  let
    depsMap = Map.fromFoldable $ packages <#> \p -> Tuple p.name p.depends
    go visited frontier
      | Set.isEmpty frontier = visited
      | otherwise =
          let
            newDeps = Set.fromFoldable $ Array.concatMap
              (\name -> fromMaybe [] $ Map.lookup name depsMap)
              (Array.fromFoldable frontier)
            unvisited = Set.difference newDeps visited
            newVisited = Set.union visited frontier
          in
            go newVisited unvisited
  in
    go Set.empty roots

-- =============================================================================
-- Node Filtering (SimNode)
-- =============================================================================

-- | Filter SimNodes based on scope
-- | Used by BubblePackBeeswarmViz for project-level filtering
-- | Uses the source field: "workspace" = local, "registry"/"extra" = dependencies
filterNodesByScope :: BeeswarmScope -> Array SimNode -> Array Package -> Array SimNode
filterNodesByScope scope nodes _packages = case scope of
  AllPackages -> nodes
  ProjectOnly ->
    -- Local packages only (source == "workspace")
    Array.filter (\n -> n.source == "workspace") nodes
  ProjectWithDeps ->
    -- Local + registry (include all for now)
    nodes
  ProjectWithTransitive ->
    -- Same as AllPackages for this view
    nodes

-- =============================================================================
-- Module Filtering (V2ModuleListItem)
-- =============================================================================

-- | Filter modules based on scope
-- | Used by ModuleBeeswarmViz and ModuleBubblePackViz
filterModulesByScope :: ModuleScope -> Array V2ModuleListItem -> Array V2ModuleListItem
filterModulesByScope scope modules = case scope of
  AllModules -> modules
  WorkspaceOnly ->
    -- Local packages only (source == "workspace")
    Array.filter (\m -> m.package.source == "workspace") modules
  WorkspaceAndLocal ->
    -- Workspace + extra (local libraries not in registry)
    Array.filter (\m -> m.package.source == "workspace" || m.package.source == "extra") modules

-- =============================================================================
-- Focal Package Filtering (for neighborhood view)
-- =============================================================================

-- | Filter SimNodes to only include a package's neighborhood
-- | Neighborhood = focal package + its dependencies + packages that depend on it
-- | Used by BubblePackBeeswarmViz when focalPackage is set
filterNodesByFocalPackage :: String -> Array SimNode -> Array Package -> Array SimNode
filterNodesByFocalPackage focalName nodes packages =
  let
    -- Find the focal package
    mFocalPkg = Array.find (\p -> p.name == focalName) packages

    -- Get direct dependencies of focal package
    focalDeps :: Set String
    focalDeps = case mFocalPkg of
      Just pkg -> Set.fromFoldable pkg.depends
      Nothing -> Set.empty

    -- Find packages that depend on the focal package (dependents)
    dependents :: Set String
    dependents = Set.fromFoldable $
      Array.mapMaybe
        (\pkg -> if Array.elem focalName pkg.depends then Just pkg.name else Nothing)
        packages

    -- Complete neighborhood: focal + deps + dependents
    neighborhood :: Set String
    neighborhood = Set.insert focalName (Set.union focalDeps dependents)

  in
    -- Filter nodes to only those whose package is in the neighborhood
    Array.filter (\n -> Set.member n.package neighborhood) nodes
