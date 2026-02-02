-- | Dependency Matrix Builder
-- |
-- | Builds NxN dependency matrices from module imports or package dependencies.
-- | The resulting matrix can be used for chord diagrams or adjacency matrices.
module CE2.Viz.DependencyMatrix
  ( DependencyData
  , buildFromModuleImports
  , buildFromPackageDependencies
  , filterToNames
  ) where

import Prelude

import Data.Array as Array
import Data.Array (length, (!!))
import Data.Foldable (foldl)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))

import CE2.Data.Loader (V2ModuleImports, V2Package, V2ModuleListItem)

-- | Generic dependency data structure for visualizations
-- | Works with both chord diagrams and adjacency matrices
type DependencyData =
  { names :: Array String           -- Entity names (modules or packages)
  , matrix :: Array (Array Number)  -- NxN connection matrix
  , totalConnections :: Int         -- Total number of non-zero connections
  }

-- | Build dependency matrix from module imports
-- | Each module becomes a row/column, imports become connections
buildFromModuleImports :: Array V2ModuleImports -> DependencyData
buildFromModuleImports imports =
  let
    -- Get all unique module names (both importers and imported)
    allNames = Set.fromFoldable $ Array.concat
      [ imports <#> _.moduleName
      , Array.concat $ imports <#> _.imports
      ]
    names = Set.toUnfoldable allNames :: Array String

    -- Build name -> index map
    nameIndex :: Map String Int
    nameIndex = Map.fromFoldable $ Array.mapWithIndex (\i n -> Tuple n i) names

    n = length names

    -- Build the matrix
    -- matrix[i][j] = 1 if module i imports module j
    matrix = Array.mapWithIndex (\i _ -> buildRow i) names

    buildRow :: Int -> Array Number
    buildRow rowIdx =
      let
        rowName = fromMaybe "" $ names !! rowIdx
        -- Find this module's imports
        moduleImports = Array.find (\m -> m.moduleName == rowName) imports
        importedNames = case moduleImports of
          Just m -> Set.fromFoldable m.imports
          Nothing -> Set.empty
      in
        Array.mapWithIndex (\colIdx _ ->
          let colName = fromMaybe "" $ names !! colIdx
          in if Set.member colName importedNames then 1.0 else 0.0
        ) names

    -- Count connections
    totalConnections = foldl (\acc row -> acc + foldl (\a v -> if v > 0.0 then a + 1 else a) 0 row) 0 matrix
  in
    { names, matrix, totalConnections }

-- | Build dependency matrix from package dependencies
-- | Each package becomes a row/column, cross-package module imports become connections
-- | Requires V2ModuleListItem to map module names to package names
buildFromPackageDependencies :: Array V2Package -> Array V2ModuleListItem -> Array V2ModuleImports -> DependencyData
buildFromPackageDependencies packages modules moduleImports =
  let
    -- Use package names
    names = packages <#> _.name
    packageSet = Set.fromFoldable names

    -- Build module name -> package name map
    moduleToPackage :: Map String String
    moduleToPackage = Map.fromFoldable $ modules <#> \m -> Tuple m.name m.package.name

    -- Build package name -> index map
    packageIndex :: Map String Int
    packageIndex = Map.fromFoldable $ Array.mapWithIndex (\i name -> Tuple name i) names

    -- Count cross-package imports: for each module import,
    -- if source and target are in different packages, increment matrix[srcPkg][tgtPkg]
    -- We'll use a Map to accumulate counts, then convert to matrix
    countImports :: Map (Tuple String String) Int
    countImports = foldl countModuleImports Map.empty moduleImports

    countModuleImports :: Map (Tuple String String) Int -> V2ModuleImports -> Map (Tuple String String) Int
    countModuleImports acc imp =
      let
        srcPkg = Map.lookup imp.moduleName moduleToPackage
      in case srcPkg of
        Nothing -> acc  -- Module not in our list, skip
        Just srcPkgName ->
          foldl (countOneImport srcPkgName) acc imp.imports

    countOneImport :: String -> Map (Tuple String String) Int -> String -> Map (Tuple String String) Int
    countOneImport srcPkgName acc importedModuleName =
      case Map.lookup importedModuleName moduleToPackage of
        Nothing -> acc  -- Imported module not in our package list (external)
        Just tgtPkgName
          | srcPkgName == tgtPkgName -> acc  -- Same package, skip
          | not (Set.member srcPkgName packageSet) -> acc  -- Source pkg not in our set
          | not (Set.member tgtPkgName packageSet) -> acc  -- Target pkg not in our set
          | otherwise ->
              let key = Tuple srcPkgName tgtPkgName
              in Map.alter (Just <<< (_ + 1) <<< fromMaybe 0) key acc

    -- Build the matrix from counts
    matrix = names <#> \rowName ->
      names <#> \colName ->
        let count = fromMaybe 0 $ Map.lookup (Tuple rowName colName) countImports
        in if count > 0 then 1.0 else 0.0  -- Binary: has dependency or not

    -- Count total connections
    totalConnections = Map.size $ Map.filter (_ > 0) countImports
  in
    { names, matrix, totalConnections }

-- | Filter dependency data to only include specified names
-- | Useful for focusing on a subset (e.g., project modules only)
filterToNames :: Set String -> DependencyData -> DependencyData
filterToNames keepNames data_ =
  let
    -- Find indices to keep
    keepIndices = Array.mapWithIndex Tuple data_.names
      # Array.filter (\(Tuple _ name) -> Set.member name keepNames)
      # map (\(Tuple i _) -> i)

    -- Filter names
    names = Array.mapMaybe (\i -> data_.names !! i) keepIndices

    -- Filter matrix (keep only rows and columns for kept indices)
    matrix = keepIndices <#> \rowIdx ->
      let fullRow = fromMaybe [] $ data_.matrix !! rowIdx
      in keepIndices <#> \colIdx -> fromMaybe 0.0 $ fullRow !! colIdx

    totalConnections = foldl (\acc row -> acc + foldl (\a v -> if v > 0.0 then a + 1 else a) 0 row) 0 matrix
  in
    { names, matrix, totalConnections }
