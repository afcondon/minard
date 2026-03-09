-- | Pure Model Transformations for Loader
-- |
-- | Graph construction, node creation, and link building extracted from
-- | Loader. These have no Effect or Aff dependencies — they take
-- | decoded data and return visualization-ready structures.
module CE2.Data.Loader.Transform
  ( -- Legacy model transforms
    transformToModel
  , buildLocMap
    -- V2 model transforms
  , transformV2ToModel
    -- Shared helpers (used by both pipelines)
  , buildSourcesMap
  , buildTargetsMap
  , buildPackageModulesMap
  , buildPackageLocMap
  , buildLinks
  , stringHash
  , charCode
    -- Legacy types (needed by legacy loaders)
  , RawModule
  , RawPackage
  , LocEntry
  , LocFile
    -- Structural type aliases (compatible with Loader types)
  , DeclarationsMap
  , LoadedModel
  , V2Package
  , V2ModuleListItem
  , V2ModuleImports
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl)
import Data.Foldable (maximum) as Foldable
import Data.Int (toNumber)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (pi, cos, sin, sqrt)
import Data.Nullable (null)
import Data.Set (Set)
import Data.Set as Set
import Data.String.CodeUnits as SCU
import Data.String.Pattern (Pattern(..))
import Data.Tuple (Tuple(..))
import Foreign.Object (Object)
import Foreign.Object as Object
import Data.Graph.Algorithms as TopoAlgorithms

import CE2.Types (SimNode, SimLink, NodeType(..), LinkType(..), Package)

-- =============================================================================
-- Legacy Types (used by legacy JSON loaders)
-- =============================================================================

-- | Raw module from JSON
type RawModule =
  { depends :: Array String
  , package :: String
  , path :: String
  }

-- | Raw package from JSON
type RawPackage =
  { depends :: Array String
  }

-- | LOC entry from JSON
type LocEntry =
  { loc :: Int
  , path :: String
  }

-- | LOC file structure
type LocFile =
  { loc :: Array LocEntry
  }

-- =============================================================================
-- Legacy Transformation
-- =============================================================================

-- | DeclarationsMap: module name -> array of declarations (structural alias matching DeclarationsMap)
type DeclarationsMap = Object (Array { kind :: String, title :: String })

-- | LoadedModel structural type (matching LoadedModel)
type LoadedModel =
  { nodes :: Array SimNode
  , links :: Array SimLink
  , packages :: Array Package
  , declarations :: DeclarationsMap
  , moduleCount :: Int
  , packageCount :: Int
  }

-- | V2 package record (structural alias matching V2Package)
type V2Package =
  { id :: Int
  , name :: String
  , version :: String
  , description :: Maybe String
  , license :: Maybe String
  , repository :: Maybe String
  , source :: String
  , bundleModule :: Maybe String
  , moduleCount :: Int
  , declarationCount :: Int
  , totalLoc :: Int
  , depends :: Array String
  , topoLayer :: Int
  }

-- | V2 module list item (structural alias matching V2ModuleListItem)
type V2ModuleListItem =
  { id :: Int
  , name :: String
  , path :: Maybe String
  , loc :: Maybe Int
  , package :: { id :: Int, name :: String, version :: String, source :: String }
  , namespacePath :: Maybe String
  , declarationCount :: Int
  }

-- | V2 module imports (structural alias matching V2ModuleImports)
type V2ModuleImports =
  { moduleId :: Int
  , moduleName :: String
  , imports :: Array String
  }

transformToModel :: Object RawModule -> Object RawPackage -> Map String Int -> DeclarationsMap -> LoadedModel
transformToModel modulesObj packagesObj locMap declarations =
  let
    -- Get arrays
    moduleNames = Object.keys modulesObj
    packageNames = Object.keys packagesObj

    -- Build package -> modules map
    packageModules = buildPackageModulesMap modulesObj

    -- Build package -> total LOC map (sum of module LOC for each package)
    packageLocMap = buildPackageLocMap modulesObj locMap

    -- Assign IDs: packages first, then modules
    packageCount = Array.length packageNames
    moduleCount = Array.length moduleNames

    -- =========================================================================
    -- Compute topological layers for packages
    -- =========================================================================

    -- Derive package dependencies from module dependencies
    derivedPackageDeps :: Map String (Set String)
    derivedPackageDeps = foldl addModuleDeps Map.empty (Object.toUnfoldable modulesObj :: Array (Tuple String RawModule))
      where
        addModuleDeps :: Map String (Set String) -> Tuple String RawModule -> Map String (Set String)
        addModuleDeps acc (Tuple moduleName rawMod) =
          let
            thisPackage = rawMod.package
            depPackages = Set.fromFoldable $ Array.mapMaybe getPackageOfModule rawMod.depends
            externalDeps = Set.delete thisPackage depPackages
            existing = fromMaybe Set.empty (Map.lookup thisPackage acc)
          in
            Map.insert thisPackage (Set.union existing externalDeps) acc

        getPackageOfModule :: String -> Maybe String
        getPackageOfModule modName = Object.lookup modName modulesObj <#> _.package

    -- Convert packages to TaskNodes for topo sort
    packageTaskNodes :: Array (TopoAlgorithms.TaskNode String)
    packageTaskNodes = packageNames <#> \name ->
      { id: name
      , depends: Array.fromFoldable $ fromMaybe Set.empty (Map.lookup name derivedPackageDeps)
      }

    -- Get layered packages (each has id, layer, depends)
    layeredPackages = TopoAlgorithms.addLayers packageTaskNodes

    -- Build a map from package name to layer
    packageLayerMap :: Map String Int
    packageLayerMap = Map.fromFoldable $ layeredPackages <#> \lp -> Tuple lp.id lp.layer

    -- Find max layer for positioning
    maxLayer = fromMaybe 0 $ Foldable.maximum (layeredPackages <#> _.layer)

    -- Count packages per layer for x-positioning within layer
    packagesByLayer :: Map Int (Array String)
    packagesByLayer = foldl addToLayer Map.empty layeredPackages
      where
        addToLayer acc lp =
          let existing = fromMaybe [] $ Map.lookup lp.layer acc
          in Map.insert lp.layer (Array.snoc existing lp.id) acc

    -- Create name -> ID maps (needed for package dependency lookup)
    packageIdMap = Map.fromFoldable $ Array.mapWithIndex (\i n -> Tuple n i) packageNames

    -- Create package nodes (IDs 0 to packageCount-1)
    packageNodes = Array.mapWithIndex (mkPackageNode packageNames packageCount packageLocMap packageLayerMap packagesByLayer maxLayer derivedPackageDeps packageIdMap) packageNames
    moduleIdMap = Map.fromFoldable $ Array.mapWithIndex (\i n -> Tuple n (i + packageCount)) moduleNames

    -- Build targets map (module ID -> array of dependency IDs)
    targetsMap = buildTargetsMap modulesObj moduleIdMap

    -- Build sources map (module ID -> array of dependents' IDs)
    sourcesMap = buildSourcesMap targetsMap

    -- Create module nodes (IDs packageCount to packageCount+moduleCount-1)
    moduleNodes = Array.mapWithIndex
      (\i name -> mkModuleNode name i modulesObj locMap packageIdMap moduleIdMap packageCount moduleCount packageNodes targetsMap sourcesMap)
      moduleNames

    -- All nodes
    nodes = packageNodes <> moduleNodes

    -- All module links
    links = buildLinks modulesObj moduleIdMap

    -- Create Package records for model
    packages = Array.mapWithIndex
      ( \_ name ->
          { name
          , depends: fromMaybe [] $ Object.lookup name packagesObj <#> _.depends
          , modules: fromMaybe [] $ Map.lookup name packageModules
          }
      )
      packageNames
  in
    { nodes, links, packages, declarations, moduleCount, packageCount }

-- =============================================================================
-- Legacy Node Creation
-- =============================================================================

mkPackageNode :: Array String -> Int -> Map String Int -> Map String Int -> Map Int (Array String) -> Int -> Map String (Set String) -> Map String Int -> Int -> String -> SimNode
mkPackageNode _allPackages totalPackages packageLocMap packageLayerMap packagesByLayer maxLayer derivedPackageDeps packageIdMap idx name =
  let
    gridCols = 8
    gridSpacing = 120.0
    gridRow = toNumber (idx / gridCols)
    gridCol = toNumber (idx `mod` gridCols)
    gx = (gridCol - toNumber gridCols / 2.0 + 0.5) * gridSpacing
    gy = (gridRow - toNumber (totalPackages / gridCols) / 2.0) * gridSpacing

    angle = 2.0 * pi * toNumber idx / toNumber totalPackages

    totalLoc = fromMaybe 100 (Map.lookup name packageLocMap)
    r = max 8.0 (sqrt (toNumber totalLoc) * 0.5)

    layer = fromMaybe 0 (Map.lookup name packageLayerMap)
    packagesInLayer = fromMaybe [] (Map.lookup layer packagesByLayer)
    indexInLayer = fromMaybe 0 (Array.elemIndex name packagesInLayer)
    countInLayer = Array.length packagesInLayer

    topoLayerSpacing = 150.0
    topoNodeSpacing = 60.0

    tx = (toNumber maxLayer / 2.0 - toNumber layer) * topoLayerSpacing
    ty = (toNumber indexInLayer - toNumber countInLayer / 2.0 + 0.5) * topoNodeSpacing

    depPackageNames = fromMaybe Set.empty (Map.lookup name derivedPackageDeps)
    targets = Array.mapMaybe (\n -> Map.lookup n packageIdMap) (Array.fromFoldable depPackageNames)

    sources = Array.mapMaybe (\n -> Map.lookup n packageIdMap) $
      Array.filter (\otherName -> Set.member name (fromMaybe Set.empty (Map.lookup otherName derivedPackageDeps))) _allPackages
  in
    { id: idx
    , name
    , nodeType: PackageNode
    , package: name
    , path: ""
    , x: gx
    , y: gy
    , vx: 0.0
    , vy: 0.0
    , fx: null
    , fy: null
    , r
    , cluster: idx
    , targets
    , sources
    , gridX: gx
    , gridY: gy
    , orbitAngle: angle
    , treeX: 0.0
    , treeY: 0.0
    , radialX: 0.0
    , radialY: 0.0
    , isInTree: false
    , topoX: tx
    , topoY: ty
    , topoLayer: layer
    , source: "registry"
    }

mkModuleNode
  :: String
  -> Int
  -> Object RawModule
  -> Map String Int
  -> Map String Int
  -> Map String Int
  -> Int
  -> Int
  -> Array SimNode
  -> Map Int (Array Int)
  -> Map Int (Array Int)
  -> SimNode
mkModuleNode name idx modulesObj locMap packageIdMap _moduleIdMap packageCount _moduleCount packageNodes targetsMap sourcesMap =
  let
    nodeId = idx + packageCount
    rawMod = Object.lookup name modulesObj
    pkgName = fromMaybe "unknown" (rawMod <#> _.package)
    path = fromMaybe "" (rawMod <#> _.path)
    loc = fromMaybe 50 (Map.lookup path locMap)

    cluster = fromMaybe 0 (Map.lookup pkgName packageIdMap)

    r = 4.0 + sqrt (toNumber loc) * 0.8

    pkgId = fromMaybe 0 (Map.lookup pkgName packageIdMap)
    pkgNode = Array.index packageNodes pkgId
    pkgGridX = fromMaybe 0.0 (pkgNode <#> _.gridX)
    pkgGridY = fromMaybe 0.0 (pkgNode <#> _.gridY)

    nameHash = stringHash name
    offsetAngle = 2.0 * pi * toNumber (nameHash `mod` 360) / 360.0
    offsetDist = 30.0 + toNumber ((nameHash / 360) `mod` 30)
    offsetX = cos offsetAngle * offsetDist
    offsetY = sin offsetAngle * offsetDist

    absGridX = pkgGridX + offsetX
    absGridY = pkgGridY + offsetY

    targets = fromMaybe [] (Map.lookup nodeId targetsMap)
    sources = fromMaybe [] (Map.lookup nodeId sourcesMap)
  in
    { id: nodeId
    , name
    , nodeType: ModuleNode
    , package: pkgName
    , path
    , x: absGridX
    , y: absGridY
    , vx: 0.0
    , vy: 0.0
    , fx: null
    , fy: null
    , r
    , cluster
    , targets
    , sources
    , gridX: absGridX
    , gridY: absGridY
    , orbitAngle: 0.0
    , treeX: 0.0
    , treeY: 0.0
    , radialX: 0.0
    , radialY: 0.0
    , isInTree: false
    , topoX: 0.0
    , topoY: 0.0
    , topoLayer: 0
    , source: "registry"
    }

-- =============================================================================
-- V2 Model Transformation
-- =============================================================================

transformV2ToModel :: Array V2Package -> Array V2ModuleListItem -> Array V2ModuleImports -> LoadedModel
transformV2ToModel v2Packages v2Modules v2Imports =
  let
    -- Build module name -> ID map for link building
    moduleIdMap :: Map String Int
    moduleIdMap = Map.fromFoldable $ v2Modules <#> \m -> Tuple m.name m.id

    -- Build imports map: moduleId -> [imported module names]
    importsMap :: Map Int (Array String)
    importsMap = Map.fromFoldable $ v2Imports <#> \mi -> Tuple mi.moduleId mi.imports

    -- Package count (for module ID offset)
    packageCount = Array.length v2Packages

    -- Create package name -> index map for cluster assignment
    packageIndexMap :: Map String Int
    packageIndexMap = Map.fromFoldable $ Array.mapWithIndex (\i p -> Tuple p.name i) v2Packages

    -- =========================================================================
    -- Use topological layers from API
    -- =========================================================================

    packageLayerMap :: Map String Int
    packageLayerMap = Map.fromFoldable $ v2Packages <#> \p -> Tuple p.name p.topoLayer

    maxLayer = fromMaybe 0 $ Foldable.maximum (v2Packages <#> _.topoLayer)

    packagesByLayer :: Map Int (Array String)
    packagesByLayer = foldl addToLayer Map.empty v2Packages
      where
        addToLayer acc pkg =
          let existing = fromMaybe [] $ Map.lookup pkg.topoLayer acc
          in Map.insert pkg.topoLayer (Array.snoc existing pkg.name) acc

    -- =========================================================================
    -- Create Package Nodes
    -- =========================================================================

    packageNodes :: Array SimNode
    packageNodes = Array.mapWithIndex mkPackageNodeV2 v2Packages

    mkPackageNodeV2 :: Int -> V2Package -> SimNode
    mkPackageNodeV2 idx pkg =
      let
        gridCols = 8
        gridSpacing = 120.0
        gridRow = toNumber (idx / gridCols)
        gridCol = toNumber (idx `mod` gridCols)
        gx = (gridCol - toNumber gridCols / 2.0 + 0.5) * gridSpacing
        gy = (gridRow - toNumber packageCount / toNumber gridCols / 2.0) * gridSpacing

        angle = 2.0 * pi * toNumber idx / toNumber packageCount

        r = max 8.0 (sqrt (toNumber (pkg.moduleCount * 100)) * 0.5)

        layer = fromMaybe 0 (Map.lookup pkg.name packageLayerMap)
        packagesInLayer = fromMaybe [] (Map.lookup layer packagesByLayer)
        indexInLayer = fromMaybe 0 (Array.elemIndex pkg.name packagesInLayer)
        countInLayer = Array.length packagesInLayer

        topoLayerSpacing = 150.0
        topoNodeSpacing = 60.0
        tx = (toNumber maxLayer / 2.0 - toNumber layer) * topoLayerSpacing
        ty = (toNumber indexInLayer - toNumber countInLayer / 2.0 + 0.5) * topoNodeSpacing

        targets = Array.mapMaybe (\n -> Map.lookup n packageIndexMap) pkg.depends

        sources = Array.mapMaybe (\n -> Map.lookup n packageIndexMap) $
          Array.filter (\other -> Array.elem pkg.name other.depends) v2Packages <#> _.name
      in
        { id: idx
        , name: pkg.name
        , nodeType: PackageNode
        , package: pkg.name
        , path: ""
        , x: gx
        , y: gy
        , vx: 0.0
        , vy: 0.0
        , fx: null
        , fy: null
        , r
        , cluster: idx
        , targets
        , sources
        , gridX: gx
        , gridY: gy
        , orbitAngle: angle
        , treeX: 0.0
        , treeY: 0.0
        , radialX: 0.0
        , radialY: 0.0
        , isInTree: false
        , topoX: tx
        , topoY: ty
        , topoLayer: layer
        , source: pkg.source
        }

    -- Build package source map for module nodes
    packageSourceMap :: Map String String
    packageSourceMap = Map.fromFoldable $ v2Packages <#> \p -> Tuple p.name p.source

    -- =========================================================================
    -- Create Module Nodes
    -- =========================================================================

    moduleTargetsMap :: Map Int (Array Int)
    moduleTargetsMap = Map.fromFoldable $ v2Modules <#> \m ->
      let
        imports = fromMaybe [] (Map.lookup m.id importsMap)
        targetIds = Array.mapMaybe (\impName -> Map.lookup impName moduleIdMap) imports
      in
        Tuple m.id targetIds

    moduleSourcesMap :: Map Int (Array Int)
    moduleSourcesMap = buildSourcesMap moduleTargetsMap

    moduleNodes :: Array SimNode
    moduleNodes = v2Modules <#> mkModuleNodeV2

    mkModuleNodeV2 :: V2ModuleListItem -> SimNode
    mkModuleNodeV2 m =
      let
        pkgIdx = fromMaybe 0 (Map.lookup m.package.name packageIndexMap)
        mPkgNode = Array.index packageNodes pkgIdx

        pkgGridX = fromMaybe 0.0 (mPkgNode <#> _.gridX)
        pkgGridY = fromMaybe 0.0 (mPkgNode <#> _.gridY)
        pkgOrbitAngle = fromMaybe 0.0 (mPkgNode <#> _.orbitAngle)
        pkgTopoX = fromMaybe 0.0 (mPkgNode <#> _.topoX)
        pkgTopoY = fromMaybe 0.0 (mPkgNode <#> _.topoY)
        pkgTopoLayer = fromMaybe 0 (mPkgNode <#> _.topoLayer)

        loc = fromMaybe 100 m.loc
        r = max 3.0 (sqrt (toNumber loc) * 0.3)

        x = pkgGridX + (toNumber (m.id `mod` 5) - 2.0) * 5.0
        y = pkgGridY + (toNumber (m.id `mod` 7) - 3.0) * 5.0

        targets = fromMaybe [] (Map.lookup m.id moduleTargetsMap)
        sources = fromMaybe [] (Map.lookup m.id moduleSourcesMap)
      in
        { id: m.id
        , name: m.name
        , nodeType: ModuleNode
        , package: m.package.name
        , path: fromMaybe "" m.path
        , x
        , y
        , vx: 0.0
        , vy: 0.0
        , fx: null
        , fy: null
        , r
        , cluster: pkgIdx
        , targets
        , sources
        , gridX: pkgGridX
        , gridY: pkgGridY
        , orbitAngle: pkgOrbitAngle
        , treeX: 0.0
        , treeY: 0.0
        , radialX: 0.0
        , radialY: 0.0
        , isInTree: false
        , topoX: pkgTopoX
        , topoY: pkgTopoY
        , topoLayer: pkgTopoLayer
        , source: fromMaybe "registry" (Map.lookup m.package.name packageSourceMap)
        }

    -- =========================================================================
    -- Create Links
    -- =========================================================================

    links :: Array SimLink
    links = Array.concatMap mkModuleLinks v2Modules

    mkModuleLinks :: V2ModuleListItem -> Array SimLink
    mkModuleLinks m =
      let
        imports = fromMaybe [] (Map.lookup m.id importsMap)
      in
        Array.mapMaybe (\impName -> mkLink m.id impName) imports

    mkLink :: Int -> String -> Maybe SimLink
    mkLink sourceId targetName = do
      targetId <- Map.lookup targetName moduleIdMap
      if targetId /= sourceId
        then Just
          { source: sourceId
          , target: targetId
          , linkType: M2M_Graph
          }
        else Nothing

    -- All nodes
    nodes = packageNodes <> moduleNodes

    -- Create Package records for model
    packages :: Array Package
    packages = v2Packages <#> \p ->
      { name: p.name
      , depends: p.depends
      , modules: Array.filter (\m -> m.package.name == p.name) v2Modules <#> _.name
      }

    -- Empty declarations (loaded on demand per module)
    declarations :: DeclarationsMap
    declarations = Object.empty
  in
    { nodes
    , links
    , packages
    , declarations
    , moduleCount: Array.length v2Modules
    , packageCount: Array.length v2Packages
    }

-- =============================================================================
-- Shared Helpers
-- =============================================================================

buildLocMap :: Array LocEntry -> Map String Int
buildLocMap entries =
  Map.fromFoldable $ map (\e -> Tuple e.path e.loc) entries

buildPackageModulesMap :: Object RawModule -> Map String (Array String)
buildPackageModulesMap modulesObj =
  foldl addModule Map.empty (Object.toUnfoldable modulesObj :: Array (Tuple String RawModule))
  where
  addModule acc (Tuple modName rawMod) =
    let
      pkg = rawMod.package
      existing = fromMaybe [] (Map.lookup pkg acc)
    in
      Map.insert pkg (Array.snoc existing modName) acc

-- | Build map from package name to total LOC (sum of all module LOC)
buildPackageLocMap :: Object RawModule -> Map String Int -> Map String Int
buildPackageLocMap modulesObj locMap =
  foldl addModuleLoc Map.empty (Object.toUnfoldable modulesObj :: Array (Tuple String RawModule))
  where
  addModuleLoc acc (Tuple _ rawMod) =
    let
      pkg = rawMod.package
      moduleLoc = fromMaybe 50 (Map.lookup rawMod.path locMap)
      existing = fromMaybe 0 (Map.lookup pkg acc)
    in
      Map.insert pkg (existing + moduleLoc) acc

buildLinks :: Object RawModule -> Map String Int -> Array SimLink
buildLinks modulesObj moduleIdMap =
  Array.concat $ map mkLinks (Object.toUnfoldable modulesObj :: Array (Tuple String RawModule))
  where
  mkLinks (Tuple modName rawMod) =
    case Map.lookup modName moduleIdMap of
      Nothing -> []
      Just sourceId ->
        Array.mapMaybe
          ( \depName ->
              Map.lookup depName moduleIdMap <#> \targetId ->
                { source: sourceId
                , target: targetId
                , linkType: M2M_Graph
                }
          )
          rawMod.depends

-- | Build map from module ID to its dependencies (targets)
buildTargetsMap :: Object RawModule -> Map String Int -> Map Int (Array Int)
buildTargetsMap modulesObj moduleIdMap =
  foldl addTargets Map.empty (Object.toUnfoldable modulesObj :: Array (Tuple String RawModule))
  where
  addTargets acc (Tuple modName rawMod) =
    case Map.lookup modName moduleIdMap of
      Nothing -> acc
      Just sourceId ->
        let
          targets = Array.mapMaybe (\depName -> Map.lookup depName moduleIdMap) rawMod.depends
        in
          Map.insert sourceId targets acc

-- | Build map from module ID to modules that depend on it (sources/dependents)
buildSourcesMap :: Map Int (Array Int) -> Map Int (Array Int)
buildSourcesMap targetsMap =
  foldl addSources Map.empty (Map.toUnfoldable targetsMap :: Array (Tuple Int (Array Int)))
  where
  addSources acc (Tuple sourceId targets) =
    foldl
      ( \acc' targetId ->
          let
            existing = fromMaybe [] (Map.lookup targetId acc')
          in
            Map.insert targetId (Array.snoc existing sourceId) acc'
      )
      acc
      targets

-- Simple string hash function for deterministic positioning
stringHash :: String -> Int
stringHash s =
  let
    chars = SCU.toCharArray s
  in
    foldl (\acc c -> (acc * 31 + charCode c) `mod` 1000000) 0 chars

-- Get char code (simple implementation)
charCode :: Char -> Int
charCode c = case SCU.indexOf (Pattern (SCU.singleton c)) alphabet of
  Just i -> i
  Nothing -> 0
  where
  alphabet = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789._-"
