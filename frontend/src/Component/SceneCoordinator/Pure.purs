-- | Pure Computations for Scene Coordinator
-- |
-- | Graph algorithms, lookup helpers, and pure transformations extracted from
-- | SceneCoordinator. These have no Halogen or Effect dependencies — they take
-- | data and return data.
module CE2.Component.SceneCoordinator.Pure
  ( themeForScene
  , canonicalStateCode
  , lookupModuleDeclarations
  , buildKnownDeclarations
  , buildModuleImportMap
  , buildModuleImportedByMap
  , computePackageReachability
  , computeGlobalReachability
  , computePackageClusters
  , computePackageGitStatus
  , solarSwarmScopedPackages
  , sceneForResult
  , ViewMode(..)
  , viewModeToString
  , viewModeFromString
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))

import CE2.Data.Loader as Loader
import CE2.Scene (Scene(..))
import CE2.Types (ViewTheme(..), BeeswarmScope(..), PackageGitStatus, PackageReachability, PackageClusters, projectPackages)
import CE2.Viz.SourceCode as SourceCode
import Data.Graph.Algorithms (reachableFrom, connectedComponents, labelPropagation) as GraphAlgo

-- =============================================================================
-- Theme
-- =============================================================================

-- | Get appropriate theme for a scene
-- | Five "Powers of Ten" levels: dark→light luminance gradient
-- |   Package Set (registry) → Midnight (near-black)
-- |   Neighborhood (project packages) → Blueprint blue
-- |   Package (modules) → Steel blue
-- |   Module (declarations) → Mist (pale blue)
-- |   Declaration → Daylight (white)
themeForScene :: Scene -> ViewTheme
themeForScene = case _ of
  GalaxyTreemap -> MidnightTheme
  GalaxyBeeswarm -> MidnightTheme
  SolarSwarm -> MidnightTheme
  PkgTreemap _ -> SteelTheme
  PkgModuleBeeswarm _ -> SteelTheme
  ModuleOverview _ _ -> MistTheme
  DeclarationDetail _ _ _ -> DaylightTheme
  ModuleSignatureMap _ _ -> MistTheme
  TypeClassGrid -> MidnightTheme
  NamespaceTree -> DaylightTheme
  PackageReport -> DaylightTheme
  AnnotationReport -> DaylightTheme
  ProjectManagement -> DaylightTheme
  ProjectAnatomy -> DaylightTheme
  StructuralDecomp -> DaylightTheme
  ModuleStructure _ _ -> DaylightTheme
  CompareModules _ _ _ _ -> DaylightTheme
  CompareSnapshots _ _ _ -> DaylightTheme
  SnapshotManagement -> DaylightTheme
  CommitModuleGrid _ -> DaylightTheme
  CoChangeCube _ -> DaylightTheme

-- =============================================================================
-- State Code
-- =============================================================================

-- | Canonical state code for precise communication
-- | See docs/kb/reference/ce2-state-machine-analysis.md for full naming system
canonicalStateCode
  :: forall r. { scene :: Scene, scope :: BeeswarmScope, focalPackage :: Maybe String, viewMode :: ViewMode | r }
  -> String
canonicalStateCode state = case state.scene of
  GalaxyTreemap -> "A"

  GalaxyBeeswarm -> "B" <> scopeDigit state.scope

  SolarSwarm -> "C" <> scopeDigit state.scope <> focalSuffix <> viewSuffix state.viewMode
    where focalSuffix = case state.focalPackage of
            Just pkg -> "(" <> pkg <> ")"
            Nothing -> ""

  PkgTreemap pkg -> "E(" <> pkg <> ")" <> viewSuffix state.viewMode

  PkgModuleBeeswarm pkg -> "F(" <> pkg <> ")"

  ModuleOverview pkg mod -> "G(" <> pkg <> "," <> mod <> ")"

  DeclarationDetail pkg mod decl -> "H(" <> pkg <> "," <> mod <> "," <> decl <> ")"

  ModuleSignatureMap pkg mod -> "S(" <> pkg <> "," <> mod <> ")"

  TypeClassGrid -> "T"
  NamespaceTree -> "N"

  PackageReport -> "PR"
  AnnotationReport -> "R"
  ProjectManagement -> "P"
  ProjectAnatomy -> "Y"
  StructuralDecomp -> "D"
  ModuleStructure pkg mod -> "X(" <> pkg <> "," <> mod <> ")"
  CompareModules p1 m1 p2 m2 -> "V(" <> p1 <> "," <> m1 <> "," <> p2 <> "," <> m2 <> ")"
  CompareSnapshots p m sid -> "Z(" <> p <> "," <> m <> "," <> show sid <> ")"
  SnapshotManagement -> "SM"
  CommitModuleGrid pkg -> "CG(" <> pkg <> ")"
  CoChangeCube pkg -> "CC(" <> pkg <> ")"

  where
  scopeDigit :: BeeswarmScope -> String
  scopeDigit = case _ of
    AllPackages -> "0"
    ProjectWithTransitive -> "1"
    ProjectWithDeps -> "2"
    ProjectOnly -> "3"

  viewSuffix :: ViewMode -> String
  viewSuffix = case _ of
    PrimaryView -> ""
    MatrixView -> "M"
    ChordView -> "C"

-- | View mode for package/solar-swarm level scenes
data ViewMode
  = PrimaryView    -- Default: BubblePack for packages, Treemap for modules
  | MatrixView     -- Adjacency matrix
  | ChordView      -- Chord diagram

derive instance eqViewMode :: Eq ViewMode

instance showViewMode :: Show ViewMode where
  show PrimaryView = "PrimaryView"
  show MatrixView = "MatrixView"
  show ChordView = "ChordView"

viewModeToString :: ViewMode -> String
viewModeToString = case _ of
  PrimaryView -> "primary"
  MatrixView -> "matrix"
  ChordView -> "chord"

viewModeFromString :: String -> ViewMode
viewModeFromString = case _ of
  "matrix" -> MatrixView
  "chord" -> ChordView
  _ -> PrimaryView

-- =============================================================================
-- Lookup Helpers
-- =============================================================================

-- | Look up declarations for a module within a package
lookupModuleDeclarations
  :: forall r1 r2
   . { v2Data :: Maybe { modules :: Array Loader.V2ModuleListItem | r1 }
     , packageDeclarations :: Map Int (Array Loader.V2Declaration)
     | r2
     }
  -> String -> String -> Maybe (Array Loader.V2Declaration)
lookupModuleDeclarations state pkgName modName = do
  v2 <- state.v2Data
  mod <- Array.find (\m -> m.name == modName && m.package.name == pkgName) v2.modules
  Map.lookup mod.id state.packageDeclarations

-- | Build cross-reference index of all loaded declarations for source code navigation
buildKnownDeclarations
  :: forall r1 r2
   . { v2Data :: Maybe { modules :: Array Loader.V2ModuleListItem | r1 }
     , packageDeclarations :: Map Int (Array Loader.V2Declaration)
     | r2
     }
  -> Array SourceCode.KnownDeclaration
buildKnownDeclarations state =
  case state.v2Data of
    Nothing -> []
    Just v2 ->
      let
        moduleInfo = Map.fromFoldable $ map (\m -> Tuple m.id { moduleName: m.name, packageName: m.package.name }) v2.modules
      in
        Array.concatMap (\(Tuple modId decls) ->
          case Map.lookup modId moduleInfo of
            Nothing -> []
            Just info -> map (\d ->
              { name: d.name
              , moduleName: info.moduleName
              , packageName: info.packageName
              , kind: d.kind
              }) decls
        ) (Map.toUnfoldable state.packageDeclarations)

-- =============================================================================
-- Module Import Maps
-- =============================================================================

-- | Build a map from module name to the modules it imports
buildModuleImportMap :: Array Loader.V2ModuleImports -> Map String (Array String)
buildModuleImportMap imports =
  Map.fromFoldable $ imports <#> \imp -> Tuple imp.moduleName imp.imports

-- | Build a reverse map: module name to modules that import it
buildModuleImportedByMap :: Array Loader.V2ModuleImports -> Map String (Array String)
buildModuleImportedByMap imports =
  let
    pairs :: Array (Tuple String String)
    pairs = Array.concatMap (\imp ->
        imp.imports <#> \imported -> Tuple imported imp.moduleName
      ) imports
  in
    foldl (\acc (Tuple imported importer) ->
      Map.alter (Just <<< Array.cons importer <<< fromMaybe []) imported acc
    ) Map.empty pairs

-- =============================================================================
-- Package Reachability
-- =============================================================================

-- | Compute which modules in a package are reachable
-- | Two modes, determined by bundleModule:
-- |   - Library mode (Nothing): entry points are modules imported by external packages
-- |   - App mode (Just mainMod): entry point is the bundle module (e.g. CE2.Main)
computePackageReachability
  :: String                         -- target package name
  -> Maybe String                   -- bundle module (Just for apps, Nothing for libraries)
  -> Array Loader.V2ModuleImports   -- all imports
  -> Array Loader.V2ModuleListItem  -- all modules (with package info)
  -> PackageReachability
computePackageReachability targetPkg bundleModule allImports allModules =
  let
    modToPkg :: Map String String
    modToPkg = Map.fromFoldable $ allModules <#> \m -> Tuple m.name m.package.name

    targetMods :: Set String
    targetMods = Set.fromFoldable $
      Array.filter (\m -> m.package.name == targetPkg) allModules <#> _.name

    importsOf :: Map String (Set String)
    importsOf = Map.fromFoldable $
      allImports <#> \imp -> Tuple imp.moduleName (Set.fromFoldable imp.imports)

    internalGraph =
      { nodes: Array.fromFoldable targetMods
      , edges: Map.fromFoldable $ (Array.fromFoldable targetMods) <#> \mod ->
          Tuple mod (Set.intersection (fromMaybe Set.empty (Map.lookup mod importsOf)) targetMods)
      }

    entryPoints = case bundleModule of
      Just mainMod | Set.member mainMod targetMods ->
        Set.singleton mainMod
      _ ->
        foldl (\acc imp ->
          let importerPkg = Map.lookup imp.moduleName modToPkg
          in if importerPkg /= Just targetPkg
             then foldl (\a imported ->
                    if Set.member imported targetMods
                    then Set.insert imported a
                    else a
                  ) acc imp.imports
             else acc
        ) Set.empty allImports

    isApp = case bundleModule of
              Just m -> Set.member m targetMods
              Nothing -> false

    reachable = Set.unions $
      (Array.fromFoldable entryPoints) <#> \ep -> GraphAlgo.reachableFrom ep internalGraph
  in
    { reachable, entryPoints, packageName: targetPkg, isApp }

-- | Compute global reachability: which modules across ALL packages are
-- | transitively reachable from the app entry point.
computeGlobalReachability
  :: Array Loader.V2ModuleImports
  -> Array Loader.V2ModuleListItem
  -> Array Loader.V2Package
  -> PackageReachability
computeGlobalReachability allImports allModules allPackages =
  let
    mEntry = Array.findMap (\p -> p.bundleModule) allPackages

    allModNames :: Set String
    allModNames = Set.fromFoldable $ allModules <#> _.name

    globalGraph =
      { nodes: Array.fromFoldable allModNames
      , edges: Map.fromFoldable $
          allImports <#> \imp -> Tuple imp.moduleName (Set.fromFoldable imp.imports)
      }

    entryPoints = case mEntry of
      Just mainMod | Set.member mainMod allModNames -> Set.singleton mainMod
      _ -> Set.empty

    reachable = Set.unions $
      (Array.fromFoldable entryPoints) <#> \ep -> GraphAlgo.reachableFrom ep globalGraph
  in
    { reachable, entryPoints, packageName: "*", isApp: true }

-- =============================================================================
-- Package Clusters
-- =============================================================================

-- | Compute dependency clusters for modules within a package
-- | Uses connectedComponents for broad grouping and labelPropagation for finer communities
computePackageClusters
  :: String                         -- target package name
  -> Array Loader.V2ModuleImports   -- all imports
  -> Array Loader.V2ModuleListItem  -- all modules
  -> PackageClusters
computePackageClusters targetPkg allImports allModules =
  let
    targetMods :: Set String
    targetMods = Set.fromFoldable $
      Array.filter (\m -> m.package.name == targetPkg) allModules <#> _.name

    importsOf :: Map String (Set String)
    importsOf = Map.fromFoldable $
      allImports <#> \imp -> Tuple imp.moduleName (Set.fromFoldable imp.imports)

    forwardEdges = Map.fromFoldable $ (Array.fromFoldable targetMods) <#> \mod ->
      Tuple mod (Set.intersection (fromMaybe Set.empty (Map.lookup mod importsOf)) targetMods)

    reverseEdges = foldl (\acc (Tuple from targets) ->
      foldl (\acc' to ->
        Map.alter (\mSet -> Just (Set.insert from (fromMaybe Set.empty mSet))) to acc'
      ) acc (Array.fromFoldable targets)
    ) (Map.empty :: Map String (Set String)) (Map.toUnfoldable forwardEdges :: Array (Tuple String (Set String)))

    symmetricEdges = Map.unionWith Set.union forwardEdges reverseEdges

    internalGraph =
      { nodes: Array.fromFoldable targetMods
      , edges: symmetricEdges
      }

    clusters = GraphAlgo.connectedComponents internalGraph
    communityLabels = GraphAlgo.labelPropagation internalGraph
    uniqueLabels = Set.fromFoldable $ Map.values communityLabels
    labelToIdx = Map.fromFoldable $ Array.mapWithIndex (\i label -> Tuple label i) (Array.fromFoldable uniqueLabels)
    communities = Map.mapMaybe (\label -> Map.lookup label labelToIdx) communityLabels
  in
    { clusters, communities, packageName: targetPkg }

-- =============================================================================
-- Package-level Git Status
-- =============================================================================

-- | Compute package-level git status from module-level status
computePackageGitStatus :: forall r. Maybe Loader.GitStatusData -> Maybe { modules :: Array Loader.V2ModuleListItem | r } -> Maybe PackageGitStatus
computePackageGitStatus mGitStatus mV2Data = do
  gitStatus <- mGitStatus
  v2 <- mV2Data
  let moduleToPackage :: Map String String
      moduleToPackage = Map.fromFoldable $ v2.modules <#> \m -> Tuple m.name m.package.name
      findPackages :: Array String -> Set String
      findPackages modNames = Set.fromFoldable $ Array.catMaybes $
        modNames <#> \modName -> Map.lookup modName moduleToPackage
  pure
    { packagesWithModified: findPackages gitStatus.modified
    , packagesWithStaged: findPackages gitStatus.staged
    , packagesWithUntracked: findPackages gitStatus.untracked
    }

-- =============================================================================
-- Scoped Package Filtering
-- =============================================================================

-- | Compute the scoped packages for SolarSwarm chord/matrix views
solarSwarmScopedPackages
  :: forall r. { focalPackage :: Maybe String | r }
  -> Array Loader.V2Package
  -> Array Loader.V2Package
solarSwarmScopedPackages state allPackages =
  let
    projectPkgNames = Set.fromFoldable projectPackages
    projectPkgs = Array.filter (\p -> Set.member p.name projectPkgNames) allPackages
  in case state.focalPackage of
    Nothing -> projectPkgs
    Just focalName ->
      let
        focalDeps = case Array.find (\p -> p.name == focalName) allPackages of
          Just pkg -> Set.fromFoldable pkg.depends
          Nothing -> Set.empty
        dependents = Set.fromFoldable $
          Array.mapMaybe
            (\pkg -> if Array.elem focalName pkg.depends then Just pkg.name else Nothing)
            allPackages
        neighborhood = Set.insert focalName (Set.union focalDeps dependents)
      in
        Array.filter (\p -> Set.member p.name neighborhood) projectPkgs

-- =============================================================================
-- Search
-- =============================================================================

-- | Derive target scene from a search result
sceneForResult :: Loader.UnifiedSearchResult -> Scene
sceneForResult r = case r.entityType of
  "package" -> PkgTreemap r.packageName
  "module" -> ModuleSignatureMap r.packageName (fromMaybe r.name r.moduleName)
  "declaration" -> DeclarationDetail r.packageName (fromMaybe "" r.moduleName) r.name
  _ -> GalaxyTreemap
