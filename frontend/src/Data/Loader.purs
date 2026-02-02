-- | Data loading and transformation
-- |
-- | Loads JSON data from ce-server API and transforms into SimNode/SimLink arrays with
-- | pre-calculated positions for Grid, Orbit, and Tree scenes.
-- |
-- | Supports multi-project/snapshot selection via:
-- | - fetchProjects: get available projects with their snapshots
-- | - loadModelForSnapshot: load model for a specific snapshot ID
-- | - loadModel: (legacy) loads latest snapshot for backward compatibility
module CE2.Data.Loader
  ( loadModel
  , loadModelForSnapshot
  , loadModelForProject
  , fetchProjects
  , fetchProjectWithSnapshots
  , fetchFunctionCalls
  -- Granular on-demand fetchers
  , fetchModuleDeclarations
  , fetchModuleDeclarationsWithSource
  , fetchModuleFunctionCalls
  , fetchModuleMetrics
  , fetchCallGraphData
  , ModuleDeclarationsResponse
  , ModuleFunctionCallsResponse
  -- Batch fetchers (single request for multiple modules)
  , fetchBatchDeclarations
  , fetchBatchFunctionCalls
  , fetchBatchCoupling
  -- Coupling metrics
  , fetchCouplingMetrics
  , DeclarationCoupling
  , CouplingMap
  -- Package Sets (Registry)
  , fetchPackageSets
  , fetchPackageSet
  , fetchPackageSetFromV2  -- New: loads from v2 API and converts to PackageSetData
  , PackageSetInfo
  , PackageSetPackage
  , PackageSetData
  -- Package Set History (GitHub)
  , fetchDefaultHistory
  , fetchPackageSetHistory
  , TemporalPackageSet
  , sampleVersions
  -- Types
  , LoadedModel
  , Project
  , Snapshot
  , apiBaseUrl
  , Declaration
  , DeclarationWithSource
  , DeclarationsMap
  , FunctionCallsMap
  , FunctionInfo
  , CallInfo
  , CallGraphData
  , GitMetrics
  -- ===========================================
  -- Unified API v2 (new schema)
  -- ===========================================
  , V2Stats
  , V2Package
  , V2Module
  , V2Declaration
  , V2ChildDeclaration
  , V2Namespace
  , V2SearchResult
  , V2FunctionCall
  , V2Import
  , V2ModuleListItem  -- Needed by LoadedModelWithV2
  , fetchV2Stats
  , fetchV2Packages
  , fetchV2Package
  , fetchV2Modules
  , fetchV2Module
  , fetchV2ModuleDeclarations
  , fetchV2ModuleImports
  , fetchV2ModuleCalls
  , fetchV2Namespaces
  , fetchV2Namespace
  , searchV2Declarations
  -- New unified loader
  , V2ModuleImports
  , fetchV2AllImports
  , loadModelFromV2
  -- Extended loader with raw V2 data (for BeeswarmViz etc.)
  , LoadedModelWithV2
  , loadModelFromV2WithRaw
  -- Module declaration stats (for bubble pack)
  , V2ModuleDeclarationStats
  , fetchV2ModuleDeclarationStats
  -- Polyglot summary (for sunburst)
  , PolyglotSummary
  , PolyglotBackend
  , PolyglotProject
  , PolyglotPackage
  , FfiLoc
  , fetchPolyglotSummary
  ) where

import Prelude

import Affjax.Web as AW
import Affjax.ResponseFormat as ResponseFormat
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson, printJsonDecodeError)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Traversable (traverse)
import Data.Int (toNumber)
import Data.Map (Map)
import Data.Map as Map
import Data.Set (Set)
import Data.Set as Set
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (pi, cos, sin, sqrt)
import Data.Nullable (null)
import Data.String.CodeUnits as SCU
import Data.String.Common (joinWith)
import Data.String.Pattern (Pattern(..))
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Foreign.Object (Object)
import Foreign.Object as Object
import Data.Graph.Algorithms as TopoAlgorithms
import Data.Foldable (maximum) as Foldable
import CE2.Types (SimNode, SimLink, NodeType(..), LinkType(..), Package)

-- | API base URL for ce-server
-- | TODO: Make this configurable via environment or runtime config
apiBaseUrl :: String
apiBaseUrl = "/code"  -- Routes through edge layer to ce-backend (production)
-- apiBaseUrl = "http://localhost:3000"  -- Direct to ce-server (local dev)

-- =============================================================================
-- Types
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

-- | Declaration summary from declarations-summary.json
type Declaration =
  { kind :: String -- "typeClass", "data", "typeSynonym", "externData", "alias", "value"
  , title :: String
  }

-- | Declaration with source code from module-declarations endpoint
type DeclarationWithSource =
  { kind :: String
  , title :: String
  , sourceCode :: Maybe String  -- Only present in per-module API response
  }

-- | Module declarations map: module name -> array of declarations
type DeclarationsMap = Object (Array Declaration)

-- | Call information from function-calls.json
type CallInfo =
  { target :: String
  , targetModule :: String
  , identifier :: String
  , isCrossModule :: Boolean
  }

-- | Function info from function-calls.json
type FunctionInfo =
  { module :: String
  , name :: String
  , calls :: Array CallInfo
  , calledBy :: Array String -- "Module.func" format
  }

-- | Function calls map: "Module.name" -> FunctionInfo
type FunctionCallsMap = Object FunctionInfo

-- | Response wrapper for function-calls.json
type FunctionCallsResponse =
  { functions :: FunctionCallsMap
  }

-- | Coupling metrics for a declaration
type DeclarationCoupling =
  { module :: String
  , name :: String
  , externalCallCount :: Int     -- How many external functions this calls
  , externalCallerCount :: Int   -- How many external functions call this
  , totalCoupling :: Int
  , couplingIntensity :: Number  -- 0.0 to 1.0 (normalized)
  }

-- | Coupling metrics map: "Module.name" -> DeclarationCoupling
type CouplingMap = Object DeclarationCoupling

-- | Response wrapper for declaration-coupling API
type CouplingResponse =
  { declarations :: CouplingMap
  }

-- | Snapshot info from API
type Snapshot =
  { id :: Int
  , gitHash :: String
  , gitRef :: String
  , label :: String
  , snapshotAt :: String
  , moduleCount :: Int
  , packageCount :: Int
  , declarationCount :: Int
  }

-- | Project info from API
type Project =
  { id :: Int
  , name :: String
  , repoPath :: String
  , description :: Maybe String
  , snapshotCount :: Int
  , latestSnapshotAt :: Maybe String
  , snapshots :: Array Snapshot  -- Populated when fetching single project
  }

-- | API response for projects list
type ProjectsListResponse =
  { projects :: Array ProjectListItem
  }

-- | Project list item (without snapshots)
type ProjectListItem =
  { id :: Int
  , name :: String
  , repoPath :: String
  , description :: Maybe String
  , snapshotCount :: Int
  , latestSnapshotAt :: Maybe String
  }

-- | API response for single project with snapshots
-- | Note: Single project response has fewer fields than list response
type ProjectWithSnapshotsResponse =
  { project :: ProjectDetail
  , snapshots :: Array Snapshot
  }

-- | Project detail (returned by GET /api/projects/:id)
type ProjectDetail =
  { id :: Int
  , name :: String
  , repoPath :: String
  , description :: Maybe String
  , createdAt :: String
  }

-- | Loaded and transformed model
type LoadedModel =
  { nodes :: Array SimNode
  , links :: Array SimLink
  , packages :: Array Package
  , declarations :: DeclarationsMap -- Module declarations for bubble packs
  , moduleCount :: Int
  , packageCount :: Int
  }

-- =============================================================================
-- Loading
-- =============================================================================

-- | Load all data from ce-server API and transform to model
loadModel :: Aff (Either String LoadedModel)
loadModel = do
  modulesResult <- fetchJson (apiBaseUrl <> "/data/spago-data/modules.json")
  packagesResult <- fetchJson (apiBaseUrl <> "/data/spago-data/packages.json")
  locResult <- fetchJson (apiBaseUrl <> "/data/spago-data/LOC.json")
  declarationsResult <- fetchJson (apiBaseUrl <> "/data/spago-data/declarations-summary.json")

  pure $ do
    modulesJson <- modulesResult
    packagesJson <- packagesResult
    locJson <- locResult
    declarationsJson <- declarationsResult

    -- Decode JSON
    modules :: Object RawModule <- decodeJson modulesJson # mapLeft printJsonDecodeError
    packages :: Object RawPackage <- decodeJson packagesJson # mapLeft printJsonDecodeError
    locFile :: LocFile <- decodeJson locJson # mapLeft printJsonDecodeError
    declarations :: DeclarationsMap <- decodeJson declarationsJson # mapLeft printJsonDecodeError

    -- Build LOC map (path -> loc)
    let locMap = buildLocMap locFile.loc

    -- Transform to model
    Right $ transformToModel modules packages locMap declarations

fetchJson :: String -> Aff (Either String Json)
fetchJson url = do
  result <- AW.get ResponseFormat.json url
  pure $ case result of
    Left err -> Left $ "Fetch error: " <> AW.printError err
    Right response -> Right response.body

mapLeft :: forall a b c. (a -> c) -> Either a b -> Either c b
mapLeft f (Left a) = Left (f a)
mapLeft _ (Right b) = Right b

-- =============================================================================
-- Multi-Project API
-- =============================================================================

-- | Fetch list of all projects with their snapshot counts
fetchProjects :: Aff (Either String (Array Project))
fetchProjects = do
  result <- fetchJson (apiBaseUrl <> "/api/projects")
  pure $ do
    json <- result
    response :: ProjectsListResponse <- decodeJson json # mapLeft printJsonDecodeError
    -- Convert ProjectListItem to Project (with empty snapshots array)
    Right $ map toProject response.projects
  where
  toProject :: ProjectListItem -> Project
  toProject p =
    { id: p.id
    , name: p.name
    , repoPath: p.repoPath
    , description: p.description
    , snapshotCount: p.snapshotCount
    , latestSnapshotAt: p.latestSnapshotAt
    , snapshots: []
    }

-- | Fetch a project with its snapshots
fetchProjectWithSnapshots :: Int -> Aff (Either String Project)
fetchProjectWithSnapshots projectId = do
  result <- fetchJson (apiBaseUrl <> "/api/projects/" <> show projectId)
  pure $ do
    json <- result
    response :: ProjectWithSnapshotsResponse <- decodeJson json # mapLeft printJsonDecodeError
    -- Compute snapshotCount and latestSnapshotAt from snapshots array
    let latestAt = Array.head response.snapshots <#> _.snapshotAt
    Right
      { id: response.project.id
      , name: response.project.name
      , repoPath: response.project.repoPath
      , description: response.project.description
      , snapshotCount: Array.length response.snapshots
      , latestSnapshotAt: latestAt
      , snapshots: response.snapshots
      }

-- | Fetch function calls data
fetchFunctionCalls :: Aff (Either String FunctionCallsMap)
fetchFunctionCalls = do
  result <- fetchJson (apiBaseUrl <> "/data/function-calls.json")
  pure $ do
    json <- result
    response :: FunctionCallsResponse <- decodeJson json # mapLeft printJsonDecodeError
    Right response.functions

-- =============================================================================
-- Granular On-Demand Fetchers
-- =============================================================================

-- | Response type for module declarations endpoint (summary version)
type ModuleDeclarationsResponse =
  { declarations :: Array Declaration
  }

-- | Response type for module declarations endpoint (with source code)
type ModuleDeclarationsWithSourceResponse =
  { declarations :: Array DeclarationWithSource
  }

-- | Response type for module function-calls endpoint
type ModuleFunctionCallsResponse =
  { module :: String
  , functions :: Object FunctionInfo
  }

-- | Fetch declarations for a specific module (without source code)
fetchModuleDeclarations :: String -> Aff (Either String (Array Declaration))
fetchModuleDeclarations moduleName = do
  result <- fetchJson (apiBaseUrl <> "/api/module-declarations/" <> moduleName)
  pure $ do
    json <- result
    response :: ModuleDeclarationsResponse <- decodeJson json # mapLeft printJsonDecodeError
    Right response.declarations

-- | Fetch declarations for a specific module (with source code)
fetchModuleDeclarationsWithSource :: String -> Aff (Either String (Array DeclarationWithSource))
fetchModuleDeclarationsWithSource moduleName = do
  result <- fetchJson (apiBaseUrl <> "/api/module-declarations/" <> moduleName)
  pure $ do
    json <- result
    response :: ModuleDeclarationsWithSourceResponse <- decodeJson json # mapLeft printJsonDecodeError
    Right response.declarations

-- | Fetch function calls for a specific module
fetchModuleFunctionCalls :: String -> Aff (Either String (Object FunctionInfo))
fetchModuleFunctionCalls moduleName = do
  result <- fetchJson (apiBaseUrl <> "/api/module-function-calls/" <> moduleName)
  pure $ do
    json <- result
    response :: ModuleFunctionCallsResponse <- decodeJson json # mapLeft printJsonDecodeError
    Right response.functions

-- =============================================================================
-- Call Graph Data (for popup)
-- =============================================================================

-- | Git metrics for a module
type GitMetrics =
  { commitCount :: Int
  , daysSinceModified :: Int
  , authorCount :: Int
  , authors :: Array String
  }

-- | Combined data for call graph popup
type CallGraphData =
  { moduleName :: String
  , declarationName :: String
  , callers :: Array CallInfo  -- Functions that call this one
  , callees :: Array CallInfo  -- Functions this one calls
  , sourceCode :: Maybe String
  , declarationKind :: Maybe String
  , gitMetrics :: Maybe GitMetrics
  }

-- | Fetch all data needed for call graph popup
-- | Combines module-function-calls, module-declarations, and module-metrics
fetchCallGraphData :: String -> String -> Aff (Either String CallGraphData)
fetchCallGraphData moduleName declarationName = do
  -- Fetch all three endpoints in parallel
  functionCallsResult <- fetchModuleFunctionCalls moduleName
  declarationsResult <- fetchModuleDeclarationsWithSource moduleName
  metricsResult <- fetchModuleMetrics moduleName

  pure $ do
    functionCalls <- functionCallsResult
    declarations <- declarationsResult
    -- Metrics are optional, don't fail if unavailable
    let metrics = case metricsResult of
          Right m -> Just m
          Left _ -> Nothing

    -- Find the specific function in the function calls map
    let functionInfo = Object.lookup declarationName functionCalls

    -- Find the declaration info
    let declaration = Array.find (\d -> d.title == declarationName) declarations

    -- Build callers from calledBy (convert "Module.func" strings to CallInfo)
    let callers = case functionInfo of
          Just fi -> map parseCallerString fi.calledBy
          Nothing -> []

    -- Build callees from calls
    let callees = case functionInfo of
          Just fi -> fi.calls
          Nothing -> []

    -- Get source code and kind from declaration
    let sourceCode = declaration >>= _.sourceCode
    let declarationKind = declaration <#> _.kind

    Right
      { moduleName
      , declarationName
      , callers
      , callees
      , sourceCode
      , declarationKind
      , gitMetrics: metrics
      }
  where
  -- Parse "Module.funcName" string into CallInfo
  parseCallerString :: String -> CallInfo
  parseCallerString str =
    case lastIndexOf "." str of
      Just idx ->
        { target: drop (idx + 1) str
        , targetModule: take idx str
        , identifier: str
        , isCrossModule: true
        }
      Nothing ->
        { target: str
        , targetModule: moduleName -- Same module if no dot
        , identifier: str
        , isCrossModule: false
        }

  lastIndexOf :: String -> String -> Maybe Int
  lastIndexOf needle haystack =
    let chars = SCU.toCharArray haystack
        needleChar = case SCU.toCharArray needle of
          [c] -> Just c
          _ -> Nothing
    in case needleChar of
      Just c -> Array.findLastIndex (\ch -> ch == c) chars
      Nothing -> Nothing

  take :: Int -> String -> String
  take n s = SCU.take n s

  drop :: Int -> String -> String
  drop n s = SCU.drop n s

-- | Fetch git metrics for a module (optional, may fail)
fetchModuleMetrics :: String -> Aff (Either String GitMetrics)
fetchModuleMetrics moduleName = do
  result <- fetchJson (apiBaseUrl <> "/api/module-metrics/" <> moduleName)
  pure $ do
    json <- result
    -- The API returns flat fields, we need to map them
    raw :: { commit_count :: Maybe Int, days_since_modified :: Maybe Int, author_count :: Maybe Int, authors :: Maybe (Array String) }
      <- decodeJson json # mapLeft printJsonDecodeError
    Right
      { commitCount: fromMaybe 0 raw.commit_count
      , daysSinceModified: fromMaybe 0 raw.days_since_modified
      , authorCount: fromMaybe 0 raw.author_count
      , authors: fromMaybe [] raw.authors
      }

-- =============================================================================
-- Batch Fetchers (single request for multiple modules)
-- =============================================================================

-- | Fetch declarations for multiple modules in a single request
-- | Returns DeclarationsMap: { "ModuleName": [{ kind, title }] }
fetchBatchDeclarations :: Array String -> Aff (Either String DeclarationsMap)
fetchBatchDeclarations moduleNames = do
  let modulesParam = joinWith "," moduleNames
  result <- fetchJson (apiBaseUrl <> "/api/batch-declarations/" <> modulesParam)
  pure $ do
    json <- result
    declarations :: DeclarationsMap <- decodeJson json # mapLeft printJsonDecodeError
    Right declarations

-- | Fetch function calls for multiple modules in a single request
-- | Returns FunctionCallsMap: { "Module.func": { module, name, calls, calledBy } }
fetchBatchFunctionCalls :: Array String -> Aff (Either String FunctionCallsMap)
fetchBatchFunctionCalls moduleNames = do
  let modulesParam = joinWith "," moduleNames
  result <- fetchJson (apiBaseUrl <> "/api/batch-function-calls/" <> modulesParam)
  pure $ do
    json <- result
    response :: FunctionCallsResponse <- decodeJson json # mapLeft printJsonDecodeError
    Right response.functions

-- | Fetch coupling metrics for specific modules (filtered batch query)
-- | Returns CouplingMap: { "Module.name": { externalCallCount, externalCallerCount, ... } }
fetchBatchCoupling :: Array String -> Aff (Either String CouplingMap)
fetchBatchCoupling moduleNames = do
  let modulesParam = joinWith "," moduleNames
  result <- fetchJson (apiBaseUrl <> "/api/batch-coupling/" <> modulesParam)
  pure $ do
    json <- result
    response :: CouplingResponse <- decodeJson json # mapLeft printJsonDecodeError
    Right response.declarations

-- | Fetch coupling metrics for all declarations
-- | Returns CouplingMap: { "Module.name": { externalCallCount, externalCallerCount, ... } }
fetchCouplingMetrics :: Aff (Either String CouplingMap)
fetchCouplingMetrics = do
  result <- fetchJson (apiBaseUrl <> "/api/declaration-coupling")
  pure $ do
    json <- result
    response :: CouplingResponse <- decodeJson json # mapLeft printJsonDecodeError
    Right response.declarations

-- | Load model for a specific snapshot ID
-- | This fetches data scoped to that snapshot from the legacy endpoints
loadModelForSnapshot :: Int -> Aff (Either String LoadedModel)
loadModelForSnapshot _snapshotId = do
  -- For now, we use the legacy endpoints which always return latest snapshot
  -- TODO: Add snapshot-specific endpoints like /api/snapshots/:id/modules
  -- For now, just use the same loadModel (legacy endpoints use latest snapshot)
  -- This will be enhanced when we add snapshot-specific data endpoints
  loadModel

-- | Load model for a specific project ID
-- | Uses project-specific endpoints to load data for the selected project
loadModelForProject :: Int -> Aff (Either String LoadedModel)
loadModelForProject projectId = do
  let projectUrl = apiBaseUrl <> "/api/project-"
  modulesResult <- fetchJson (projectUrl <> "modules/" <> show projectId)
  packagesResult <- fetchJson (projectUrl <> "packages/" <> show projectId)
  locResult <- fetchJson (projectUrl <> "loc/" <> show projectId)
  declarationsResult <- fetchJson (projectUrl <> "declarations-summary/" <> show projectId)

  pure $ do
    modulesJson <- modulesResult
    packagesJson <- packagesResult
    locJson <- locResult
    declarationsJson <- declarationsResult

    -- Decode JSON
    modules :: Object RawModule <- decodeJson modulesJson # mapLeft printJsonDecodeError
    packages :: Object RawPackage <- decodeJson packagesJson # mapLeft printJsonDecodeError
    locFile :: LocFile <- decodeJson locJson # mapLeft printJsonDecodeError
    declarations :: DeclarationsMap <- decodeJson declarationsJson # mapLeft printJsonDecodeError

    -- Build LOC map (path -> loc)
    let locMap = buildLocMap locFile.loc

    -- Transform to model
    Right $ transformToModel modules packages locMap declarations

-- =============================================================================
-- Transformation
-- =============================================================================

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
    -- For each module, look at its dependencies and find which packages they belong to
    -- Package A depends on Package B if any module in A depends on any module in B
    derivedPackageDeps :: Map String (Set String)
    derivedPackageDeps = foldl addModuleDeps Map.empty (Object.toUnfoldable modulesObj :: Array (Tuple String RawModule))
      where
        addModuleDeps :: Map String (Set String) -> Tuple String RawModule -> Map String (Set String)
        addModuleDeps acc (Tuple moduleName rawMod) =
          let
            thisPackage = rawMod.package
            -- Find packages of all dependencies
            depPackages = Set.fromFoldable $ Array.mapMaybe getPackageOfModule rawMod.depends
            -- Remove self-dependency
            externalDeps = Set.delete thisPackage depPackages
            -- Add to existing deps for this package
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
    -- Pass derivedPackageDeps and packageIdMap to populate targets/sources
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

    -- All nodes - tree positions will be computed on demand by Viz layer
    nodes = packageNodes <> moduleNodes

    -- All module links - tree/graph classification done by Viz layer
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
-- Node Creation
-- =============================================================================

mkPackageNode :: Array String -> Int -> Map String Int -> Map String Int -> Map Int (Array String) -> Int -> Map String (Set String) -> Map String Int -> Int -> String -> SimNode
mkPackageNode _allPackages totalPackages packageLocMap packageLayerMap packagesByLayer maxLayer derivedPackageDeps packageIdMap idx name =
  let
    -- Grid position (arrange in rows of ~8)
    gridCols = 8
    gridSpacing = 120.0
    gridRow = toNumber (idx / gridCols)
    gridCol = toNumber (idx `mod` gridCols)
    gx = (gridCol - toNumber gridCols / 2.0 + 0.5) * gridSpacing
    gy = (gridRow - toNumber (totalPackages / gridCols) / 2.0) * gridSpacing

    -- Orbit angle (distribute evenly around circle)
    angle = 2.0 * pi * toNumber idx / toNumber totalPackages

    -- Package radius based on total LOC (sqrt scale, with minimum)
    totalLoc = fromMaybe 100 (Map.lookup name packageLocMap)
    r = max 8.0 (sqrt (toNumber totalLoc) * 0.5)

    -- Topological position (DAG layout)
    -- y = layer (top to bottom: layer 0 at top, max layer at bottom)
    -- x = position within layer (centered)
    layer = fromMaybe 0 (Map.lookup name packageLayerMap)
    packagesInLayer = fromMaybe [] (Map.lookup layer packagesByLayer)
    indexInLayer = fromMaybe 0 (Array.elemIndex name packagesInLayer)
    countInLayer = Array.length packagesInLayer

    -- Layout constants for topo view (horizontal: right-to-left)
    -- Project code (high layer) on left, base libs (low layer) on right
    topoLayerSpacing = 150.0  -- Horizontal spacing between layers
    topoNodeSpacing = 60.0    -- Vertical spacing within layer (tighter)

    -- X: layer 0 (base libs) at right, higher layers (project code) at left
    tx = (toNumber maxLayer / 2.0 - toNumber layer) * topoLayerSpacing

    -- Y: center nodes within their layer
    ty = (toNumber indexInLayer - toNumber countInLayer / 2.0 + 0.5) * topoNodeSpacing

    -- Package dependencies (targets = packages this depends on)
    depPackageNames = fromMaybe Set.empty (Map.lookup name derivedPackageDeps)
    targets = Array.mapMaybe (\n -> Map.lookup n packageIdMap) (Array.fromFoldable depPackageNames)

    -- Package dependents (sources = packages that depend on this)
    -- Find all packages whose deps include this package
    sources = Array.mapMaybe (\n -> Map.lookup n packageIdMap) $
      Array.filter (\otherName -> Set.member name (fromMaybe Set.empty (Map.lookup otherName derivedPackageDeps))) _allPackages
  in
    { id: idx
    , name
    , nodeType: PackageNode
    , package: name
    , path: ""  -- Packages don't have source files
    , x: gx
    , y: gy
    , vx: 0.0
    , vy: 0.0
    , fx: null
    , fy: null
    , r -- Package sized by total LOC
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
    , isInTree: false -- Packages are never in the tree
    , topoX: tx
    , topoY: ty
    , topoLayer: layer
    , source: "registry"  -- Legacy loader doesn't have source info
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
  -> Array SimNode -- Pass package nodes to get their positions
  -> Map Int (Array Int) -- targetsMap
  -> Map Int (Array Int) -- sourcesMap
  -> SimNode
mkModuleNode name idx modulesObj locMap packageIdMap _moduleIdMap packageCount _moduleCount packageNodes targetsMap sourcesMap =
  let
    nodeId = idx + packageCount
    rawMod = Object.lookup name modulesObj
    pkgName = fromMaybe "unknown" (rawMod <#> _.package)
    path = fromMaybe "" (rawMod <#> _.path)
    loc = fromMaybe 50 (Map.lookup path locMap)

    -- Get cluster from package
    cluster = fromMaybe 0 (Map.lookup pkgName packageIdMap)

    -- Module radius based on LOC (sqrt scale, visible range ~4-25)
    r = 4.0 + sqrt (toNumber loc) * 0.8

    -- Get parent package's grid position
    pkgId = fromMaybe 0 (Map.lookup pkgName packageIdMap)
    pkgNode = Array.index packageNodes pkgId
    pkgGridX = fromMaybe 0.0 (pkgNode <#> _.gridX)
    pkgGridY = fromMaybe 0.0 (pkgNode <#> _.gridY)

    -- Slight random offset from package center for modules
    -- Use a deterministic "random" based on name hash
    nameHash = stringHash name
    offsetAngle = 2.0 * pi * toNumber (nameHash `mod` 360) / 360.0
    offsetDist = 30.0 + toNumber ((nameHash / 360) `mod` 30)
    offsetX = cos offsetAngle * offsetDist
    offsetY = sin offsetAngle * offsetDist

    -- Absolute grid position = package position + offset
    absGridX = pkgGridX + offsetX
    absGridY = pkgGridY + offsetY

    -- Get targets (dependencies) and sources (dependents) from maps
    targets = fromMaybe [] (Map.lookup nodeId targetsMap)
    sources = fromMaybe [] (Map.lookup nodeId sourcesMap)
  in
    { id: nodeId
    , name
    , nodeType: ModuleNode
    , package: pkgName
    , path  -- Source file path for opening in editor
    , x: absGridX -- Start at grid position
    , y: absGridY
    , vx: 0.0
    , vy: 0.0
    , fx: null
    , fy: null
    , r
    , cluster
    , targets
    , sources
    , gridX: absGridX -- Absolute position for Grid scene
    , gridY: absGridY
    , orbitAngle: 0.0 -- Modules don't have orbit angle
    , treeX: 0.0
    , treeY: 0.0
    , radialX: 0.0
    , radialY: 0.0
    , isInTree: false -- Will be set true by updateNodeWithTreeData if reachable
    , topoX: 0.0
    , topoY: 0.0
    , topoLayer: 0  -- Modules don't use topo positioning
    , source: "registry"  -- Legacy loader doesn't have source info
    }

-- =============================================================================
-- Helpers
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
                , linkType: M2M_Graph -- Default to graph; markLinkType will change to M2M_Tree if in spanning tree
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

-- =============================================================================
-- Package Sets (Registry)
-- =============================================================================

-- | Package set summary info
type PackageSetInfo =
  { id :: Int
  , name :: String
  , compilerVersion :: String
  , source :: String
  , publishedAt :: Maybe String
  , packageCount :: Int
  }

-- | A package within a package set
type PackageSetPackage =
  { id :: Int
  , name :: String
  , version :: String
  , description :: Maybe String
  , license :: Maybe String
  , repositoryOwner :: Maybe String
  , repositoryName :: Maybe String
  , depends :: Array String
  , topoLayer :: Int
  , publishedAt :: Maybe String   -- ISO date string when this version was published
  , releaseNumber :: Int          -- How many versions this package has had
  , moduleCount :: Int            -- Number of modules (from unified schema, 0 if unknown)
  , totalLoc :: Int               -- Total lines of code (0 if unknown)
  }

-- | Full package set data with all packages
type PackageSetData =
  { packageSet :: PackageSetInfo
  , packages :: Array PackageSetPackage
  }

-- | Raw types for JSON decoding
type RawPackageSetInfo =
  { id :: Int
  , name :: String
  , compilerVersion :: String
  , source :: String
  , publishedAt :: Maybe String
  , packageCount :: Int
  }

type RawPackageSetPackage =
  { id :: Int
  , name :: String
  , version :: String
  , description :: Maybe String
  , license :: Maybe String
  , repositoryOwner :: Maybe String
  , repositoryName :: Maybe String
  , depends :: Array String
  , topoLayer :: Int
  , publishedAt :: Maybe String
  , releaseNumber :: Int
  , moduleCount :: Int
  , totalLoc :: Int
  }

type RawPackageSetsResponse =
  { packageSets :: Array RawPackageSetInfo
  }

type RawPackageSetResponse =
  { packageSet :: RawPackageSetInfo
  , packages :: Array RawPackageSetPackage
  }

-- | Fetch list of available package sets
fetchPackageSets :: Aff (Either String (Array PackageSetInfo))
fetchPackageSets = do
  result <- AW.get ResponseFormat.json (apiBaseUrl <> "/api/package-sets")
  case result of
    Left err -> pure $ Left $ "HTTP error: " <> AW.printError err
    Right response -> do
      case decodeJson response.body :: Either _ RawPackageSetsResponse of
        Left err -> pure $ Left $ "JSON decode error: " <> printJsonDecodeError err
        Right raw -> pure $ Right raw.packageSets

-- | Fetch a single package set with all its packages
fetchPackageSet :: Int -> Aff (Either String PackageSetData)
fetchPackageSet packageSetId = do
  result <- AW.get ResponseFormat.json (apiBaseUrl <> "/api/package-sets/" <> show packageSetId)
  case result of
    Left err -> pure $ Left $ "HTTP error: " <> AW.printError err
    Right response -> do
      case decodeJson response.body :: Either _ RawPackageSetResponse of
        Left err -> pure $ Left $ "JSON decode error: " <> printJsonDecodeError err
        Right raw -> pure $ Right
          { packageSet: raw.packageSet
          , packages: raw.packages
          }

-- | Fetch packages from V2 API and convert to PackageSetData format
-- | This provides backward compatibility for visualizations that expect PackageSetData
fetchPackageSetFromV2 :: Aff (Either String PackageSetData)
fetchPackageSetFromV2 = do
  packagesResult <- fetchV2Packages
  pure $ do
    v2Packages <- packagesResult
    Right $ v2ToPackageSetData v2Packages

-- | Convert V2 packages to PackageSetData format
-- | Computes topological layers from package dependencies
v2ToPackageSetData :: Array V2Package -> PackageSetData
v2ToPackageSetData v2Packages =
  let
    -- Build TaskNodes for topo sort using dependency data from API
    packageTaskNodes :: Array (TopoAlgorithms.TaskNode String)
    packageTaskNodes = v2Packages <#> \p ->
      { id: p.name
      , depends: p.depends
      }

    -- Get layered packages (each has id, layer, depends)
    layeredPackages = TopoAlgorithms.addLayers packageTaskNodes

    -- Build a map from package name to layer
    layerMap :: Map String Int
    layerMap = Map.fromFoldable $ layeredPackages <#> \lp -> Tuple lp.id lp.layer

    -- Convert packages with computed topo layers
    convertWithLayer :: V2Package -> PackageSetPackage
    convertWithLayer pkg =
      { id: pkg.id
      , name: pkg.name
      , version: pkg.version
      , description: pkg.description
      , license: pkg.license
      , repositoryOwner: Nothing
      , repositoryName: pkg.repository
      , depends: pkg.depends
      , topoLayer: fromMaybe 0 (Map.lookup pkg.name layerMap)
      , publishedAt: Nothing
      , releaseNumber: 0
      , moduleCount: pkg.moduleCount
      , totalLoc: pkg.totalLoc
      }
  in
    { packageSet:
        { id: 0
        , name: "PSD3 Monorepo (V2)"
        , compilerVersion: "0.15.15"
        , source: "v2-api"
        , publishedAt: Nothing
        , packageCount: Array.length v2Packages
        }
    , packages: map convertWithLayer v2Packages
    }

-- | Convert a single V2Package to PackageSetPackage format (without topo layer)
-- | Note: Use v2ToPackageSetData instead to get proper topo layers
v2PackageToPackageSetPackage :: V2Package -> PackageSetPackage
v2PackageToPackageSetPackage pkg =
  { id: pkg.id
  , name: pkg.name
  , version: pkg.version
  , description: pkg.description
  , license: pkg.license
  , repositoryOwner: Nothing  -- V2 API doesn't split repository
  , repositoryName: pkg.repository  -- Use full repo URL as name
  , depends: pkg.depends  -- From package_dependencies table
  , topoLayer: 0  -- Use v2ToPackageSetData for proper layer computation
  , publishedAt: Nothing
  , releaseNumber: 0
  , moduleCount: pkg.moduleCount
  , totalLoc: pkg.totalLoc  -- Sum of module LOC from v2 API
  }

-- =============================================================================
-- Package Set History (GitHub Registry)
-- =============================================================================

-- | GitHub raw content URL for package sets
registryRawUrl :: String
registryRawUrl = "https://raw.githubusercontent.com/purescript/registry/main/package-sets"

-- | Package set version info (minimal, from GitHub)
type PackageSetVersion =
  { version :: String
  , compiler :: String
  , packages :: Object String  -- Package name -> version
  }

-- | Sampled package set for temporal visualization
type TemporalPackageSet =
  { version :: String
  , compiler :: String
  , packageNames :: Array String
  , publishedDate :: Maybe String  -- Extracted from version if available
  }

-- | Fetch a single package set from GitHub by version string
fetchPackageSetFromGitHub :: String -> Aff (Either String PackageSetVersion)
fetchPackageSetFromGitHub version = do
  let url = registryRawUrl <> "/" <> version <> ".json"
  result <- AW.get ResponseFormat.json url
  case result of
    Left err -> pure $ Left $ "HTTP error fetching " <> version <> ": " <> AW.printError err
    Right response -> do
      case decodeJson response.body :: Either _ PackageSetVersion of
        Left err -> pure $ Left $ "JSON decode error for " <> version <> ": " <> printJsonDecodeError err
        Right ps -> pure $ Right ps

-- | Sample versions for temporal view (every Nth major/minor)
-- | Returns version strings like "0.0.1", "5.0.0", "10.0.0", etc.
sampleVersions :: Array String
sampleVersions =
  -- Hand-picked sample: early versions, then every ~5 major versions
  [ "0.0.1", "1.0.0", "5.0.0", "10.0.0", "15.0.0", "20.0.0"
  , "25.0.0", "30.0.0", "35.0.0", "40.0.0", "45.0.0", "50.0.0"
  , "55.0.0", "60.0.0", "65.0.0", "70.0.0", "71.0.0"
  ]

-- | Fetch multiple package sets and convert to temporal format
fetchPackageSetHistory :: Array String -> Aff (Array TemporalPackageSet)
fetchPackageSetHistory versions = do
  results <- traverse fetchAndConvert versions
  pure $ Array.catMaybes results
  where
  fetchAndConvert :: String -> Aff (Maybe TemporalPackageSet)
  fetchAndConvert ver = do
    result <- fetchPackageSetFromGitHub ver
    pure $ case result of
      Left _ -> Nothing
      Right ps -> Just
        { version: ps.version
        , compiler: ps.compiler
        , packageNames: Object.keys ps.packages
        , publishedDate: Nothing  -- Could parse from git if needed
        }

-- | Fetch default sampled history
fetchDefaultHistory :: Aff (Array TemporalPackageSet)
fetchDefaultHistory = fetchPackageSetHistory sampleVersions

-- =============================================================================
-- Unified API v2 (new schema)
-- =============================================================================
-- These types and functions use the new unified schema where:
--   - Package versions are the core identity (name@version)
--   - Modules belong to package versions (not snapshots)
--   - Namespaces form a tree independent of packages
-- =============================================================================

-- | Database statistics from v2 API
type V2Stats =
  { packages :: { total :: Int, registry :: Int, local :: Int }
  , modules :: Int
  , declarations :: Int
  , childDeclarations :: Int
  , namespaces :: Int
  , imports :: Int
  , functionCalls :: Int
  }

-- | Package from unified schema
type V2Package =
  { id :: Int
  , name :: String
  , version :: String
  , description :: Maybe String
  , license :: Maybe String
  , repository :: Maybe String
  , source :: String  -- "registry" | "workspace" | "extra"
  , moduleCount :: Int
  , declarationCount :: Int
  , totalLoc :: Int         -- Sum of all module LOC in this package
  , depends :: Array String -- Package dependency names
  }

-- | Package with modules from v2 API
type V2PackageWithModules =
  { id :: Int
  , name :: String
  , version :: String
  , description :: Maybe String
  , license :: Maybe String
  , repository :: Maybe String
  , source :: String
  , modules :: Array V2ModuleSummary
  }

-- | Module summary (when listing)
type V2ModuleSummary =
  { id :: Int
  , name :: String
  , path :: Maybe String
  , loc :: Maybe Int
  , namespacePath :: Maybe String
  , declarationCount :: Int
  }

-- | Module from unified schema
type V2Module =
  { id :: Int
  , name :: String
  , path :: Maybe String
  , comments :: Maybe String
  , loc :: Maybe Int
  , package :: { id :: Int, name :: String, version :: String, source :: String }
  , namespace :: Maybe { path :: String, depth :: Int }
  }

-- | Module in list response
type V2ModuleListItem =
  { id :: Int
  , name :: String
  , path :: Maybe String
  , loc :: Maybe Int
  , package :: { id :: Int, name :: String, version :: String, source :: String }
  , namespacePath :: Maybe String
  , declarationCount :: Int
  }

-- | Declaration from unified schema
type V2Declaration =
  { id :: Int
  , name :: String
  , kind :: String  -- "value" | "data" | "newtype" | "type_synonym" | "type_class" | "foreign"
  , typeSignature :: Maybe String
  , comments :: Maybe String
  , dataDeclType :: Maybe String
  , sourceSpan :: Maybe { start :: Array Int, end :: Array Int, name :: String }
  , children :: Array V2ChildDeclaration
  }

-- | Child declaration (constructor, instance, class member)
type V2ChildDeclaration =
  { id :: Int
  , name :: String
  , kind :: String  -- "constructor" | "instance" | "class_member"
  , typeSignature :: Maybe String
  , comments :: Maybe String
  }

-- | Import from v2 API
type V2Import =
  { moduleName :: String
  , moduleId :: Maybe Int
  , packageName :: Maybe String
  , packageVersion :: Maybe String
  }

-- | Function call from v2 API
type V2FunctionCall =
  { callerName :: String
  , calleeModule :: String
  , calleeName :: String
  , isCrossModule :: Boolean
  , callCount :: Int
  }

-- | Namespace from unified schema
type V2Namespace =
  { id :: Int
  , path :: String
  , segment :: String
  , depth :: Int
  , parentId :: Maybe Int
  , isLeaf :: Boolean
  , moduleCount :: Int
  , packageCount :: Int
  }

-- | Namespace with children and modules
type V2NamespaceWithChildren =
  { id :: Int
  , path :: String
  , segment :: String
  , depth :: Int
  , parentId :: Maybe Int
  , isLeaf :: Boolean
  , children :: Array V2NamespaceChild
  , modules :: Array V2NamespaceModule
  }

type V2NamespaceChild =
  { id :: Int
  , path :: String
  , segment :: String
  , depth :: Int
  , isLeaf :: Boolean
  , moduleCount :: Int
  }

type V2NamespaceModule =
  { id :: Int
  , name :: String
  , packageName :: String
  , packageVersion :: String
  , declarationCount :: Int
  }

-- | Search result from v2 API
type V2SearchResult =
  { id :: Int
  , name :: String
  , kind :: String
  , typeSignature :: Maybe String
  , moduleName :: String
  , packageName :: String
  , packageVersion :: String
  }

-- | Module with all its imports (from bulk all-imports endpoint)
type V2ModuleImports =
  { moduleId :: Int
  , moduleName :: String
  , imports :: Array String  -- Imported module names
  }

-- Response wrappers for JSON decoding
type V2PackagesResponse = { packages :: Array V2Package, count :: Int }
type V2ModulesResponse = { modules :: Array V2ModuleListItem, count :: Int }
type V2DeclarationsResponse = { declarations :: Array V2Declaration, count :: Int }
type V2ImportsResponse = { imports :: Array V2Import, count :: Int }
type V2CallsResponse = { calls :: Array V2FunctionCall, count :: Int }
type V2NamespacesResponse = { namespaces :: Array V2Namespace, count :: Int }
type V2SearchResponse = { results :: Array V2SearchResult, count :: Int }
type V2AllImportsResponse = { imports :: Array V2ModuleImports, count :: Int }

-- | Fetch database statistics
fetchV2Stats :: Aff (Either String V2Stats)
fetchV2Stats = do
  result <- fetchJson (apiBaseUrl <> "/api/v2/stats")
  pure $ result >>= \json -> decodeJson json # mapLeft printJsonDecodeError

-- | Fetch all packages
fetchV2Packages :: Aff (Either String (Array V2Package))
fetchV2Packages = do
  result <- fetchJson (apiBaseUrl <> "/api/v2/packages")
  pure $ do
    json <- result
    response :: V2PackagesResponse <- decodeJson json # mapLeft printJsonDecodeError
    Right response.packages

-- | Fetch a single package with its modules
fetchV2Package :: Int -> Aff (Either String V2PackageWithModules)
fetchV2Package packageId = do
  result <- fetchJson (apiBaseUrl <> "/api/v2/packages/" <> show packageId)
  pure $ result >>= \json -> decodeJson json # mapLeft printJsonDecodeError

-- | Fetch modules (paginated, 500 limit)
fetchV2Modules :: Aff (Either String (Array V2ModuleListItem))
fetchV2Modules = do
  result <- fetchJson (apiBaseUrl <> "/api/v2/modules")
  pure $ do
    json <- result
    response :: V2ModulesResponse <- decodeJson json # mapLeft printJsonDecodeError
    Right response.modules

-- | Fetch a single module
fetchV2Module :: Int -> Aff (Either String V2Module)
fetchV2Module moduleId = do
  result <- fetchJson (apiBaseUrl <> "/api/v2/modules/" <> show moduleId)
  pure $ result >>= \json -> decodeJson json # mapLeft printJsonDecodeError

-- | Fetch declarations for a module
fetchV2ModuleDeclarations :: Int -> Aff (Either String (Array V2Declaration))
fetchV2ModuleDeclarations moduleId = do
  result <- fetchJson (apiBaseUrl <> "/api/v2/module-declarations/" <> show moduleId)
  pure $ do
    json <- result
    response :: V2DeclarationsResponse <- decodeJson json # mapLeft printJsonDecodeError
    Right response.declarations

-- | Fetch imports for a module
fetchV2ModuleImports :: Int -> Aff (Either String (Array V2Import))
fetchV2ModuleImports moduleId = do
  result <- fetchJson (apiBaseUrl <> "/api/v2/module-imports/" <> show moduleId)
  pure $ do
    json <- result
    response :: V2ImportsResponse <- decodeJson json # mapLeft printJsonDecodeError
    Right response.imports

-- | Fetch function calls for a module
fetchV2ModuleCalls :: Int -> Aff (Either String (Array V2FunctionCall))
fetchV2ModuleCalls moduleId = do
  result <- fetchJson (apiBaseUrl <> "/api/v2/module-calls/" <> show moduleId)
  pure $ do
    json <- result
    response :: V2CallsResponse <- decodeJson json # mapLeft printJsonDecodeError
    Right response.calls

-- | Fetch top-level namespaces
fetchV2Namespaces :: Aff (Either String (Array V2Namespace))
fetchV2Namespaces = do
  result <- fetchJson (apiBaseUrl <> "/api/v2/namespaces")
  pure $ do
    json <- result
    response :: V2NamespacesResponse <- decodeJson json # mapLeft printJsonDecodeError
    Right response.namespaces

-- | Fetch a namespace with children and modules
fetchV2Namespace :: String -> Aff (Either String V2NamespaceWithChildren)
fetchV2Namespace nsPath = do
  result <- fetchJson (apiBaseUrl <> "/api/v2/namespaces/" <> nsPath)
  pure $ result >>= \json -> decodeJson json # mapLeft printJsonDecodeError

-- | Search declarations by name or type signature
searchV2Declarations :: String -> Aff (Either String (Array V2SearchResult))
searchV2Declarations query = do
  result <- fetchJson (apiBaseUrl <> "/api/v2/declarations/search/" <> query)
  pure $ do
    json <- result
    response :: V2SearchResponse <- decodeJson json # mapLeft printJsonDecodeError
    Right response.results

-- =============================================================================
-- Bulk Data Fetchers
-- =============================================================================

-- | Fetch all module imports in one request (for building dependency graph)
fetchV2AllImports :: Aff (Either String (Array V2ModuleImports))
fetchV2AllImports = do
  result <- fetchJson (apiBaseUrl <> "/api/v2/all-imports")
  pure $ do
    json <- result
    response :: V2AllImportsResponse <- decodeJson json # mapLeft printJsonDecodeError
    Right response.imports

-- =============================================================================
-- Unified Model Loader (V2 API)
-- =============================================================================

-- | Load model from unified v2 API
-- | This replaces the legacy project/snapshot-based loading with direct access
-- | to the unified schema.
loadModelFromV2 :: Aff (Either String LoadedModel)
loadModelFromV2 = do
  -- Fetch all data in parallel
  packagesResult <- fetchV2Packages
  modulesResult <- fetchV2Modules
  importsResult <- fetchV2AllImports

  pure $ do
    packages <- packagesResult
    modules <- modulesResult
    allImports <- importsResult

    -- Transform to model
    Right $ transformV2ToModel packages modules allImports

-- | Transform V2 API data to LoadedModel
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
    -- Compute topological layers for packages
    -- =========================================================================

    -- Derive package dependencies from module imports
    derivedPackageDeps :: Map String (Set String)
    derivedPackageDeps = foldl addModuleDeps Map.empty v2Modules
      where
        -- Get module's package name
        modulePackageMap :: Map String String
        modulePackageMap = Map.fromFoldable $ v2Modules <#> \m -> Tuple m.name m.package.name

        addModuleDeps :: Map String (Set String) -> V2ModuleListItem -> Map String (Set String)
        addModuleDeps acc mod =
          let
            thisPackage = mod.package.name
            -- Get imports for this module
            moduleImports = fromMaybe [] (Map.lookup mod.id importsMap)
            -- Find packages of all imported modules
            depPackages = Set.fromFoldable $ Array.mapMaybe (\impName -> Map.lookup impName modulePackageMap) moduleImports
            -- Remove self-dependency
            externalDeps = Set.delete thisPackage depPackages
            -- Add to existing deps for this package
            existing = fromMaybe Set.empty (Map.lookup thisPackage acc)
          in
            Map.insert thisPackage (Set.union existing externalDeps) acc

    -- Convert packages to TaskNodes for topo sort
    packageTaskNodes :: Array (TopoAlgorithms.TaskNode String)
    packageTaskNodes = v2Packages <#> \p ->
      { id: p.name
      , depends: Array.fromFoldable $ fromMaybe Set.empty (Map.lookup p.name derivedPackageDeps)
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

    -- =========================================================================
    -- Create Package Nodes
    -- =========================================================================

    packageNodes :: Array SimNode
    packageNodes = Array.mapWithIndex mkPackageNodeV2 v2Packages

    mkPackageNodeV2 :: Int -> V2Package -> SimNode
    mkPackageNodeV2 idx pkg =
      let
        -- Grid position (arrange in rows of ~8)
        gridCols = 8
        gridSpacing = 120.0
        gridRow = toNumber (idx / gridCols)
        gridCol = toNumber (idx `mod` gridCols)
        gx = (gridCol - toNumber gridCols / 2.0 + 0.5) * gridSpacing
        gy = (gridRow - toNumber packageCount / toNumber gridCols / 2.0) * gridSpacing

        -- Orbit angle (distribute evenly around circle)
        angle = 2.0 * pi * toNumber idx / toNumber packageCount

        -- Package radius based on module count (sqrt scale, with minimum)
        r = max 8.0 (sqrt (toNumber (pkg.moduleCount * 100)) * 0.5)

        -- Topological position
        layer = fromMaybe 0 (Map.lookup pkg.name packageLayerMap)
        packagesInLayer = fromMaybe [] (Map.lookup layer packagesByLayer)
        indexInLayer = fromMaybe 0 (Array.elemIndex pkg.name packagesInLayer)
        countInLayer = Array.length packagesInLayer

        topoLayerSpacing = 150.0
        topoNodeSpacing = 60.0
        tx = (toNumber maxLayer / 2.0 - toNumber layer) * topoLayerSpacing
        ty = (toNumber indexInLayer - toNumber countInLayer / 2.0 + 0.5) * topoNodeSpacing

        -- Package dependencies (targets)
        depPackageNames = fromMaybe Set.empty (Map.lookup pkg.name derivedPackageDeps)
        targets = Array.mapMaybe (\n -> Map.lookup n packageIndexMap) (Array.fromFoldable depPackageNames)

        -- Package dependents (sources)
        sources = Array.mapMaybe (\n -> Map.lookup n packageIndexMap) $
          Array.filter (\otherName -> Set.member pkg.name (fromMaybe Set.empty (Map.lookup otherName derivedPackageDeps)))
            (v2Packages <#> _.name)
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
        , source: pkg.source  -- "workspace" | "registry" | "extra"
        }

    -- Build package source map for module nodes
    packageSourceMap :: Map String String
    packageSourceMap = Map.fromFoldable $ v2Packages <#> \p -> Tuple p.name p.source

    -- =========================================================================
    -- Create Module Nodes
    -- =========================================================================

    -- Build module targets and sources maps
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
        -- Find package index for cluster
        pkgIdx = fromMaybe 0 (Map.lookup m.package.name packageIndexMap)
        mPkgNode = Array.index packageNodes pkgIdx

        -- Extract package node position info (with defaults)
        pkgGridX = fromMaybe 0.0 (mPkgNode <#> _.gridX)
        pkgGridY = fromMaybe 0.0 (mPkgNode <#> _.gridY)
        pkgOrbitAngle = fromMaybe 0.0 (mPkgNode <#> _.orbitAngle)
        pkgTopoX = fromMaybe 0.0 (mPkgNode <#> _.topoX)
        pkgTopoY = fromMaybe 0.0 (mPkgNode <#> _.topoY)
        pkgTopoLayer = fromMaybe 0 (mPkgNode <#> _.topoLayer)

        -- Module radius based on declaration count
        loc = fromMaybe 100 m.loc
        r = max 3.0 (sqrt (toNumber loc) * 0.3)

        -- Initial position near package center
        x = pkgGridX + (toNumber (m.id `mod` 5) - 2.0) * 5.0
        y = pkgGridY + (toNumber (m.id `mod` 7) - 3.0) * 5.0

        -- Targets and sources for this module
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
      -- Only create link if target exists and is different from source
      if targetId /= sourceId
        then Just
          { source: sourceId
          , target: targetId
          , linkType: M2M_Graph  -- Tree classification done by Viz layer
          }
        else Nothing

    -- All nodes
    nodes = packageNodes <> moduleNodes

    -- Create Package records for model
    -- depends is derived from module imports (derivedPackageDeps)
    packages :: Array Package
    packages = v2Packages <#> \p ->
      { name: p.name
      , depends: Array.fromFoldable $ fromMaybe Set.empty (Map.lookup p.name derivedPackageDeps)
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
-- Extended Loader with Raw V2 Data
-- =============================================================================

-- | LoadedModel with raw V2 data preserved for views that need it (e.g., BeeswarmViz)
type LoadedModelWithV2 =
  { model :: LoadedModel
  , v2Packages :: Array V2Package
  , v2Modules :: Array V2ModuleListItem
  , v2Imports :: Array V2ModuleImports
  }

-- | Load model from v2 API, also returning raw V2 data
-- | Use this when you need both the transformed model and raw data for
-- | specialized visualizations like the topological beeswarm.
loadModelFromV2WithRaw :: Aff (Either String LoadedModelWithV2)
loadModelFromV2WithRaw = do
  -- Fetch all data
  packagesResult <- fetchV2Packages
  modulesResult <- fetchV2Modules
  importsResult <- fetchV2AllImports

  pure $ do
    v2Packages <- packagesResult
    v2Modules <- modulesResult
    v2Imports <- importsResult

    -- Transform to model
    let model = transformV2ToModel v2Packages v2Modules v2Imports

    Right { model, v2Packages, v2Modules, v2Imports }

-- =============================================================================
-- Module Declaration Stats (for bubble pack visualization)
-- =============================================================================

-- | Declaration counts by kind for a single module
type V2ModuleDeclarationStats =
  { moduleId :: Int
  , kinds :: Object Int  -- "value" -> 10, "type_class" -> 2, etc.
  , total :: Int
  }

-- | Response wrapper for JSON decoding
type V2ModuleDeclarationStatsResponse =
  { stats :: Array V2ModuleDeclarationStats
  , count :: Int
  }

-- | Fetch declaration stats for all modules
-- | Returns declaration counts grouped by kind for each module
fetchV2ModuleDeclarationStats :: Aff (Either String (Array V2ModuleDeclarationStats))
fetchV2ModuleDeclarationStats = do
  result <- fetchJson (apiBaseUrl <> "/api/v2/module-declaration-stats")
  pure $ do
    json <- result
    response :: V2ModuleDeclarationStatsResponse <- decodeJson json # mapLeft printJsonDecodeError
    Right response.stats

-- =============================================================================
-- Polyglot Summary (for sunburst visualization)
-- =============================================================================

-- | FFI LOC breakdown by backend
type FfiLoc =
  { js :: Int
  , erlang :: Int
  , python :: Int
  , lua :: Int
  }

-- | Package in polyglot summary
type PolyglotPackage =
  { id :: Int
  , name :: String
  , version :: String
  , source :: String
  , value :: Int        -- For sunburst sizing (totalLoc with minimum)
  , totalLoc :: Int
  , moduleCount :: Int
  , ffiFileCount :: Int
  , ffiLoc :: FfiLoc
  }

-- | Project in polyglot summary
type PolyglotProject =
  { id :: Int
  , name :: String
  , backend :: String
  , packageCount :: Int
  , children :: Array PolyglotPackage
  , ffiLoc :: FfiLoc
  }

-- | Backend in polyglot summary
type PolyglotBackend =
  { name :: String
  , displayName :: String
  , totalLoc :: Int
  , packageCount :: Int
  , children :: Array PolyglotProject
  }

-- | Full polyglot summary (hierarchical: root -> backends -> projects -> packages)
type PolyglotSummary =
  { name :: String
  , children :: Array PolyglotBackend
  , backendCount :: Int
  , projectCount :: Int
  , packageCount :: Int
  }

-- | Fetch polyglot summary for sunburst visualization
fetchPolyglotSummary :: Aff (Either String PolyglotSummary)
fetchPolyglotSummary = do
  result <- fetchJson (apiBaseUrl <> "/api/v2/polyglot-summary")
  pure $ result >>= \json -> decodeJson json # mapLeft printJsonDecodeError
