-- | Data loading from Minard V2 API
-- |
-- | Loads data from the unified schema API and transforms into visualization-ready structures.
module CE2.Data.Loader
  ( -- API base
    apiBaseUrl
    -- Model types
  , LoadedModel
  , LoadedModelWithV2
  , DeclarationsMap
  , Declaration
    -- Package Set types (for galaxy visualizations)
  , PackageSetInfo
  , PackageSetPackage
  , PackageSetData
    -- V2 API types
  , V2Stats
  , V2Package
  , V2Module
  , V2ModuleListItem
  , V2ModuleImports
  , V2ModuleDeclarationStats
  , V2Declaration
  , V2Superclass
  , V2ChildDeclaration
  , V2Namespace
  , V2SearchResult
  , SourceSpan
  , V2FunctionCall
  , V2Import
    -- Polyglot types
  , PolyglotSummary
  , PolyglotBackend
  , PolyglotProject
  , PolyglotPackage
  , FfiLoc
    -- Loaders
  , loadModelFromV2WithRaw
  , fetchPackageSetFromV2
    -- V2 API fetchers
  , fetchV2Stats
  , fetchV2Packages
  , fetchUnusedPackages
  , v2PackageToPackageSetPackage
  , fetchV2Modules
  , fetchV2ModuleDeclarations
  , fetchV2PackageDeclarations
  , fetchV2ModuleCalls
  , fetchV2PackageCalls
  , fetchV2ModuleDeclarationStats
  , fetchV2AllImports
  , fetchV2AllCalls
  , V2ModuleCalls
  , fetchPolyglotSummary
    -- Type Class Stats
  , TypeClassStats
  , TypeClassInfo
  , TypeClassSummary
  , fetchTypeClassStats
    -- Git Status
  , GitStatusData
  , fetchGitStatus
    -- Combined Search
  , UnifiedSearchResult
  , searchAll
    -- Declaration Usage
  , UsageNode
  , DeclarationUsage
  , fetchDeclarationUsage
    -- Module Source
  , ModuleSource
  , fetchModuleSource
  , fetchModuleSourceForSnapshot
    -- Annotations
  , V2Annotation
  , V2AnnotationsResponse
  , fetchAllAnnotations
  , fetchModuleAnnotations
  , patchAnnotationStatus
  , createAnnotation
    -- Namespace Tree
  , V2NamespaceTreeNode
  , fetchNamespaceTree
    -- Namespace Packages mapping
  , NamespacePackageEntry
  , fetchNamespacePackages
    -- Source Location (editor integration)
  , SourceLocation
  , fetchSourceLocation
    -- Git Blame
  , BlameLineInfo
  , BlameResult
  , fetchModuleBlame
    -- Project Management (V2)
  , ProjectInfo
  , ProjectStats
  , PathValidation
  , ValidationCheck
  , ValidationIssue
  , LoadResult
  , fetchV2Projects
  , validateProjectPath
  , loadProject
  , deleteProject
  , reloadProjects
    -- Structural Complexity
  , ModuleStructuralComplexity
  , fetchModuleStructuralComplexity
    -- Snapshots
  , V2Snapshot
  , fetchSnapshots
  , fetchV2ModulesForSnapshot
  , fetchV2AllCallsForSnapshot
    -- Snapshot Management
  , GitCommit
  , SnapshotDetail
  , fetchGitLog
  , fetchSnapshotDetails
  , createSnapshotFromRef
  , deleteSnapshotsByIds
    -- Commit-Module Grid
  , CommitFileEntry
  , fetchCommitFiles
    -- Module Numstat (per-commit line additions/deletions)
  , NumstatCommit
  , ModuleLineCounts
  , fetchModuleNumstat
  ) where

import Prelude

import Affjax.Web as AW
import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat as ResponseFormat
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson, printJsonDecodeError)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Traversable (traverse)
import Control.Parallel (parTraverse)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.CodeUnits as SCU
import Data.String.Common (joinWith, replaceAll)
import Data.String.Pattern (Pattern(..), Replacement(..))
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Foreign.Object (Object)
import Foreign.Object as Object
import CE2.Types (SimNode, SimLink, Package)
import CE2.Data.Loader.Transform as Transform

-- | API base URL for ce-server
-- | Detected at runtime: "/code" when served behind the edge router, "http://localhost:3000" for local dev
foreign import apiBaseUrl :: String

-- =============================================================================
-- Types
-- =============================================================================

-- | Raw module from JSON (defined in Transform, re-aliased here for legacy loaders)
type RawModule = Transform.RawModule

-- | Raw package from JSON
type RawPackage = Transform.RawPackage

-- | LOC entry from JSON
type LocEntry = Transform.LocEntry

-- | LOC file structure
type LocFile = Transform.LocFile

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
-- Transformation (delegated to CE2.Data.Loader.Transform)
-- =============================================================================

transformToModel :: Object RawModule -> Object RawPackage -> Map String Int -> DeclarationsMap -> LoadedModel
transformToModel = Transform.transformToModel

buildLocMap :: Array LocEntry -> Map String Int
buildLocMap = Transform.buildLocMap

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
  , source :: String              -- "registry" | "workspace" | "extra"
  , bundleModule :: Maybe String  -- Spago bundle entry module (apps only)
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
          , packages: raw.packages <#> \pkg ->
              { id: pkg.id
              , name: pkg.name
              , version: pkg.version
              , description: pkg.description
              , license: pkg.license
              , repositoryOwner: pkg.repositoryOwner
              , repositoryName: pkg.repositoryName
              , depends: pkg.depends
              , topoLayer: pkg.topoLayer
              , publishedAt: pkg.publishedAt
              , releaseNumber: pkg.releaseNumber
              , moduleCount: pkg.moduleCount
              , totalLoc: pkg.totalLoc
              , source: "registry"  -- Old API doesn't have source, default to registry
              , bundleModule: Nothing
              }
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
-- | Uses topoLayer from API (computed by loader, single source of truth)
v2ToPackageSetData :: Array V2Package -> PackageSetData
v2ToPackageSetData v2Packages =
  let
    -- Convert packages using topoLayer from API
    convert :: V2Package -> PackageSetPackage
    convert pkg =
      { id: pkg.id
      , name: pkg.name
      , version: pkg.version
      , description: pkg.description
      , license: pkg.license
      , repositoryOwner: Nothing
      , repositoryName: pkg.repository
      , depends: pkg.depends
      , topoLayer: pkg.topoLayer  -- From API (computed by loader)
      , publishedAt: Nothing
      , releaseNumber: 0
      , moduleCount: pkg.moduleCount
      , totalLoc: pkg.totalLoc
      , source: pkg.source  -- "registry" | "workspace" | "extra"
      , bundleModule: pkg.bundleModule
      }
  in
    { packageSet:
        { id: 0
        , name: "Package Set (V2)"
        , compilerVersion: "0.15.15"
        , source: "v2-api"
        , publishedAt: Nothing
        , packageCount: Array.length v2Packages
        }
    , packages: map convert v2Packages
    }

-- | Convert a single V2Package to PackageSetPackage format
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
  , topoLayer: pkg.topoLayer  -- From API (computed by loader)
  , publishedAt: Nothing
  , releaseNumber: 0
  , moduleCount: pkg.moduleCount
  , totalLoc: pkg.totalLoc  -- Sum of module LOC from v2 API
  , source: pkg.source  -- "registry" | "workspace" | "extra"
  , bundleModule: pkg.bundleModule
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
  , bundleModule :: Maybe String  -- Spago bundle entry module (apps only, e.g. "CE2.Main")
  , moduleCount :: Int
  , declarationCount :: Int
  , totalLoc :: Int         -- Sum of all module LOC in this package
  , depends :: Array String -- Package dependency names
  , topoLayer :: Int        -- Topological layer (0 = no deps, computed by loader)
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

-- | Superclass info for type class declarations
type V2Superclass =
  { name :: String
  , methods :: Array { name :: String, typeSignature :: Maybe String }
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
  , sourceCode :: Maybe String
  , superclasses :: Array V2Superclass
  , typeArguments :: Array String
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

-- | Source span from PureScript source (via corefn.json)
type SourceSpan =
  { start_line :: Int
  , start_col :: Int
  , end_line :: Int
  , end_col :: Int
  }

-- | Function call from v2 API
type V2FunctionCall =
  { callerName :: String
  , calleeModule :: String
  , calleeName :: String
  , isCrossModule :: Boolean
  , callCount :: Int
  , sourceSpan :: Maybe SourceSpan
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

-- | Module with all its function calls (from bulk all-calls endpoint)
type V2ModuleCalls =
  { moduleId :: Int
  , moduleName :: String
  , calls :: Array { callerName :: String, calleeModule :: String, calleeName :: String, isCrossModule :: Boolean, callCount :: Int, sourceSpan :: Maybe SourceSpan }
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
type V2AllCallsResponse = { calls :: Array V2ModuleCalls, count :: Int }

-- | Annotation from v2 API
type V2Annotation =
  { id :: Int
  , targetType :: String
  , targetId :: String
  , kind :: String
  , value :: String
  , source :: String
  , confidence :: Number
  , status :: String
  , supersedes :: Maybe Int
  }

type V2AnnotationsResponse = { annotations :: Array V2Annotation, count :: Int }

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

-- | Fetch registry packages NOT used by the current project
fetchUnusedPackages :: Aff (Either String (Array V2Package))
fetchUnusedPackages = do
  result <- fetchJson (apiBaseUrl <> "/api/v2/packages/unused")
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

-- | Fetch declarations for all modules in a package (parallel requests)
-- | Returns Map from moduleId to Array of declarations
fetchV2PackageDeclarations :: Array V2ModuleListItem -> Aff (Map Int (Array V2Declaration))
fetchV2PackageDeclarations modules = do
  -- Fetch declarations for each module in parallel
  results <- parTraverse fetchModuleDecls modules
  -- Build map from successful results
  pure $ Map.fromFoldable $ Array.catMaybes results
  where
  fetchModuleDecls :: V2ModuleListItem -> Aff (Maybe (Tuple Int (Array V2Declaration)))
  fetchModuleDecls m = do
    result <- fetchV2ModuleDeclarations m.id
    pure $ case result of
      Right decls -> Just (Tuple m.id decls)
      Left _ -> Nothing  -- Silently skip failed fetches

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

-- | Fetch function calls for all modules in a package (parallel requests)
-- | Returns Map from moduleId to Array of function calls originating from that module
fetchV2PackageCalls :: Array V2ModuleListItem -> Aff (Map Int (Array V2FunctionCall))
fetchV2PackageCalls modules = do
  results <- parTraverse fetchModuleCalls modules
  pure $ Map.fromFoldable $ Array.catMaybes results
  where
  fetchModuleCalls :: V2ModuleListItem -> Aff (Maybe (Tuple Int (Array V2FunctionCall)))
  fetchModuleCalls m = do
    result <- fetchV2ModuleCalls m.id
    pure $ case result of
      Right calls -> Just (Tuple m.id calls)
      Left _ -> Nothing  -- Silently skip failed fetches

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

-- =============================================================================
-- Namespace Tree (full tree, all depths)
-- =============================================================================

-- | A node in the full namespace tree
type V2NamespaceTreeNode =
  { id :: Int
  , path :: String
  , segment :: String
  , depth :: Int
  , parentId :: Maybe Int
  , isLeaf :: Boolean
  , moduleCount :: Int
  , totalLoc :: Int
  }

type V2NamespaceTreeResponse = { nodes :: Array V2NamespaceTreeNode, count :: Int }

-- | Fetch the full namespace tree (all depths, with total LOC)
fetchNamespaceTree :: Aff (Either String (Array V2NamespaceTreeNode))
fetchNamespaceTree = do
  result <- fetchJson (apiBaseUrl <> "/api/v2/namespace-tree")
  pure $ do
    json <- result
    response :: V2NamespaceTreeResponse <- decodeJson json # mapLeft printJsonDecodeError
    Right response.nodes

-- =============================================================================
-- Namespace Packages Mapping
-- =============================================================================

-- | An entry mapping a namespace to a contributing package
type NamespacePackageEntry =
  { namespaceId :: Int
  , packageId :: Int
  , packageName :: String
  , moduleCount :: Int
  }

type NamespacePackagesResponse = { entries :: Array NamespacePackageEntry, count :: Int }

-- | Fetch the namespace → packages mapping
fetchNamespacePackages :: Aff (Either String (Array NamespacePackageEntry))
fetchNamespacePackages = do
  result <- fetchJson (apiBaseUrl <> "/api/v2/namespace-packages")
  pure $ do
    json <- result
    response :: NamespacePackagesResponse <- decodeJson json # mapLeft printJsonDecodeError
    Right response.entries

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

-- | Fetch all function calls (bulk endpoint - single request)
-- | Returns Array of V2ModuleCalls which can be converted to Map as needed
fetchV2AllCalls :: Aff (Either String (Array V2ModuleCalls))
fetchV2AllCalls = do
  result <- fetchJson (apiBaseUrl <> "/api/v2/all-calls")
  pure $ do
    json <- result
    response :: V2AllCallsResponse <- decodeJson json # mapLeft printJsonDecodeError
    Right response.calls

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

-- | Transform V2 API data to LoadedModel (delegated to Transform)
transformV2ToModel :: Array V2Package -> Array V2ModuleListItem -> Array V2ModuleImports -> LoadedModel
transformV2ToModel = Transform.transformV2ToModel

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
-- Module Structural Complexity (for coupling score heat map)
-- =============================================================================

-- | Per-module structural complexity metrics from function call graph
type ModuleStructuralComplexity =
  { moduleId :: Int
  , moduleName :: String
  , declCount :: Int
  , internalCalls :: Int
  , crossModuleCalls :: Int
  , internalDensity :: Number
  , maxFanIn :: Int
  , maxFanOut :: Int
  , couplingScore :: Number
  }

type ModuleStructuralComplexityResponse =
  { modules :: Array ModuleStructuralComplexity }

-- | Fetch structural complexity metrics for all modules
fetchModuleStructuralComplexity :: Aff (Either String (Array ModuleStructuralComplexity))
fetchModuleStructuralComplexity = do
  result <- fetchJson (apiBaseUrl <> "/api/v2/module-structural-complexity")
  pure $ do
    json <- result
    response :: ModuleStructuralComplexityResponse <- decodeJson json # mapLeft printJsonDecodeError
    Right response.modules

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

-- =============================================================================
-- Type Class Stats (V2)
-- =============================================================================

-- | Summary statistics for type classes
type TypeClassSummary =
  { totalMethods :: Int
  , totalInstances :: Int
  , avgMethodsPerClass :: String
  , avgInstancesPerClass :: String
  }

-- | Information about a single type class
type TypeClassInfo =
  { id :: Int
  , name :: String
  , moduleName :: String
  , packageName :: String
  , methodCount :: Int
  , instanceCount :: Int
  }

-- | Full type class stats response
type TypeClassStats =
  { typeClasses :: Array TypeClassInfo
  , count :: Int
  , summary :: TypeClassSummary
  }

-- | Fetch type class statistics
fetchTypeClassStats :: Aff (Either String TypeClassStats)
fetchTypeClassStats = do
  result <- fetchJson (apiBaseUrl <> "/api/v2/type-class-stats")
  pure $ result >>= \json -> decodeJson json # mapLeft printJsonDecodeError

-- =============================================================================
-- Git Status (live query)
-- =============================================================================

-- | Git working tree status
-- | modified: modules with unstaged changes
-- | staged: modules with staged changes
-- | untracked: new untracked modules
type GitStatusData =
  { modified :: Array String    -- Module names (e.g., "CE2.Component.SceneCoordinator")
  , staged :: Array String
  , untracked :: Array String
  , timestamp :: Number         -- When the status was fetched
  }

-- | Fetch current git status
-- | This is a live query - always fetches fresh data
fetchGitStatus :: Aff (Either String GitStatusData)
fetchGitStatus = do
  result <- fetchJson (apiBaseUrl <> "/api/v2/git/status")
  pure $ result >>= \json -> decodeJson json # mapLeft printJsonDecodeError

-- =============================================================================
-- Combined Search (declarations + modules + packages)
-- =============================================================================

-- | Unified search result from combined search endpoint
type UnifiedSearchResult =
  { entityType :: String    -- "declaration" | "module" | "package"
  , id :: Int
  , name :: String
  , kind :: Maybe String
  , typeSignature :: Maybe String
  , moduleName :: Maybe String
  , packageName :: String
  , packageVersion :: String
  }

type UnifiedSearchResponse = { results :: Array UnifiedSearchResult, count :: Int }

-- | Search across declarations, modules, and packages
-- | Supports prefix sugar: class:, module:, package:, type:
searchAll :: String -> Aff (Either String (Array UnifiedSearchResult))
searchAll query = do
  result <- fetchJson (apiBaseUrl <> "/api/v2/search/" <> query)
  pure $ do
    json <- result
    response :: UnifiedSearchResponse <- decodeJson json # mapLeft printJsonDecodeError
    Right response.results

-- =============================================================================
-- Declaration Usage (cross-module call graph)
-- =============================================================================

-- | A node in the usage graph (caller or callee)
type UsageNode =
  { moduleName :: String
  , declName :: String
  , hop :: Int
  , kind :: String
  , typeSignature :: Maybe String
  }

-- | Bidirectional cross-module usage for a declaration
type DeclarationUsage =
  { callers :: Array UsageNode
  , callees :: Array UsageNode
  , callerCount :: Int
  , calleeCount :: Int
  , focusTypeSignature :: Maybe String
  }

-- | Fetch cross-module usage for a declaration (callers + callees, transitive)
fetchDeclarationUsage :: String -> String -> Aff (Either String DeclarationUsage)
fetchDeclarationUsage moduleName declName = do
  result <- fetchJson (apiBaseUrl <> "/api/v2/declaration-usage?module=" <> moduleName <> "&decl=" <> declName)
  pure $ result >>= \json -> decodeJson json # mapLeft printJsonDecodeError

-- =============================================================================
-- Module Source (read .purs file from disk)
-- =============================================================================

-- | Module source file content and path
type ModuleSource =
  { source :: String
  , path :: String
  }

-- | Fetch the full source file for a module
fetchModuleSource :: String -> Aff (Either String ModuleSource)
fetchModuleSource moduleName = do
  result <- fetchJson (apiBaseUrl <> "/api/v2/module-source?module=" <> moduleName)
  pure $ result >>= \json -> decodeJson json # mapLeft printJsonDecodeError

-- | Fetch module source from a specific snapshot (worktree on disk)
fetchModuleSourceForSnapshot :: String -> Int -> Aff (Either String ModuleSource)
fetchModuleSourceForSnapshot moduleName snapshotId = do
  result <- fetchJson (apiBaseUrl <> "/api/v2/module-source?module="
    <> moduleName <> "&snapshot=" <> show snapshotId)
  pure $ result >>= \json -> decodeJson json # mapLeft printJsonDecodeError

-- =============================================================================
-- Source Location (editor integration)
-- =============================================================================

-- | Resolved file path for editor integration
type SourceLocation =
  { filePath :: String
  }

-- | Resolve a module name to its absolute file path on disk
fetchSourceLocation :: String -> Aff (Either String SourceLocation)
fetchSourceLocation moduleName = do
  result <- fetchJson (apiBaseUrl <> "/api/v2/source-location?module=" <> moduleName)
  pure $ result >>= \json -> decodeJson json # mapLeft printJsonDecodeError

-- =============================================================================
-- Git Blame (per-line)
-- =============================================================================

-- | Per-line blame info from git
type BlameLineInfo =
  { lineNum :: Int
  , hash :: String
  , shortHash :: String
  , author :: String
  , authorTime :: Int
  , summary :: String
  }

-- | Full blame result for a module
type BlameResult =
  { lines :: Array BlameLineInfo
  , filePath :: String
  , oldestTime :: Int
  , newestTime :: Int
  }

-- | Fetch per-line git blame for a module
fetchModuleBlame :: String -> Aff (Either String BlameResult)
fetchModuleBlame moduleName = do
  result <- fetchJson (apiBaseUrl <> "/api/v2/git/blame?module=" <> moduleName)
  pure $ result >>= \json -> decodeJson json # mapLeft printJsonDecodeError

-- =============================================================================
-- Annotations
-- =============================================================================

-- | Fetch all annotations (no filters, gets everything)
fetchAllAnnotations :: Aff (Either String (Array V2Annotation))
fetchAllAnnotations = do
  result <- fetchJson (apiBaseUrl <> "/api/v2/annotations")
  pure $ do
    json <- result
    response :: V2AnnotationsResponse <- decodeJson json # mapLeft printJsonDecodeError
    Right response.annotations

-- | Fetch annotations for a module
fetchModuleAnnotations :: String -> Aff (Either String (Array V2Annotation))
fetchModuleAnnotations moduleName = do
  result <- fetchJson (apiBaseUrl <> "/api/v2/annotations?target_type=module&target_id=" <> moduleName)
  pure $ do
    json <- result
    response :: V2AnnotationsResponse <- decodeJson json # mapLeft printJsonDecodeError
    Right response.annotations

-- | Update an annotation's status (confirm/dispute)
patchAnnotationStatus :: Int -> String -> Aff (Either String Unit)
patchAnnotationStatus annId newStatus = do
  let url = apiBaseUrl <> "/api/v2/annotations/" <> show annId
      body = RequestBody.string ("{\"status\":\"" <> newStatus <> "\"}")
  result <- AW.patch ResponseFormat.json url body
  pure $ case result of
    Left err -> Left $ "PATCH error: " <> AW.printError err
    Right _ -> Right unit

-- | Create a new annotation via POST
createAnnotation
  :: { targetType :: String, targetId :: String, kind :: String, value :: String, source :: String, supersedes :: Maybe Int }
  -> Aff (Either String V2Annotation)
createAnnotation args = do
  let supersededStr = case args.supersedes of
        Just sid -> ", \"supersedes\": " <> show sid
        Nothing -> ""
      jsonBody = "{\"target_type\": " <> escapeJsonStr args.targetType
              <> ", \"target_id\": " <> escapeJsonStr args.targetId
              <> ", \"kind\": " <> escapeJsonStr args.kind
              <> ", \"value\": " <> escapeJsonStr args.value
              <> ", \"source\": " <> escapeJsonStr args.source
              <> supersededStr <> "}"
      url = apiBaseUrl <> "/api/v2/annotations"
      body = RequestBody.string jsonBody
  result <- AW.post ResponseFormat.json url (Just body)
  pure $ case result of
    Left err -> Left $ "POST error: " <> AW.printError err
    Right response -> decodeJson response.body # mapLeft printJsonDecodeError

-- | Minimal JSON string escaping: wraps in quotes, escapes backslash, double-quote, and newlines
escapeJsonStr :: String -> String
escapeJsonStr s =
  let escaped = replaceAll (Pattern "\\") (Replacement "\\\\")
              $ replaceAll (Pattern "\"") (Replacement "\\\"")
              $ replaceAll (Pattern "\n") (Replacement "\\n")
              $ s
  in "\"" <> escaped <> "\""

-- =============================================================================
-- Project Management (V2)
-- =============================================================================

-- | Project info from the V2 projects endpoint
type ProjectInfo =
  { id :: Int
  , name :: String
  , repoPath :: String
  , primaryBackend :: String
  , createdAt :: Maybe String
  , stats :: ProjectStats
  }

type ProjectStats =
  { packageCount :: Int
  , moduleCount :: Int
  , declarationCount :: Int
  }

-- | Path validation result
type PathValidation =
  { valid :: Boolean
  , path :: String
  , projectName :: String
  , checks :: ValidationCheck
  , issues :: Array ValidationIssue
  }

type ValidationCheck =
  { directoryExists :: Boolean
  , spagoLockExists :: Boolean
  , outputDirExists :: Boolean
  , docsJsonCount :: Int
  , loaderBinaryExists :: Boolean
  }

type ValidationIssue =
  { severity :: String  -- "error" | "warning"
  , check :: String
  , message :: String
  }

-- | Result of loading a project
type LoadResult =
  { success :: Boolean
  , stats :: Maybe { packages :: Maybe Int, modules :: Maybe Int, declarations :: Maybe Int }
  , error :: Maybe String
  , elapsedMs :: Maybe Number
  }

-- | Projects list response wrapper
type V2ProjectsResponse = { projects :: Array ProjectInfo, count :: Int }

-- | Fetch list of loaded projects with stats
fetchV2Projects :: Aff (Either String (Array ProjectInfo))
fetchV2Projects = do
  result <- fetchJson (apiBaseUrl <> "/api/v2/projects")
  pure $ do
    json <- result
    response :: V2ProjectsResponse <- decodeJson json # mapLeft printJsonDecodeError
    Right response.projects

-- | Validate a project path (prerequisites check)
validateProjectPath :: String -> Aff (Either String PathValidation)
validateProjectPath projectPath = do
  let url = apiBaseUrl <> "/api/v2/projects/validate"
      body = RequestBody.string ("{\"path\":" <> escapeJsonStr projectPath <> "}")
  result <- AW.post ResponseFormat.json url (Just body)
  pure $ case result of
    Left err -> Left $ "POST error: " <> AW.printError err
    Right response -> decodeJson response.body # mapLeft printJsonDecodeError

-- | Load a project via the Rust loader
loadProject :: { path :: String, name :: Maybe String, label :: Maybe String } -> Aff (Either String LoadResult)
loadProject args = do
  let nameStr = case args.name of
        Just n -> ", \"name\": " <> escapeJsonStr n
        Nothing -> ""
      labelStr = case args.label of
        Just l -> ", \"label\": " <> escapeJsonStr l
        Nothing -> ""
      jsonBody = "{\"path\": " <> escapeJsonStr args.path <> nameStr <> labelStr <> "}"
      url = apiBaseUrl <> "/api/v2/projects/load"
      body = RequestBody.string jsonBody
  result <- AW.post ResponseFormat.json url (Just body)
  pure $ case result of
    Left err -> Left $ "POST error: " <> AW.printError err
    Right response -> decodeJson response.body # mapLeft printJsonDecodeError

-- | Delete a project and all its data
deleteProject :: Int -> Aff (Either String Unit)
deleteProject projectId = do
  let url = apiBaseUrl <> "/api/v2/projects/" <> show projectId
  result <- AW.delete ResponseFormat.json url
  pure $ case result of
    Left err -> Left $ "DELETE error: " <> AW.printError err
    Right _ -> Right unit

-- | Re-run the loader for each project (sequential — server blocks on execSync)
reloadProjects :: Array ProjectInfo -> Aff (Array (Either String LoadResult))
reloadProjects projects = traverse reloadOne projects
  where
  reloadOne project = loadProject
    { path: project.repoPath
    , name: Just project.name
    , label: Nothing
    }

-- =============================================================================
-- Snapshots
-- =============================================================================

-- | Snapshot metadata from the API
type V2Snapshot =
  { id :: Int
  , projectId :: Int
  , gitHash :: Maybe String
  , gitRef :: Maybe String
  , label :: Maybe String
  , packageCount :: Int
  , moduleCount :: Int
  , workspacePackageCount :: Int
  }

type V2SnapshotsResponse = { snapshots :: Array V2Snapshot, count :: Int }

-- | Fetch available snapshots for the default project
fetchSnapshots :: Aff (Either String (Array V2Snapshot))
fetchSnapshots = do
  result <- fetchJson (apiBaseUrl <> "/api/v2/snapshots")
  pure $ do
    json <- result
    response :: V2SnapshotsResponse <- decodeJson json # mapLeft printJsonDecodeError
    Right response.snapshots

-- | Fetch modules scoped to a specific snapshot
fetchV2ModulesForSnapshot :: Int -> Aff (Either String (Array V2ModuleListItem))
fetchV2ModulesForSnapshot snapshotId = do
  result <- fetchJson (apiBaseUrl <> "/api/v2/modules?snapshot=" <> show snapshotId)
  pure $ do
    json <- result
    response :: V2ModulesResponse <- decodeJson json # mapLeft printJsonDecodeError
    Right response.modules

-- | Fetch all function calls scoped to a specific snapshot
fetchV2AllCallsForSnapshot :: Int -> Aff (Either String (Array V2ModuleCalls))
fetchV2AllCallsForSnapshot snapshotId = do
  result <- fetchJson (apiBaseUrl <> "/api/v2/all-calls?snapshot=" <> show snapshotId)
  pure $ do
    json <- result
    response :: V2AllCallsResponse <- decodeJson json # mapLeft printJsonDecodeError
    Right response.calls

-- =============================================================================
-- Snapshot Management
-- =============================================================================

-- | A git commit from the log endpoint
type GitCommit =
  { hash :: String
  , shortHash :: String
  , message :: String
  , author :: String
  , date :: String
  , relativeDate :: String
  , refs :: Array String
  , hasSnapshot :: Boolean
  }

-- | Enhanced snapshot detail with worktree info
type SnapshotDetail =
  { id :: Int
  , projectId :: Int
  , gitHash :: Maybe String
  , gitRef :: Maybe String
  , label :: Maybe String
  , repoPath :: String
  , projectName :: Maybe String
  , packageCount :: Int
  , moduleCount :: Int
  , workspacePackageCount :: Int
  , isCurrentCheckout :: Boolean
  , canDelete :: Boolean
  }

type GitLogResponse = { commits :: Array GitCommit, hasMore :: Boolean }
type SnapshotDetailsResponse = { snapshots :: Array SnapshotDetail, count :: Int }
type DeleteResult = { snapshotId :: Int, deleted :: Boolean, warning :: Maybe String, error :: Maybe String }
type DeleteResultsResponse = { results :: Array DeleteResult }

-- | Fetch git commit log with pagination
fetchGitLog :: Int -> Int -> Aff (Either String { commits :: Array GitCommit, hasMore :: Boolean })
fetchGitLog count offset = do
  result <- fetchJson (apiBaseUrl <> "/api/v2/git/log?count=" <> show count <> "&offset=" <> show offset)
  pure $ do
    json <- result
    response :: GitLogResponse <- decodeJson json # mapLeft printJsonDecodeError
    Right { commits: response.commits, hasMore: response.hasMore }

-- | Fetch enhanced snapshot listing with worktree info
fetchSnapshotDetails :: Aff (Either String (Array SnapshotDetail))
fetchSnapshotDetails = do
  result <- fetchJson (apiBaseUrl <> "/api/v2/snapshots/details")
  pure $ do
    json <- result
    response :: SnapshotDetailsResponse <- decodeJson json # mapLeft printJsonDecodeError
    Right response.snapshots

-- | Create a snapshot from a git ref (commit hash, branch, or tag)
createSnapshotFromRef :: String -> Maybe String -> Aff (Either String LoadResult)
createSnapshotFromRef ref mLabel = do
  let labelStr = case mLabel of
        Just l -> ", \"label\": " <> escapeJsonStr l
        Nothing -> ""
      jsonBody = "{\"ref\": " <> escapeJsonStr ref <> labelStr <> "}"
      url = apiBaseUrl <> "/api/v2/snapshots/create"
      body = RequestBody.string jsonBody
  result <- AW.post ResponseFormat.json url (Just body)
  pure $ case result of
    Left err -> Left $ "POST error: " <> AW.printError err
    Right response -> decodeJson response.body # mapLeft printJsonDecodeError

-- | Delete snapshots by IDs (removes worktrees + cascade deletes from DB)
deleteSnapshotsByIds :: Array Int -> Aff (Either String (Array DeleteResult))
deleteSnapshotsByIds ids = do
  let idsStr = joinWith ", " (map show ids)
      jsonBody = "{\"snapshotIds\": [" <> idsStr <> "]}"
      url = apiBaseUrl <> "/api/v2/snapshots/delete"
      body = RequestBody.string jsonBody
  result <- AW.post ResponseFormat.json url (Just body)
  pure $ case result of
    Left err -> Left $ "POST error: " <> AW.printError err
    Right response -> do
      r :: DeleteResultsResponse <- decodeJson response.body # mapLeft printJsonDecodeError
      Right r.results

-- =============================================================================
-- Commit-Module Grid (which modules changed in each commit)
-- =============================================================================

-- | A commit with the list of modules it touched within a package
type CommitFileEntry =
  { hash :: String
  , shortHash :: String
  , message :: String
  , relativeDate :: String
  , modules :: Array String
  , moduleStatuses :: Object String  -- module name -> "A"/"M"/"D"/"R"
  }

type CommitFilesResponse =
  { commits :: Array CommitFileEntry
  , allModules :: Array String
  , count :: Int
  }

-- | Fetch commit-module data for a package
fetchCommitFiles :: Int -> String -> Aff (Either String { commits :: Array CommitFileEntry, allModules :: Array String })
fetchCommitFiles count pkg = do
  result <- fetchJson (apiBaseUrl <> "/api/v2/git/commit-files?count=" <> show count <> "&package=" <> pkg)
  pure $ do
    json <- result
    response :: CommitFilesResponse <- decodeJson json # mapLeft printJsonDecodeError
    Right { commits: response.commits, allModules: response.allModules }

-- =============================================================================
-- Module Numstat (per-commit line additions/deletions by module)
-- =============================================================================

-- | Per-module line additions and deletions within a commit
type ModuleLineCounts =
  { added :: Int
  , deleted :: Int
  }

-- | A commit with total lines added/deleted and per-module breakdown
type NumstatCommit =
  { hash :: String
  , shortHash :: String
  , message :: String
  , relativeDate :: String
  , totalAdded :: Int
  , totalDeleted :: Int
  , modules :: Object ModuleLineCounts  -- module name -> { added, deleted }
  }

type NumstatResponse =
  { commits :: Array NumstatCommit
  , count :: Int
  }

-- | Fetch per-commit line stats for a package
fetchModuleNumstat :: Int -> String -> Aff (Either String (Array NumstatCommit))
fetchModuleNumstat count pkg = do
  result <- fetchJson (apiBaseUrl <> "/api/v2/git/module-numstat?count=" <> show count <> "&package=" <> pkg)
  pure $ do
    json <- result
    response :: NumstatResponse <- decodeJson json # mapLeft printJsonDecodeError
    Right response.commits
