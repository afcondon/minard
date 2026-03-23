-- | Shared types for the SceneCoordinator family of modules.
-- |
-- | All type definitions live here so that helper modules (Loaders, Overlays,
-- | Search) can import them without depending on the coordinator itself.
module CE2.Component.SceneCoordinator.Types
  ( State
  , Action(..)
  , Slot
  , Slots
  , Input
  , Output(..)
  , Query(..)
  , V2Data
  , TransitionState
  , CapturedPosition
  , HistoryState
  , smallPackageThreshold
  , pushHistoryState
  , replaceHistoryState
  , readPopState
  , mkHistoryState
  , historyFocalPackage
  , historyScope
  , readSceneFromHash
  , HashState
  -- Slot proxies
  , _bubblePackBeeswarmViz
  , _galaxyBeeswarmViz
  , _galaxyTreemapViz
  , _moduleTreemapViz
  , _moduleOverviewViz
  , _declarationDetailViz
  , _pkgModuleBeeswarmViz
  , _typeClassGridViz
  , _moduleStructureViz
  , _moduleSignaturesViz
  , _gitOverviewViz
  , _dependencyChordViz
  , _dependencyAdjacencyViz
  , _slideOutPanel
  , _packageReportViz
  , _annotationReportViz
  , _landingPageViz
  , _projectManagementViz
  , _projectAnatomyViz
  , _namespaceTreeViz
  , _packageAnatomyViz
  , _moduleAnatomyViz
  , _compareModuleViz
  , _snapshotManagementViz
  , _commitModuleGridViz
  , _coChangeCubeViz
  ) where

import Prelude

import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Data.String as String
import Effect (Effect)
import Foreign (unsafeToForeign, unsafeFromForeign, isNull, isUndefined)
import Halogen as H
import Type.Proxy (Proxy(..))
import Web.Event.Event as WE
import Web.HTML (window)
import Web.HTML.Location as Location
import Web.HTML.Window (history, location) as Win
import Web.HTML.History (pushState, replaceState, DocumentTitle(..), URL(..)) as History
import Web.HTML.Event.PopStateEvent as PopStateEvent
import Web.UIEvent.KeyboardEvent (KeyboardEvent)

-- Child visualization components
import CE2.Component.BubblePackBeeswarmViz as BubblePackBeeswarmViz
import CE2.Component.GalaxyBeeswarmViz as GalaxyBeeswarmViz
import CE2.Component.ModuleTreemapEnrichedViz as ModuleTreemapEnrichedViz
import CE2.Component.ModuleOverviewViz as ModuleOverviewViz
import CE2.Component.DeclarationDetailViz as DeclarationDetailViz
import CE2.Component.GalaxyTreemapViz as GalaxyTreemapViz
import CE2.Component.PkgModuleBeeswarmViz as PkgModuleBeeswarmViz
import CE2.Component.TypeClassGridViz as TypeClassGridViz
import CE2.Component.ModuleStructureViz as ModuleStructureViz
import CE2.Component.ModuleSignaturesViz as ModuleSignaturesViz
import CE2.Component.GitOverviewViz as GitOverviewViz
import CE2.Component.AnnotationReportViz as AnnotationReportViz
import CE2.Component.PackageReportViz as PackageReportViz
import CE2.Component.LandingPageViz as LandingPageViz
import CE2.Component.ProjectManagementViz as ProjectManagementViz
import CE2.Component.SnapshotManagementViz as SnapshotManagementViz
import CE2.Component.CommitModuleGridViz as CommitModuleGridViz
import CE2.Component.CoChangeCubeViz as CoChangeCubeViz
import CE2.Component.ProjectAnatomyViz as ProjectAnatomyViz
import CE2.Component.NamespaceTreeViz as NamespaceTreeViz
import CE2.Component.PackageAnatomyViz as PackageAnatomyViz
import CE2.Component.ModuleAnatomyViz as ModuleAnatomyViz
import CE2.Component.CompareModuleViz as CompareModuleViz
import CE2.Component.DependencyChordViz as DependencyChordViz
import CE2.Component.DependencyAdjacencyViz as DependencyAdjacencyViz
import CE2.Component.SlideOutPanel as SlideOutPanel

import CE2.Data.Loader as Loader
import CE2.Scene (Scene, sceneFromString)
import CE2.Types (ColorMode, BeeswarmScope(..), RefreshPhase, PackageReachability, PackageClusters, PackagePurity)
import CE2.Component.SceneCoordinator.Pure (ViewMode)

-- =============================================================================
-- Browser History (pure PureScript, no FFI)
-- =============================================================================

type HistoryState =
  { scene :: String
  , viewMode :: String
  , focalPackage :: String  -- "" for Nothing, package name for Just
  , scope :: String         -- "all", "project", "deps", "transitive"
  }

pushHistoryState :: HistoryState -> Effect Unit
pushHistoryState hs = do
  w <- window
  h <- Win.history w
  History.pushState (unsafeToForeign hs) (History.DocumentTitle "") (historyUrl hs) h

replaceHistoryState :: HistoryState -> Effect Unit
replaceHistoryState hs = do
  w <- window
  h <- Win.history w
  History.replaceState (unsafeToForeign hs) (History.DocumentTitle "") (historyUrl hs) h

-- | Build a URL hash from a HistoryState
-- | Only includes non-default params to keep URLs clean
historyUrl :: HistoryState -> History.URL
historyUrl hs =
  let
    params = Array.catMaybes
      [ if hs.viewMode /= "primary" then Just ("view=" <> hs.viewMode) else Nothing
      , if hs.focalPackage /= "" then Just ("focal=" <> hs.focalPackage) else Nothing
      , if hs.scope /= "project" then Just ("scope=" <> hs.scope) else Nothing
      ]
    queryStr = case params of
      [] -> ""
      ps -> "?" <> String.joinWith "&" ps
  in History.URL ("#/" <> hs.scene <> queryStr)

-- | Read history state from a PopStateEvent, returning Nothing if state is null
readPopState :: WE.Event -> Maybe HistoryState
readPopState evt = do
  popEvt <- PopStateEvent.fromEvent evt
  let st = PopStateEvent.state popEvt
  if isNull st || isUndefined st then Nothing
  else Just (unsafeFromForeign st :: HistoryState)

-- | Build a HistoryState from typed values
mkHistoryState :: String -> String -> Maybe String -> BeeswarmScope -> HistoryState
mkHistoryState sceneStr viewModeStr mFocal scope =
  { scene: sceneStr
  , viewMode: viewModeStr
  , focalPackage: case mFocal of
      Just pkg -> pkg
      Nothing -> ""
  , scope: scopeToStr scope
  }
  where
  scopeToStr = case _ of
    AllPackages -> "all"
    ProjectOnly -> "project"
    ProjectWithDeps -> "deps"
    ProjectWithTransitive -> "transitive"

-- | Extract focal package from history state
historyFocalPackage :: HistoryState -> Maybe String
historyFocalPackage hs =
  if hs.focalPackage == "" then Nothing else Just hs.focalPackage

-- | Parsed URL hash state
type HashState =
  { scene :: Scene
  , viewMode :: String
  , focalPackage :: Maybe String
  , scope :: BeeswarmScope
  }

-- | Read scene and params from the current URL hash
-- | Parses: #/SolarSwarm?focal=minard-frontend&scope=project&view=matrix
readSceneFromHash :: Effect (Maybe HashState)
readSceneFromHash = do
  w <- window
  loc <- Win.location w
  hash <- Location.hash loc
  -- Strip leading "#/"
  let raw = case String.stripPrefix (String.Pattern "#/") hash of
        Just s -> s
        Nothing -> case String.stripPrefix (String.Pattern "#") hash of
          Just s -> s
          Nothing -> ""
  if String.null raw then pure Nothing
  else do
    -- Split on "?" to separate scene from params
    let { sceneStr, params } = case String.indexOf (String.Pattern "?") raw of
          Just idx ->
            { sceneStr: String.take idx raw
            , params: parseParams (String.drop (idx + 1) raw)
            }
          Nothing ->
            { sceneStr: raw, params: Map.empty }
    case sceneFromString sceneStr of
      Nothing -> pure Nothing
      Just scene -> pure $ Just
        { scene
        , viewMode: fromMaybe "primary" (Map.lookup "view" params)
        , focalPackage: Map.lookup "focal" params
        , scope: case Map.lookup "scope" params of
            Just "all" -> AllPackages
            Just "deps" -> ProjectWithDeps
            Just "transitive" -> ProjectWithTransitive
            _ -> ProjectOnly
        }
  where
  parseParams :: String -> Map.Map String String
  parseParams str =
    let pairs = String.split (String.Pattern "&") str
    in Map.fromFoldable $ Array.mapMaybe parsePair pairs

  parsePair :: String -> Maybe (Tuple String String)
  parsePair s = case String.indexOf (String.Pattern "=") s of
    Just idx -> Just (Tuple (String.take idx s) (String.drop (idx + 1) s))
    Nothing -> Nothing

-- | Extract scope from history state
historyScope :: HistoryState -> BeeswarmScope
historyScope hs = case hs.scope of
  "all" -> AllPackages
  "project" -> ProjectOnly
  "deps" -> ProjectWithDeps
  "transitive" -> ProjectWithTransitive
  _ -> ProjectOnly

-- =============================================================================
-- Types
-- =============================================================================

-- | V2 data for specialized visualizations
type V2Data =
  { packages :: Array Loader.V2Package
  , modules :: Array Loader.V2ModuleListItem
  , imports :: Array Loader.V2ModuleImports
  }

-- | Transition state for animated scene changes
-- | Captures positions from source scene to initialize target scene
type TransitionState =
  { from :: Scene
  , to :: Scene
  , positions :: Map String { x :: Number, y :: Number, r :: Number }
  , progress :: Number  -- 0.0 to 1.0
  }

-- | Input from parent (AppShell)
type Input =
  { modelData :: Maybe Loader.LoadedModel
  , v2Data :: Maybe V2Data
  , packageSetData :: Maybe Loader.PackageSetData
  , initialScene :: Scene
  , initialFocalPackage :: Maybe String
  , initialScope :: BeeswarmScope
  }

-- | Output to parent
data Output
  = RequestPackageSetData
  | SceneChanged Scene
  | ProjectLoaded          -- A project was loaded; AppShell should re-fetch all data
  | RequestDataRefresh (Array Loader.ProjectInfo)  -- User clicked Sync; re-run loader + re-fetch

-- | Slot type for parent component
type Slot = H.Slot Query Output

-- | Queries from parent
data Query a
  = SetScene Scene a
  | NotifyRefreshError String a  -- Refresh failed; set error state

-- | Child component slots (streamlined - removed debug components)
type Slots =
  ( bubblePackBeeswarmViz :: BubblePackBeeswarmViz.Slot Unit
  , galaxyBeeswarmViz :: GalaxyBeeswarmViz.Slot Unit
  , galaxyTreemapViz :: GalaxyTreemapViz.Slot Unit
  , moduleTreemapViz :: ModuleTreemapEnrichedViz.Slot Unit
  , moduleOverviewViz :: ModuleOverviewViz.Slot Unit
  , declarationDetailViz :: DeclarationDetailViz.Slot Unit
  , pkgModuleBeeswarmViz :: PkgModuleBeeswarmViz.Slot Unit
  , typeClassGridViz :: H.Slot TypeClassGridViz.Query TypeClassGridViz.Output Unit
  , moduleStructureViz :: ModuleStructureViz.Slot Unit
  , moduleSignaturesViz :: ModuleSignaturesViz.Slot Unit
  , gitOverviewViz :: GitOverviewViz.Slot Unit
  , dependencyChordViz :: DependencyChordViz.Slot String
  , dependencyAdjacencyViz :: DependencyAdjacencyViz.Slot String
  , slideOutPanel :: SlideOutPanel.Slot Unit
  , packageReportViz :: PackageReportViz.Slot Unit
  , annotationReportViz :: AnnotationReportViz.Slot Unit
  , landingPageViz :: LandingPageViz.Slot Unit
  , projectManagementViz :: ProjectManagementViz.Slot Unit
  , projectAnatomyViz :: ProjectAnatomyViz.Slot Unit
  , namespaceTreeViz :: H.Slot NamespaceTreeViz.Query NamespaceTreeViz.Output Unit
  , packageAnatomyViz :: PackageAnatomyViz.Slot Unit
  , moduleAnatomyViz :: ModuleAnatomyViz.Slot Unit
  , compareModuleViz :: CompareModuleViz.Slot Unit
  , snapshotManagementViz :: SnapshotManagementViz.Slot Unit
  , commitModuleGridViz :: CommitModuleGridViz.Slot Unit
  , coChangeCubeViz :: CoChangeCubeViz.Slot Unit
  )

_bubblePackBeeswarmViz :: Proxy "bubblePackBeeswarmViz"
_bubblePackBeeswarmViz = Proxy

_galaxyBeeswarmViz :: Proxy "galaxyBeeswarmViz"
_galaxyBeeswarmViz = Proxy

_galaxyTreemapViz :: Proxy "galaxyTreemapViz"
_galaxyTreemapViz = Proxy

_moduleTreemapViz :: Proxy "moduleTreemapViz"
_moduleTreemapViz = Proxy

_moduleOverviewViz :: Proxy "moduleOverviewViz"
_moduleOverviewViz = Proxy

_declarationDetailViz :: Proxy "declarationDetailViz"
_declarationDetailViz = Proxy

_pkgModuleBeeswarmViz :: Proxy "pkgModuleBeeswarmViz"
_pkgModuleBeeswarmViz = Proxy

_typeClassGridViz :: Proxy "typeClassGridViz"
_typeClassGridViz = Proxy

_moduleStructureViz :: Proxy "moduleStructureViz"
_moduleStructureViz = Proxy

_moduleSignaturesViz :: Proxy "moduleSignaturesViz"
_moduleSignaturesViz = Proxy

_gitOverviewViz :: Proxy "gitOverviewViz"
_gitOverviewViz = Proxy

_dependencyChordViz :: Proxy "dependencyChordViz"
_dependencyChordViz = Proxy

_dependencyAdjacencyViz :: Proxy "dependencyAdjacencyViz"
_dependencyAdjacencyViz = Proxy

_slideOutPanel :: Proxy "slideOutPanel"
_slideOutPanel = Proxy

_packageReportViz :: Proxy "packageReportViz"
_packageReportViz = Proxy

_annotationReportViz :: Proxy "annotationReportViz"
_annotationReportViz = Proxy

_landingPageViz :: Proxy "landingPageViz"
_landingPageViz = Proxy

_projectManagementViz :: Proxy "projectManagementViz"
_projectManagementViz = Proxy

_projectAnatomyViz :: Proxy "projectAnatomyViz"
_projectAnatomyViz = Proxy

_namespaceTreeViz :: Proxy "namespaceTreeViz"
_namespaceTreeViz = Proxy

_packageAnatomyViz :: Proxy "packageAnatomyViz"
_packageAnatomyViz = Proxy

_moduleAnatomyViz :: Proxy "moduleAnatomyViz"
_moduleAnatomyViz = Proxy

_compareModuleViz :: Proxy "compareModuleViz"
_compareModuleViz = Proxy

_snapshotManagementViz :: Proxy "snapshotManagementViz"
_snapshotManagementViz = Proxy

_commitModuleGridViz :: Proxy "commitModuleGridViz"
_commitModuleGridViz = Proxy

_coChangeCubeViz :: Proxy "coChangeCubeViz"
_coChangeCubeViz = Proxy

-- | Captured position for transitions (from treemap cells or beeswarm)
type CapturedPosition = { name :: String, x :: Number, y :: Number, r :: Number }

-- | Module count threshold for skipping treemap overview
-- | Packages with fewer modules go directly to module flow view
-- | Larger packages show treemap first for orientation
smallPackageThreshold :: Int
smallPackageThreshold = 200

-- | Component state - streamlined for teaser navigation
type State =
  { -- Current scene
    scene :: Scene

    -- Data from parent (immutable)
  , modelData :: Maybe Loader.LoadedModel
  , v2Data :: Maybe V2Data
  , packageSetData :: Maybe Loader.PackageSetData

    -- Scope (for GUP in beeswarm)
  , scope :: BeeswarmScope

    -- Focal package (for neighborhood filtering in SolarSwarm)
    -- When set, SolarSwarm filters to show only this package + its deps/dependents
  , focalPackage :: Maybe String

    -- Color mode (persists through transitions)
  , colorMode :: ColorMode

    -- View mode (resets to PrimaryView on scene change)
  , viewMode :: ViewMode

    -- Transition (during animated transitions)
  , transition :: Maybe TransitionState
  , capturedPositions :: Maybe (Array CapturedPosition)  -- For animated transitions

    -- Declaration stats for module bubblepack view (lazy loaded)
  , declarationStats :: Maybe (Map.Map Int Loader.V2ModuleDeclarationStats)

    -- Package declarations for enriched treemap (lazy loaded per package)
  , packageDeclarations :: Map.Map Int (Array Loader.V2Declaration)

    -- Function calls for declaration-level dependency highlighting (lazy loaded once)
  , packageCalls :: Map.Map Int (Array Loader.V2FunctionCall)
  , allCallsLoaded :: Boolean

    -- Module annotations (lazy loaded per module, keyed by module name)
  , moduleAnnotations :: Map.Map String (Array Loader.V2Annotation)

    -- All annotations (lazy loaded for AnnotationReport scene)
  , allAnnotations :: Maybe (Array Loader.V2Annotation)

    -- Panel state (tracked by coordinator for visibility)
  , panelOpen :: Boolean
  , panelContent :: SlideOutPanel.PanelContent

    -- Coordinated hover state
  , hoveredPackage :: Maybe String    -- Package name currently being hovered
  , hoveredModule :: Maybe { packageName :: String, moduleName :: String }  -- Module being hovered

    -- Type class stats (lazy loaded for TypeClassGrid scene)
  , typeClassStats :: Maybe Loader.TypeClassStats

    -- Namespace tree data (lazy loaded for NamespaceTree scene)
  , namespaceTreeData :: Maybe (Array Loader.V2NamespaceTreeNode)

    -- Namespace → packages mapping (lazy loaded for NamespaceTree scene)
  , namespacePackages :: Maybe (Array Loader.NamespacePackageEntry)

    -- Git status (lazy loaded when Git mode activated)
  , gitStatus :: Maybe Loader.GitStatusData

    -- Reachability data (lazy computed when Reachability mode activated)
  , reachabilityData :: Maybe PackageReachability

    -- Reachability peek (hold R key to overlay text labels)
  , reachabilityPeek :: Boolean
  , keyboardCleanup :: Maybe (Effect Unit)

    -- Cluster data (lazy computed when Cluster mode activated)
  , clusterData :: Maybe PackageClusters

    -- Purity data (lazy computed when P key peek activated)
  , purityData :: Maybe PackagePurity
  , purityPeek :: Boolean

    -- Structural complexity data (lazy loaded when StructuralComplexity mode activated)
  , complexityData :: Maybe (Map.Map String Loader.ModuleStructuralComplexity)
  , complexityPeek :: Boolean

    -- Source overlay (shows registry vs local vs workspace)
  , sourcePeek :: Boolean

    -- Change frequency data (lazy loaded from git commit history)
  , changeFrequencyData :: Maybe (Map.Map String Number)

    -- Co-change cluster data (lazy computed from git commit history)
  , coChangeClusterData :: Maybe (Map.Map String Int)

    -- Size-by-change-frequency toggle
  , sizeByChangeFrequency :: Boolean

    -- Infrastructure link filtering (Tidy mode)
  , hideInfraLinks :: Boolean  -- When true, hide dependency links to low topo-layer packages

    -- Project management
  , loadedProjects :: Array Loader.ProjectInfo

    -- Browser history integration
  , historyCleanup :: Maybe (Effect Unit)  -- Cleanup function for popstate listener

    -- Incremental refresh
  , refreshPhase :: RefreshPhase

    -- Scene loading (shows wait cursor during data fetch)
  , sceneLoading :: Boolean

    -- Search typeahead
  , searchQuery :: String
  , searchResults :: Array Loader.UnifiedSearchResult
  , searchSelectedIndex :: Int
  , searchOpen :: Boolean
  , searchSeqId :: Int  -- Monotonic counter for debounce (ignore stale responses)
  }

-- | Actions - streamlined
data Action
  = Initialize
  | Receive Input
  | NavigateTo Scene
  | HandlePopstate Scene ViewMode (Maybe String) BeeswarmScope  -- Browser back/forward: scene, viewMode, focalPackage, scope
  | HandleBubblePackBeeswarmOutput BubblePackBeeswarmViz.Output
  | HandleGalaxyBeeswarmOutput GalaxyBeeswarmViz.Output
  | HandleGalaxyTreemapOutput GalaxyTreemapViz.Output
  | HandleModuleTreemapOutput ModuleTreemapEnrichedViz.Output
  | HandleModuleOverviewOutput ModuleOverviewViz.Output
  | HandleDeclarationDetailOutput DeclarationDetailViz.Output
  | HandleModuleStructureOutput ModuleStructureViz.Output
  | HandleModuleSignaturesOutput ModuleSignaturesViz.Output
  | HandleGitOverviewOutput GitOverviewViz.Output
  | HandlePackageReportOutput PackageReportViz.Output
  | HandleTypeClassGridOutput TypeClassGridViz.Output
  | HandleNamespaceTreeOutput NamespaceTreeViz.Output
  | HandleAnnotationReportOutput AnnotationReportViz.Output
  | HandleLandingPageOutput LandingPageViz.Output
  | HandleProjectManagementOutput ProjectManagementViz.Output
  | HandleProjectAnatomyOutput ProjectAnatomyViz.Output
  | HandlePackageAnatomyOutput PackageAnatomyViz.Output
  | HandleSnapshotManagementOutput SnapshotManagementViz.Output
  | HandleCommitModuleGridOutput CommitModuleGridViz.Output
  | HandleCoChangeCubeOutput CoChangeCubeViz.Output
  | HandleModuleAnatomyOutput ModuleAnatomyViz.Output
  | SetScope BeeswarmScope
  | SetFocalPackage (Maybe String)        -- Set/clear focal package for neighborhood view
  | SetViewMode ViewMode                  -- Switch between primary/matrix/chord
  | HandleSlideOutPanelOutput SlideOutPanel.Output
  | OpenModulePanel String String         -- packageName, moduleName
  | OpenPackagePanel String               -- packageName - opens panel with first module
  | ToggleGitMode                         -- Click toggle: GitStatus color mode
  | ToggleTidyMode                        -- Click toggle: infrastructure link filtering
  | ToggleReachabilityMode                -- Click toggle: reachability coloring
  | ToggleClusterMode                     -- Click toggle: cluster coloring
  | ToggleComplexityMode                  -- Click toggle: structural complexity coloring
  | ToggleChangeFrequencyMode             -- Click toggle: change frequency heat map
  | ToggleCoChangeClusterMode             -- Click toggle: co-change community coloring
  | ToggleSizeByFrequency                 -- Click toggle: treemap sizing by change frequency
  | ToggleReachabilityPeek               -- Click toggle for reachability peek
  | TogglePurityPeek                     -- Click toggle for purity peek
  | ToggleCouplingPeek                   -- Click toggle for coupling peek
  | ToggleSourcePeek                    -- Click toggle for source overlay (registry/local/workspace)
  -- Momentary keyboard peeks (hold key = show, release = revert)
  | OverlayPeekOn String                  -- Key pressed — activate overlay by key name
  | OverlayPeekOff                        -- Any overlay key released — revert to default
  -- Incremental refresh (two-click confirmation)
  | ArmSync                               -- First click: show "Confirm?" + start timeout
  | ConfirmSync                           -- Second click: actually trigger sync
  | RevertSyncArm                         -- Auto-revert pending state after timeout
  | RequestRefresh                        -- Internal: actually starts the sync
  | ClearRefreshDone                      -- Timer fires 1.5s after sync completion
  -- Search typeahead
  | SearchInput String                    -- User typed in search box
  | SearchResultsReceived Int (Array Loader.UnifiedSearchResult)  -- Results arrived (seqId, results)
  | SearchKeyDown KeyboardEvent           -- Keyboard event on search input
  | SearchConfirmIndex Int                -- Mouse click on specific result
  | SearchDismiss                         -- Escape or blur
