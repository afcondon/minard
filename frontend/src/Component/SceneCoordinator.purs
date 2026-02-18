-- | Scene Coordinator Component (Streamlined)
-- |
-- | Streamlined coordinator for "YouTube teaser" navigation path.
-- | Clean linear navigation: Treemap → Beeswarm → SolarSwarm → Neighborhood → PkgTreemap
-- |
-- | Key features:
-- | - 6 scenes (down from 12)
-- | - Animated transitions between scenes
-- | - Linear navigation with instant back jumps
-- | - Scope filtering (GUP) for beeswarm views
module CE2.Component.SceneCoordinator
  ( component
  , Input
  , Output(..)
  , Slot
  , Query(..)
  , V2Data
  , TransitionState
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Milliseconds(..))
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Type.Proxy (Proxy(..))
import Web.Event.Event as WE
import Web.Event.Event (EventType(..))
import Web.Event.EventTarget (addEventListener, removeEventListener, eventListener) as ET
import Web.HTML (window)
import Web.HTML.HTMLDocument (toEventTarget) as HTMLDoc
import Web.HTML.Window (document) as Win
import Web.UIEvent.KeyboardEvent (KeyboardEvent, toEvent, key, repeat)
import Web.UIEvent.KeyboardEvent as KE

-- PSD3 Imports
import Hylograph.HATS.InterpreterTick (clearContainer, clearAllHighlights)

-- Child visualization components (streamlined)
import CE2.Component.BubblePackBeeswarmViz as BubblePackBeeswarmViz
import CE2.Component.GalaxyBeeswarmViz as GalaxyBeeswarmViz
import CE2.Component.ModuleTreemapEnrichedViz as ModuleTreemapEnrichedViz
import CE2.Component.ModuleOverviewViz as ModuleOverviewViz
import CE2.Component.DeclarationDetailViz as DeclarationDetailViz
import CE2.Component.GalaxyTreemapViz as GalaxyTreemapViz
import CE2.Component.PkgModuleBeeswarmViz as PkgModuleBeeswarmViz
import CE2.Component.TypeClassGridViz as TypeClassGridViz
import CE2.Component.ModuleSignatureMapViz as ModuleSignatureMapViz
import CE2.Component.AnnotationReportViz as AnnotationReportViz
import CE2.Component.ProjectManagementViz as ProjectManagementViz
import CE2.Component.ProjectAnatomyViz as ProjectAnatomyViz
import CE2.Component.DependencyChordViz as DependencyChordViz
import CE2.Component.DependencyAdjacencyViz as DependencyAdjacencyViz
import CE2.Component.SlideOutPanel as SlideOutPanel

import CE2.Containers as C
import CE2.Data.Loader as Loader
import CE2.Scene (Scene(..), BreadcrumbSegment, sceneBreadcrumbs, sceneFromString, sceneToString)
import CE2.Viz.DependencyMatrix as DependencyMatrix
import CE2.Viz.SourceCode as SourceCode
import Data.Graph.Algorithms (reachableFrom, connectedComponents, labelPropagation) as GraphAlgo
import CE2.Types (projectPackages, ViewTheme(..), ColorMode(..), BeeswarmScope(..), themeColors, isDarkTheme, PackageGitStatus, PackageReachability, PackageClusters, PackagePurity)
import CE2.Viz.DeclarationArcDiagram (isEffectful) as ArcDiagram

-- FFI declarations for browser history integration
foreign import pushHistoryState :: String -> String -> Effect Unit
foreign import replaceHistoryState :: String -> String -> Effect Unit
foreign import setupPopstateListener :: (String -> String -> Effect Unit) -> Effect (Effect Unit)

-- | Serialize ViewMode to string for browser history
viewModeToString :: ViewMode -> String
viewModeToString = case _ of
  PrimaryView -> "primary"
  MatrixView -> "matrix"
  ChordView -> "chord"

-- | Deserialize ViewMode from string
viewModeFromString :: String -> ViewMode
viewModeFromString = case _ of
  "matrix" -> MatrixView
  "chord" -> ChordView
  _ -> PrimaryView

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
  }

-- | Output to parent
data Output
  = RequestPackageSetData
  | SceneChanged Scene
  | ProjectLoaded          -- A project was loaded; AppShell should re-fetch all data

-- | Slot type for parent component
type Slot = H.Slot Query Output

-- | Queries from parent
data Query a
  = SetScene Scene a

-- | Child component slots (streamlined - removed debug components)
type Slots =
  ( bubblePackBeeswarmViz :: BubblePackBeeswarmViz.Slot Unit
  , galaxyBeeswarmViz :: GalaxyBeeswarmViz.Slot Unit
  , galaxyTreemapViz :: GalaxyTreemapViz.Slot Unit
  , moduleTreemapViz :: ModuleTreemapEnrichedViz.Slot Unit
  , moduleOverviewViz :: ModuleOverviewViz.Slot Unit
  , declarationDetailViz :: DeclarationDetailViz.Slot Unit
  , pkgModuleBeeswarmViz :: PkgModuleBeeswarmViz.Slot Unit
  , typeClassGridViz :: TypeClassGridViz.Slot Unit
  , moduleSignatureMapViz :: ModuleSignatureMapViz.Slot Unit
  , dependencyChordViz :: DependencyChordViz.Slot String
  , dependencyAdjacencyViz :: DependencyAdjacencyViz.Slot String
  , slideOutPanel :: SlideOutPanel.Slot Unit
  , annotationReportViz :: AnnotationReportViz.Slot Unit
  , projectManagementViz :: ProjectManagementViz.Slot Unit
  , projectAnatomyViz :: ProjectAnatomyViz.Slot Unit
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

_moduleSignatureMapViz :: Proxy "moduleSignatureMapViz"
_moduleSignatureMapViz = Proxy

_dependencyChordViz :: Proxy "dependencyChordViz"
_dependencyChordViz = Proxy

_dependencyAdjacencyViz :: Proxy "dependencyAdjacencyViz"
_dependencyAdjacencyViz = Proxy

_slideOutPanel :: Proxy "slideOutPanel"
_slideOutPanel = Proxy

_annotationReportViz :: Proxy "annotationReportViz"
_annotationReportViz = Proxy

_projectManagementViz :: Proxy "projectManagementViz"
_projectManagementViz = Proxy

_projectAnatomyViz :: Proxy "projectAnatomyViz"
_projectAnatomyViz = Proxy

-- | Captured position for transitions (from treemap cells or beeswarm)
type CapturedPosition = { name :: String, x :: Number, y :: Number, r :: Number }

-- | Module count threshold for skipping treemap overview
-- | Packages with fewer modules go directly to module flow view
-- | Larger packages show treemap first for orientation
smallPackageThreshold :: Int
smallPackageThreshold = 200

-- | Unified view mode for visualizations
-- | Replaces separate ModuleViewMode and PackageViewMode
-- | Resets to PrimaryView on scene change
data ViewMode
  = PrimaryView    -- Default: BubblePack for packages, Treemap for modules
  | MatrixView     -- Adjacency matrix
  | ChordView      -- Chord diagram

derive instance eqViewMode :: Eq ViewMode

instance showViewMode :: Show ViewMode where
  show PrimaryView = "PrimaryView"
  show MatrixView = "MatrixView"
  show ChordView = "ChordView"

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

    -- Infrastructure link filtering (Tidy mode)
  , hideInfraLinks :: Boolean  -- When true, hide dependency links to low topo-layer packages

    -- Project management
  , loadedProjects :: Array Loader.ProjectInfo

    -- Browser history integration
  , historyCleanup :: Maybe (Effect Unit)  -- Cleanup function for popstate listener

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
  | HandlePopstate Scene ViewMode     -- Browser back/forward button pressed
  | HandleBubblePackBeeswarmOutput BubblePackBeeswarmViz.Output
  | HandleGalaxyBeeswarmOutput GalaxyBeeswarmViz.Output
  | HandleGalaxyTreemapOutput GalaxyTreemapViz.Output
  | HandleModuleTreemapOutput ModuleTreemapEnrichedViz.Output
  | HandleModuleOverviewOutput ModuleOverviewViz.Output
  | HandleDeclarationDetailOutput DeclarationDetailViz.Output
  | HandleModuleSignatureMapOutput ModuleSignatureMapViz.Output
  | HandleAnnotationReportOutput AnnotationReportViz.Output
  | HandleProjectManagementOutput ProjectManagementViz.Output
  | HandleProjectAnatomyOutput ProjectAnatomyViz.Output
  | SetScope BeeswarmScope
  | SetFocalPackage (Maybe String)        -- Set/clear focal package for neighborhood view
  | SetViewMode ViewMode                  -- Switch between primary/matrix/chord
  | HandleSlideOutPanelOutput SlideOutPanel.Output
  | OpenModulePanel String String         -- packageName, moduleName
  | OpenPackagePanel String               -- packageName - opens panel with first module
  | ToggleGitMode                         -- Toggle between GitStatus color mode and previous mode
  | ToggleTidyMode                        -- Toggle infrastructure link filtering
  | ToggleReachabilityMode                -- Toggle reachability coloring (dead code detection)
  | ToggleClusterMode                     -- Toggle cluster coloring (connected components)
  | ReachabilityPeekOn                    -- R key pressed - show peek overlay
  | ReachabilityPeekOff                   -- R key released - hide peek overlay
  | PurityPeekOn                          -- P key pressed - show purity overlay
  | PurityPeekOff                         -- P key released - hide purity overlay
  -- Search typeahead
  | SearchInput String                    -- User typed in search box
  | SearchResultsReceived Int (Array Loader.UnifiedSearchResult)  -- Results arrived (seqId, results)
  | SearchKeyDown KeyboardEvent           -- Keyboard event on search input
  | SearchConfirmIndex Int                -- Mouse click on specific result
  | SearchDismiss                         -- Escape or blur

-- =============================================================================
-- Component
-- =============================================================================

component :: forall m. MonadAff m => H.Component Query Input Output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , handleQuery = handleQuery
        , receive = Just <<< Receive
        , initialize = Just Initialize
        }
    }

initialState :: Input -> State
initialState input =
  { scene: input.initialScene
  , modelData: input.modelData
  , v2Data: input.v2Data
  , packageSetData: input.packageSetData
  , scope: AllPackages
  , focalPackage: Nothing        -- No focal package initially
  , colorMode: FullRegistryTopo  -- Topo coloring persists through transitions
  , viewMode: PrimaryView        -- Resets to PrimaryView on scene change
  , transition: Nothing
  , capturedPositions: Nothing
  , declarationStats: Nothing    -- Lazy loaded when needed for module bubblepack
  , packageDeclarations: Map.empty  -- Lazy loaded per package for enriched treemap
  , packageCalls: Map.empty       -- Lazy loaded once (all calls) for dependency highlighting
  , allCallsLoaded: false
  , moduleAnnotations: Map.empty
  , allAnnotations: Nothing
  , panelOpen: false
  , panelContent: SlideOutPanel.NoContent
  , hoveredPackage: Nothing
  , hoveredModule: Nothing
  , typeClassStats: Nothing
  , gitStatus: Nothing
  , reachabilityData: Nothing
  , reachabilityPeek: false
  , keyboardCleanup: Nothing
  , clusterData: Nothing
  , purityData: Nothing
  , purityPeek: false
  , hideInfraLinks: false
  , loadedProjects: []
  , historyCleanup: Nothing
  , searchQuery: ""
  , searchResults: []
  , searchSelectedIndex: 0
  , searchOpen: false
  , searchSeqId: 0
  }

-- =============================================================================
-- Render
-- =============================================================================

render :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
render state =
  let theme = themeForScene state.scene
      colors = themeColors theme
  in HH.div
    [ HP.class_ (HH.ClassName "scene-coordinator")
    -- Note: MUST use height: 100vh (not min-height) for flex-grow to work with the child's height: 0 pattern
    , HP.style $ "display: flex; flex-direction: column; height: 100vh; background: " <> colors.background <> "; transition: background 0.5s ease;"
    ]
    [ -- Header bar with breadcrumb navigation
      renderHeaderBar state

      -- Scene-specific visualization (fills remaining space)
      -- Note: height: 0 + flex: 1 allows flex-grow to work properly with children that use height: 100%
    , HH.div
        [ HP.id "viz"
        , HP.class_ (HH.ClassName "scene-viz-container")
        , HP.style "flex: 1; position: relative; overflow: hidden; height: 0; min-height: 0;"
        ]
        [ renderScene state ]

      -- Footer bar (persistent, shows stats and selection)
    , renderFooterBar state

      -- Slide-out panel (for source/documentation)
    , HH.slot _slideOutPanel unit SlideOutPanel.component
        { initiallyOpen: false }
        HandleSlideOutPanelOutput
    ]

-- | Check if scene is a package treemap
isPackageTreemap :: Scene -> Boolean
isPackageTreemap (PkgTreemap _) = true
isPackageTreemap _ = false

-- =============================================================================
-- Header, Tab Strip, and Footer
-- =============================================================================

-- | Style for header toggle buttons (Types, Git)
toggleButtonStyle :: Boolean -> String -> String
toggleButtonStyle isActive textColor =
  "background: " <> (if isActive then "rgba(0,0,0,0.15)" else "none") <> "; "
    <> "border: 1px solid " <> (if isActive then textColor else "rgba(0,0,0,0.25)") <> "; "
    <> "color: " <> textColor <> "; "
    <> "cursor: pointer; font-size: 9px; padding: 2px 6px; border-radius: 3px;"

-- | Render the header bar with breadcrumb navigation
renderHeaderBar :: forall m. State -> H.ComponentHTML Action Slots m
renderHeaderBar state =
  let
    textColor = "#333333"
    bgColor = "#D4C9A8"
    crumbs = sceneBreadcrumbs state.scene
    lastIdx = Array.length crumbs - 1

    -- Render a single breadcrumb segment
    renderSegment :: Int -> BreadcrumbSegment -> Array (H.ComponentHTML Action Slots m)
    renderSegment idx seg =
      let
        isFinal = idx == lastIdx
        separator = if idx > 0
          then [ HH.span
                   [ HP.style "margin: 0 6px; opacity: 0.5;" ]
                   [ HH.text "›" ] ]
          else []
        kindPrefix = if seg.kind == ""
          then []
          else [ HH.span
                   [ HP.style "opacity: 0.45; font-weight: normal;" ]
                   [ HH.text (seg.kind <> " ") ] ]
        label =
          if isFinal
            then
              HH.span
                [ HP.style "font-weight: bold;" ]
                (kindPrefix <> [ HH.text seg.label ])
            else
              HH.span
                [ HE.onClick \_ -> NavigateTo seg.scene
                , HP.style "cursor: pointer; text-decoration: underline; text-underline-offset: 2px; text-decoration-color: rgba(0,0,0,0.3);"
                ]
                (kindPrefix <> [ HH.text seg.label ])
      in separator <> [label]

  in HH.div
    [ HP.class_ (HH.ClassName "scene-header-bar")
    , HP.style $ "height: 36px; padding: 0 16px; display: flex; align-items: center; justify-content: space-between; "
        <> "background: " <> bgColor <> "; color: " <> textColor <> "; "
        <> "font-family: 'Courier New', Courier, monospace; font-size: 11px; "
        <> "border-bottom: 1px solid #999;"
    ]
    [ -- Left: Branding + Breadcrumbs
      HH.div
        [ HP.style "display: flex; align-items: center; gap: 8px;" ]
        ( [ HH.span
              [ HP.style "font-weight: bold; font-size: 12px; letter-spacing: 1px; text-transform: uppercase; margin-right: 8px;" ]
              [ HH.text "MINARD" ]
          ]
          <> Array.concat (Array.mapWithIndex renderSegment crumbs)
        )

      -- Right: Search + Types + Git toggle + state code (debug)
    , HH.div
        [ HP.style "display: flex; align-items: center; gap: 8px;" ]
        [ -- Search input with dropdown
          renderSearchInput state
        , HH.button
            [ HE.onClick \_ -> NavigateTo ProjectManagement
            , HP.style $ toggleButtonStyle (state.scene == ProjectManagement) textColor
            ]
            [ HH.text "Projects" ]
        , HH.button
            [ HE.onClick \_ -> NavigateTo ProjectAnatomy
            , HP.style $ toggleButtonStyle (state.scene == ProjectAnatomy) textColor
            ]
            [ HH.text "Anatomy" ]
        , HH.button
            [ HE.onClick \_ -> NavigateTo TypeClassGrid
            , HP.style $ toggleButtonStyle (state.scene == TypeClassGrid) textColor
            ]
            [ HH.text "Types" ]
        , HH.button
            [ HE.onClick \_ -> NavigateTo AnnotationReport
            , HP.style $ toggleButtonStyle (state.scene == AnnotationReport) textColor
            ]
            [ HH.text "Report" ]
        , HH.button
            [ HE.onClick \_ -> ToggleGitMode
            , HP.style $ toggleButtonStyle (state.colorMode == GitStatus) textColor
            ]
            [ HH.text "Git" ]
        , HH.button
            [ HE.onClick \_ -> ToggleTidyMode
            , HP.style $ toggleButtonStyle state.hideInfraLinks textColor
            ]
            [ HH.text "Tidy" ]
        , HH.button
            [ HE.onClick \_ -> ToggleClusterMode
            , HP.style $ toggleButtonStyle (state.colorMode == ClusterView) textColor
            , HP.title "Cluster: modules colored by dependency cluster (connected components). Hold R to peek reachability."
            ]
            [ HH.text "Cluster" ]
        , HH.span
            [ HP.style "font-size: 9px; opacity: 0.6;" ]
            [ HH.text $ "[" <> canonicalStateCode state <> "]" ]
        ]
    ]

-- | Render the footer bar (persistent, shows stats and selection info)
renderFooterBar :: forall m. State -> H.ComponentHTML Action Slots m
renderFooterBar state =
  let
    theme = themeForScene state.scene
    textColor = if isDarkTheme theme then "rgba(255,255,255,0.8)" else "rgba(0,0,0,0.7)"
    bgColor = if isDarkTheme theme then "rgba(0,0,0,0.3)" else "rgba(0,0,0,0.05)"
  in HH.div
    [ HP.class_ (HH.ClassName "scene-footer-bar")
    , HP.style $ "height: 28px; padding: 0 16px; display: flex; align-items: center; justify-content: space-between; "
        <> "background: " <> bgColor <> "; color: " <> textColor <> "; "
        <> "font-size: 11px; backdrop-filter: blur(4px);"
    ]
    [ -- Left: Total stats
      HH.div
        [ HP.style "display: flex; align-items: center; gap: 16px;" ]
        [ renderFooterStats state ]

      -- Center: Selection info (if any)
    , HH.div
        [ HP.style "display: flex; align-items: center; gap: 8px;" ]
        [ renderSelectionInfo state ]

      -- Right: View mode / controls
    , HH.div
        [ HP.style "display: flex; align-items: center; gap: 8px;" ]
        [ renderFooterControls state ]
    ]

-- | Footer stats (total counts)
renderFooterStats :: forall m. State -> H.ComponentHTML Action Slots m
renderFooterStats state =
  case state.packageSetData of
    Just psData ->
      HH.span_
        [ HH.text $ show (Array.length psData.packages) <> " total packages in registry" ]
    Nothing ->
      case state.modelData of
        Just model ->
          HH.span_
            [ HH.text $ show model.packageCount <> " packages • " <> show model.moduleCount <> " modules" ]
        Nothing -> HH.text ""

-- | Selection info (hovered/selected item) - shows legend for PkgTreemap
renderSelectionInfo :: forall m. State -> H.ComponentHTML Action Slots m
renderSelectionInfo state =
  case state.scene of
    PkgTreemap _ -> renderDeclarationLegend
    ModuleOverview _ _ -> renderDeclarationLegend
    DeclarationDetail _ _ _ -> renderDeclarationLegend
    ModuleSignatureMap _ _ -> renderDeclarationLegend
    _ -> renderHoverInfo state

-- | Default hover info display
renderHoverInfo :: forall m. State -> H.ComponentHTML Action Slots m
renderHoverInfo state =
  case state.hoveredPackage of
    Just pkgName ->
      HH.span
        [ HP.style "font-weight: 500;" ]
        [ HH.text $ "▸ " <> pkgName ]
    Nothing ->
      case state.hoveredModule of
        Just { moduleName } ->
          HH.span
            [ HP.style "font-weight: 500;" ]
            [ HH.text $ "▸ " <> moduleName ]
        Nothing ->
          HH.span
            [ HP.style "opacity: 0.5; font-style: italic;" ]
            [ HH.text "hover for details" ]

-- | Legend for declaration kinds in enriched treemap
renderDeclarationLegend :: forall m. H.ComponentHTML Action Slots m
renderDeclarationLegend =
  HH.div
    [ HP.style "display: flex; align-items: center; gap: 12px; font-size: 10px;" ]
    [ legendItem "#4e79a7" "value"
    , legendItem "#59a14f" "data"
    , legendItem "#76b7b2" "newtype"
    , legendItem "#f28e2b" "class"
    , legendItem "#edc948" "synonym"
    , legendItem "#e15759" "foreign"
    ]
  where
    legendItem color label =
      HH.div
        [ HP.style "display: flex; align-items: center; gap: 3px;" ]
        [ HH.span
            [ HP.style $ "width: 8px; height: 8px; border-radius: 50%; background: " <> color <> ";" ]
            []
        , HH.span_ [ HH.text label ]
        ]

-- =============================================================================
-- Search Typeahead
-- =============================================================================

-- | Render the search input with dropdown overlay
renderSearchInput :: forall m. State -> H.ComponentHTML Action Slots m
renderSearchInput state =
  HH.div
    [ HP.class_ (HH.ClassName "header-search-wrapper") ]
    [ HH.input
        [ HP.type_ HP.InputText
        , HP.class_ (HH.ClassName "module-search-input")
        , HP.placeholder "search..."
        , HP.value state.searchQuery
        , HE.onValueInput SearchInput
        , HE.onKeyDown SearchKeyDown
        , HE.onBlur \_ -> SearchDismiss
        ]
    , if state.searchOpen && Array.length state.searchResults > 0
        then renderSearchDropdown state
        else HH.text ""
    ]

-- | Render the search results dropdown
renderSearchDropdown :: forall m. State -> H.ComponentHTML Action Slots m
renderSearchDropdown state =
  HH.div
    [ HP.class_ (HH.ClassName "module-search-dropdown") ]
    (Array.mapWithIndex renderResult state.searchResults)
  where
  renderResult :: Int -> Loader.UnifiedSearchResult -> H.ComponentHTML Action Slots m
  renderResult idx result =
    let
      isSelected = idx == state.searchSelectedIndex
      entityIcon = case result.entityType of
        "package" -> "pkg"
        "module" -> "mod"
        _ -> fromMaybe "val" (result.kind <#> kindAbbrev)
      contextText = case result.entityType of
        "package" -> result.packageVersion
        "module" -> result.packageName
        "declaration" -> fromMaybe "" result.moduleName <> " / " <> result.packageName
        _ -> ""
      typeSigSnippet = case result.typeSignature of
        Just sig -> " :: " <> String.take 50 sig
        Nothing -> ""
    in
      HH.div
        [ HP.classes
            [ HH.ClassName "module-search-result"
            , HH.ClassName (if isSelected then "module-search-result--selected" else "")
            ]
        , HE.onMouseDown \_ -> SearchConfirmIndex idx
        ]
        [ HH.div
            [ HP.style "display: flex; align-items: baseline; gap: 6px;" ]
            [ HH.span
                [ HP.style $ "font-size: 8px; padding: 1px 3px; border-radius: 2px; background: " <> entityColor result.entityType <> "; color: #fff;" ]
                [ HH.text entityIcon ]
            , HH.span
                [ HP.style "font-weight: bold;" ]
                [ HH.text result.name ]
            , HH.span
                [ HP.style "opacity: 0.5; font-size: 10px;" ]
                [ HH.text contextText ]
            ]
        , if typeSigSnippet /= ""
            then HH.div
              [ HP.style "font-size: 9px; opacity: 0.4; margin-top: 1px; white-space: nowrap; overflow: hidden; text-overflow: ellipsis;" ]
              [ HH.text typeSigSnippet ]
            else HH.text ""
        ]

  -- Abbreviate declaration kind for the tag
  kindAbbrev :: String -> String
  kindAbbrev = case _ of
    "value" -> "val"
    "data" -> "dat"
    "newtype" -> "new"
    "type_synonym" -> "syn"
    "type_class" -> "cls"
    "foreign" -> "ffi"
    other -> String.take 3 other

  -- Color for entity type badge
  entityColor :: String -> String
  entityColor = case _ of
    "package" -> "#7c3aed"    -- purple
    "module" -> "#0891b2"     -- cyan
    "declaration" -> "#4e79a7" -- blue
    _ -> "#666"

-- | Footer controls (view mode, scope)
renderFooterControls :: forall m. State -> H.ComponentHTML Action Slots m
renderFooterControls state =
  let
    btnStyle isActive = "padding: 2px 6px; font-size: 9px; border-radius: 2px; cursor: pointer; "
      <> "border: 1px solid " <> (if isActive then "#fff" else "rgba(255,255,255,0.3)") <> "; "
      <> "background: " <> (if isActive then "rgba(255,255,255,0.2)" else "transparent") <> "; "
      <> "color: inherit;"
  in
    case state.scene of
      _ | state.scene == SolarSwarm || isPackageTreemap state.scene ->
        HH.div
          [ HP.style "display: flex; gap: 4px;" ]
          [ HH.button
              [ HE.onClick \_ -> SetViewMode PrimaryView
              , HP.style $ btnStyle (state.viewMode == PrimaryView)
              ]
              [ HH.text "Primary" ]
          , HH.button
              [ HE.onClick \_ -> SetViewMode ChordView
              , HP.style $ btnStyle (state.viewMode == ChordView)
              ]
              [ HH.text "Chord" ]
          , HH.button
              [ HE.onClick \_ -> SetViewMode MatrixView
              , HP.style $ btnStyle (state.viewMode == MatrixView)
              ]
              [ HH.text "Matrix" ]
          ]
      ModuleOverview pkg mod ->
        HH.div
          [ HP.style "display: flex; gap: 4px;" ]
          [ HH.button
              [ HE.onClick \_ -> NavigateTo (ModuleSignatureMap pkg mod)
              , HP.style $ btnStyle false
              ]
              [ HH.text "Sig Map" ]
          ]
      ModuleSignatureMap pkg mod ->
        HH.div
          [ HP.style "display: flex; gap: 4px;" ]
          [ HH.button
              [ HE.onClick \_ -> NavigateTo (ModuleOverview pkg mod)
              , HP.style $ btnStyle false
              ]
              [ HH.text "Overview" ]
          ]
      _ -> HH.text ""

-- | Render the current scene using child component slots
-- | Streamlined to 6 scenes for teaser navigation
renderScene :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
renderScene state =
  let theme = themeForScene state.scene
  in case state.scene of
  GalaxyTreemap ->
    case state.packageSetData of
      Just psData ->
        HH.slot _galaxyTreemapViz unit GalaxyTreemapViz.component
          { packages: psData.packages
          , theme: theme
          , colorMode: state.colorMode
          , infraLayerThreshold: if state.hideInfraLinks then 2 else 0
          , modules: case state.v2Data of
              Just v2 -> v2.modules
              Nothing -> []
          , gitStatus: state.gitStatus
          , reachabilityData: state.reachabilityData
          , reachabilityPeek: state.reachabilityPeek
          }
          HandleGalaxyTreemapOutput
      Nothing ->
        HH.div
          [ HP.class_ (HH.ClassName "loading") ]
          [ HH.text "Loading package set data..." ]

  GalaxyBeeswarm ->
    -- Use GalaxyBeeswarmViz component (beeswarm with scope filtering)
    -- Pass captured positions for Treemap -> Beeswarm hero transition
    case state.packageSetData of
      Just psData ->
        HH.slot _galaxyBeeswarmViz unit GalaxyBeeswarmViz.component
          { packages: psData.packages
          , scope: state.scope
          , theme: theme
          , colorMode: state.colorMode  -- Persists through transitions
          , gitStatus: computePackageGitStatus state.gitStatus state.v2Data
          , initialPositions: state.capturedPositions
          , infraLayerThreshold: if state.hideInfraLinks then 2 else 0
          }
          HandleGalaxyBeeswarmOutput
      Nothing ->
        HH.div
          [ HP.class_ (HH.ClassName "loading") ]
          [ HH.text "Loading package set data..." ]

  SolarSwarm ->
    -- Project packages view with switchable visualization
    -- Options: BubblePack (default), Chord diagram, Adjacency matrix
    -- View mode toggle moved to LHS navigation header
    HH.div
      [ HP.class_ (HH.ClassName "solar-swarm-viz")
      , HP.style "position: relative; width: 100%; height: 100%;"
      ]
      [ case state.viewMode of
          PrimaryView ->
            case state.modelData of
              Just model ->
                let
                  -- Compute import maps for coordinated hover highlighting
                  importMaps = case state.v2Data of
                    Just v2 ->
                      { imports: buildModuleImportMap v2.imports
                      , importedBy: buildModuleImportedByMap v2.imports
                      }
                    Nothing ->
                      { imports: Map.empty, importedBy: Map.empty }
                  -- Compute app packages from V2 data
                  appPkgs = case state.v2Data of
                    Just v2 -> Set.fromFoldable $
                      Array.mapMaybe (\p -> if p.bundleModule /= Nothing then Just p.name else Nothing) v2.packages
                    Nothing -> Set.empty
                in
                  HH.slot _bubblePackBeeswarmViz unit BubblePackBeeswarmViz.component
                    { nodes: model.nodes
                    , packages: model.packages
                    , scope: state.scope
                    , focalPackage: state.focalPackage
                    , theme: theme
                    , colorMode: state.colorMode
                    , initialPositions: state.capturedPositions
                    , moduleImports: importMaps.imports
                    , moduleImportedBy: importMaps.importedBy
                    , appPackages: appPkgs
                    }
                    HandleBubblePackBeeswarmOutput
              Nothing ->
                HH.div
                  [ HP.class_ (HH.ClassName "loading") ]
                  [ HH.text "Loading project data..." ]

          ChordView ->
            case state.v2Data of
              Just v2 ->
                let scopedPackages = solarSwarmScopedPackages state v2.packages
                    depData = DependencyMatrix.buildFromPackageDepends scopedPackages
                in HH.slot _dependencyChordViz "package" DependencyChordViz.component
                     { depData, containerId: C.packageChordContainerId
                     , width: 800.0, height: 800.0, innerRadius: 280.0, outerRadius: 320.0 }
                     absurd
              Nothing ->
                HH.div
                  [ HP.class_ (HH.ClassName "loading") ]
                  [ HH.text "Loading..." ]

          MatrixView ->
            case state.v2Data of
              Just v2 ->
                let scopedPackages = solarSwarmScopedPackages state v2.packages
                    depData = DependencyMatrix.buildFromPackageDepends scopedPackages
                in HH.slot _dependencyAdjacencyViz "package" DependencyAdjacencyViz.component
                     { depData, containerId: C.packageAdjacencyContainerId
                     , width: 1200.0, height: 900.0, cellSize: 20.0
                     , labelWidth: 250.0, labelHeight: 250.0, matrixMode: false }
                     absurd
              Nothing ->
                HH.div
                  [ HP.class_ (HH.ClassName "loading") ]
                  [ HH.text "Loading..." ]
      ]

  PkgTreemap _pkg ->
    -- Package modules view with switchable visualization
    -- Options: Treemap (default), Chord diagram, Adjacency matrix
    -- View mode toggle moved to LHS navigation header
    HH.div
      [ HP.class_ (HH.ClassName "pkg-module-viz")
      , HP.style "position: absolute; top: 0; left: 0; width: 100%; height: 100%;"
      ]
      [ case state.viewMode of
          PrimaryView ->
            -- Use Halogen slot for enriched treemap (treemap + individual declarations)
            case state.v2Data of
              Just v2 ->
                HH.slot _moduleTreemapViz unit ModuleTreemapEnrichedViz.component
                  { packageName: _pkg
                  , modules: v2.modules
                  , imports: v2.imports
                  , declarations: state.packageDeclarations
                  , functionCalls: state.packageCalls
                  , gitStatus: state.gitStatus
                  , colorMode: state.colorMode
                  , reachabilityData: state.reachabilityData
                  , reachabilityPeek: state.reachabilityPeek
                  , clusterData: state.clusterData
                  , isAppPackage: fromMaybe false (state.reachabilityData <#> _.isApp)
                  , purityData: state.purityData
                  , purityPeek: state.purityPeek
                  }
                  HandleModuleTreemapOutput
              Nothing ->
                HH.div
                  [ HP.class_ (HH.ClassName "loading") ]
                  [ HH.text "Loading module data..." ]
          ChordView ->
            case state.v2Data of
              Just v2 ->
                let pkgModules = Array.filter (\m -> m.package.name == _pkg) v2.modules
                    pkgModuleNames = Set.fromFoldable $ map _.name pkgModules
                    pkgModuleIds = Set.fromFoldable $ map _.id pkgModules
                    pkgImports = Array.filter (\imp -> Set.member imp.moduleId pkgModuleIds) v2.imports
                    depData = DependencyMatrix.filterToNames pkgModuleNames
                                (DependencyMatrix.buildFromModuleImports pkgImports)
                in HH.slot _dependencyChordViz "module" DependencyChordViz.component
                     { depData, containerId: C.moduleChordContainerId
                     , width: 800.0, height: 800.0, innerRadius: 280.0, outerRadius: 320.0 }
                     absurd
              Nothing ->
                HH.div
                  [ HP.class_ (HH.ClassName "loading") ]
                  [ HH.text "Loading..." ]
          MatrixView ->
            case state.v2Data of
              Just v2 ->
                let pkgModules = Array.filter (\m -> m.package.name == _pkg) v2.modules
                    pkgModuleNames = Set.fromFoldable $ map _.name pkgModules
                    pkgModuleIds = Set.fromFoldable $ map _.id pkgModules
                    pkgImports = Array.filter (\imp -> Set.member imp.moduleId pkgModuleIds) v2.imports
                    depData = DependencyMatrix.filterToNames pkgModuleNames
                                (DependencyMatrix.buildFromModuleImports pkgImports)
                in HH.slot _dependencyAdjacencyViz "module" DependencyAdjacencyViz.component
                     { depData, containerId: C.moduleAdjacencyContainerId
                     , width: 1200.0, height: 900.0, cellSize: 12.0
                     , labelWidth: 200.0, labelHeight: 200.0, matrixMode: false }
                     absurd
              Nothing ->
                HH.div
                  [ HP.class_ (HH.ClassName "loading") ]
                  [ HH.text "Loading..." ]
      ]

  PkgModuleBeeswarm pkg ->
    case state.v2Data of
      Just v2 ->
        HH.slot _pkgModuleBeeswarmViz unit PkgModuleBeeswarmViz.component
          { packageName: pkg
          , v2Data: { packages: v2.packages, modules: v2.modules, imports: v2.imports }
          , declarationStats: state.declarationStats
          }
          absurd
      Nothing ->
        HH.div
          [ HP.class_ (HH.ClassName "loading") ]
          [ HH.text "Loading module data..." ]

  ModuleOverview pkgName modName ->
    -- Module overview: bubble pack + declaration listing
    case lookupModuleDeclarations state pkgName modName of
      Just decls ->
        HH.slot _moduleOverviewViz unit ModuleOverviewViz.component
          { packageName: pkgName
          , moduleName: modName
          , declarations: decls
          , functionCalls: state.packageCalls
          }
          HandleModuleOverviewOutput
      Nothing ->
        HH.div
          [ HP.class_ (HH.ClassName "loading") ]
          [ HH.text "Loading module declarations..." ]

  DeclarationDetail pkgName modName declName ->
    -- Declaration detail: usage graph + expanded info
    case lookupModuleDeclarations state pkgName modName of
      Just decls ->
        HH.slot _declarationDetailViz unit DeclarationDetailViz.component
          { packageName: pkgName
          , moduleName: modName
          , declarationName: declName
          , declarations: decls
          , knownDeclarations: buildKnownDeclarations state
          }
          HandleDeclarationDetailOutput
      Nothing ->
        HH.div
          [ HP.class_ (HH.ClassName "loading") ]
          [ HH.text "Loading declaration data..." ]

  ModuleSignatureMap pkgName modName ->
    case lookupModuleDeclarations state pkgName modName of
      Just decls ->
        let anns = fromMaybe [] (Map.lookup modName state.moduleAnnotations)
        in HH.slot _moduleSignatureMapViz unit ModuleSignatureMapViz.component
          { packageName: pkgName
          , moduleName: modName
          , declarations: decls
          , annotations: anns
          , functionCalls: state.packageCalls
          }
          HandleModuleSignatureMapOutput
      Nothing ->
        HH.div
          [ HP.class_ (HH.ClassName "loading") ]
          [ HH.text "Loading module declarations..." ]

  TypeClassGrid ->
    case state.typeClassStats of
      Just stats ->
        HH.slot _typeClassGridViz unit TypeClassGridViz.component
          { typeClassStats: stats
          , theme: theme
          }
          absurd
      Nothing ->
        HH.div
          [ HP.class_ (HH.ClassName "loading") ]
          [ HH.text "Loading type class data..." ]

  AnnotationReport ->
    case state.allAnnotations, state.v2Data of
      Just anns, Just v2 ->
        HH.slot _annotationReportViz unit AnnotationReportViz.component
          { annotations: anns, packages: v2.packages, modules: v2.modules
          , moduleDeclarations: state.packageDeclarations
          }
          HandleAnnotationReportOutput
      _, _ ->
        HH.div
          [ HP.class_ (HH.ClassName "loading") ]
          [ HH.text "Loading annotations..." ]

  ProjectManagement ->
    HH.slot _projectManagementViz unit ProjectManagementViz.component
      { projects: state.loadedProjects }
      HandleProjectManagementOutput

  ProjectAnatomy ->
    case state.packageSetData of
      Just psData ->
        HH.slot _projectAnatomyViz unit ProjectAnatomyViz.component
          { packages: psData.packages }
          HandleProjectAnatomyOutput
      Nothing ->
        HH.div
          [ HP.class_ (HH.ClassName "loading") ]
          [ HH.text "Loading package data..." ]

-- =============================================================================
-- Action Handlers
-- =============================================================================

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action Slots Output m Unit
handleAction = case _ of
  Initialize -> do
    log "[SceneCoordinator] Initializing..."

    -- Set up browser history integration
    state <- H.get

    -- Replace current history state with initial scene (so back works from start)
    liftEffect $ replaceHistoryState (sceneToString state.scene) (viewModeToString state.viewMode)

    -- Set up popstate listener for back/forward navigation
    { emitter: historyEmitter, listener: historyListener } <- liftEffect HS.create
    void $ H.subscribe historyEmitter

    cleanup <- liftEffect $ setupPopstateListener \sceneStr viewModeStr -> do
      case sceneFromString sceneStr of
        Just scene -> HS.notify historyListener (HandlePopstate scene (viewModeFromString viewModeStr))
        Nothing -> pure unit

    H.modify_ _ { historyCleanup = Just cleanup }

    log "[SceneCoordinator] Browser history integration enabled"

    -- Set up keyboard listener for reachability peek (hold R)
    { emitter: keyEmitter, listener: keyListener } <- liftEffect HS.create
    void $ H.subscribe keyEmitter

    doc <- liftEffect $ Win.document =<< window
    let docTarget = HTMLDoc.toEventTarget doc

    keydownListener <- liftEffect $ ET.eventListener \e ->
      case KE.fromEvent e of
        Just ke | key ke == "r" && not (repeat ke) ->
          HS.notify keyListener ReachabilityPeekOn
        Just ke | key ke == "p" && not (repeat ke) ->
          HS.notify keyListener PurityPeekOn
        _ -> pure unit

    keyupListener <- liftEffect $ ET.eventListener \e ->
      case KE.fromEvent e of
        Just ke | key ke == "r" ->
          HS.notify keyListener ReachabilityPeekOff
        Just ke | key ke == "p" ->
          HS.notify keyListener PurityPeekOff
        _ -> pure unit

    liftEffect do
      ET.addEventListener (EventType "keydown") keydownListener false docTarget
      ET.addEventListener (EventType "keyup") keyupListener false docTarget

    let kbCleanup = do
          ET.removeEventListener (EventType "keydown") keydownListener false docTarget
          ET.removeEventListener (EventType "keyup") keyupListener false docTarget

    H.modify_ _ { keyboardCleanup = Just kbCleanup }

    log "[SceneCoordinator] Keyboard listener for reachability peek enabled"

    prepareSceneData state

  Receive input -> do
    state <- H.get
    let modelDataChanged = case input.modelData, state.modelData of
          Just new, Just old -> new.moduleCount /= old.moduleCount
          Just _, Nothing -> true
          Nothing, Just _ -> true
          Nothing, Nothing -> false

        packageSetDataChanged = case input.packageSetData, state.packageSetData of
          Just _, Nothing -> true
          Nothing, Just _ -> true
          _, _ -> false

        dataChanged = modelDataChanged || packageSetDataChanged

    H.modify_ _
      { modelData = input.modelData
      , v2Data = input.v2Data
      , packageSetData = input.packageSetData
      }

    when dataChanged do
      log "[SceneCoordinator] Data changed, re-rendering"
      newState <- H.get
      prepareSceneData newState

  NavigateTo targetScene -> do
    state <- H.get
    log $ "[SceneCoordinator] Navigating to: " <> show targetScene

    -- Capture positions before clearing (for hero transitions)
    -- GalaxyTreemap → GalaxyBeeswarm: capture treemap cell positions
    capturedPos <- case state.scene, targetScene of
      GalaxyTreemap, GalaxyBeeswarm ->
        H.request _galaxyTreemapViz unit GalaxyTreemapViz.GetPositions
      _, _ -> pure Nothing

    -- Clear existing viz containers and dismiss any visible tooltips
    liftEffect clearAllVizContainers
    liftEffect clearAllHighlights

    -- SolarSwarm is the "Project Packages" view - always start with ProjectOnly scope
    -- to show only workspace packages by default
    let scopeForScene = case targetScene of
          SolarSwarm -> ProjectOnly
          _ -> state.scope  -- Keep current scope for other scenes

    H.modify_ _
      { scene = targetScene
      , viewMode = PrimaryView  -- Reset view mode on scene change
      , scope = scopeForScene
      , capturedPositions = capturedPos
      , reachabilityData = Nothing  -- Clear stale reachability (package-specific)
      , clusterData = Nothing       -- Clear stale cluster data (package-specific)
      , purityData = Nothing        -- Clear stale purity data (package-specific)
      }

    -- Push to browser history (enables back/forward buttons)
    -- ViewMode resets to PrimaryView on scene change
    liftEffect $ pushHistoryState (sceneToString targetScene) (viewModeToString PrimaryView)

    -- If reachability mode is active, recompute for the target scene
    when (state.colorMode == Reachability) $ case targetScene of
      PkgTreemap pkg -> computeAndStoreReachabilityForPeek pkg
      PkgModuleBeeswarm pkg -> computeAndStoreReachabilityForPeek pkg
      GalaxyTreemap -> computeAndStoreGlobalReachability
      _ -> pure unit

    -- If cluster mode is active and we're entering a package view, recompute
    when (state.colorMode == ClusterView) $ case targetScene of
      PkgTreemap pkg -> computeAndStoreClusters pkg
      PkgModuleBeeswarm pkg -> computeAndStoreClusters pkg
      _ -> pure unit

    H.raise (SceneChanged targetScene)
    newState <- H.get
    prepareSceneData newState

  -- Browser back/forward button navigation
  -- Navigate to the scene without pushing to history (it's already there)
  HandlePopstate targetScene targetViewMode -> do
    state <- H.get
    log $ "[SceneCoordinator] Popstate navigation to: " <> show targetScene <> " viewMode=" <> show targetViewMode

    -- Skip if already at this scene with same viewMode
    when (state.scene /= targetScene || state.viewMode /= targetViewMode) do
      -- Clear existing viz containers and dismiss any visible tooltips
      liftEffect clearAllVizContainers
      liftEffect clearAllHighlights

      -- Determine appropriate scope for the scene
      let scopeForScene = case targetScene of
            SolarSwarm -> ProjectOnly
            _ -> state.scope

      H.modify_ _
        { scene = targetScene
        , viewMode = targetViewMode
        , scope = scopeForScene
        , capturedPositions = Nothing  -- Clear stale positions
        , focalPackage = Nothing  -- Clear focal when navigating via history
        , reachabilityData = Nothing  -- Clear stale reachability (package-specific)
        , clusterData = Nothing       -- Clear stale cluster data (package-specific)
        , purityData = Nothing        -- Clear stale purity data (package-specific)
        }

      -- If reachability mode is active, recompute for the target scene
      when (state.colorMode == Reachability) $ case targetScene of
        PkgTreemap pkg -> computeAndStoreReachabilityForPeek pkg
        PkgModuleBeeswarm pkg -> computeAndStoreReachabilityForPeek pkg
        GalaxyTreemap -> computeAndStoreGlobalReachability
        _ -> pure unit

      H.raise (SceneChanged targetScene)
      newState <- H.get
      prepareSceneData newState

  HandleBubblePackBeeswarmOutput output -> case output of
    BubblePackBeeswarmViz.PackageClicked pkgName -> do
      log $ "[SceneCoordinator] BubblePack package circle clicked: " <> pkgName
      -- Plain click → drill into package (module-level detail)
      handleAction (NavigateTo (PkgTreemap pkgName))
    BubblePackBeeswarmViz.PackageModifierClicked pkgName -> do
      log $ "[SceneCoordinator] BubblePack package modifier+clicked: " <> pkgName
      -- Modifier+click → set focal package (neighborhood filter)
      handleAction (SetFocalPackage (Just pkgName))
    BubblePackBeeswarmViz.PackageLabelClicked pkgName -> do
      log $ "[SceneCoordinator] BubblePack package label clicked: " <> pkgName
      -- Label click → package treemap (module-level detail)
      handleAction (NavigateTo (PkgTreemap pkgName))
    BubblePackBeeswarmViz.PackageHovered mPkgName ->
      H.modify_ _ { hoveredPackage = mPkgName }
    BubblePackBeeswarmViz.ModuleClicked pkgName modName -> do
      log $ "[SceneCoordinator] BubblePack module clicked: " <> pkgName <> "/" <> modName
      -- RETIRED: handleAction (OpenModulePanel pkgName modName)  -- Panel retired, info now in tooltips
      pure unit
    BubblePackBeeswarmViz.ModuleHovered pkgName mModName ->
      -- Track hovered module for coordinated highlighting
      case mModName of
        Just modName ->
          H.modify_ _ { hoveredModule = Just { packageName: pkgName, moduleName: modName } }
        Nothing ->
          H.modify_ _ { hoveredModule = Nothing }

  HandleGalaxyBeeswarmOutput output -> case output of
    GalaxyBeeswarmViz.PackageClicked pkgName -> do
      log $ "[SceneCoordinator] Galaxy package circle clicked: " <> pkgName
      -- Plain click → drill into package (module-level detail)
      handleAction (NavigateTo (PkgTreemap pkgName))
    GalaxyBeeswarmViz.PackageLabelClicked pkgName -> do
      log $ "[SceneCoordinator] Galaxy package label clicked: " <> pkgName
      -- Label click → package treemap (module-level detail)
      handleAction (NavigateTo (PkgTreemap pkgName))
    GalaxyBeeswarmViz.PackageHovered mPkgName ->
      H.modify_ _ { hoveredPackage = mPkgName }

  HandleGalaxyTreemapOutput output -> case output of
    GalaxyTreemapViz.CircleClicked pkgName -> do
      log $ "[SceneCoordinator] GalaxyTreemap circle clicked: " <> pkgName
      handleAction (SetFocalPackage (Just pkgName))
      handleAction (NavigateTo SolarSwarm)
    GalaxyTreemapViz.RectClicked pkgName -> do
      log $ "[SceneCoordinator] GalaxyTreemap rect clicked: " <> pkgName
      handleAction (NavigateTo (PkgTreemap pkgName))
    GalaxyTreemapViz.PackageHovered mPkgName ->
      H.modify_ _ { hoveredPackage = mPkgName }

  HandleModuleTreemapOutput output -> case output of
    ModuleTreemapEnrichedViz.ModuleClicked pkgName modName -> do
      log $ "[SceneCoordinator] Module treemap clicked: " <> pkgName <> "/" <> modName
      handleAction (NavigateTo (ModuleSignatureMap pkgName modName))
    ModuleTreemapEnrichedViz.ModuleHovered _mModName ->
      pure unit  -- Future: coordinated hover
    ModuleTreemapEnrichedViz.DeclarationClicked pkgName modName declName -> do
      log $ "[SceneCoordinator] Declaration clicked in treemap: " <> pkgName <> "/" <> modName <> "/" <> declName
      handleAction (NavigateTo (DeclarationDetail pkgName modName declName))

  HandleModuleOverviewOutput output -> case output of
    ModuleOverviewViz.DeclarationClicked pkgName modName declName -> do
      log $ "[SceneCoordinator] Declaration clicked in overview: " <> declName
      handleAction (NavigateTo (DeclarationDetail pkgName modName declName))
    ModuleOverviewViz.DeclarationHovered _ ->
      pure unit

  HandleModuleSignatureMapOutput output -> case output of
    ModuleSignatureMapViz.DeclarationClicked pkgName modName declName -> do
      log $ "[SceneCoordinator] Declaration clicked in signature map: " <> declName
      handleAction (NavigateTo (DeclarationDetail pkgName modName declName))
    ModuleSignatureMapViz.AnnotationStatusChanged annId newStatus -> do
      log $ "[SceneCoordinator] Annotation " <> show annId <> " -> " <> newStatus
      void $ liftAff $ Loader.patchAnnotationStatus annId newStatus
      -- Optimistically update cached annotations
      state <- H.get
      let updated = map (map (\a -> if a.id == annId then a { status = newStatus } else a))
                        state.moduleAnnotations
      H.modify_ _ { moduleAnnotations = updated }
    ModuleSignatureMapViz.AnnotationReplyCreated reply -> do
      log $ "[SceneCoordinator] Creating reply annotation on " <> reply.targetId <> " supersedes=" <> show reply.supersedes
      result <- liftAff $ Loader.createAnnotation
        { targetType: reply.targetType
        , targetId: reply.targetId
        , kind: reply.kind
        , value: reply.value
        , source: "human"
        , supersedes: Just reply.supersedes
        }
      case result of
        Right newAnn -> do
          state <- H.get
          let modAnns = fromMaybe [] (Map.lookup reply.targetId state.moduleAnnotations)
              updatedAnns = Array.snoc modAnns newAnn
          H.modify_ _ { moduleAnnotations = Map.insert reply.targetId updatedAnns state.moduleAnnotations }
        Left err ->
          log $ "[SceneCoordinator] Failed to create reply: " <> err

  HandleAnnotationReportOutput output -> case output of
    AnnotationReportViz.NavigateToModule pkgName modName -> do
      log $ "[SceneCoordinator] Report navigation to: " <> pkgName <> "/" <> modName
      handleAction (NavigateTo (ModuleSignatureMap pkgName modName))

  HandleProjectManagementOutput output -> case output of
    ProjectManagementViz.ProjectAdded _loadResult -> do
      log "[SceneCoordinator] Project loaded, notifying AppShell"
      H.raise ProjectLoaded
    ProjectManagementViz.NavigateToProject _projectId -> do
      log "[SceneCoordinator] Navigate to loaded project"
      H.raise ProjectLoaded
    ProjectManagementViz.ProjectDeleted _projectId -> do
      log "[SceneCoordinator] Project deleted"
      -- Re-fetch projects list
      result <- liftAff Loader.fetchV2Projects
      case result of
        Right projects -> H.modify_ _ { loadedProjects = projects }
        Left _ -> pure unit

  HandleProjectAnatomyOutput output -> case output of
    ProjectAnatomyViz.PackageClicked pkgName -> do
      log $ "[SceneCoordinator] Anatomy package clicked: " <> pkgName
      handleAction (NavigateTo (PkgTreemap pkgName))

  HandleDeclarationDetailOutput output -> case output of
    DeclarationDetailViz.BackToModuleOverview -> do
      state <- H.get
      case state.scene of
        DeclarationDetail pkg mod _ ->
          handleAction (NavigateTo (ModuleSignatureMap pkg mod))
        _ ->
          pure unit  -- Shouldn't happen
    DeclarationDetailViz.DeclarationClicked pkgName modName declName -> do
      log $ "[SceneCoordinator] Declaration clicked in detail: " <> declName
      handleAction (NavigateTo (DeclarationDetail pkgName modName declName))

  SetScope targetScope -> do
    log $ "[SceneCoordinator] Setting scope: " <> show targetScope
    -- Phase 3: Explicit scope transitions - no auto-escalation
    -- Scope changes just filter in place via GUP
    -- Navigation to SolarSwarm requires explicit "+" button click
    H.modify_ _ { scope = targetScope }

  SetFocalPackage mPkg -> do
    log $ "[SceneCoordinator] Setting focal package: " <> show mPkg
    -- Set focal package for neighborhood filtering in SolarSwarm
    -- When focal is set, the visualization filters to show the focal package
    -- and its dependencies/dependents within the current scope
    H.modify_ _ { focalPackage = mPkg }

  SetViewMode targetMode -> do
    state <- H.get
    log $ "[SceneCoordinator] Setting view mode: " <> show targetMode
    H.modify_ _ { viewMode = targetMode }
    -- Push view mode change to browser history
    liftEffect $ pushHistoryState (sceneToString state.scene) (viewModeToString targetMode)
    -- Re-render the visualization with new mode
    newState <- H.get
    prepareSceneData newState

  ToggleGitMode -> do
    state <- H.get
    if state.colorMode == GitStatus
      then do
        -- Toggle OFF: return to default topo coloring
        log "[SceneCoordinator] Git mode OFF"
        H.modify_ _ { colorMode = FullRegistryTopo }
      else do
        -- Toggle ON: activate git mode and fetch status if needed
        log "[SceneCoordinator] Git mode ON"
        H.modify_ _ { colorMode = GitStatus }
        -- Fetch git status if not already loaded
        when (state.gitStatus == Nothing) do
          log "[SceneCoordinator] Fetching git status..."
          result <- liftAff Loader.fetchGitStatus
          case result of
            Right gitData -> do
              log $ "[SceneCoordinator] Git status: "
                  <> show (Array.length gitData.modified) <> " modified, "
                  <> show (Array.length gitData.staged) <> " staged, "
                  <> show (Array.length gitData.untracked) <> " untracked"
              H.modify_ _ { gitStatus = Just gitData }
            Left err ->
              log $ "[SceneCoordinator] Failed to fetch git status: " <> err

  ToggleTidyMode -> do
    state <- H.get
    let newVal = not state.hideInfraLinks
        threshold = if newVal then 2 else 0
    log $ "[SceneCoordinator] Tidy mode " <> (if newVal then "ON" else "OFF")
        <> ", scene=" <> show state.scene
        <> ", infraLayerThreshold=" <> show threshold
    H.modify_ _ { hideInfraLinks = newVal }
    -- Note: no clearAllHighlights here. All primary views are slot-managed and
    -- handle their own HATS lifecycle. Calling clearAllHighlights from the parent
    -- corrupts global HATS state before the child's deferred Receive can re-render.
    -- Re-render current scene to apply/remove infrastructure link filtering
    newState <- H.get
    prepareSceneData newState

  ToggleReachabilityMode -> do
    state <- H.get
    if state.colorMode == Reachability
      then do
        log "[SceneCoordinator] Reachability mode OFF"
        H.modify_ _ { colorMode = FullRegistryTopo }
      else do
        log "[SceneCoordinator] Reachability mode ON"
        H.modify_ _ { colorMode = Reachability }
        -- Compute reachability for current package (if in a package view)
        case state.scene of
          PkgTreemap pkg -> computeAndStoreReachability pkg
          PkgModuleBeeswarm pkg -> computeAndStoreReachability pkg
          GalaxyTreemap -> computeAndStoreGlobalReachability
          _ -> pure unit
    where
      computeAndStoreReachability pkg = do
        state' <- H.get
        case state'.v2Data of
          Just v2 -> do
            -- Find bundle module for this package (deterministic app detection)
            let bundleMod = Array.find (\p -> p.name == pkg) v2.packages
                              >>= _.bundleModule
                reach = computePackageReachability pkg bundleMod v2.imports v2.modules
                modeLabel = if reach.isApp
                  then case bundleMod of
                    Just m  -> "App reachability from " <> m <> " (explicit)"
                    Nothing -> "App reachability from " <> show (Set.toUnfoldable reach.entryPoints :: Array String) <> " (heuristic)"
                  else "Library reachability"
            log $ "[SceneCoordinator] " <> modeLabel <> " for " <> pkg <> ": "
                <> show (Set.size reach.reachable) <> " reachable, "
                <> show (Set.size reach.entryPoints) <> " entry points"
                <> " (allImports=" <> show (Array.length v2.imports) <> ", allModules=" <> show (Array.length v2.modules) <> ")"
            log $ "[SceneCoordinator]   entry points: " <> show (Set.toUnfoldable reach.entryPoints :: Array String)
            log $ "[SceneCoordinator]   unreachable: " <> show (Array.filter (\m -> m.package.name == pkg && not (Set.member m.name reach.reachable)) v2.modules <#> _.name)
            H.modify_ _ { reachabilityData = Just reach }
          Nothing -> pure unit

  ToggleClusterMode -> do
    state <- H.get
    if state.colorMode == ClusterView
      then do
        log "[SceneCoordinator] Cluster mode OFF"
        H.modify_ _ { colorMode = FullRegistryTopo }
      else do
        log "[SceneCoordinator] Cluster mode ON"
        H.modify_ _ { colorMode = ClusterView }
        -- Compute clusters for current package (if in a package view)
        case state.scene of
          PkgTreemap pkg -> computeAndStoreClusters pkg
          PkgModuleBeeswarm pkg -> computeAndStoreClusters pkg
          _ -> pure unit

  ReachabilityPeekOn -> do
    state <- H.get
    -- Only activate peek when not typing in search
    when (not state.searchOpen) do
      H.modify_ _ { reachabilityPeek = true }
      -- Compute reachability if not cached
      when (state.reachabilityData == Nothing) $ case state.scene of
        PkgTreemap pkg -> computeAndStoreReachabilityForPeek pkg
        PkgModuleBeeswarm pkg -> computeAndStoreReachabilityForPeek pkg
        GalaxyTreemap -> computeAndStoreGlobalReachability
        _ -> pure unit

  ReachabilityPeekOff -> do
    H.modify_ _ { reachabilityPeek = false }

  PurityPeekOn -> do
    state <- H.get
    when (not state.searchOpen) do
      H.modify_ _ { purityPeek = true }
      when (state.purityData == Nothing) $ case state.scene of
        PkgTreemap pkg -> computeAndStorePurityForPeek pkg
        PkgModuleBeeswarm pkg -> computeAndStorePurityForPeek pkg
        _ -> pure unit

  PurityPeekOff -> do
    H.modify_ _ { purityPeek = false }

  -- =========================================================================
  -- Search Typeahead Actions
  -- =========================================================================

  SearchInput query -> do
    state <- H.get
    let seqId = state.searchSeqId + 1
    if String.length query < 2
      then
        H.modify_ _ { searchQuery = query, searchResults = [], searchOpen = false, searchSeqId = seqId }
      else do
        H.modify_ _ { searchQuery = query, searchOpen = true, searchSelectedIndex = 0, searchSeqId = seqId }
        -- Fork async search with simple debounce: delay then check if seqId still matches
        void $ H.fork do
          liftAff $ Aff.delay (Milliseconds 150.0)
          currentState <- H.get
          when (currentState.searchSeqId == seqId) do
            result <- liftAff $ Loader.searchAll query
            case result of
              Right results ->
                -- Only apply if seqId still matches (user hasn't typed more)
                H.modify_ _ { searchResults = results, searchSelectedIndex = 0 }
              Left err ->
                log $ "[SceneCoordinator] Search error: " <> err

  SearchResultsReceived seqId results -> do
    state <- H.get
    when (state.searchSeqId == seqId) do
      H.modify_ _ { searchResults = results, searchSelectedIndex = 0 }

  SearchKeyDown evt -> do
    state <- H.get
    case key evt of
      "ArrowDown" -> do
        liftEffect $ WE.preventDefault (toEvent evt)
        let maxIdx = Array.length state.searchResults - 1
            newIdx = min maxIdx (state.searchSelectedIndex + 1)
        H.modify_ _ { searchSelectedIndex = newIdx }
      "ArrowUp" -> do
        liftEffect $ WE.preventDefault (toEvent evt)
        let newIdx = max 0 (state.searchSelectedIndex - 1)
        H.modify_ _ { searchSelectedIndex = newIdx }
      "Enter" -> do
        liftEffect $ WE.preventDefault (toEvent evt)
        confirmSearchSelection state state.searchSelectedIndex
      "Escape" -> do
        H.modify_ _ { searchQuery = "", searchResults = [], searchOpen = false }
      _ -> pure unit

  SearchConfirmIndex idx -> do
    state <- H.get
    confirmSearchSelection state idx

  SearchDismiss -> do
    -- Small delay to allow mousedown events on results to fire first
    void $ H.fork do
      liftAff $ Aff.delay (Milliseconds 200.0)
      H.modify_ _ { searchOpen = false }

  HandleSlideOutPanelOutput output -> case output of
    SlideOutPanel.PanelClosed -> do
      log "[SceneCoordinator] Panel closed"
      H.modify_ _ { panelOpen = false }
    SlideOutPanel.NavigateToModule pkgName modName -> do
      log $ "[SceneCoordinator] Panel navigation to module: " <> pkgName <> "/" <> modName
      -- Load the module's declarations in the panel
      handleAction (OpenModulePanel pkgName modName)
    SlideOutPanel.NavigateToPackage pkgName -> do
      log $ "[SceneCoordinator] Panel navigation to package: " <> pkgName
      -- Future: navigate to package

  OpenModulePanel pkgName modName -> do
    log $ "[SceneCoordinator] Opening panel for module: " <> pkgName <> "/" <> modName
    -- Open the panel with loading state first
    let loadingContent = SlideOutPanel.ModuleLoading
          { moduleName: modName
          , packageName: pkgName
          }
    -- Track panel state in coordinator
    H.modify_ _ { panelOpen = true, panelContent = loadingContent }
    void $ H.tell _slideOutPanel unit (SlideOutPanel.Open loadingContent)
    -- Look up module info from v2Data
    state <- H.get
    let mModuleInfo = do
          v2 <- state.v2Data
          mod <- Array.find (\m -> m.name == modName && m.package.name == pkgName) v2.modules
          pure { id: mod.id, version: mod.package.version, source: mod.package.source }
    case mModuleInfo of
      Nothing -> do
        log $ "[SceneCoordinator] Module not found in v2Data: " <> pkgName <> "/" <> modName
        -- Show empty declarations
        let emptyContent = SlideOutPanel.ModuleDeclarations
              { moduleName: modName
              , packageName: pkgName
              , packageVersion: Nothing
              , pursuitUrl: Nothing
              , declarations: []
              }
        H.modify_ _ { panelContent = emptyContent }
        void $ H.tell _slideOutPanel unit (SlideOutPanel.SetContent emptyContent)
      Just { id: moduleId, version, source } -> do
        -- Construct Pursuit URL for registry packages
        let pursuitUrl = if source == "registry"
              then Just $ "https://pursuit.purescript.org/packages/purescript-"
                       <> pkgName <> "/" <> version <> "/docs/" <> modName
              else Nothing
        -- Fetch declarations from API
        result <- liftAff $ Loader.fetchV2ModuleDeclarations moduleId
        case result of
          Left err -> do
            log $ "[SceneCoordinator] Failed to fetch declarations: " <> err
            -- Show empty declarations on failure
            let emptyContent = SlideOutPanel.ModuleDeclarations
                  { moduleName: modName
                  , packageName: pkgName
                  , packageVersion: Just version
                  , pursuitUrl
                  , declarations: []
                  }
            H.modify_ _ { panelContent = emptyContent }
            void $ H.tell _slideOutPanel unit (SlideOutPanel.SetContent emptyContent)
          Right decls -> do
            log $ "[SceneCoordinator] Loaded " <> show (Array.length decls) <> " declarations for " <> modName
            -- Convert V2Declaration to SlideOutPanel.Declaration
            let panelDecls = map (\d ->
                  { name: d.name
                  , kind: d.kind
                  , typeSignature: d.typeSignature
                  , comments: d.comments
                  }) decls
            let content = SlideOutPanel.ModuleDeclarations
                  { moduleName: modName
                  , packageName: pkgName
                  , packageVersion: Just version
                  , pursuitUrl
                  , declarations: panelDecls
                  }
            H.modify_ _ { panelContent = content }
            void $ H.tell _slideOutPanel unit (SlideOutPanel.SetContent content)

  OpenPackagePanel pkgName -> do
    log $ "[SceneCoordinator] Opening panel for package: " <> pkgName
    -- Find first module in this package and open panel for it
    state <- H.get
    case state.v2Data of
      Nothing -> do
        log "[SceneCoordinator] No v2Data for package panel"
      Just v2 -> do
        let pkgModules = Array.filter (\m -> m.package.name == pkgName) v2.modules
        case Array.head pkgModules of
          Nothing -> do
            log $ "[SceneCoordinator] No modules found in package: " <> pkgName
          Just firstMod -> do
            log $ "[SceneCoordinator] Opening first module: " <> firstMod.name
            handleAction (OpenModulePanel pkgName firstMod.name)

-- | Prepare data for the current scene
prepareSceneData :: forall m. MonadAff m => State -> H.HalogenM State Action Slots Output m Unit
prepareSceneData state = case state.scene of
  GalaxyTreemap -> do
    case state.packageSetData of
      Just _ ->
        log "[SceneCoordinator] GalaxyTreemap: data available, slot will render"
      Nothing -> do
        log "[SceneCoordinator] Requesting package set data"
        H.raise RequestPackageSetData

  GalaxyBeeswarm -> do
    case state.packageSetData of
      Just _ ->
        log "[SceneCoordinator] GalaxyBeeswarm: data available, slot will render"
      Nothing -> do
        log "[SceneCoordinator] Requesting package set data"
        H.raise RequestPackageSetData

  SolarSwarm -> do
    case state.viewMode of
      PrimaryView ->
        -- BubblePack is handled by the Halogen slot
        case state.modelData of
          Just model ->
            log $ "[SceneCoordinator] SolarSwarm (BubblePack): "
                <> show model.packageCount <> " packages, "
                <> show model.moduleCount <> " modules"
                <> ", scope=" <> show state.scope
          Nothing ->
            log "[SceneCoordinator] No modelData for SolarSwarm"

      ChordView ->
        log "[SceneCoordinator] SolarSwarm ChordView: rendering handled by slot"

      MatrixView ->
        log "[SceneCoordinator] SolarSwarm MatrixView: rendering handled by slot"

  PkgTreemap pkgName -> do
    case state.v2Data of
      Just v2 -> do
        let pkgModules = Array.filter (\m -> m.package.name == pkgName) v2.modules

        log $ "[SceneCoordinator] PkgTreemap (" <> show state.viewMode <> "): "
            <> pkgName <> " - " <> show (Array.length pkgModules) <> " modules"

        case state.viewMode of
          PrimaryView -> do
            -- Enriched treemap needs full declarations for bubble packs
            -- Check if we already have declarations for this package's modules
            let missingDeclModules = Array.filter (\m -> not (Map.member m.id state.packageDeclarations)) pkgModules

            -- Fetch declarations if missing (per-package, parallel)
            when (Array.length missingDeclModules > 0) do
              log $ "[SceneCoordinator] Fetching declarations for " <> show (Array.length missingDeclModules) <> " modules"
              newDecls <- liftAff $ Loader.fetchV2PackageDeclarations missingDeclModules
              let merged = Map.union newDecls state.packageDeclarations
              H.modify_ _ { packageDeclarations = merged }

            -- Fetch ALL function calls once via bulk endpoint (for declaration-level dependency highlighting)
            when (not state.allCallsLoaded) do
              log "[SceneCoordinator] Fetching all function calls (bulk endpoint)"
              result <- liftAff Loader.fetchV2AllCalls
              case result of
                Right allCalls -> do
                  log $ "[SceneCoordinator] Loaded function calls for " <> show (Array.length allCalls) <> " modules"
                  -- Convert Array V2ModuleCalls to Map Int (Array V2FunctionCall)
                  let callsMap = Map.fromFoldable $ allCalls <#> \mc ->
                        Tuple mc.moduleId (mc.calls <#> \c ->
                          { callerName: c.callerName
                          , calleeModule: c.calleeModule
                          , calleeName: c.calleeName
                          , isCrossModule: c.isCrossModule
                          , callCount: c.callCount
                          })
                  H.modify_ _ { packageCalls = callsMap, allCallsLoaded = true }
                Left err ->
                  log $ "[SceneCoordinator] Failed to fetch function calls: " <> err

            log "[SceneCoordinator] PrimaryView (Enriched Treemap): rendering handled by slot"

          ChordView ->
            log "[SceneCoordinator] PkgTreemap ChordView: rendering handled by slot"

          MatrixView ->
            log "[SceneCoordinator] PkgTreemap MatrixView: rendering handled by slot"

      Nothing ->
        log "[SceneCoordinator] No v2Data for PkgTreemap"

  ModuleOverview pkgName _modName -> do
    -- Ensure declarations are loaded for this package
    ensurePackageDeclarationsLoaded state pkgName
    log "[SceneCoordinator] ModuleOverview: rendering handled by slot"

  DeclarationDetail pkgName _modName _declName -> do
    -- Ensure declarations are loaded for this package
    ensurePackageDeclarationsLoaded state pkgName
    log "[SceneCoordinator] DeclarationDetail: rendering handled by slot"

  ModuleSignatureMap pkgName modName -> do
    -- Ensure declarations are loaded for this package
    ensurePackageDeclarationsLoaded state pkgName
    -- Fetch annotations for this module if not cached
    when (not $ Map.member modName state.moduleAnnotations) do
      result <- liftAff $ Loader.fetchModuleAnnotations modName
      case result of
        Right anns -> H.modify_ _ { moduleAnnotations = Map.insert modName anns state.moduleAnnotations }
        Left _err -> pure unit  -- Annotations are optional; silent fail
    log "[SceneCoordinator] ModuleSignatureMap: rendering handled by slot"

  PkgModuleBeeswarm pkgName -> do
    case state.v2Data of
      Just v2 -> do
        -- Fetch declaration stats if not cached (needed for bubblepack overlay)
        let pkgModules = Array.filter (\m -> m.package.name == pkgName) v2.modules
            moduleCount = Array.length pkgModules
            isSmallPackage = moduleCount < smallPackageThreshold
        log $ "[SceneCoordinator] PkgModuleBeeswarm: " <> pkgName
            <> " (" <> show moduleCount <> " modules), slot will render"
        when (isSmallPackage && state.declarationStats == Nothing) do
          log "[SceneCoordinator] Fetching declaration stats for bubblepack view"
          result <- liftAff Loader.fetchV2ModuleDeclarationStats
          case result of
            Right statsArray -> do
              let stats = Map.fromFoldable $ statsArray <#> \s -> Tuple s.moduleId s
              H.modify_ _ { declarationStats = Just stats }
            Left err ->
              log $ "[SceneCoordinator] Failed to fetch declaration stats: " <> err
      Nothing ->
        log "[SceneCoordinator] No v2Data for PkgModuleBeeswarm"

  TypeClassGrid -> do
    log "[SceneCoordinator] TypeClassGrid"
    case state.typeClassStats of
      Just stats ->
        log $ "[SceneCoordinator] TypeClassGrid: " <> show stats.count <> " classes, slot will render"
      Nothing -> do
        log "[SceneCoordinator] Loading type class stats..."
        result <- liftAff Loader.fetchTypeClassStats
        case result of
          Right stats -> do
            log $ "[SceneCoordinator] Loaded " <> show stats.count <> " type classes"
            H.modify_ _ { typeClassStats = Just stats }
          Left err ->
            log $ "[SceneCoordinator] Failed to load type class stats: " <> err

  AnnotationReport -> do
    case state.allAnnotations of
      Just _ -> log "[SceneCoordinator] AnnotationReport: data cached"
      Nothing -> do
        log "[SceneCoordinator] Loading all annotations..."
        result <- liftAff Loader.fetchAllAnnotations
        case result of
          Right anns -> do
            log $ "[SceneCoordinator] Loaded " <> show (Array.length anns) <> " annotations"
            H.modify_ _ { allAnnotations = Just anns }
          Left err ->
            log $ "[SceneCoordinator] Failed to load annotations: " <> err
    -- Fork declarations fetch so it doesn't block rendering (Halogen re-renders on completion)
    _ <- H.fork do
      st <- H.get
      case st.v2Data, st.allAnnotations of
        Just v2, Just anns -> do
          -- Only fetch declarations for modules that appear in annotations
          let annotatedModuleNames = Set.fromFoldable $ anns <#> _.targetId
              annotatedModules = Array.filter (\m -> Set.member m.name annotatedModuleNames) v2.modules
              missingDeclModules = Array.filter (\m -> not (Map.member m.id st.packageDeclarations)) annotatedModules
          when (Array.length missingDeclModules > 0) do
            log $ "[SceneCoordinator] Fetching declarations for " <> show (Array.length missingDeclModules) <> " annotated modules (bubblepacks)"
            newDecls <- liftAff $ Loader.fetchV2PackageDeclarations missingDeclModules
            st2 <- H.get
            H.modify_ _ { packageDeclarations = Map.union newDecls st2.packageDeclarations }
        _, _ -> pure unit
    pure unit

  ProjectManagement -> do
    log "[SceneCoordinator] ProjectManagement: fetching projects list"
    result <- liftAff Loader.fetchV2Projects
    case result of
      Right projects -> do
        log $ "[SceneCoordinator] Loaded " <> show (Array.length projects) <> " projects"
        H.modify_ _ { loadedProjects = projects }
      Left err ->
        log $ "[SceneCoordinator] Failed to load projects: " <> err

  ProjectAnatomy -> do
    -- Package set data is already loaded in DataLoaded phase — no extra fetch needed
    case state.packageSetData of
      Just psData ->
        log $ "[SceneCoordinator] ProjectAnatomy: " <> show (Array.length psData.packages) <> " packages available"
      Nothing -> do
        log "[SceneCoordinator] ProjectAnatomy: requesting package set data"
        H.raise RequestPackageSetData

-- =============================================================================
-- Query Handlers
-- =============================================================================

handleQuery :: forall m a. MonadAff m => Query a -> H.HalogenM State Action Slots Output m (Maybe a)
handleQuery = case _ of
  SetScene targetScene a -> do
    handleAction (NavigateTo targetScene)
    pure (Just a)

-- =============================================================================
-- Rendering Helpers
-- =============================================================================

-- | Clear all visualization containers (prevents stale SVGs when switching scenes)
clearAllVizContainers :: Effect Unit
clearAllVizContainers = do
  clearContainer "#galaxy-beeswarm-container"
  clearContainer (C.bubblePackBeeswarmContainer)
  clearContainer "#pkg-treemap-container"
  clearContainer "#circlepack-container"

-- | Compute the scoped packages for SolarSwarm chord/matrix views
-- | Respects focalPackage: when set, filters to the focal neighborhood
-- | (focal + deps + dependents), matching what BubblePackBeeswarm shows
solarSwarmScopedPackages :: State -> Array Loader.V2Package -> Array Loader.V2Package
solarSwarmScopedPackages state allPackages =
  let
    projectPkgNames = Set.fromFoldable projectPackages
    projectPkgs = Array.filter (\p -> Set.member p.name projectPkgNames) allPackages
  in case state.focalPackage of
    Nothing -> projectPkgs
    Just focalName ->
      let
        -- Find the focal package's direct dependencies
        focalDeps = case Array.find (\p -> p.name == focalName) allPackages of
          Just pkg -> Set.fromFoldable pkg.depends
          Nothing -> Set.empty
        -- Find packages that depend on the focal package (reverse deps)
        dependents = Set.fromFoldable $
          Array.mapMaybe
            (\pkg -> if Array.elem focalName pkg.depends then Just pkg.name else Nothing)
            allPackages
        -- Complete neighborhood: focal + deps + dependents
        neighborhood = Set.insert focalName (Set.union focalDeps dependents)
      in
        Array.filter (\p -> Set.member p.name neighborhood) projectPkgs

-- | Compute package-level git status from module-level status
-- | Maps module names to their containing packages using v2Data
computePackageGitStatus :: Maybe Loader.GitStatusData -> Maybe V2Data -> Maybe PackageGitStatus
computePackageGitStatus mGitStatus mV2Data = do
  gitStatus <- mGitStatus
  v2 <- mV2Data
  -- Build module name → package name map
  let moduleToPackage :: Map String String
      moduleToPackage = Map.fromFoldable $ v2.modules <#> \m -> Tuple m.name m.package.name
      -- Helper to find packages for a list of module names
      findPackages :: Array String -> Set String
      findPackages modNames = Set.fromFoldable $ Array.catMaybes $
        modNames <#> \modName -> Map.lookup modName moduleToPackage
  pure
    { packagesWithModified: findPackages gitStatus.modified
    , packagesWithStaged: findPackages gitStatus.staged
    , packagesWithUntracked: findPackages gitStatus.untracked
    }

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
  SolarSwarm -> BlueprintTheme
  PkgTreemap _ -> SteelTheme
  PkgModuleBeeswarm _ -> SteelTheme
  ModuleOverview _ _ -> MistTheme
  DeclarationDetail _ _ _ -> DaylightTheme
  ModuleSignatureMap _ _ -> MistTheme
  TypeClassGrid -> MidnightTheme
  AnnotationReport -> DaylightTheme
  ProjectManagement -> DaylightTheme
  ProjectAnatomy -> DaylightTheme

-- | Canonical state code for precise communication
-- | See docs/kb/reference/ce2-state-machine-analysis.md for full naming system
-- |
-- | Galaxy level:  A (Treemap), B (Beeswarm)
-- | Solar level:   C + scope digit + optional focal + view suffix
-- |   Scopes: 0=All, 1=Trans, 2=Deps, 3=Proj
-- |   Focal: (pkgName) when focal package set
-- |   Views: (none)=Primary, M=Matrix, C=Chord
-- | Module level:  E(pkg) + view suffix, F(pkg)=Beeswarm
-- | Panel: +P suffix when open
-- | Note: D (PkgNeighborhood) was merged into C with focalPackage
canonicalStateCode :: State -> String
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

  TypeClassGrid -> "T"       -- Type class grid view

  AnnotationReport -> "R"    -- Annotation report view
  ProjectManagement -> "P"   -- Project management view
  ProjectAnatomy -> "Y"      -- Project anatomy view

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

-- =============================================================================
-- Module/Declaration Lookup Helpers
-- =============================================================================

-- | Look up declarations for a module within a package
lookupModuleDeclarations :: State -> String -> String -> Maybe (Array Loader.V2Declaration)
lookupModuleDeclarations state pkgName modName = do
  v2 <- state.v2Data
  mod <- Array.find (\m -> m.name == modName && m.package.name == pkgName) v2.modules
  Map.lookup mod.id state.packageDeclarations

-- | Build cross-reference index of all loaded declarations for source code navigation
buildKnownDeclarations :: State -> Array SourceCode.KnownDeclaration
buildKnownDeclarations state =
  case state.v2Data of
    Nothing -> []
    Just v2 ->
      let
        -- Build module ID → { moduleName, packageName } lookup
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

-- | Ensure declarations are loaded for a package's modules
ensurePackageDeclarationsLoaded :: forall m. MonadAff m => State -> String -> H.HalogenM State Action Slots Output m Unit
ensurePackageDeclarationsLoaded state pkgName =
  case state.v2Data of
    Just v2 -> do
      let pkgModules = Array.filter (\m -> m.package.name == pkgName) v2.modules
          missingDeclModules = Array.filter (\m -> not (Map.member m.id state.packageDeclarations)) pkgModules

      when (Array.length missingDeclModules > 0) do
        log $ "[SceneCoordinator] Fetching declarations for " <> show (Array.length missingDeclModules) <> " modules in " <> pkgName
        newDecls <- liftAff $ Loader.fetchV2PackageDeclarations missingDeclModules
        currentState <- H.get
        let merged = Map.union newDecls currentState.packageDeclarations
        H.modify_ _ { packageDeclarations = merged }

      -- Also ensure function calls are loaded
      when (not state.allCallsLoaded) do
        log "[SceneCoordinator] Fetching all function calls (bulk endpoint)"
        result <- liftAff Loader.fetchV2AllCalls
        case result of
          Right allCalls -> do
            log $ "[SceneCoordinator] Loaded function calls for " <> show (Array.length allCalls) <> " modules"
            let callsMap = Map.fromFoldable $ allCalls <#> \mc ->
                  Tuple mc.moduleId (mc.calls <#> \c ->
                    { callerName: c.callerName
                    , calleeModule: c.calleeModule
                    , calleeName: c.calleeName
                    , isCrossModule: c.isCrossModule
                    , callCount: c.callCount
                    })
            H.modify_ _ { packageCalls = callsMap, allCallsLoaded = true }
          Left err ->
            log $ "[SceneCoordinator] Failed to fetch function calls: " <> err
    Nothing ->
      log "[SceneCoordinator] No v2Data available for declaration loading"

-- =============================================================================
-- Search Helpers
-- =============================================================================

-- | Confirm selection of a search result and navigate
confirmSearchSelection :: forall m. MonadAff m => State -> Int -> H.HalogenM State Action Slots Output m Unit
confirmSearchSelection state idx =
  case Array.index state.searchResults idx of
    Nothing -> pure unit
    Just result -> do
      let targetScene = sceneForResult result
      log $ "[SceneCoordinator] Search navigation to: " <> show targetScene
      H.modify_ _ { searchQuery = "", searchResults = [], searchOpen = false }
      handleAction (NavigateTo targetScene)

-- | Derive target scene from a search result
sceneForResult :: Loader.UnifiedSearchResult -> Scene
sceneForResult r = case r.entityType of
  "package" -> PkgTreemap r.packageName
  "module" -> ModuleSignatureMap r.packageName (fromMaybe r.name r.moduleName)
  "declaration" -> DeclarationDetail r.packageName (fromMaybe "" r.moduleName) r.name
  _ -> GalaxyTreemap

-- =============================================================================
-- Module Import Maps (for coordinated hover highlighting)
-- =============================================================================

-- | Build a map from module name to the modules it imports
buildModuleImportMap :: Array Loader.V2ModuleImports -> Map String (Array String)
buildModuleImportMap imports =
  Map.fromFoldable $ imports <#> \imp -> Tuple imp.moduleName imp.imports

-- | Build a reverse map: module name to modules that import it
buildModuleImportedByMap :: Array Loader.V2ModuleImports -> Map String (Array String)
buildModuleImportedByMap imports =
  let
    -- For each import relationship, record the reverse
    pairs :: Array (Tuple String String)
    pairs = Array.concatMap (\imp ->
        imp.imports <#> \imported -> Tuple imported imp.moduleName
      ) imports
  in
    -- Group by imported module
    foldl (\acc (Tuple imported importer) ->
      Map.alter (Just <<< Array.cons importer <<< fromMaybe []) imported acc
    ) Map.empty pairs

-- =============================================================================
-- Package Reachability Computation
-- =============================================================================

-- | Compute which modules in a package are reachable
-- | Two modes, determined by bundleModule:
-- |   - Library mode (Nothing): entry points are modules imported by external packages
-- |   - App mode (Just mainMod): entry point is the bundle module (e.g. CE2.Main)
-- | Uses `reachableFrom` from Data.Graph.Algorithms for BFS traversal
computePackageReachability
  :: String                         -- target package name
  -> Maybe String                   -- bundle module (Just for apps, Nothing for libraries)
  -> Array Loader.V2ModuleImports   -- all imports
  -> Array Loader.V2ModuleListItem  -- all modules (with package info)
  -> PackageReachability
computePackageReachability targetPkg bundleModule allImports allModules =
  let
    -- Module name → package name
    modToPkg :: Map String String
    modToPkg = Map.fromFoldable $ allModules <#> \m -> Tuple m.name m.package.name

    -- Modules in target package
    targetMods :: Set String
    targetMods = Set.fromFoldable $
      Array.filter (\m -> m.package.name == targetPkg) allModules <#> _.name

    -- Entry module comes directly from bundleModule in the database (e.g. "CE2.Main")

    -- Forward import map: module → Set of what it imports
    importsOf :: Map String (Set String)
    importsOf = Map.fromFoldable $
      allImports <#> \imp -> Tuple imp.moduleName (Set.fromFoldable imp.imports)

    -- Build import graph restricted to target package
    internalGraph =
      { nodes: Array.fromFoldable targetMods
      , edges: Map.fromFoldable $ (Array.fromFoldable targetMods) <#> \mod ->
          Tuple mod (Set.intersection (fromMaybe Set.empty (Map.lookup mod importsOf)) targetMods)
      }

    -- Entry points depend on mode
    entryPoints = case bundleModule of
      Just mainMod | Set.member mainMod targetMods ->
        Set.singleton mainMod  -- App mode: Main is the sole entry point
      _ ->
        -- Library mode: target modules imported by external modules
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

    -- Track whether we resolved as an app (for callers to know)
    isApp = case bundleModule of
              Just m -> Set.member m targetMods
              Nothing -> false

    -- BFS using library function, union results from all entry points
    reachable = Set.unions $
      (Array.fromFoldable entryPoints) <#> \ep -> GraphAlgo.reachableFrom ep internalGraph
  in
    { reachable, entryPoints, packageName: targetPkg, isApp }

-- | Compute global reachability: which modules across ALL packages are
-- | transitively reachable from the app entry point (first package with bundleModule).
-- | Returns PackageReachability with packageName: "*" for galaxy-level coloring.
computeGlobalReachability
  :: Array Loader.V2ModuleImports
  -> Array Loader.V2ModuleListItem
  -> Array Loader.V2Package
  -> PackageReachability
computeGlobalReachability allImports allModules allPackages =
  let
    -- Find entry point: first package with a bundleModule
    mEntry = Array.findMap (\p -> p.bundleModule) allPackages

    -- All module names
    allModNames :: Set String
    allModNames = Set.fromFoldable $ allModules <#> _.name

    -- Global forward-import graph (no package restriction)
    globalGraph =
      { nodes: Array.fromFoldable allModNames
      , edges: Map.fromFoldable $
          allImports <#> \imp -> Tuple imp.moduleName (Set.fromFoldable imp.imports)
      }

    -- Entry points and BFS
    entryPoints = case mEntry of
      Just mainMod | Set.member mainMod allModNames -> Set.singleton mainMod
      _ -> Set.empty

    reachable = Set.unions $
      (Array.fromFoldable entryPoints) <#> \ep -> GraphAlgo.reachableFrom ep globalGraph
  in
    { reachable, entryPoints, packageName: "*", isApp: true }

-- =============================================================================
-- Package Cluster Computation
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
    -- Modules in target package
    targetMods :: Set String
    targetMods = Set.fromFoldable $
      Array.filter (\m -> m.package.name == targetPkg) allModules <#> _.name

    -- Forward import map: module → Set of what it imports (within package)
    importsOf :: Map String (Set String)
    importsOf = Map.fromFoldable $
      allImports <#> \imp -> Tuple imp.moduleName (Set.fromFoldable imp.imports)

    -- Build import graph restricted to target package (bidirectional for undirected clustering)
    -- Make edges symmetric: if A imports B, both A→B and B→A are edges
    forwardEdges = Map.fromFoldable $ (Array.fromFoldable targetMods) <#> \mod ->
      Tuple mod (Set.intersection (fromMaybe Set.empty (Map.lookup mod importsOf)) targetMods)

    -- Build reverse edges
    reverseEdges = foldl (\acc (Tuple from targets) ->
      foldl (\acc' to ->
        Map.alter (\mSet -> Just (Set.insert from (fromMaybe Set.empty mSet))) to acc'
      ) acc (Array.fromFoldable targets)
    ) (Map.empty :: Map String (Set String)) (Map.toUnfoldable forwardEdges :: Array (Tuple String (Set String)))

    -- Merge forward and reverse for undirected graph
    symmetricEdges = Map.unionWith Set.union forwardEdges reverseEdges

    internalGraph =
      { nodes: Array.fromFoldable targetMods
      , edges: symmetricEdges
      }

    -- Connected components
    clusters = GraphAlgo.connectedComponents internalGraph

    -- Label propagation for finer communities
    communityLabels = GraphAlgo.labelPropagation internalGraph

    -- Convert community labels (Map String String) to (Map String Int)
    -- by assigning each unique label a numeric index
    uniqueLabels = Set.fromFoldable $ Map.values communityLabels
    labelToIdx = Map.fromFoldable $ Array.mapWithIndex (\i label -> Tuple label i) (Array.fromFoldable uniqueLabels)
    communities = Map.mapMaybe (\label -> Map.lookup label labelToIdx) communityLabels
  in
    { clusters, communities, packageName: targetPkg }

-- | Helper: compute and store clusters for a package (used in action handlers)
computeAndStoreClusters :: forall m. MonadAff m => String -> H.HalogenM State Action Slots Output m Unit
computeAndStoreClusters pkg = do
  state <- H.get
  case state.v2Data of
    Just v2 -> do
      let clusters = computePackageClusters pkg v2.imports v2.modules
      log $ "[SceneCoordinator] Clusters for " <> pkg <> ": "
          <> show (Array.length clusters.clusters) <> " components, "
          <> show (Map.size clusters.communities) <> " community assignments"
      H.modify_ _ { clusterData = Just clusters }
    Nothing -> pure unit

-- | Helper: compute and store reachability for peek (reuses existing logic)
computeAndStoreReachabilityForPeek :: forall m. MonadAff m => String -> H.HalogenM State Action Slots Output m Unit
computeAndStoreReachabilityForPeek pkg = do
  state <- H.get
  case state.v2Data of
    Just v2 -> do
      let bundleMod = Array.find (\p -> p.name == pkg) v2.packages
                        >>= _.bundleModule
          reach = computePackageReachability pkg bundleMod v2.imports v2.modules
          modeLabel = if reach.isApp
            then case bundleMod of
              Just m  -> "App reachability from " <> m <> " (explicit)"
              Nothing -> "App reachability from " <> show (Set.toUnfoldable reach.entryPoints :: Array String) <> " (heuristic)"
            else "Library reachability"
      log $ "[SceneCoordinator] " <> modeLabel <> " for " <> pkg <> ": "
          <> show (Set.size reach.reachable) <> " reachable, "
          <> show (Set.size reach.entryPoints) <> " entry points"
      H.modify_ _ { reachabilityData = Just reach }
    Nothing -> pure unit

-- | Helper: compute and store global reachability (galaxy-level, all packages)
computeAndStoreGlobalReachability :: forall m. MonadAff m => H.HalogenM State Action Slots Output m Unit
computeAndStoreGlobalReachability = do
  state <- H.get
  case state.v2Data of
    Just v2 -> do
      let reach = computeGlobalReachability v2.imports v2.modules v2.packages
      log $ "[SceneCoordinator] Global reachability: "
          <> show (Set.size reach.reachable) <> " reachable, "
          <> show (Set.size reach.entryPoints) <> " entry points"
      H.modify_ _ { reachabilityData = Just reach }
    Nothing -> pure unit

-- | Helper: compute and store purity data for peek overlay
computeAndStorePurityForPeek :: forall m. MonadAff m => String -> H.HalogenM State Action Slots Output m Unit
computeAndStorePurityForPeek pkg = do
  state <- H.get
  case state.v2Data of
    Just v2 -> do
      let pkgModules = Array.filter (\m -> m.package.name == pkg) v2.modules
          modulePurity = Map.fromFoldable $ pkgModules <#> \m ->
            let
              decls = fromMaybe [] $ Map.lookup m.id state.packageDeclarations
              valueDecls = Array.filter (\d -> d.kind == "value" && d.typeSignature /= Nothing) decls
              effectfulCount = Array.length $ Array.filter (\d -> ArcDiagram.isEffectful d.typeSignature) valueDecls
              totalCount = Array.length valueDecls
            in Tuple m.name { effectfulCount, totalCount }
          purity = { modulePurity, packageName: pkg }
      log $ "[SceneCoordinator] Purity for " <> pkg <> ": "
          <> show (Map.size modulePurity) <> " modules analyzed"
      H.modify_ _ { purityData = Just purity }
    Nothing -> pure unit
