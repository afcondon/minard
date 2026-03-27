-- | Scene Coordinator Component
-- |
-- | Thin dispatcher: owns component lifecycle, render, handleAction routing,
-- | and output handlers. Data loading, overlay toggles, and search are
-- | delegated to sibling modules.
module CE2.Component.SceneCoordinator
  ( module CE2.Component.SceneCoordinator.Types
  , component
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
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
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Web.Event.Event (EventType(..))
import Web.Event.Event as WE
import Web.Event.EventTarget (addEventListener, removeEventListener, eventListener) as ET
import Web.HTML (window)
import Web.HTML.HTMLDocument (toEventTarget) as HTMLDoc
import Web.HTML.Window (document, toEventTarget) as Win
import Web.UIEvent.KeyboardEvent (toEvent, key, repeat, metaKey, ctrlKey, shiftKey, altKey)
import Web.UIEvent.KeyboardEvent as KE

import Halogen.HTML.Events as HE

-- PSD3 Imports
import Hylograph.HATS.InterpreterTick (clearContainer, clearAllHighlights)
import CE2.Viz.DOMHelpers (setDocumentTitle)

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
import CE2.Component.ModulePlanetViz as ModulePlanetViz
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
import CE2.Component.Header.Branding as Branding
import CE2.Component.Header.Breadcrumbs as Breadcrumbs
import CE2.Component.Header.Navigation as Navigation
import CE2.Component.Header.Search as Search

import CE2.Containers as C
import CE2.Data.Loader as Loader
import CE2.Scene (Scene(..), sceneFromString, sceneToString)
import CE2.Viz.DependencyMatrix as DependencyMatrix
import CE2.Types (ColorMode(..), BeeswarmScope(..), RefreshPhase(..), themeColors, isDarkTheme)
import CE2.Component.SceneCoordinator.Pure (ViewMode(..), viewModeToString, viewModeFromString)
import CE2.Component.SceneCoordinator.Pure as Pure

-- Extracted modules
import CE2.Component.SceneCoordinator.Types
import CE2.Component.SceneCoordinator.Loaders as Loaders
import CE2.Component.SceneCoordinator.Overlays as Overlays
import CE2.Component.SceneCoordinator.Search as Search.Handler

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
  , scope: input.initialScope
  , focalPackage: input.initialFocalPackage
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
  , namespaceTreeData: Nothing
  , namespacePackages: Nothing
  , gitStatus: Nothing
  , reachabilityData: Nothing
  , reachabilityPeek: false
  , keyboardCleanup: Nothing
  , clusterData: Nothing
  , purityData: Nothing
  , purityPeek: false
  , complexityData: Nothing
  , complexityPeek: false
  , sourcePeek: true
  , changeFrequencyData: Nothing
  , coChangeClusterData: Nothing
  , sizeByChangeFrequency: false
  , hideInfraLinks: false
  , loadedProjects: []
  , historyCleanup: Nothing
  , refreshPhase: RefreshIdle
  , sceneLoading: false
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
  let theme = Pure.themeForScene state.scene
      colors = themeColors theme
      -- Override background for light-canvas color modes
      bgColor = case state.colorMode of
        CoChangeCluster -> "#FFFFFF"
        _ -> colors.background
      cursorStyle = if state.sceneLoading then " cursor: wait; pointer-events: none;" else ""
  in HH.div
    [ HP.class_ (HH.ClassName "scene-coordinator")
    -- Note: MUST use height: 100vh (not min-height) for flex-grow to work with the child's height: 0 pattern
    , HP.style $ "display: flex; flex-direction: column; height: 100vh; background: " <> bgColor <> "; transition: background 0.5s ease;" <> cursorStyle
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

-- =============================================================================
-- Header, Tab Strip, and Footer
-- =============================================================================

-- | Render the two-tier header bar
-- | Row 1: branding + breadcrumbs | search + scene shortcuts + sync + debug
-- | Row 2: contextual controls (view modes, overlays, peeks) — hidden when empty
renderHeaderBar :: forall m. State -> H.ComponentHTML Action Slots m
renderHeaderBar state =
  let
    showRow2 = Navigation.hasRow2 state.scene
    headerStyle = "padding: 0 16px; display: flex; align-items: center; justify-content: space-between; "
        <> "background: #D4C9A8; color: #333333; "
        <> "font-family: 'Courier New', Courier, monospace; font-size: 11px;"
  in HH.div
    [ HP.class_ (HH.ClassName "scene-header-bar")
    , HP.style "border-bottom: 1px solid #999;"
    ]
    [ -- Row 1: global bar
      HH.div
        [ HP.style $ headerStyle <> " height: 36px;"
            <> if showRow2 then "" else " border-bottom: none;"
        ]
        [ -- Left: Branding (double-height, home link) + Breadcrumbs
          HH.div
            [ HP.style "display: flex; align-items: center;" ]
            [ Branding.render (NavigateTo ProjectManagement)
            , Breadcrumbs.render NavigateTo state.scene
            ]
        -- Right: Search + scene shortcuts + sync + debug
        , HH.div
            [ HP.style "display: flex; align-items: center; gap: 8px;" ]
            ( [ Search.render
                  { query: state.searchQuery
                  , results: state.searchResults
                  , selectedIndex: state.searchSelectedIndex
                  , open: state.searchOpen
                  }
                  { onInput: SearchInput
                  , onKeyDown: SearchKeyDown
                  , onDismiss: SearchDismiss
                  , onConfirmIndex: SearchConfirmIndex
                  }
              ]
              <> Navigation.renderRow1
                  { scene: state.scene
                  , refreshPhase: state.refreshPhase
                  }
                  { onNavigateTo: NavigateTo
                  , onArmSync: ArmSync
                  , onConfirmSync: ConfirmSync
                  }
              <> []
            )
        ]
    -- Row 2: contextual controls (hidden when empty)
    , if showRow2
        then HH.div
          [ HP.style $ headerStyle <> " height: 28px; border-top: 1px solid rgba(0,0,0,0.1);" ]
          [ HH.div
              [ HP.style "display: flex; align-items: center; gap: 12px;" ]
              ( Navigation.renderRow2
                  { scene: state.scene
                  , colorMode: state.colorMode
                  , viewMode: state.viewMode
                  , hideInfraLinks: state.hideInfraLinks
                  , sizeByChangeFrequency: state.sizeByChangeFrequency
                  , reachabilityPeek: state.reachabilityPeek
                  , purityPeek: state.purityPeek
                  , complexityPeek: state.complexityPeek
                  , sourcePeek: state.sourcePeek
                  }
                  { onNavigateTo: NavigateTo
                  , onSetViewMode: SetViewMode
                  , onToggleGit: ToggleGitMode
                  , onToggleTidy: ToggleTidyMode
                  , onToggleCluster: ToggleClusterMode
                  , onToggleChangeFreq: ToggleChangeFrequencyMode
                  , onToggleCoChange: ToggleCoChangeClusterMode
                  , onToggleSizeByFreq: ToggleSizeByFrequency
                  , onToggleReachability: ToggleReachabilityPeek
                  , onTogglePurity: TogglePurityPeek
                  , onToggleCoupling: ToggleCouplingPeek
                  , onToggleSource: ToggleSourcePeek
                  }
              )
          ]
        else HH.text ""
    ]

-- | Render the footer bar (persistent, shows stats and selection info)
renderFooterBar :: forall m. State -> H.ComponentHTML Action Slots m
renderFooterBar state =
  let
    theme = Pure.themeForScene state.scene
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
    ModuleStructure _ _ -> renderDeclarationLegend
    ModuleSignatures _ _ -> renderDeclarationLegend
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

-- | Footer controls — view modes moved to header Row 2
renderFooterControls :: forall m. State -> H.ComponentHTML Action Slots m
renderFooterControls _state = HH.text ""

-- | Deprecated scene notice — shown when navigating to old module views
renderDeprecatedScene :: forall m. String -> String -> String -> H.ComponentHTML Action Slots m
renderDeprecatedScene sceneName pkgName modName =
  HH.div
    [ HP.style "display: flex; align-items: center; justify-content: center; width: 100%; height: 100%; background: #fff3cd;" ]
    [ HH.div
        [ HP.style "text-align: center; padding: 40px; max-width: 500px;" ]
        [ HH.div [ HP.style "font-size: 48px; margin-bottom: 16px;" ] [ HH.text "\x26A0\xFE0F" ]
        , HH.div [ HP.style "font-size: 18px; font-weight: 700; color: #856404; margin-bottom: 8px;" ]
            [ HH.text $ sceneName <> " has moved" ]
        , HH.div [ HP.style "font-size: 13px; color: #856404; margin-bottom: 20px; line-height: 1.5;" ]
            [ HH.text "This view has been merged into ModulePlanet. All its features are available as panels on the unified module page." ]
        , HH.div
            [ HP.style "font-size: 13px; color: #0E4C8A; cursor: pointer; font-weight: 600; padding: 8px 16px; border: 1px solid #0E4C8A; border-radius: 4px; display: inline-block;"
            , HE.onClick \_ -> NavigateTo (ModuleStructure pkgName modName)
            ]
            [ HH.text "\x2192 Go to ModulePlanet" ]
        ]
    ]

-- | Render the current scene using child component slots
-- | Streamlined to 6 scenes for teaser navigation
renderScene :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
renderScene state =
  let theme = Pure.themeForScene state.scene
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
          , sourcePeek: state.sourcePeek
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
          , gitStatus: Pure.computePackageGitStatus state.gitStatus state.v2Data
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
                      { imports: Pure.buildModuleImportMap v2.imports
                      , importedBy: Pure.buildModuleImportedByMap v2.imports
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
                let scopedPackages = Pure.solarSwarmScopedPackages state v2.packages
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
                let scopedPackages = Pure.solarSwarmScopedPackages state v2.packages
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
                  , complexityData: state.complexityData
                  , complexityPeek: state.complexityPeek
                  , changeFrequencyData: state.changeFrequencyData
                  , coChangeClusterData: state.coChangeClusterData
                  , sizeByChangeFrequency: state.sizeByChangeFrequency
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
    renderDeprecatedScene "ModuleOverview" pkgName modName

  DeclarationDetail pkgName modName _declName ->
    renderDeprecatedScene "DeclarationDetail" pkgName modName

  ModuleStructure pkgName modName ->
    let decls = fromMaybe [] (Pure.lookupModuleDeclarations state pkgName modName)
        anns = fromMaybe [] (Map.lookup modName state.moduleAnnotations)
        modNameToId2 = case state.v2Data of
          Just v2 -> Map.fromFoldable $ v2.modules <#> \m -> Tuple m.name m.id
          Nothing -> Map.empty
    in HH.slot _modulePlanetViz unit ModulePlanetViz.component
      { packageName: pkgName
      , moduleName: modName
      , declarations: decls
      , annotations: anns
      , functionCalls: state.packageCalls
      , moduleNameToId: modNameToId2
      }
      HandleModulePlanetOutput

  ModuleSignatures pkgName modName ->
    renderDeprecatedScene "ModuleSignatures" pkgName modName

  TypeClassGrid ->
    case state.typeClassStats of
      Just stats ->
        HH.slot _typeClassGridViz unit TypeClassGridViz.component
          { typeClassStats: stats
          , theme: theme
          }
          HandleTypeClassGridOutput
      Nothing ->
        HH.div
          [ HP.class_ (HH.ClassName "loading") ]
          [ HH.text "Loading type class data..." ]

  NamespaceTree ->
    case state.namespaceTreeData of
      Just nsData ->
        HH.slot _namespaceTreeViz unit NamespaceTreeViz.component
          { namespaceTree: nsData
          , namespacePackages: state.namespacePackages
          , packages: case state.v2Data of
              Just v2 -> v2.packages
              Nothing -> []
          , theme: theme
          }
          HandleNamespaceTreeOutput
      Nothing ->
        HH.div
          [ HP.class_ (HH.ClassName "loading") ]
          [ HH.text "Loading namespace tree..." ]

  PackageReport ->
    case state.allAnnotations, state.v2Data of
      Just anns, Just v2 ->
        HH.slot _packageReportViz unit PackageReportViz.component
          { annotations: anns, packages: v2.packages, modules: v2.modules
          , moduleDeclarations: state.packageDeclarations
          }
          HandlePackageReportOutput
      _, _ ->
        HH.div
          [ HP.class_ (HH.ClassName "loading") ]
          [ HH.text "Loading package report..." ]

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

  PackageAnatomy pkg ->
    case state.v2Data of
      Just v2 ->
        HH.slot _packageAnatomyViz unit PackageAnatomyViz.component
          { packageName: pkg
          , allImports: v2.imports <#> \mi -> { moduleName: mi.moduleName, imports: mi.imports }
          , packages: v2.packages <#> \p -> { name: p.name, source: p.source, modules: v2.modules
              # Array.filter (\m -> m.package.name == p.name)
              # map _.name }
          }
          HandlePackageAnatomyOutput
      Nothing ->
        HH.div
          [ HP.class_ (HH.ClassName "loading") ]
          [ HH.text "Loading module data..." ]

  ModuleAnatomy pkgName modName ->
    case state.v2Data of
      Just v2 ->
        case Pure.lookupModuleDeclarations state pkgName modName of
          Just decls ->
            let
              modId = Array.find (\m -> m.name == modName && m.package.name == pkgName) v2.modules
                        <#> _.id
              moduleCalls = case modId of
                Just mid -> fromMaybe [] (Map.lookup mid state.packageCalls)
                Nothing -> []
              internalCalls = Array.filter (not <<< _.isCrossModule) moduleCalls
              crossCalls = Array.filter _.isCrossModule moduleCalls
              declInfos = decls <#> \d -> { name: d.name, kind: d.kind }
              moduleSourceMap = Map.fromFoldable $ v2.modules <#> \m -> Tuple m.name m.package.source
              siblingMods = Array.filter (\m -> m.package.name == pkgName && m.name /= modName) v2.modules
                              <#> _.name
            in HH.slot _moduleAnatomyViz unit ModuleAnatomyViz.component
              { packageName: pkgName
              , moduleName: modName
              , declarations: declInfos
              , functionCalls: internalCalls
              , crossModuleCalls: crossCalls
              , moduleSourceMap
              , siblingModules: siblingMods
              }
              HandleModuleAnatomyOutput
          Nothing ->
            HH.div [ HP.class_ (HH.ClassName "loading") ]
              [ HH.text "Loading declarations..." ]
      Nothing ->
        HH.div [ HP.class_ (HH.ClassName "loading") ]
          [ HH.text "Loading module data..." ]

  CompareModules pkg1 mod1 pkg2 mod2 ->
    case state.v2Data of
      Just _v2 ->
        HH.slot _compareModuleViz unit CompareModuleViz.component
          { leftPackage: pkg1
          , leftModule: mod1
          , rightPackage: pkg2
          , rightModule: mod2
          , declarations: state.packageDeclarations
          , functionCalls: state.packageCalls
          , allModules: case state.v2Data of
              Just v2 -> v2.modules
              Nothing -> []
          , beforeSnapshotId: Nothing
          }
          (\_ -> NavigateTo GalaxyTreemap)
      Nothing ->
        HH.div [ HP.class_ (HH.ClassName "loading") ]
          [ HH.text "Loading module data..." ]

  CompareSnapshots pkg mod beforeSnapshotId ->
    case state.v2Data of
      Just _v2 ->
        HH.slot _compareModuleViz unit CompareModuleViz.component
          { leftPackage: pkg
          , leftModule: mod
          , rightPackage: pkg
          , rightModule: mod
          , declarations: state.packageDeclarations
          , functionCalls: state.packageCalls
          , allModules: case state.v2Data of
              Just v2 -> v2.modules
              Nothing -> []
          , beforeSnapshotId: Just beforeSnapshotId
          }
          (\_ -> NavigateTo (ModuleStructure pkg mod))
      Nothing ->
        HH.div [ HP.class_ (HH.ClassName "loading") ]
          [ HH.text "Loading snapshot data..." ]

  ProjectManagement ->
    HH.slot _landingPageViz unit LandingPageViz.component
      { dataReady: state.packageSetData /= Nothing }
      HandleLandingPageOutput

  ProjectSetup ->
    HH.slot _projectManagementViz unit ProjectManagementViz.component
      { projects: state.loadedProjects
      , dataReady: state.packageSetData /= Nothing
      }
      HandleProjectManagementOutput

  SnapshotManagement ->
    HH.slot _snapshotManagementViz unit SnapshotManagementViz.component
      { dataReady: state.packageSetData /= Nothing }
      HandleSnapshotManagementOutput

  GitOverview ->
    case state.v2Data of
      Just v2 ->
        HH.slot _gitOverviewViz unit GitOverviewViz.component
          { packages: v2.packages }
          HandleGitOverviewOutput
      Nothing ->
        HH.div [ HP.class_ (HH.ClassName "loading") ] [ HH.text "Loading packages..." ]

  CommitModuleGrid pkg ->
    HH.slot _commitModuleGridViz unit CommitModuleGridViz.component
      { packageName: pkg }
      HandleCommitModuleGridOutput

  CoChangeCube pkg ->
    HH.slot _coChangeCubeViz unit CoChangeCubeViz.component
      { packageName: pkg }
      HandleCoChangeCubeOutput

  ProjectAnatomy ->
    case state.packageSetData of
      Just psData ->
        let analyzedPkgs = case state.v2Data of
              Just v2 ->
                -- A package is "analyzed" if it has modules with function call data loaded
                let modsWithCalls = Array.filter (\m ->
                      Map.member m.id state.packageCalls
                    ) v2.modules
                in Array.nub $ map (_.package >>> _.name) modsWithCalls
              Nothing -> []
        in HH.slot _projectAnatomyViz unit ProjectAnatomyViz.component
          { packages: psData.packages, analyzedPackages: analyzedPkgs }
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
    liftEffect $ replaceHistoryState $ mkHistoryState (sceneToString state.scene) (viewModeToString state.viewMode) state.focalPackage state.scope

    -- Set up popstate listener for back/forward navigation
    { emitter: historyEmitter, listener: historyListener } <- liftEffect HS.create
    void $ H.subscribe historyEmitter

    doc <- liftEffect $ Win.document =<< window
    let docTarget = HTMLDoc.toEventTarget doc
    w <- liftEffect window
    let winTarget = Win.toEventTarget w

    popstateListener <- liftEffect $ ET.eventListener \e ->
      case readPopState e of
        Just hs -> case sceneFromString hs.scene of
          Just scene -> HS.notify historyListener (HandlePopstate scene (viewModeFromString hs.viewMode) (historyFocalPackage hs) (historyScope hs))
          Nothing -> pure unit
        Nothing -> pure unit

    liftEffect $ ET.addEventListener (EventType "popstate") popstateListener false winTarget

    let historyCleanup = ET.removeEventListener (EventType "popstate") popstateListener false winTarget
    H.modify_ _ { historyCleanup = Just historyCleanup }

    log "[SceneCoordinator] Browser history integration enabled"

    -- Set up keyboard listener for overlay peeks
    { emitter: keyEmitter, listener: keyListener } <- liftEffect HS.create
    void $ H.subscribe keyEmitter

    let overlayKeys = ["c", "g", "h", "k", "o", "p", "r", "x"]
        -- Ignore key events when modifier keys are held (Cmd, Ctrl, Shift, Alt)
        -- This prevents Cmd-Shift-4 (screenshot) from triggering overlays
        hasModifier ke = metaKey ke || ctrlKey ke || shiftKey ke || altKey ke

    keydownListener <- liftEffect $ ET.eventListener \e ->
      case KE.fromEvent e of
        Just ke | Array.elem (key ke) overlayKeys && not (repeat ke) && not (hasModifier ke) ->
          HS.notify keyListener (OverlayPeekOn (key ke))
        _ -> pure unit

    keyupListener <- liftEffect $ ET.eventListener \e ->
      case KE.fromEvent e of
        Just ke | Array.elem (key ke) overlayKeys ->
          HS.notify keyListener OverlayPeekOff
        _ -> pure unit

    liftEffect do
      ET.addEventListener (EventType "keydown") keydownListener false docTarget
      ET.addEventListener (EventType "keyup") keyupListener false docTarget

    let kbCleanup = do
          ET.removeEventListener (EventType "keydown") keydownListener false docTarget
          ET.removeEventListener (EventType "keyup") keyupListener false docTarget

    H.modify_ _ { keyboardCleanup = Just kbCleanup }

    log "[SceneCoordinator] Keyboard listener for reachability peek enabled"

    Loaders.prepareSceneData state

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
          Just new, Just old -> Array.length new.packages /= Array.length old.packages
          Nothing, Nothing -> false

        dataChanged = modelDataChanged || packageSetDataChanged

    H.modify_ _
      { modelData = input.modelData
      , v2Data = input.v2Data
      , packageSetData = input.packageSetData
      }

    -- If we were syncing, mark as done regardless of whether data changed
    preState <- H.get
    when (preState.refreshPhase == RefreshSyncing) do
      H.modify_ _ { refreshPhase = RefreshDone }
      void $ H.fork do
        liftAff $ Aff.delay (Milliseconds 1500.0)
        handleAction ClearRefreshDone

    when dataChanged do
      log "[SceneCoordinator] Data changed — invalidating lazy caches"
      H.modify_ _
        { packageDeclarations = Map.empty
        , packageCalls = Map.empty
        , allCallsLoaded = false
        , moduleAnnotations = Map.empty
        , allAnnotations = Nothing
        , declarationStats = Nothing
        , typeClassStats = Nothing
        , namespaceTreeData = Nothing
        , namespacePackages = Nothing
        , gitStatus = Nothing
        , reachabilityData = Nothing
        , clusterData = Nothing
        , purityData = Nothing
        , changeFrequencyData = Nothing
        , coChangeClusterData = Nothing
        }
      newState <- H.get
      Loaders.prepareSceneData newState

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
      , changeFrequencyData = Nothing  -- Clear stale change frequency (package-specific)
      , coChangeClusterData = Nothing  -- Clear stale co-change clusters (package-specific)
      }

    -- Update browser tab title for debugging
    liftEffect $ setDocumentTitle $ "Minard — " <> show targetScene

    -- Push to browser history (enables back/forward buttons)
    -- ViewMode resets to PrimaryView on scene change; preserve focal and scope
    currentState <- H.get
    liftEffect $ pushHistoryState $ mkHistoryState (sceneToString targetScene) (viewModeToString PrimaryView) currentState.focalPackage currentState.scope

    -- If reachability mode is active, recompute for the target scene
    when (state.colorMode == Reachability) $ case targetScene of
      PkgTreemap pkg -> Overlays.computeAndStoreReachabilityForPeek pkg
      PkgModuleBeeswarm pkg -> Overlays.computeAndStoreReachabilityForPeek pkg
      GalaxyTreemap -> Overlays.computeAndStoreGlobalReachability
      _ -> pure unit

    -- If cluster mode is active and we're entering a package view, recompute
    when (state.colorMode == ClusterView) $ case targetScene of
      PkgTreemap pkg -> Overlays.computeAndStoreClusters pkg
      PkgModuleBeeswarm pkg -> Overlays.computeAndStoreClusters pkg
      _ -> pure unit

    -- If change frequency mode is active, reload for the new package
    when (state.colorMode == ChangeFrequency) $ case targetScene of
      PkgTreemap pkg -> Loaders.loadChangeFrequencyData pkg
      PkgModuleBeeswarm pkg -> Loaders.loadChangeFrequencyData pkg
      _ -> pure unit

    -- If co-change cluster mode is active, reload for the new package
    when (state.colorMode == CoChangeCluster) $ case targetScene of
      PkgTreemap pkg -> Loaders.loadCoChangeClusterData pkg
      PkgModuleBeeswarm pkg -> Loaders.loadCoChangeClusterData pkg
      _ -> pure unit

    H.raise (SceneChanged targetScene)
    H.modify_ _ { sceneLoading = true }
    newState <- H.get
    Loaders.prepareSceneData newState
    H.modify_ _ { sceneLoading = false }

  -- Browser back/forward button navigation
  -- Navigate to the scene without pushing to history (it's already there)
  HandlePopstate targetScene targetViewMode targetFocal targetScope -> do
    state <- H.get
    log $ "[SceneCoordinator] Popstate navigation to: " <> show targetScene
        <> " viewMode=" <> show targetViewMode
        <> " focal=" <> show targetFocal
        <> " scope=" <> show targetScope

    -- Skip if already at this scene with same viewMode and focal
    when (state.scene /= targetScene || state.viewMode /= targetViewMode || state.focalPackage /= targetFocal) do
      -- Clear existing viz containers and dismiss any visible tooltips
      liftEffect clearAllVizContainers
      liftEffect clearAllHighlights

      H.modify_ _
        { scene = targetScene
        , viewMode = targetViewMode
        , scope = targetScope
        , capturedPositions = Nothing  -- Clear stale positions
        , focalPackage = targetFocal   -- Restore focal from history
        , reachabilityData = Nothing  -- Clear stale reachability (package-specific)
        , clusterData = Nothing       -- Clear stale cluster data (package-specific)
        , purityData = Nothing        -- Clear stale purity data (package-specific)
        , changeFrequencyData = Nothing  -- Clear stale change frequency (package-specific)
        , coChangeClusterData = Nothing  -- Clear stale co-change clusters (package-specific)
        }

      -- If reachability mode is active, recompute for the target scene
      when (state.colorMode == Reachability) $ case targetScene of
        PkgTreemap pkg -> Overlays.computeAndStoreReachabilityForPeek pkg
        PkgModuleBeeswarm pkg -> Overlays.computeAndStoreReachabilityForPeek pkg
        GalaxyTreemap -> Overlays.computeAndStoreGlobalReachability
        _ -> pure unit

      -- If change frequency mode is active, reload for the new package
      when (state.colorMode == ChangeFrequency) $ case targetScene of
        PkgTreemap pkg -> Loaders.loadChangeFrequencyData pkg
        PkgModuleBeeswarm pkg -> Loaders.loadChangeFrequencyData pkg
        _ -> pure unit

      -- If co-change cluster mode is active, reload for the new package
      when (state.colorMode == CoChangeCluster) $ case targetScene of
        PkgTreemap pkg -> Loaders.loadCoChangeClusterData pkg
        PkgModuleBeeswarm pkg -> Loaders.loadCoChangeClusterData pkg
        _ -> pure unit

      H.raise (SceneChanged targetScene)
      newState <- H.get
      Loaders.prepareSceneData newState

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
    GalaxyTreemapViz.ModuleClicked pkgName modName -> do
      log $ "[SceneCoordinator] GalaxyTreemap module clicked: " <> pkgName <> "/" <> modName
      handleAction (NavigateTo (ModuleStructure pkgName modName))
    GalaxyTreemapViz.PackageHovered mPkgName ->
      H.modify_ _ { hoveredPackage = mPkgName }

  HandleModuleTreemapOutput output -> case output of
    ModuleTreemapEnrichedViz.ModuleClicked pkgName modName -> do
      log $ "[SceneCoordinator] Module treemap clicked: " <> pkgName <> "/" <> modName
      handleAction (NavigateTo (ModuleStructure pkgName modName))
    ModuleTreemapEnrichedViz.ModuleHovered _mModName ->
      pure unit  -- Future: coordinated hover
    ModuleTreemapEnrichedViz.DeclarationClicked pkgName modName declName -> do
      log $ "[SceneCoordinator] Declaration clicked in treemap: " <> pkgName <> "/" <> modName <> "/" <> declName
      handleAction (NavigateTo (ModuleStructure pkgName modName))

  HandleModuleOverviewOutput output -> case output of
    ModuleOverviewViz.DeclarationClicked pkgName modName declName -> do
      log $ "[SceneCoordinator] Declaration clicked in overview: " <> declName
      handleAction (NavigateTo (ModuleStructure pkgName modName))
    ModuleOverviewViz.DeclarationHovered _ ->
      pure unit

  HandleModuleStructureOutput output -> case output of
    ModuleStructureViz.DeclarationClicked pkgName modName declName -> do
      log $ "[SceneCoordinator] Declaration clicked in signature map: " <> declName
      handleAction (NavigateTo (ModuleStructure pkgName modName))
    ModuleStructureViz.AnnotationStatusChanged annId newStatus -> do
      log $ "[SceneCoordinator] Annotation " <> show annId <> " -> " <> newStatus
      void $ liftAff $ Loader.patchAnnotationStatus annId newStatus
      -- Optimistically update cached annotations
      state <- H.get
      let updated = map (map (\a -> if a.id == annId then a { status = newStatus } else a))
                        state.moduleAnnotations
      H.modify_ _ { moduleAnnotations = updated }
    ModuleStructureViz.AnnotationReplyCreated reply -> do
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

    ModuleStructureViz.CompareSnapshotsClicked -> do
      log "[SceneCoordinator] Compare snapshots requested"
      state <- H.get
      case state.scene of
        ModuleStructure pkg mod -> do
          snapshotsResult <- liftAff Loader.fetchSnapshots
          case snapshotsResult of
            Right snapshots | Array.length snapshots > 1 -> do
              -- Find the "current" snapshot (most modules among high ws-pkg-count snapshots)
              let withWsPkgs = Array.filter (\s -> s.workspacePackageCount > 1) snapshots
              let currentSnap = Array.sortBy (\a b -> compare b.moduleCount a.moduleCount) withWsPkgs # Array.head
              let currentGitHash = currentSnap >>= _.gitHash
              -- "Before" candidates: different git hash (different commit = different worktree)
              -- with workspace packages > 1 (real projects, not small sub-packages)
              let candidates = Array.filter (\s ->
                    s.gitHash /= currentGitHash && s.workspacePackageCount > 1
                  ) snapshots
              case Array.sortBy (\a b -> compare b.moduleCount a.moduleCount) candidates # Array.head of
                Just before -> do
                  log $ "[SceneCoordinator] Comparing with snapshot " <> show before.id
                    <> " (" <> fromMaybe "?" before.label <> ")"
                    <> " hash=" <> fromMaybe "?" (before.gitHash <#> String.take 7)
                  handleAction (NavigateTo (CompareSnapshots pkg mod before.id))
                Nothing ->
                  log "[SceneCoordinator] No suitable 'before' snapshot found (need a different commit)"
            Right _ ->
              log "[SceneCoordinator] Only one snapshot available — load a second snapshot to compare"
            Left err ->
              log $ "[SceneCoordinator] Failed to fetch snapshots: " <> err
        _ -> pure unit

  HandleModulePlanetOutput output -> case output of
    ModulePlanetViz.DeclarationClicked pkgName modName declName -> do
      log $ "[SceneCoordinator] Planet declaration clicked: " <> declName
      -- Navigate to the target module's planet page
      handleAction (NavigateTo (ModuleStructure pkgName modName))
    ModulePlanetViz.AnnotationStatusChanged annId newStatus -> do
      log $ "[SceneCoordinator] Planet annotation " <> show annId <> " -> " <> newStatus
      void $ liftAff $ Loader.patchAnnotationStatus annId newStatus
      st3 <- H.get
      let updated = map (map (\a -> if a.id == annId then a { status = newStatus } else a))
                        st3.moduleAnnotations
      H.modify_ _ { moduleAnnotations = updated }
    ModulePlanetViz.AnnotationReplyCreated reply -> do
      log $ "[SceneCoordinator] Planet creating reply on " <> reply.targetId
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
          st4 <- H.get
          let modAnns = fromMaybe [] (Map.lookup reply.targetId st4.moduleAnnotations)
              updatedAnns = Array.snoc modAnns newAnn
          H.modify_ _ { moduleAnnotations = Map.insert reply.targetId updatedAnns st4.moduleAnnotations }
        Left err ->
          log $ "[SceneCoordinator] Failed to create reply: " <> err
    ModulePlanetViz.CompareSnapshotsClicked ->
      pure unit  -- Not yet supported in Planet view
    ModulePlanetViz.NavigateToGitView pkgName -> do
      log $ "[SceneCoordinator] Planet → Git view for " <> pkgName
      handleAction (NavigateTo (CommitModuleGrid pkgName))

  HandleModuleSignaturesOutput output -> case output of
    ModuleSignaturesViz.DeclarationClicked pkgName modName declName -> do
      log $ "[SceneCoordinator] Declaration clicked in signatures: " <> declName
      handleAction (NavigateTo (ModuleStructure pkgName modName))
    ModuleSignaturesViz.NavigateToStructure -> do
      state <- H.get
      case state.scene of
        ModuleSignatures pkg mod ->
          handleAction (NavigateTo (ModuleStructure pkg mod))
        _ -> pure unit

  HandleGitOverviewOutput output -> case output of
    GitOverviewViz.NavigateToCommitGrid pkgName ->
      handleAction (NavigateTo (CommitModuleGrid pkgName))
    GitOverviewViz.NavigateToPackage pkgName ->
      handleAction (NavigateTo (PkgTreemap pkgName))

  HandlePackageReportOutput output -> case output of
    PackageReportViz.NavigateToPackage pkgName -> do
      log $ "[SceneCoordinator] Package report → package: " <> pkgName
      handleAction (NavigateTo (PkgTreemap pkgName))
    PackageReportViz.NavigateToModuleReport pkgName -> do
      log $ "[SceneCoordinator] Package report → module report: " <> pkgName
      -- TODO: pass package filter to AnnotationReport when filtering is implemented
      handleAction (NavigateTo AnnotationReport)
    PackageReportViz.NavigateToCommits pkgName -> do
      log $ "[SceneCoordinator] Package report → commits: " <> pkgName
      handleAction (NavigateTo (CommitModuleGrid pkgName))

  HandleTypeClassGridOutput output -> case output of
    TypeClassGridViz.NavigateToModule pkgName modName -> do
      log $ "[SceneCoordinator] Type class grid → module: " <> pkgName <> "/" <> modName
      handleAction (NavigateTo (ModuleStructure pkgName modName))

  HandleNamespaceTreeOutput output -> case output of
    NamespaceTreeViz.NavigateToPackage pkgName -> do
      log $ "[SceneCoordinator] Namespace tree → package: " <> pkgName
      handleAction (NavigateTo (PkgTreemap pkgName))

  HandleAnnotationReportOutput output -> case output of
    AnnotationReportViz.NavigateToModule pkgName modName -> do
      log $ "[SceneCoordinator] Report navigation to: " <> pkgName <> "/" <> modName
      handleAction (NavigateTo (ModuleStructure pkgName modName))

  HandleLandingPageOutput output -> case output of
    LandingPageViz.NavigateToScene scene -> do
      log $ "[SceneCoordinator] Landing page navigation to: " <> show scene
      handleAction (NavigateTo scene)

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

  HandleSnapshotManagementOutput output -> case output of
    SnapshotManagementViz.NavigateToScene scene -> do
      handleAction (NavigateTo scene)
    SnapshotManagementViz.SnapshotCreated -> do
      log "[SceneCoordinator] Snapshot created, refreshing data"
      -- Reload projects list since new worktree project was added
      result <- liftAff Loader.fetchV2Projects
      case result of
        Right projects -> H.modify_ _ { loadedProjects = projects }
        Left _ -> pure unit
    SnapshotManagementViz.SnapshotsDeleted -> do
      log "[SceneCoordinator] Snapshots deleted, refreshing data"
      result <- liftAff Loader.fetchV2Projects
      case result of
        Right projects -> H.modify_ _ { loadedProjects = projects }
        Left _ -> pure unit

  HandleCommitModuleGridOutput output -> case output of
    CommitModuleGridViz.NavigateToScene scene ->
      handleAction (NavigateTo scene)

  HandleCoChangeCubeOutput output -> case output of
    CoChangeCubeViz.NavigateToScene scene ->
      handleAction (NavigateTo scene)

  HandleProjectAnatomyOutput output -> case output of
    ProjectAnatomyViz.PackageClicked pkgName -> do
      log $ "[SceneCoordinator] Anatomy package clicked: " <> pkgName
      handleAction (NavigateTo (PkgTreemap pkgName))
    ProjectAnatomyViz.PackageAnatomyClicked pkgName -> do
      log $ "[SceneCoordinator] Anatomy deep-dive clicked: " <> pkgName
      handleAction (NavigateTo (PackageAnatomy pkgName))
    ProjectAnatomyViz.ModuleClicked pkgName modName -> do
      log $ "[SceneCoordinator] Anatomy module clicked: " <> modName
      handleAction (NavigateTo (ModuleStructure pkgName modName))
    ProjectAnatomyViz.NavigateToGalaxy -> do
      log "[SceneCoordinator] Anatomy → Galaxy"
      handleAction (NavigateTo GalaxyTreemap)
    ProjectAnatomyViz.NavigateToProjects -> do
      log "[SceneCoordinator] Anatomy → Projects"
      handleAction (NavigateTo ProjectSetup)

  HandlePackageAnatomyOutput output -> case output of
    PackageAnatomyViz.ModuleClicked modName -> do
      state <- H.get
      case state.scene of
        PackageAnatomy pkg -> do
          log $ "[SceneCoordinator] PackageAnatomy module clicked: " <> modName
          handleAction (NavigateTo (ModuleAnatomy pkg modName))
        _ -> pure unit

  HandleModuleAnatomyOutput output -> case output of
    ModuleAnatomyViz.NavigateToDeclaration declName -> do
      state <- H.get
      case state.scene of
        ModuleAnatomy pkg mod ->
          handleAction (NavigateTo (ModuleStructure pkg mod))
        _ -> pure unit
    ModuleAnatomyViz.CompareWith targetMod -> do
      state <- H.get
      case state.scene of
        ModuleAnatomy pkg mod ->
          handleAction (NavigateTo (CompareModules pkg mod pkg targetMod))
        _ -> pure unit

  HandleDeclarationDetailOutput output -> case output of
    DeclarationDetailViz.BackToModuleOverview -> do
      state <- H.get
      case state.scene of
        DeclarationDetail pkg mod _ ->
          handleAction (NavigateTo (ModuleStructure pkg mod))
        _ ->
          pure unit  -- Shouldn't happen
    DeclarationDetailViz.DeclarationClicked pkgName modName declName -> do
      log $ "[SceneCoordinator] Declaration clicked in detail: " <> declName
      handleAction (NavigateTo (ModuleStructure pkgName modName))
    DeclarationDetailViz.NavigateToModule modName -> do
      log $ "[SceneCoordinator] Navigate to module from detail: " <> modName
      st <- H.get
      case st.v2Data of
        Just v2 -> case Array.find (\m -> m.name == modName) v2.modules of
          Just mod -> handleAction (NavigateTo (ModuleStructure mod.package.name modName))
          Nothing -> log $ "[SceneCoordinator] Module not found: " <> modName
        Nothing -> pure unit
    DeclarationDetailViz.NavigateToModuleSignatures modName -> do
      log $ "[SceneCoordinator] Navigate to signatures from detail: " <> modName
      st2 <- H.get
      case st2.v2Data of
        Just v2 -> case Array.find (\m -> m.name == modName) v2.modules of
          Just mod -> handleAction (NavigateTo (ModuleStructure mod.package.name modName))
          Nothing -> log $ "[SceneCoordinator] Module not found: " <> modName
        Nothing -> pure unit
    DeclarationDetailViz.NavigateToPackage pkgName -> do
      log $ "[SceneCoordinator] Navigate to package from detail: " <> pkgName
      handleAction (NavigateTo (PkgTreemap pkgName))

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
    liftEffect $ pushHistoryState $ mkHistoryState (sceneToString state.scene) (viewModeToString targetMode) state.focalPackage state.scope
    -- Re-render the visualization with new mode
    newState <- H.get
    Loaders.prepareSceneData newState

  ToggleGitMode -> Overlays.handleToggleGitMode
  ToggleTidyMode -> Overlays.handleToggleTidyMode
  ToggleReachabilityMode -> Overlays.handleToggleReachabilityMode
  ToggleClusterMode -> Overlays.handleToggleClusterMode
  ToggleComplexityMode -> Overlays.handleToggleComplexityMode
  ToggleChangeFrequencyMode -> Overlays.handleToggleChangeFrequencyMode
  ToggleSizeByFrequency -> Overlays.handleToggleSizeByFrequency
  ToggleCoChangeClusterMode -> Overlays.handleToggleCoChangeClusterMode
  ToggleReachabilityPeek -> Overlays.handleToggleReachabilityPeek
  TogglePurityPeek -> Overlays.handleTogglePurityPeek
  ToggleCouplingPeek -> Overlays.handleToggleCouplingPeek
  ToggleSourcePeek -> Overlays.handleToggleSourcePeek
  OverlayPeekOn k -> Overlays.handleOverlayPeekOn k
  OverlayPeekOff -> Overlays.handleOverlayPeekOff

  -- =========================================================================
  -- Search Typeahead Actions
  -- =========================================================================

  SearchInput query -> Search.Handler.handleSearchInput query

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
        case Search.Handler.resolveSearchSelection state.searchResults state.searchSelectedIndex of
          Just targetScene -> do
            H.modify_ _ { searchQuery = "", searchResults = [], searchOpen = false }
            handleAction (NavigateTo targetScene)
          Nothing -> pure unit
      "Escape" -> do
        H.modify_ _ { searchQuery = "", searchResults = [], searchOpen = false }
      _ -> pure unit

  SearchConfirmIndex idx -> do
    state <- H.get
    case Search.Handler.resolveSearchSelection state.searchResults idx of
      Just targetScene -> do
        log $ "[SceneCoordinator] Search navigation to: " <> show targetScene
        H.modify_ _ { searchQuery = "", searchResults = [], searchOpen = false }
        handleAction (NavigateTo targetScene)
      Nothing -> pure unit

  SearchDismiss -> Search.Handler.handleSearchDismiss

  -- Two-click sync: arm → confirm → execute
  ArmSync -> do
    state <- H.get
    case state.refreshPhase of
      RefreshIdle -> do
        H.modify_ _ { refreshPhase = RefreshPending }
        -- Auto-revert after 3 seconds if not confirmed
        void $ H.fork do
          liftAff $ Aff.delay (Milliseconds 3000.0)
          st <- H.get
          when (st.refreshPhase == RefreshPending) $
            H.modify_ _ { refreshPhase = RefreshIdle }
      RefreshError _ -> do
        H.modify_ _ { refreshPhase = RefreshPending }
        void $ H.fork do
          liftAff $ Aff.delay (Milliseconds 3000.0)
          st <- H.get
          when (st.refreshPhase == RefreshPending) $
            H.modify_ _ { refreshPhase = RefreshIdle }
      _ -> pure unit

  ConfirmSync -> do
    state <- H.get
    when (state.refreshPhase == RefreshPending) $
      handleAction RequestRefresh

  RevertSyncArm -> do
    state <- H.get
    when (state.refreshPhase == RefreshPending) $
      H.modify_ _ { refreshPhase = RefreshIdle }

  RequestRefresh -> do
    state <- H.get
    -- Ensure loadedProjects is populated before syncing
    projects <- if Array.null state.loadedProjects
      then do
        result <- liftAff Loader.fetchV2Projects
        case result of
          Right ps -> do
            H.modify_ _ { loadedProjects = ps }
            pure ps
          Left _ -> pure []
      else pure state.loadedProjects
    log $ "[SceneCoordinator] Sync requested for " <> show (Array.length projects) <> " project(s)"
    H.modify_ _ { refreshPhase = RefreshSyncing }
    H.raise (RequestDataRefresh projects)

  ClearRefreshDone -> do
    H.modify_ _ { refreshPhase = RefreshIdle }

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

-- =============================================================================
-- Query Handlers
-- =============================================================================

handleQuery :: forall m a. MonadAff m => Query a -> H.HalogenM State Action Slots Output m (Maybe a)
handleQuery = case _ of
  SetScene targetScene a -> do
    handleAction (NavigateTo targetScene)
    pure (Just a)
  NotifyRefreshError msg a -> do
    log $ "[SceneCoordinator] Refresh error: " <> msg
    H.modify_ _ { refreshPhase = RefreshError msg }
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
