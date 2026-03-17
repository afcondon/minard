-- | Data loading functions for SceneCoordinator.
-- |
-- | All data fetching logic: prepareSceneData, ensurePackageDeclarationsLoaded,
-- | loadAnnotationsIfNeeded, and per-overlay data loaders.
module CE2.Component.SceneCoordinator.Loaders
  ( prepareSceneData
  , ensurePackageDeclarationsLoaded
  , loadAnnotationsIfNeeded
  , loadComplexityData
  , loadChangeFrequencyData
  , loadCoChangeClusterData
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Int as Data.Int
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class.Console (log)
import Halogen as H

import CE2.Component.SceneCoordinator.Types (State, Action, Slots, Output(..), smallPackageThreshold)
import CE2.Component.SceneCoordinator.Pure (ViewMode(..))
import CE2.Data.CoChange as CoChange
import CE2.Data.Loader as Loader
import CE2.Scene (Scene(..))

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
                          , sourceSpan: c.sourceSpan
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

  ModuleStructure pkgName modName -> do
    -- Ensure declarations are loaded for this package
    ensurePackageDeclarationsLoaded state pkgName
    -- Fetch annotations for this module if not cached
    when (not $ Map.member modName state.moduleAnnotations) do
      result <- liftAff $ Loader.fetchModuleAnnotations modName
      case result of
        Right anns -> H.modify_ _ { moduleAnnotations = Map.insert modName anns state.moduleAnnotations }
        Left _err -> pure unit  -- Annotations are optional; silent fail
    log "[SceneCoordinator] ModuleStructure: rendering handled by slot"

  GitOverview ->
    log "[SceneCoordinator] GitOverview: rendering handled by slot"

  ModuleSignatures pkgName _modName -> do
    ensurePackageDeclarationsLoaded state pkgName
    log "[SceneCoordinator] ModuleSignatures: rendering handled by slot"

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

  NamespaceTree -> do
    log "[SceneCoordinator] NamespaceTree"
    -- Fetch tree data if not cached
    case state.namespaceTreeData of
      Just nsData ->
        log $ "[SceneCoordinator] NamespaceTree: " <> show (Array.length nsData) <> " nodes cached"
      Nothing -> do
        log "[SceneCoordinator] Loading namespace tree..."
        result <- liftAff Loader.fetchNamespaceTree
        case result of
          Right nsData -> do
            log $ "[SceneCoordinator] Loaded " <> show (Array.length nsData) <> " namespace nodes"
            H.modify_ _ { namespaceTreeData = Just nsData }
          Left err ->
            log $ "[SceneCoordinator] Failed to load namespace tree: " <> err
    -- Fetch namespace→packages mapping if not cached
    case state.namespacePackages of
      Just nsPkgs ->
        log $ "[SceneCoordinator] Namespace packages: " <> show (Array.length nsPkgs) <> " entries cached"
      Nothing -> do
        log "[SceneCoordinator] Loading namespace packages..."
        nsPkgResult <- liftAff Loader.fetchNamespacePackages
        case nsPkgResult of
          Right entries -> do
            log $ "[SceneCoordinator] Loaded " <> show (Array.length entries) <> " namespace-package entries"
            H.modify_ _ { namespacePackages = Just entries }
          Left err ->
            log $ "[SceneCoordinator] Failed to load namespace packages: " <> err

  PackageReport -> do
    -- Same data loading as AnnotationReport — annotations needed for both
    loadAnnotationsIfNeeded state

  AnnotationReport -> do
    loadAnnotationsIfNeeded state

  ProjectManagement -> do
    log "[SceneCoordinator] ProjectManagement: landing page (static)"

  ProjectSetup -> do
    log "[SceneCoordinator] ProjectSetup: fetching projects list"
    result <- liftAff Loader.fetchV2Projects
    case result of
      Right projects -> do
        log $ "[SceneCoordinator] Loaded " <> show (Array.length projects) <> " projects"
        H.modify_ _ { loadedProjects = projects }
      Left err ->
        log $ "[SceneCoordinator] Failed to load projects: " <> err

  SnapshotManagement -> do
    log "[SceneCoordinator] SnapshotManagement"
    -- Component handles its own data loading

  CommitModuleGrid pkg -> do
    log $ "[SceneCoordinator] CommitModuleGrid: " <> pkg
    -- Component handles its own data loading

  CoChangeCube pkg -> do
    log $ "[SceneCoordinator] CoChangeCube: " <> pkg
    -- Component handles its own data loading

  PackageAnatomy pkg -> do
    -- Data is already available in v2Data (imports loaded upfront)
    log $ "[SceneCoordinator] PackageAnatomy: " <> pkg

  ModuleAnatomy pkgName modName -> do
    log $ "[SceneCoordinator] ModuleAnatomy: " <> modName
    -- Need declarations and function calls for this module
    st <- H.get
    case st.v2Data of
      Just v2 -> do
        -- Ensure declarations loaded
        let mod = Array.find (\m -> m.name == modName && m.package.name == pkgName) v2.modules
        case mod of
          Just m -> do
            when (not (Map.member m.id st.packageDeclarations)) do
              log $ "[SceneCoordinator] Fetching declarations for " <> modName
              newDecls <- liftAff $ Loader.fetchV2PackageDeclarations [m]
              st2 <- H.get
              H.modify_ _ { packageDeclarations = Map.union newDecls st2.packageDeclarations }
            -- Ensure function calls loaded
            when (not st.allCallsLoaded) do
              log "[SceneCoordinator] Fetching all function calls for module structure"
              result <- liftAff Loader.fetchV2AllCalls
              case result of
                Right allCalls -> do
                  let callsMap = Map.fromFoldable $ allCalls <#> \mc ->
                        Tuple mc.moduleId (mc.calls <#> \c -> { callerName: c.callerName
                          , calleeModule: c.calleeModule
                          , calleeName: c.calleeName
                          , isCrossModule: c.isCrossModule
                          , callCount: c.callCount
                          , sourceSpan: c.sourceSpan
                          })
                  H.modify_ _ { packageCalls = callsMap, allCallsLoaded = true }
                Left err ->
                  log $ "[SceneCoordinator] Failed to fetch function calls: " <> err
          Nothing ->
            log $ "[SceneCoordinator] Module not found: " <> modName
      Nothing -> pure unit

  CompareModules pkg1 mod1 pkg2 mod2 -> do
    log $ "[SceneCoordinator] CompareModules: " <> mod1 <> " vs " <> mod2
    st <- H.get
    case st.v2Data of
      Just v2 -> do
        -- Ensure declarations loaded for both modules
        let mods = Array.filter (\m ->
              (m.name == mod1 && m.package.name == pkg1) ||
              (m.name == mod2 && m.package.name == pkg2)) v2.modules
            missingDeclModules = Array.filter (\m -> not (Map.member m.id st.packageDeclarations)) mods
        when (Array.length missingDeclModules > 0) do
          log $ "[SceneCoordinator] Fetching declarations for " <> show (Array.length missingDeclModules) <> " compared modules"
          newDecls <- liftAff $ Loader.fetchV2PackageDeclarations missingDeclModules
          st2 <- H.get
          H.modify_ _ { packageDeclarations = Map.union newDecls st2.packageDeclarations }
        -- Ensure function calls loaded
        when (not st.allCallsLoaded) do
          log "[SceneCoordinator] Fetching all function calls for compare view"
          result <- liftAff Loader.fetchV2AllCalls
          case result of
            Right allCalls -> do
              let callsMap = Map.fromFoldable $ allCalls <#> \mc ->
                    Tuple mc.moduleId (mc.calls <#> \c -> { callerName: c.callerName
                      , calleeModule: c.calleeModule
                      , calleeName: c.calleeName
                      , isCrossModule: c.isCrossModule
                      , callCount: c.callCount
                      , sourceSpan: c.sourceSpan
                      })
              H.modify_ _ { packageCalls = callsMap, allCallsLoaded = true }
            Left err ->
              log $ "[SceneCoordinator] Failed to fetch function calls: " <> err
      Nothing -> pure unit

  CompareSnapshots pkg mod _beforeSnapshotId -> do
    -- Data loading for CompareSnapshots is handled in CompareModuleViz itself
    -- (it fetches from the snapshot-scoped API). We just need current-snapshot data.
    log $ "[SceneCoordinator] CompareSnapshots: " <> mod
    st <- H.get
    case st.v2Data of
      Just v2 -> do
        let mods = Array.filter (\m -> m.name == mod && m.package.name == pkg) v2.modules
            missingDeclModules = Array.filter (\m -> not (Map.member m.id st.packageDeclarations)) mods
        when (Array.length missingDeclModules > 0) do
          newDecls <- liftAff $ Loader.fetchV2PackageDeclarations missingDeclModules
          st2 <- H.get
          H.modify_ _ { packageDeclarations = Map.union newDecls st2.packageDeclarations }
        when (not st.allCallsLoaded) do
          result <- liftAff Loader.fetchV2AllCalls
          case result of
            Right allCalls -> do
              let callsMap = Map.fromFoldable $ allCalls <#> \mc ->
                    Tuple mc.moduleId (mc.calls <#> \c -> { callerName: c.callerName
                      , calleeModule: c.calleeModule
                      , calleeName: c.calleeName
                      , isCrossModule: c.isCrossModule
                      , callCount: c.callCount
                      , sourceSpan: c.sourceSpan
                      })
              H.modify_ _ { packageCalls = callsMap, allCallsLoaded = true }
            Left err ->
              log $ "[SceneCoordinator] Failed to fetch function calls: " <> err
      Nothing -> pure unit

  ProjectAnatomy -> do
    -- Package set data is already loaded in DataLoaded phase — no extra fetch needed
    case state.packageSetData of
      Just psData ->
        log $ "[SceneCoordinator] ProjectAnatomy: " <> show (Array.length psData.packages) <> " packages available"
      Nothing -> do
        log "[SceneCoordinator] ProjectAnatomy: requesting package set data"
        H.raise RequestPackageSetData

-- | Shared annotation loading for PackageReport and AnnotationReport
loadAnnotationsIfNeeded :: forall m. MonadAff m => State -> H.HalogenM State Action Slots Output m Unit
loadAnnotationsIfNeeded state = do
  case state.allAnnotations of
    Just _ -> log "[SceneCoordinator] Annotations: data cached"
    Nothing -> do
      log "[SceneCoordinator] Loading all annotations..."
      result <- liftAff Loader.fetchAllAnnotations
      case result of
        Right anns -> do
          log $ "[SceneCoordinator] Loaded " <> show (Array.length anns) <> " annotations"
          H.modify_ _ { allAnnotations = Just anns }
        Left err ->
          log $ "[SceneCoordinator] Failed to load annotations: " <> err
  -- Eagerly fetch declarations for annotated modules (needed for bubblepacks)
  st <- H.get
  case st.v2Data, st.allAnnotations of
    Just v2, Just anns -> do
      let annotatedModuleNames = Set.fromFoldable $ anns <#> _.targetId
          annotatedModules = Array.filter (\m -> Set.member m.name annotatedModuleNames) v2.modules
          missingDeclModules = Array.filter (\m -> not (Map.member m.id st.packageDeclarations)) annotatedModules
      when (Array.length missingDeclModules > 0) do
        log $ "[SceneCoordinator] Fetching declarations for " <> show (Array.length missingDeclModules) <> " annotated modules (bubblepacks)"
        newDecls <- liftAff $ Loader.fetchV2PackageDeclarations missingDeclModules
        st2 <- H.get
        H.modify_ _ { packageDeclarations = Map.union newDecls st2.packageDeclarations }
    _, _ -> log "[SceneCoordinator] Annotations: v2Data or annotations not ready for declaration fetch"

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
                    , sourceSpan: c.sourceSpan
                    })
            H.modify_ _ { packageCalls = callsMap, allCallsLoaded = true }
          Left err ->
            log $ "[SceneCoordinator] Failed to fetch function calls: " <> err
    Nothing ->
      log "[SceneCoordinator] No v2Data available for declaration loading"

-- | Load structural complexity data from backend (global, not per-package)
loadComplexityData :: forall m. MonadAff m => H.HalogenM State Action Slots Output m Unit
loadComplexityData = do
  result <- liftAff Loader.fetchModuleStructuralComplexity
  case result of
    Right modules -> do
      let complexityMap = Map.fromFoldable $ modules <#> \m -> Tuple m.moduleName m
      log $ "[SceneCoordinator] Loaded structural complexity for " <> show (Map.size complexityMap) <> " modules"
      H.modify_ _ { complexityData = Just complexityMap }
    Left err ->
      log $ "[SceneCoordinator] Failed to load complexity: " <> err

-- | Load change frequency data from git commit history for a package
loadChangeFrequencyData :: forall m. MonadAff m => String -> H.HalogenM State Action Slots Output m Unit
loadChangeFrequencyData pkg = do
  log $ "[SceneCoordinator] Fetching change frequency for " <> pkg
  result <- liftAff $ Loader.fetchCommitFiles 200 pkg
  case result of
    Right r -> do
      let freqs = CoChange.moduleFrequencies r.commits
          freqPairs = Map.toUnfoldable freqs :: Array (Tuple String Int)
          maxFreq = Array.foldl (\acc (Tuple _ v) -> max acc v) 1 freqPairs
          normalized = Map.fromFoldable $ freqPairs <#> \(Tuple k v) ->
            Tuple k (Data.Int.toNumber v / Data.Int.toNumber maxFreq)
      log $ "[SceneCoordinator] Change frequency for " <> pkg <> ": "
          <> show (Map.size normalized) <> " modules from "
          <> show (Array.length r.commits) <> " commits"
      H.modify_ _ { changeFrequencyData = Just normalized }
    Left err ->
      log $ "[SceneCoordinator] Failed to load change frequency: " <> err

-- | Load co-change cluster data from git commit history for a package
loadCoChangeClusterData :: forall m. MonadAff m => String -> H.HalogenM State Action Slots Output m Unit
loadCoChangeClusterData pkg = do
  log $ "[SceneCoordinator] Computing co-change clusters for " <> pkg
  result <- liftAff $ Loader.fetchCommitFiles 200 pkg
  case result of
    Right r -> do
      let { communities, clusters } = CoChange.coChangeCommunities r.commits r.allModules
      log $ "[SceneCoordinator] Co-change clusters for " <> pkg <> ": "
          <> show (Array.length clusters) <> " communities, "
          <> show (Map.size communities) <> " modules assigned"
      H.modify_ _ { coChangeClusterData = Just communities }
    Left err ->
      log $ "[SceneCoordinator] Failed to load co-change clusters: " <> err
