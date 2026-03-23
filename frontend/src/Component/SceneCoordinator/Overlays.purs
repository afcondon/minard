-- | Overlay toggle and peek handlers for SceneCoordinator.
-- |
-- | All color-mode toggles (Git, Reachability, Cluster, Complexity,
-- | ChangeFrequency, CoChange, Tidy, SizeByFrequency) and peek handlers
-- | (Reachability, Purity, Coupling) plus momentary keyboard peeks.
module CE2.Component.SceneCoordinator.Overlays
  ( handleToggleGitMode
  , handleToggleTidyMode
  , handleToggleReachabilityMode
  , handleToggleClusterMode
  , handleToggleComplexityMode
  , handleToggleChangeFrequencyMode
  , handleToggleCoChangeClusterMode
  , handleToggleSizeByFrequency
  , handleToggleReachabilityPeek
  , handleTogglePurityPeek
  , handleToggleCouplingPeek
  , handleToggleSourcePeek
  , handleOverlayPeekOn
  , handleOverlayPeekOff
  , computeAndStoreClusters
  , computeAndStoreReachabilityForPeek
  , computeAndStoreGlobalReachability
  , computeAndStorePurityForPeek
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class.Console (log)
import Halogen as H

import CE2.Component.SceneCoordinator.Types (State, Action, Slots, Output)
import CE2.Component.SceneCoordinator.Loaders as Loaders
import CE2.Component.SceneCoordinator.Pure as Pure
import CE2.Data.Loader as Loader
import CE2.Scene (Scene(..))
import CE2.Types (ColorMode(..))
import CE2.Viz.DeclarationArcDiagram (isEffectful) as ArcDiagram

handleToggleGitMode :: forall m. MonadAff m => H.HalogenM State Action Slots Output m Unit
handleToggleGitMode = do
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

handleToggleTidyMode :: forall m. MonadAff m => H.HalogenM State Action Slots Output m Unit
handleToggleTidyMode = do
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
  Loaders.prepareSceneData newState

handleToggleReachabilityMode :: forall m. MonadAff m => H.HalogenM State Action Slots Output m Unit
handleToggleReachabilityMode = do
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
              reach = Pure.computePackageReachability pkg bundleMod v2.imports v2.modules
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

handleToggleClusterMode :: forall m. MonadAff m => H.HalogenM State Action Slots Output m Unit
handleToggleClusterMode = do
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

handleToggleComplexityMode :: forall m. MonadAff m => H.HalogenM State Action Slots Output m Unit
handleToggleComplexityMode = do
  state <- H.get
  if state.colorMode == StructuralComplexity
    then do
      log "[SceneCoordinator] Structural complexity mode OFF"
      H.modify_ _ { colorMode = DefaultUniform }
    else do
      log "[SceneCoordinator] Structural complexity mode ON"
      H.modify_ _ { colorMode = StructuralComplexity }
      when (state.complexityData == Nothing) Loaders.loadComplexityData

handleToggleChangeFrequencyMode :: forall m. MonadAff m => H.HalogenM State Action Slots Output m Unit
handleToggleChangeFrequencyMode = do
  state <- H.get
  if state.colorMode == ChangeFrequency
    then do
      log "[SceneCoordinator] Change frequency mode OFF"
      H.modify_ _ { colorMode = FullRegistryTopo }
    else do
      log "[SceneCoordinator] Change frequency mode ON"
      H.modify_ _ { colorMode = ChangeFrequency }
      when (state.changeFrequencyData == Nothing) do
        case state.scene of
          PkgTreemap pkg -> Loaders.loadChangeFrequencyData pkg
          PkgModuleBeeswarm pkg -> Loaders.loadChangeFrequencyData pkg
          _ -> pure unit

handleToggleSizeByFrequency :: forall m. MonadAff m => H.HalogenM State Action Slots Output m Unit
handleToggleSizeByFrequency = do
  state <- H.get
  let newVal = not state.sizeByChangeFrequency
  log $ "[SceneCoordinator] Size by change frequency: " <> show newVal
  H.modify_ _ { sizeByChangeFrequency = newVal }
  -- Ensure frequency data is loaded when toggling on
  when (newVal && state.changeFrequencyData == Nothing) do
    case state.scene of
      PkgTreemap pkg -> Loaders.loadChangeFrequencyData pkg
      PkgModuleBeeswarm pkg -> Loaders.loadChangeFrequencyData pkg
      _ -> pure unit

handleToggleCoChangeClusterMode :: forall m. MonadAff m => H.HalogenM State Action Slots Output m Unit
handleToggleCoChangeClusterMode = do
  state <- H.get
  if state.colorMode == CoChangeCluster
    then do
      log "[SceneCoordinator] Co-change cluster mode OFF"
      H.modify_ _ { colorMode = FullRegistryTopo }
    else do
      log "[SceneCoordinator] Co-change cluster mode ON"
      H.modify_ _ { colorMode = CoChangeCluster }
      when (state.coChangeClusterData == Nothing) do
        case state.scene of
          PkgTreemap pkg -> Loaders.loadCoChangeClusterData pkg
          PkgModuleBeeswarm pkg -> Loaders.loadCoChangeClusterData pkg
          _ -> pure unit

-- =========================================================================
-- Sticky Peek Toggles (click button to toggle on/off)
-- =========================================================================

handleToggleReachabilityPeek :: forall m. MonadAff m => H.HalogenM State Action Slots Output m Unit
handleToggleReachabilityPeek = do
  state <- H.get
  let newVal = not state.reachabilityPeek
  H.modify_ _ { reachabilityPeek = newVal }
  when (newVal && state.reachabilityData == Nothing) $ case state.scene of
    PkgTreemap pkg -> computeAndStoreReachabilityForPeek pkg
    PkgModuleBeeswarm pkg -> computeAndStoreReachabilityForPeek pkg
    GalaxyTreemap -> computeAndStoreGlobalReachability
    _ -> pure unit

handleTogglePurityPeek :: forall m. MonadAff m => H.HalogenM State Action Slots Output m Unit
handleTogglePurityPeek = do
  state <- H.get
  let newVal = not state.purityPeek
  H.modify_ _ { purityPeek = newVal }
  when (newVal && state.purityData == Nothing) $ case state.scene of
    PkgTreemap pkg -> computeAndStorePurityForPeek pkg
    PkgModuleBeeswarm pkg -> computeAndStorePurityForPeek pkg
    _ -> pure unit

handleToggleCouplingPeek :: forall m. MonadAff m => H.HalogenM State Action Slots Output m Unit
handleToggleCouplingPeek = do
  state <- H.get
  let newVal = not state.complexityPeek
  H.modify_ _ { complexityPeek = newVal }
  when (newVal && state.complexityData == Nothing) Loaders.loadComplexityData

handleToggleSourcePeek :: forall m. MonadAff m => H.HalogenM State Action Slots Output m Unit
handleToggleSourcePeek = do
  state <- H.get
  H.modify_ _ { sourcePeek = not state.sourcePeek }

-- =========================================================================
-- Momentary Keyboard Peeks (hold key = show overlay, release = revert)
-- Radio behavior: activating one clears all others
-- =========================================================================

handleOverlayPeekOn :: forall m. MonadAff m => String -> H.HalogenM State Action Slots Output m Unit
handleOverlayPeekOn k = do
  state <- H.get
  when (not state.searchOpen) do
    -- Clear all peeks and reset colorMode, then activate the requested one
    -- Clear peeks and colorMode but preserve sourcePeek (it's a background overlay, composes with others)
    H.modify_ _ { reachabilityPeek = false, purityPeek = false, complexityPeek = false, colorMode = DefaultUniform }
    case k of
      "c" -> do
        H.modify_ _ { complexityPeek = true }
        when (state.complexityData == Nothing) Loaders.loadComplexityData
      "g" -> do
        H.modify_ _ { colorMode = GitStatus }
        when (state.gitStatus == Nothing) do
          result <- liftAff Loader.fetchGitStatus
          case result of
            Right gitData -> H.modify_ _ { gitStatus = Just gitData }
            Left _ -> pure unit
      "h" -> do
        H.modify_ _ { colorMode = ChangeFrequency }
        when (state.changeFrequencyData == Nothing) $ case state.scene of
          PkgTreemap pkg -> Loaders.loadChangeFrequencyData pkg
          PkgModuleBeeswarm pkg -> Loaders.loadChangeFrequencyData pkg
          _ -> pure unit
      "k" -> do
        H.modify_ _ { colorMode = ClusterView }
        case state.scene of
          PkgTreemap pkg -> computeAndStoreClusters pkg
          PkgModuleBeeswarm pkg -> computeAndStoreClusters pkg
          _ -> pure unit
      "p" -> do
        H.modify_ _ { purityPeek = true }
        when (state.purityData == Nothing) $ case state.scene of
          PkgTreemap pkg -> computeAndStorePurityForPeek pkg
          PkgModuleBeeswarm pkg -> computeAndStorePurityForPeek pkg
          _ -> pure unit
      "r" -> do
        H.modify_ _ { reachabilityPeek = true }
        when (state.reachabilityData == Nothing) $ case state.scene of
          PkgTreemap pkg -> computeAndStoreReachabilityForPeek pkg
          PkgModuleBeeswarm pkg -> computeAndStoreReachabilityForPeek pkg
          GalaxyTreemap -> computeAndStoreGlobalReachability
          _ -> pure unit
      "o" -> do
        -- Source overlay toggles independently (composes with other overlays)
        H.modify_ _ { sourcePeek = not state.sourcePeek }
      "x" -> do
        H.modify_ _ { colorMode = CoChangeCluster }
        when (state.coChangeClusterData == Nothing) $ case state.scene of
          PkgTreemap pkg -> Loaders.loadCoChangeClusterData pkg
          PkgModuleBeeswarm pkg -> Loaders.loadCoChangeClusterData pkg
          _ -> pure unit
      _ -> pure unit

handleOverlayPeekOff :: forall m. MonadAff m => H.HalogenM State Action Slots Output m Unit
handleOverlayPeekOff = do
  -- Preserve sourcePeek — it's a sticky toggle, not a momentary peek
  H.modify_ _ { reachabilityPeek = false, purityPeek = false, complexityPeek = false, colorMode = DefaultUniform }

-- =========================================================================
-- Compute Helpers
-- =========================================================================

-- | Compute and store clusters for a package (used in action handlers)
computeAndStoreClusters :: forall m. MonadAff m => String -> H.HalogenM State Action Slots Output m Unit
computeAndStoreClusters pkg = do
  state <- H.get
  case state.v2Data of
    Just v2 -> do
      let clusters = Pure.computePackageClusters pkg v2.imports v2.modules
      log $ "[SceneCoordinator] Clusters for " <> pkg <> ": "
          <> show (Array.length clusters.clusters) <> " components, "
          <> show (Map.size clusters.communities) <> " community assignments"
      H.modify_ _ { clusterData = Just clusters }
    Nothing -> pure unit

-- | Compute and store reachability for peek (reuses existing logic)
computeAndStoreReachabilityForPeek :: forall m. MonadAff m => String -> H.HalogenM State Action Slots Output m Unit
computeAndStoreReachabilityForPeek pkg = do
  state <- H.get
  case state.v2Data of
    Just v2 -> do
      let bundleMod = Array.find (\p -> p.name == pkg) v2.packages
                        >>= _.bundleModule
          reach = Pure.computePackageReachability pkg bundleMod v2.imports v2.modules
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

-- | Compute and store global reachability (galaxy-level, all packages)
computeAndStoreGlobalReachability :: forall m. MonadAff m => H.HalogenM State Action Slots Output m Unit
computeAndStoreGlobalReachability = do
  state <- H.get
  case state.v2Data of
    Just v2 -> do
      let reach = Pure.computeGlobalReachability v2.imports v2.modules v2.packages
      log $ "[SceneCoordinator] Global reachability: "
          <> show (Set.size reach.reachable) <> " reachable, "
          <> show (Set.size reach.entryPoints) <> " entry points"
      H.modify_ _ { reachabilityData = Just reach }
    Nothing -> pure unit

-- | Compute and store purity data for peek overlay
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
