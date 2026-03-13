-- | Header navigation — two-tier layout.
-- |
-- | Row 1: global scene shortcuts + sync button (always visible).
-- | Row 2: contextual controls — view modes, color overlays, peek buttons,
-- |        context-dependent scene links. Hidden when no controls apply.
module CE2.Component.Header.Navigation
  ( Row1State
  , Row1Actions
  , Row2State
  , Row2Actions
  , renderRow1
  , renderRow2
  , hasRow2
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import CE2.Scene (Scene(..))
import CE2.Types (ColorMode(..), RefreshPhase(..))
import CE2.Component.SceneCoordinator.Pure (ViewMode(..))

-- =============================================================================
-- Row 1: Global scene shortcuts + sync
-- =============================================================================

type Row1State =
  { scene :: Scene
  , refreshPhase :: RefreshPhase
  }

type Row1Actions i =
  { onNavigateTo :: Scene -> i
  , onArmSync :: i
  , onConfirmSync :: i
  }

-- | Global scene shortcuts: Anatomy, Types, Namespaces, Report + Sync
renderRow1 :: forall w i. Row1State -> Row1Actions i -> Array (HH.HTML w i)
renderRow1 state actions =
  [ navButton "Anatomy" ProjectAnatomy (state.scene == ProjectAnatomy)
  , navButton "Types" TypeClassGrid (state.scene == TypeClassGrid)
  , navButton "Namespaces" NamespaceTree (state.scene == NamespaceTree)
  , navButton "Report" PackageReport (state.scene == PackageReport || state.scene == AnnotationReport)
  -- Sync is visually separated from nav buttons
  , HH.span
      [ HP.style "border-left: 1px solid rgba(0,0,0,0.15); padding-left: 8px; margin-left: 4px;" ]
      [ syncButton state.refreshPhase actions.onArmSync actions.onConfirmSync ]
  ]
  where
  navButton :: String -> Scene -> Boolean -> HH.HTML w i
  navButton label target isActive =
    HH.button
      [ HE.onClick \_ -> actions.onNavigateTo target
      , HP.style (buttonStyle isActive)
      ]
      [ HH.text label ]

-- =============================================================================
-- Row 2: Contextual controls
-- =============================================================================

type Row2State =
  { scene :: Scene
  , colorMode :: ColorMode
  , viewMode :: ViewMode
  , hideInfraLinks :: Boolean
  , sizeByChangeFrequency :: Boolean
  , reachabilityPeek :: Boolean
  , purityPeek :: Boolean
  , complexityPeek :: Boolean
  }

type Row2Actions i =
  { onNavigateTo :: Scene -> i
  , onSetViewMode :: ViewMode -> i
  -- Color mode toggles
  , onToggleGit :: i
  , onToggleTidy :: i
  , onToggleCluster :: i
  , onToggleChangeFreq :: i
  , onToggleCoChange :: i
  , onToggleSizeByFreq :: i
  -- Peek toggles (click = sticky toggle, hotkey = hold-to-peek)
  , onToggleReachability :: i
  , onTogglePurity :: i
  , onToggleCoupling :: i
  }

-- | Whether Row 2 should be rendered for this scene
hasRow2 :: Scene -> Boolean
hasRow2 = case _ of
  GalaxyTreemap -> true
  GalaxyBeeswarm -> true
  SolarSwarm -> true
  PkgTreemap _ -> true
  PkgModuleBeeswarm _ -> true
  ModuleSignatureMap _ _ -> true
  ModuleOverview _ _ -> true
  ModuleStructure _ _ -> true
  DeclarationDetail _ _ _ -> true
  _ -> false

-- | Contextual controls — returns empty array when nothing applies
renderRow2 :: forall w i. Row2State -> Row2Actions i -> Array (HH.HTML w i)
renderRow2 state actions =
  case state.scene of
    -- Galaxy: layout toggle + overlays
    GalaxyTreemap -> concat
      [ galaxyLayoutGroup
      , colorGroup [ gitToggle, tidyToggle ]
      , peekGroup [ reachPeek, couplingPeek ]
      ]

    GalaxyBeeswarm -> concat
      [ galaxyLayoutGroup
      , colorGroup [ gitToggle, tidyToggle ]
      ]

    -- SolarSwarm: layout toggle + view modes + Git, Tidy (only for Primary view)
    SolarSwarm -> concat
      [ galaxyLayoutGroup
      , viewModeGroup
      , if state.viewMode == PrimaryView
          then colorGroup [ gitToggle, tidyToggle ]
          else []
      ]

    -- Package treemap: layout + view modes; color/peek/modifiers only for Primary view
    PkgTreemap pkg -> concat
      [ packageLayoutGroup pkg
      , viewModeGroup
      , if state.viewMode == PrimaryView
          then concat
            [ colorGroup [ gitToggle, clusterToggle, changesToggle, coChgToggle ]
            , modifierGroup [ tidyToggle, sizeToggle ]
            , peekGroup [ reachPeek, purityPeek, couplingPeek ]
            ]
          else []
      , sceneLinks (packageSceneLinks pkg)
      ]

    PkgModuleBeeswarm pkg -> concat
      [ packageLayoutGroup pkg
      , colorGroup [ gitToggle, clusterToggle, changesToggle, coChgToggle ]
      , modifierGroup [ tidyToggle, sizeToggle ]
      , peekGroup [ reachPeek, purityPeek, couplingPeek ]
      , sceneLinks (packageSceneLinks pkg)
      ]

    -- Module views: view toggle + context links
    ModuleSignatureMap pkg mod -> concat
      [ moduleViewGroup pkg mod
      , sceneLinks (moduleSceneLinks pkg)
      ]

    ModuleOverview pkg mod -> concat
      [ moduleViewGroup pkg mod
      , sceneLinks (moduleSceneLinks pkg)
      ]

    ModuleStructure pkg _ -> concat
      [ sceneLinks (moduleSceneLinks pkg)
      ]

    DeclarationDetail pkg _ _ -> concat
      [ sceneLinks (moduleSceneLinks pkg)
      ]

    _ -> []

  where
  concat = Array.concat

  -- -------------------------------------------------------------------------
  -- Control groups (each returns Array (HH.HTML w i))
  -- -------------------------------------------------------------------------

  colorGroup :: Array (HH.HTML w i) -> Array (HH.HTML w i)
  colorGroup items =
    [ HH.div
        [ HP.style "display: flex; align-items: center; gap: 4px;" ]
        ( [ groupLabel "Color" ] <> items )
    ]

  modifierGroup :: Array (HH.HTML w i) -> Array (HH.HTML w i)
  modifierGroup items =
    [ HH.div
        [ HP.style "display: flex; align-items: center; gap: 4px;" ]
        items
    ]

  peekGroup :: Array (HH.HTML w i) -> Array (HH.HTML w i)
  peekGroup items =
    [ HH.div
        [ HP.style "display: flex; align-items: center; gap: 4px;" ]
        ( [ groupLabel "Peek" ] <> items )
    ]

  sceneLinks :: Array (HH.HTML w i) -> Array (HH.HTML w i)
  sceneLinks items = case items of
    [] -> []
    _ ->
      [ HH.div
          [ HP.style "display: flex; align-items: center; gap: 4px;" ]
          items
      ]

  viewModeGroup :: Array (HH.HTML w i)
  viewModeGroup =
    [ HH.div
        [ HP.style "display: flex; align-items: center; gap: 4px;" ]
        [ groupLabel "View"
        , viewBtn "Primary" PrimaryView
        , viewBtn "Chord" ChordView
        , viewBtn "Matrix" MatrixView
        ]
    ]

  -- Galaxy level: Treemap ↔ Bubblepack layout toggle
  galaxyLayoutGroup :: Array (HH.HTML w i)
  galaxyLayoutGroup =
    [ HH.div
        [ HP.style "display: flex; align-items: center; gap: 4px;" ]
        [ groupLabel "Layout"
        , layoutBtn "Treemap" GalaxyTreemap (isGalaxyTreemap state.scene)
        , layoutBtn "Bubblepack" SolarSwarm (isSolarSwarm state.scene)
        ]
    ]

  -- Package level: Treemap ↔ Beeswarm layout toggle
  packageLayoutGroup :: String -> Array (HH.HTML w i)
  packageLayoutGroup pkg =
    [ HH.div
        [ HP.style "display: flex; align-items: center; gap: 4px;" ]
        [ groupLabel "Layout"
        , layoutBtn "Treemap" (PkgTreemap pkg) (isPkgTreemap state.scene)
        , layoutBtn "Beeswarm" (PkgModuleBeeswarm pkg) (isPkgBeeswarm state.scene)
        ]
    ]

  moduleViewGroup :: String -> String -> Array (HH.HTML w i)
  moduleViewGroup pkg mod =
    [ HH.div
        [ HP.style "display: flex; align-items: center; gap: 4px;" ]
        [ groupLabel "View"
        , moduleViewBtn "Signatures" (ModuleSignatureMap pkg mod)
            (isSignatureMap state.scene)
        , moduleViewBtn "Overview" (ModuleOverview pkg mod)
            (isOverview state.scene)
        , moduleViewBtn "Structure" (ModuleStructure pkg mod)
            (isStructure state.scene)
        ]
    ]

  -- -------------------------------------------------------------------------
  -- Individual controls
  -- -------------------------------------------------------------------------

  gitToggle :: HH.HTML w i
  gitToggle = toggleButton "Git" actions.onToggleGit
    (state.colorMode == GitStatus) Nothing

  tidyToggle :: HH.HTML w i
  tidyToggle = toggleButton "Tidy" actions.onToggleTidy
    state.hideInfraLinks Nothing

  clusterToggle :: HH.HTML w i
  clusterToggle = toggleButton "Cluster" actions.onToggleCluster
    (state.colorMode == ClusterView)
    (Just "Cluster: modules colored by dependency cluster")

  changesToggle :: HH.HTML w i
  changesToggle = toggleButton "Changes" actions.onToggleChangeFreq
    (state.colorMode == ChangeFrequency)
    (Just "Changes: heat map by git change frequency (blue=cold, red=hot)")

  coChgToggle :: HH.HTML w i
  coChgToggle = toggleButton "Co-chg" actions.onToggleCoChange
    (state.colorMode == CoChangeCluster)
    (Just "Co-change: modules colored by co-change community")

  sizeToggle :: HH.HTML w i
  sizeToggle = toggleButton "Size" actions.onToggleSizeByFreq
    state.sizeByChangeFrequency
    (Just "Size: treemap area proportional to git change frequency instead of LOC")

  reachPeek :: HH.HTML w i
  reachPeek = peekButton "Reach" "R" actions.onToggleReachability
    (state.reachabilityPeek || state.colorMode == Reachability)

  purityPeek :: HH.HTML w i
  purityPeek = peekButton "Purity" "P" actions.onTogglePurity
    state.purityPeek

  couplingPeek :: HH.HTML w i
  couplingPeek = peekButton "Coupling" "C" actions.onToggleCoupling
    (state.complexityPeek || state.colorMode == StructuralComplexity)

  -- Context-dependent scene links
  packageSceneLinks :: String -> Array (HH.HTML w i)
  packageSceneLinks pkg =
    [ sceneLinkButton "Structure" StructuralDecomp
        (isStructuralDecomp state.scene)
    , sceneLinkButton "Commits" (CommitModuleGrid pkg)
        (isCommitGrid state.scene)
    , sceneLinkButton "Cube" (CoChangeCube pkg)
        (isCube state.scene)
    ]

  moduleSceneLinks :: String -> Array (HH.HTML w i)
  moduleSceneLinks pkg =
    [ sceneLinkButton "Commits" (CommitModuleGrid pkg)
        (isCommitGrid state.scene)
    , sceneLinkButton "Cube" (CoChangeCube pkg)
        (isCube state.scene)
    ]

  -- -------------------------------------------------------------------------
  -- Button renderers
  -- -------------------------------------------------------------------------

  groupLabel :: String -> HH.HTML w i
  groupLabel text =
    HH.span
      [ HP.style "font-size: 8px; opacity: 0.5; text-transform: uppercase; letter-spacing: 0.5px; margin-right: 2px;" ]
      [ HH.text text ]

  toggleButton :: String -> i -> Boolean -> Maybe String -> HH.HTML w i
  toggleButton label action isActive mTitle =
    HH.button
      ( [ HE.onClick \_ -> action
        , HP.style (buttonStyle isActive)
        ] <> case mTitle of
               Just t -> [ HP.title t ]
               Nothing -> []
      )
      [ HH.text label ]

  peekButton :: String -> String -> i -> Boolean -> HH.HTML w i
  peekButton label hotkey action isActive =
    HH.button
      [ HE.onClick \_ -> action
      , HP.style (buttonStyle isActive)
      , HP.title $ label <> " (hold " <> hotkey <> " for momentary peek)"
      ]
      [ HH.text $ label <> " (" <> hotkey <> ")" ]

  viewBtn :: String -> ViewMode -> HH.HTML w i
  viewBtn label mode =
    HH.button
      [ HE.onClick \_ -> actions.onSetViewMode mode
      , HP.style (buttonStyle (state.viewMode == mode))
      ]
      [ HH.text label ]

  moduleViewBtn :: String -> Scene -> Boolean -> HH.HTML w i
  moduleViewBtn label target isActive =
    HH.button
      [ HE.onClick \_ -> actions.onNavigateTo target
      , HP.style (buttonStyle isActive)
      ]
      [ HH.text label ]

  layoutBtn :: String -> Scene -> Boolean -> HH.HTML w i
  layoutBtn label target isActive =
    HH.button
      [ HE.onClick \_ -> actions.onNavigateTo target
      , HP.style (buttonStyle isActive)
      ]
      [ HH.text label ]

  sceneLinkButton :: String -> Scene -> Boolean -> HH.HTML w i
  sceneLinkButton label target isActive =
    HH.button
      [ HE.onClick \_ -> actions.onNavigateTo target
      , HP.style (buttonStyle isActive)
      ]
      [ HH.text label ]

  -- Scene predicates
  isGalaxyTreemap :: Scene -> Boolean
  isGalaxyTreemap GalaxyTreemap = true
  isGalaxyTreemap _ = false

  isGalaxyBeeswarm :: Scene -> Boolean
  isGalaxyBeeswarm GalaxyBeeswarm = true
  isGalaxyBeeswarm _ = false

  isSolarSwarm :: Scene -> Boolean
  isSolarSwarm SolarSwarm = true
  isSolarSwarm _ = false

  isPkgTreemap :: Scene -> Boolean
  isPkgTreemap (PkgTreemap _) = true
  isPkgTreemap _ = false

  isPkgBeeswarm :: Scene -> Boolean
  isPkgBeeswarm (PkgModuleBeeswarm _) = true
  isPkgBeeswarm _ = false

  isSignatureMap :: Scene -> Boolean
  isSignatureMap (ModuleSignatureMap _ _) = true
  isSignatureMap _ = false

  isOverview :: Scene -> Boolean
  isOverview (ModuleOverview _ _) = true
  isOverview _ = false

  isStructure :: Scene -> Boolean
  isStructure (ModuleStructure _ _) = true
  isStructure _ = false

  isStructuralDecomp :: Scene -> Boolean
  isStructuralDecomp StructuralDecomp = true
  isStructuralDecomp _ = false

  isCommitGrid :: Scene -> Boolean
  isCommitGrid (CommitModuleGrid _) = true
  isCommitGrid _ = false

  isCube :: Scene -> Boolean
  isCube (CoChangeCube _) = true
  isCube _ = false

-- =============================================================================
-- Shared
-- =============================================================================

buttonStyle :: Boolean -> String
buttonStyle isActive =
  "background: " <> (if isActive then "rgba(0,0,0,0.15)" else "none") <> "; "
    <> "border: 1px solid " <> (if isActive then textColor else "rgba(0,0,0,0.25)") <> "; "
    <> "color: " <> textColor <> "; "
    <> "cursor: pointer; font-size: 9px; padding: 2px 6px; border-radius: 3px;"
  where
  textColor = "#333333"

-- | Sync button with two-click confirmation.
-- | Idle → click → "Confirm?" (pending) → click → actually syncs.
-- | The pending state auto-reverts after ~3s via the coordinator.
syncButton :: forall w i. RefreshPhase -> i -> i -> HH.HTML w i
syncButton phase onArmSync onConfirmSync = case phase of
  RefreshIdle ->
    HH.button
      [ HE.onClick \_ -> onArmSync
      , HP.style (syncStyle "none" "rgba(0,0,0,0.25)" "#333" "pointer" "1")
      ]
      [ HH.text "Sync" ]
  RefreshPending ->
    HH.button
      [ HE.onClick \_ -> onConfirmSync
      , HP.style (syncStyle "rgba(200,150,0,0.15)" "rgba(200,150,0,0.5)" "#8b6914" "pointer" "1")
      ]
      [ HH.text "Confirm?" ]
  RefreshSyncing ->
    HH.button
      [ HP.disabled true
      , HP.style (syncStyle "none" "rgba(0,0,0,0.15)" "#333" "wait" "0.7")
      ]
      [ HH.text "Syncing\x2026" ]
  RefreshDone ->
    HH.button
      [ HP.disabled true
      , HP.style (syncStyle "rgba(0,128,0,0.1)" "rgba(0,128,0,0.4)" "#333" "default" "1")
      ]
      [ HH.text "\x2713 Synced" ]
  RefreshError _msg ->
    HH.button
      [ HE.onClick \_ -> onArmSync
      , HP.style (syncStyle "rgba(200,0,0,0.1)" "rgba(200,0,0,0.4)" "#8b0000" "pointer" "1")
      , HP.title "Sync failed \x2014 click to retry"
      ]
      [ HH.text "Sync \x2718" ]
  where
  syncStyle bg border color cursor opacity =
    "background: " <> bg <> "; border: 1px solid " <> border <> "; "
      <> "color: " <> color <> "; cursor: " <> cursor <> "; "
      <> "font-size: 9px; padding: 2px 6px; border-radius: 3px; opacity: " <> opacity <> ";"
