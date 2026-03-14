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
import CE2.Scene (Scene(..), isMapScene, isAnatomyScene, isReportScene, isProjectScene)
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

-- | Global scene shortcuts: Maps, Reports, Anatomy, Projects + Sync
renderRow1 :: forall w i. Row1State -> Row1Actions i -> Array (HH.HTML w i)
renderRow1 state actions =
  [ navButton "Maps" GalaxyTreemap (isMapScene state.scene)
  , navButton "Reports" PackageReport (isReportScene state.scene)
  , navButton "Anatomy" ProjectAnatomy (isAnatomyScene state.scene)
  , navButton "Projects" ProjectSetup (isProjectScene state.scene)
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
  _ -> false

-- | Contextual controls — only view-transforming controls, no scene links
renderRow2 :: forall w i. Row2State -> Row2Actions i -> Array (HH.HTML w i)
renderRow2 state actions =
  case state.scene of
    -- Galaxy: reachability peek only (coupling is module-level, no-op here)
    GalaxyTreemap ->
      peekGroup [ reachPeek ]

    GalaxyBeeswarm -> []

    -- SolarSwarm: single overlay
    SolarSwarm ->
      overlayGroup [ gitOverlay ]

    -- Package treemap: all overlays, alphabetical
    PkgTreemap _pkg ->
      overlayGroup [ changesOverlay, clusterOverlay, coChangeOverlay, couplingOverlay, gitOverlay, purityOverlay, reachOverlay ]

    PkgModuleBeeswarm _pkg ->
      overlayGroup [ changesOverlay, clusterOverlay, coChangeOverlay, couplingOverlay, gitOverlay, purityOverlay, reachOverlay ]

    _ -> []

  where
  concat = Array.concat

  -- -------------------------------------------------------------------------
  -- Control groups
  -- -------------------------------------------------------------------------

  overlayGroup :: Array (HH.HTML w i) -> Array (HH.HTML w i)
  overlayGroup items =
    [ HH.div
        [ HP.style "display: flex; align-items: center; gap: 4px;" ]
        items
    ]

  peekGroup :: Array (HH.HTML w i) -> Array (HH.HTML w i)
  peekGroup = overlayGroup

  -- -------------------------------------------------------------------------
  -- Overlay controls (unified style with hotkeys, alphabetical)
  -- -------------------------------------------------------------------------

  changesOverlay :: HH.HTML w i
  changesOverlay = overlayButton "Changes" "H" actions.onToggleChangeFreq
    (state.colorMode == ChangeFrequency)

  clusterOverlay :: HH.HTML w i
  clusterOverlay = overlayButton "Cluster" "K" actions.onToggleCluster
    (state.colorMode == ClusterView)

  coChangeOverlay :: HH.HTML w i
  coChangeOverlay = overlayButton "Co-change" "X" actions.onToggleCoChange
    (state.colorMode == CoChangeCluster)

  couplingOverlay :: HH.HTML w i
  couplingOverlay = overlayButton "Coupling" "C" actions.onToggleCoupling
    (state.complexityPeek || state.colorMode == StructuralComplexity)

  gitOverlay :: HH.HTML w i
  gitOverlay = overlayButton "Git" "G" actions.onToggleGit
    (state.colorMode == GitStatus)

  purityOverlay :: HH.HTML w i
  purityOverlay = overlayButton "Purity" "P" actions.onTogglePurity
    state.purityPeek

  reachOverlay :: HH.HTML w i
  reachOverlay = overlayButton "Reach" "R" actions.onToggleReachability
    (state.reachabilityPeek || state.colorMode == Reachability)

  sizeOverlay :: HH.HTML w i
  sizeOverlay = overlayButton "Size" "S" actions.onToggleSizeByFreq
    state.sizeByChangeFrequency

  tidyOverlay :: HH.HTML w i
  tidyOverlay = overlayButton "Tidy" "T" actions.onToggleTidy
    state.hideInfraLinks

  -- Galaxy-level reach (same as reachOverlay)
  reachPeek :: HH.HTML w i
  reachPeek = reachOverlay

  -- -------------------------------------------------------------------------
  -- Button renderers
  -- -------------------------------------------------------------------------

  overlayButton :: String -> String -> i -> Boolean -> HH.HTML w i
  overlayButton label hotkey action isActive =
    HH.button
      [ HE.onClick \_ -> action
      , HP.style (buttonStyle isActive)
      , HP.title $ label <> " (" <> hotkey <> ")"
      ]
      [ HH.text $ label <> " (" <> hotkey <> ")" ]


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
