-- | Header navigation buttons and mode toggles.
-- | Scene navigation, color mode toggles, and sync button.
module CE2.Component.Header.Navigation
  ( NavState
  , NavActions
  , render
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import CE2.Scene (Scene(..))
import CE2.Types (ColorMode(..), RefreshPhase(..))

type NavState =
  { scene :: Scene
  , colorMode :: ColorMode
  , hideInfraLinks :: Boolean
  , sizeByChangeFrequency :: Boolean
  , refreshPhase :: RefreshPhase
  }

type NavActions i =
  { onNavigateTo :: Scene -> i
  , onToggleGit :: i
  , onToggleTidy :: i
  , onToggleCluster :: i
  , onToggleChangeFreq :: i
  , onToggleCoChange :: i
  , onToggleSizeByFreq :: i
  , onRequestRefresh :: i
  }

render :: forall w i. NavState -> NavActions i -> Array (HH.HTML w i)
render state actions =
  sceneButtons <> modeToggles <> [ syncButton state actions ]
  where
  textColor = "#333333"

  sceneButtons :: Array (HH.HTML w i)
  sceneButtons =
    [ navButton "Snapshots" SnapshotManagement (state.scene == SnapshotManagement)
    , navButton "Anatomy" ProjectAnatomy (state.scene == ProjectAnatomy)
    , navButton "Types" TypeClassGrid (state.scene == TypeClassGrid)
    , navButton "Namespaces" NamespaceTree (state.scene == NamespaceTree)
    , navButton "Report" AnnotationReport (state.scene == AnnotationReport)
    , structureButton
    , commitsButton
    , cubeButton
    ]

  modeToggles :: Array (HH.HTML w i)
  modeToggles =
    [ toggleButton "Git" actions.onToggleGit (state.colorMode == GitStatus) Nothing
    , toggleButton "Tidy" actions.onToggleTidy state.hideInfraLinks Nothing
    , toggleButton "Cluster" actions.onToggleCluster (state.colorMode == ClusterView)
        (Just "Cluster: modules colored by dependency cluster. Hold R to peek reachability.")
    , toggleButton "Changes" actions.onToggleChangeFreq (state.colorMode == ChangeFrequency)
        (Just "Changes: heat map by git change frequency (blue=cold, red=hot)")
    , toggleButton "Co-chg" actions.onToggleCoChange (state.colorMode == CoChangeCluster)
        (Just "Co-change: modules colored by co-change community")
    , toggleButton "Size" actions.onToggleSizeByFreq state.sizeByChangeFrequency
        (Just "Size: treemap area proportional to git change frequency instead of LOC")
    ]

  navButton :: String -> Scene -> Boolean -> HH.HTML w i
  navButton label target isActive =
    HH.button
      [ HE.onClick \_ -> actions.onNavigateTo target
      , HP.style (buttonStyle isActive)
      ]
      [ HH.text label ]

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

  -- Structure button: context-dependent target
  structureButton :: HH.HTML w i
  structureButton =
    let target = case state.scene of
          ModuleSignatureMap pkg mod -> ModuleStructure pkg mod
          ModuleOverview pkg mod -> ModuleStructure pkg mod
          DeclarationDetail pkg mod _ -> ModuleStructure pkg mod
          ModuleStructure _ _ -> state.scene
          _ -> StructuralDecomp
        isActive = case state.scene of
          StructuralDecomp -> true
          ModuleStructure _ _ -> true
          _ -> false
    in navButton "Structure" target isActive

  -- Commits button: only shown when a package is in scope
  commitsButton :: HH.HTML w i
  commitsButton =
    let mTarget = case state.scene of
          PkgTreemap pkg -> Just (CommitModuleGrid pkg)
          PkgModuleBeeswarm pkg -> Just (CommitModuleGrid pkg)
          ModuleSignatureMap pkg _ -> Just (CommitModuleGrid pkg)
          ModuleOverview pkg _ -> Just (CommitModuleGrid pkg)
          DeclarationDetail pkg _ _ -> Just (CommitModuleGrid pkg)
          ModuleStructure pkg _ -> Just (CommitModuleGrid pkg)
          CommitModuleGrid _ -> Just state.scene
          CoChangeCube pkg -> Just (CommitModuleGrid pkg)
          _ -> Nothing
        isActive = case state.scene of
          CommitModuleGrid _ -> true
          _ -> false
    in case mTarget of
      Just target ->
        HH.button
          [ HE.onClick \_ -> actions.onNavigateTo target
          , HP.style (buttonStyle isActive)
          , HP.title "Commit-module change grid for this package"
          ]
          [ HH.text "Commits" ]
      Nothing -> HH.text ""

  -- Cube button: only shown when a package is in scope
  cubeButton :: HH.HTML w i
  cubeButton =
    let mTarget = case state.scene of
          CommitModuleGrid pkg -> Just (CoChangeCube pkg)
          CoChangeCube _ -> Just state.scene
          PkgTreemap pkg -> Just (CoChangeCube pkg)
          PkgModuleBeeswarm pkg -> Just (CoChangeCube pkg)
          ModuleSignatureMap pkg _ -> Just (CoChangeCube pkg)
          ModuleOverview pkg _ -> Just (CoChangeCube pkg)
          DeclarationDetail pkg _ _ -> Just (CoChangeCube pkg)
          ModuleStructure pkg _ -> Just (CoChangeCube pkg)
          _ -> Nothing
        isActive = case state.scene of
          CoChangeCube _ -> true
          _ -> false
    in case mTarget of
      Just target ->
        HH.button
          [ HE.onClick \_ -> actions.onNavigateTo target
          , HP.style (buttonStyle isActive)
          , HP.title "3D co-change tensor cube"
          ]
          [ HH.text "Cube" ]
      Nothing -> HH.text ""

  buttonStyle :: Boolean -> String
  buttonStyle isActive =
    "background: " <> (if isActive then "rgba(0,0,0,0.15)" else "none") <> "; "
      <> "border: 1px solid " <> (if isActive then textColor else "rgba(0,0,0,0.25)") <> "; "
      <> "color: " <> textColor <> "; "
      <> "cursor: pointer; font-size: 9px; padding: 2px 6px; border-radius: 3px;"

syncButton :: forall w i. NavState -> NavActions i -> HH.HTML w i
syncButton state actions = case state.refreshPhase of
  RefreshIdle ->
    HH.button
      [ HE.onClick \_ -> actions.onRequestRefresh
      , HP.style (syncStyle "none" "rgba(0,0,0,0.25)" "#333" "pointer" "1")
      ]
      [ HH.text "Sync" ]
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
      [ HE.onClick \_ -> actions.onRequestRefresh
      , HP.style (syncStyle "rgba(200,0,0,0.1)" "rgba(200,0,0,0.4)" "#8b0000" "pointer" "1")
      , HP.title "Sync failed \x2014 click to retry"
      ]
      [ HH.text "Sync \x2718" ]
  where
  syncStyle bg border color cursor opacity =
    "background: " <> bg <> "; border: 1px solid " <> border <> "; "
      <> "color: " <> color <> "; cursor: " <> cursor <> "; "
      <> "font-size: 9px; padding: 2px 6px; border-radius: 3px; opacity: " <> opacity <> ";"
