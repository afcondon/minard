-- | Snapshot Management Scene
-- |
-- | Pure Halogen HTML component for creating and cleaning up snapshots.
-- | Two sections: commit log picker (create) and existing snapshots (cleanup).
module CE2.Component.SnapshotManagementViz
  ( component
  , Input
  , Output(..)
  , Query(..)
  , Slot
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import CE2.Data.Loader as Loader
import CE2.Scene (Scene(..)) as Scene

-- =============================================================================
-- Types
-- =============================================================================

type Input = { dataReady :: Boolean }

data Output
  = NavigateToScene Scene.Scene
  | SnapshotCreated
  | SnapshotsDeleted

type Slot = H.Slot Query Output

data Query a = Noop a

-- =============================================================================
-- State
-- =============================================================================

data CreatePhase
  = CreateIdle
  | Creating
  | CreateSuccess Loader.LoadResult
  | CreateError String

derive instance eqCreatePhase :: Eq CreatePhase

data DeletePhase
  = DeleteIdle
  | DeleteConfirming
  | Deleting
  | DeleteDone

derive instance eqDeletePhase :: Eq DeletePhase

type State =
  { commits :: Array Loader.GitCommit
  , hasMoreCommits :: Boolean
  , commitOffset :: Int
  , snapshots :: Array Loader.SnapshotDetail
  , selectedHash :: Maybe String
  , label :: String
  , createPhase :: CreatePhase
  , selectedForDelete :: Set Int
  , deletePhase :: DeletePhase
  , loading :: Boolean
  }

-- =============================================================================
-- Actions
-- =============================================================================

data Action
  = Initialize
  | SelectCommit String
  | SetLabel String
  | DoCreate
  | LoadMoreCommits
  | ToggleDeleteSelection Int
  | SelectAllForDelete
  | SelectNoneForDelete
  | ConfirmDelete
  | CancelDelete
  | DoDelete
  | DismissResult
  | GoToScene Scene.Scene

-- =============================================================================
-- Component
-- =============================================================================

component :: forall m. MonadAff m => H.Component Query Input Output m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }

initialState :: Input -> State
initialState _input =
  { commits: []
  , hasMoreCommits: false
  , commitOffset: 0
  , snapshots: []
  , selectedHash: Nothing
  , label: ""
  , createPhase: CreateIdle
  , selectedForDelete: Set.empty
  , deletePhase: DeleteIdle
  , loading: true
  }

-- =============================================================================
-- Render
-- =============================================================================

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div [ HP.class_ (HH.ClassName "snapshot-management") ]
    [ HH.div [ HP.class_ (HH.ClassName "snapshot-management-inner") ]
        [ renderHeader
        , renderCreateSection state
        , renderExistingSnapshots state
        ]
    ]

renderHeader :: forall w. HH.HTML w Action
renderHeader =
  HH.div [ HP.class_ (HH.ClassName "snapshot-header") ]
    [ HH.h2 [] [ HH.text "Snapshot Management" ]
    , HH.p [ HP.class_ (HH.ClassName "snapshot-subtitle") ]
        [ HH.text "Create snapshots from git history to compare how your codebase has changed over time." ]
    ]

renderCreateSection :: forall m. State -> H.ComponentHTML Action () m
renderCreateSection state =
  HH.div [ HP.class_ (HH.ClassName "snapshot-create-section") ]
    [ HH.h3 [] [ HH.text "Create from commit" ]
    , renderCommitLog state
    , renderCreateControls state
    , renderCreateStatus state
    ]

renderCommitLog :: forall m. State -> H.ComponentHTML Action () m
renderCommitLog state =
  HH.div [ HP.class_ (HH.ClassName "commit-log") ]
    ( Array.concat
        [ map (renderCommitRow state.selectedHash) state.commits
        , if state.hasMoreCommits
            then [ HH.div [ HP.class_ (HH.ClassName "commit-show-more"), HE.onClick \_ -> LoadMoreCommits ]
                     [ HH.text "Show more..." ] ]
            else []
        , if state.loading
            then [ HH.div [ HP.class_ (HH.ClassName "commit-loading") ] [ HH.text "Loading..." ] ]
            else []
        ]
    )

renderCommitRow :: forall m. Maybe String -> Loader.GitCommit -> H.ComponentHTML Action () m
renderCommitRow selectedHash commit =
  let isSelected = selectedHash == Just commit.hash
      isLoaded = commit.hasSnapshot
      rowClass = "commit-row"
        <> (if isSelected then " selected" else "")
        <> (if isLoaded then " loaded" else "")
  in HH.div
    [ HP.class_ (HH.ClassName rowClass)
    , HE.onClick \_ -> if isLoaded then DoCreate else SelectCommit commit.hash
    ]
    [ HH.div [ HP.class_ (HH.ClassName "commit-radio") ]
        [ if isLoaded
            then HH.span [ HP.class_ (HH.ClassName "commit-loaded-indicator") ] [ HH.text "●" ]
            else HH.span [ HP.class_ (HH.ClassName (if isSelected then "radio-selected" else "radio-empty")) ]
                   [ HH.text (if isSelected then "●" else "○") ]
        ]
    , HH.div [ HP.class_ (HH.ClassName "commit-hash") ] [ HH.text commit.shortHash ]
    , HH.div [ HP.class_ (HH.ClassName "commit-message") ]
        [ HH.text commit.message
        , HH.span [ HP.class_ (HH.ClassName "commit-refs") ]
            (map (\ref -> HH.span [ HP.class_ (HH.ClassName "commit-ref-badge") ] [ HH.text ref ]) commit.refs)
        ]
    , HH.div [ HP.class_ (HH.ClassName "commit-time") ] [ HH.text commit.relativeDate ]
    ]

renderCreateControls :: forall m. State -> H.ComponentHTML Action () m
renderCreateControls state =
  HH.div [ HP.class_ (HH.ClassName "create-controls") ]
    [ HH.label [] [ HH.text "Label:" ]
    , HH.input
        [ HP.type_ HP.InputText
        , HP.placeholder "Optional snapshot label"
        , HP.value state.label
        , HE.onValueInput SetLabel
        , HP.class_ (HH.ClassName "label-input")
        ]
    , HH.button
        [ HP.class_ (HH.ClassName "btn-create")
        , HP.disabled (not canCreate)
        , HE.onClick \_ -> DoCreate
        ]
        [ HH.text (if state.createPhase == Creating then "Creating..." else "Create Snapshot") ]
    ]
  where
  canCreate = isJust state.selectedHash && state.createPhase /= Creating

renderCreateStatus :: forall m. State -> H.ComponentHTML Action () m
renderCreateStatus state = case state.createPhase of
  CreateIdle -> HH.text ""
  Creating ->
    HH.div [ HP.class_ (HH.ClassName "create-status creating") ]
      [ HH.text "Creating snapshot... (this may take a moment if a build is needed)" ]
  CreateSuccess result ->
    HH.div [ HP.class_ (HH.ClassName "create-status success") ]
      [ HH.text $ "Snapshot created successfully"
          <> case result.stats of
              Just s -> " (" <> show (fromMaybe 0 s.modules) <> " modules)"
              Nothing -> ""
      , HH.button [ HP.class_ (HH.ClassName "btn-dismiss"), HE.onClick \_ -> DismissResult ] [ HH.text "OK" ]
      ]
  CreateError err ->
    HH.div [ HP.class_ (HH.ClassName "create-status error") ]
      [ HH.text $ "Error: " <> err
      , HH.button [ HP.class_ (HH.ClassName "btn-dismiss"), HE.onClick \_ -> DismissResult ] [ HH.text "Dismiss" ]
      ]

renderExistingSnapshots :: forall m. State -> H.ComponentHTML Action () m
renderExistingSnapshots state =
  HH.div [ HP.class_ (HH.ClassName "snapshot-existing-section") ]
    [ HH.div [ HP.class_ (HH.ClassName "existing-header") ]
        [ HH.h3 [] [ HH.text "Existing Snapshots" ]
        , renderDeleteControls state
        ]
    , HH.div [ HP.class_ (HH.ClassName "snapshot-list") ]
        (map (renderSnapshotRow state) state.snapshots)
    , renderDeleteConfirm state
    ]

renderDeleteControls :: forall m. State -> H.ComponentHTML Action () m
renderDeleteControls state =
  HH.div [ HP.class_ (HH.ClassName "delete-controls") ]
    [ HH.button [ HP.class_ (HH.ClassName "btn-select"), HE.onClick \_ -> SelectAllForDelete ]
        [ HH.text "Select All" ]
    , HH.button [ HP.class_ (HH.ClassName "btn-select"), HE.onClick \_ -> SelectNoneForDelete ]
        [ HH.text "Select None" ]
    , HH.button
        [ HP.class_ (HH.ClassName "btn-delete")
        , HP.disabled (Set.isEmpty state.selectedForDelete || state.deletePhase == Deleting)
        , HE.onClick \_ -> ConfirmDelete
        ]
        [ HH.text $ "Delete Selected (" <> show (Set.size state.selectedForDelete) <> ")" ]
    ]

renderSnapshotRow :: forall m. State -> Loader.SnapshotDetail -> H.ComponentHTML Action () m
renderSnapshotRow state snap =
  let isSelected = Set.member snap.id state.selectedForDelete
      rowClass = "snapshot-row"
        <> (if snap.isCurrentCheckout then " current" else "")
        <> (if isSelected then " selected-for-delete" else "")
  in HH.div [ HP.class_ (HH.ClassName rowClass) ]
    [ HH.div [ HP.class_ (HH.ClassName "snapshot-checkbox") ]
        [ if snap.canDelete
            then HH.input
              [ HP.type_ HP.InputCheckbox
              , HP.checked isSelected
              , HE.onChecked \_ -> ToggleDeleteSelection snap.id
              ]
            else HH.text ""
        ]
    , HH.div [ HP.class_ (HH.ClassName "snapshot-info") ]
        [ HH.div [ HP.class_ (HH.ClassName "snapshot-id") ] [ HH.text $ "#" <> show snap.id ]
        , HH.div [ HP.class_ (HH.ClassName "snapshot-label") ]
            [ HH.text $ fromMaybe "—" snap.label ]
        , HH.div [ HP.class_ (HH.ClassName "snapshot-hash") ]
            [ HH.text $ fromMaybe "—" (map (\h -> String.take 7 h) snap.gitHash) ]
        , HH.div [ HP.class_ (HH.ClassName "snapshot-modules") ]
            [ HH.text $ show snap.moduleCount <> " modules" ]
        , if snap.isCurrentCheckout
            then HH.span [ HP.class_ (HH.ClassName "current-badge") ] [ HH.text "current" ]
            else HH.text ""
        ]
    ]

renderDeleteConfirm :: forall m. State -> H.ComponentHTML Action () m
renderDeleteConfirm state = case state.deletePhase of
  DeleteConfirming ->
    HH.div [ HP.class_ (HH.ClassName "delete-confirm") ]
      [ HH.text $ "Delete " <> show (Set.size state.selectedForDelete) <> " snapshot(s)? This removes worktrees and all associated data."
      , HH.button [ HP.class_ (HH.ClassName "btn-confirm-delete"), HE.onClick \_ -> DoDelete ] [ HH.text "Delete" ]
      , HH.button [ HP.class_ (HH.ClassName "btn-cancel"), HE.onClick \_ -> CancelDelete ] [ HH.text "Cancel" ]
      ]
  Deleting ->
    HH.div [ HP.class_ (HH.ClassName "delete-confirm deleting") ]
      [ HH.text "Deleting..." ]
  DeleteDone ->
    HH.div [ HP.class_ (HH.ClassName "delete-confirm done") ]
      [ HH.text "Snapshots deleted."
      , HH.button [ HP.class_ (HH.ClassName "btn-dismiss"), HE.onClick \_ -> DismissResult ] [ HH.text "OK" ]
      ]
  _ -> HH.text ""

-- =============================================================================
-- Actions
-- =============================================================================

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  Initialize -> do
    loadData

  SelectCommit hash -> do
    H.modify_ _ { selectedHash = Just hash }

  SetLabel label -> do
    H.modify_ _ { label = label }

  DoCreate -> do
    state <- H.get
    case state.selectedHash of
      Nothing -> pure unit
      Just hash -> do
        H.modify_ _ { createPhase = Creating }
        let mLabel = if state.label == "" then Nothing else Just state.label
        result <- liftAff $ Loader.createSnapshotFromRef hash mLabel
        case result of
          Left err -> do
            log $ "[SnapshotMgmt] Create error: " <> err
            H.modify_ _ { createPhase = CreateError err }
          Right loadResult ->
            if loadResult.success
              then do
                log "[SnapshotMgmt] Snapshot created successfully"
                H.modify_ _ { createPhase = CreateSuccess loadResult, selectedHash = Nothing, label = "" }
                loadData  -- Refresh both lists
                H.raise SnapshotCreated
              else
                H.modify_ _ { createPhase = CreateError (fromMaybe "Unknown loader error" loadResult.error) }

  LoadMoreCommits -> do
    state <- H.get
    let newOffset = state.commitOffset + 30
    result <- liftAff $ Loader.fetchGitLog 30 newOffset
    case result of
      Left err -> log $ "[SnapshotMgmt] Error loading more commits: " <> err
      Right r -> H.modify_ _
        { commits = state.commits <> r.commits
        , hasMoreCommits = r.hasMore
        , commitOffset = newOffset
        }

  ToggleDeleteSelection snapId -> do
    state <- H.get
    let newSet = if Set.member snapId state.selectedForDelete
          then Set.delete snapId state.selectedForDelete
          else Set.insert snapId state.selectedForDelete
    H.modify_ _ { selectedForDelete = newSet }

  SelectAllForDelete -> do
    state <- H.get
    let deletableIds = Array.mapMaybe
          (\s -> if s.canDelete then Just s.id else Nothing)
          state.snapshots
    H.modify_ _ { selectedForDelete = Set.fromFoldable deletableIds }

  SelectNoneForDelete -> do
    H.modify_ _ { selectedForDelete = Set.empty }

  ConfirmDelete -> do
    H.modify_ _ { deletePhase = DeleteConfirming }

  CancelDelete -> do
    H.modify_ _ { deletePhase = DeleteIdle }

  DoDelete -> do
    state <- H.get
    H.modify_ _ { deletePhase = Deleting }
    let ids = Array.fromFoldable state.selectedForDelete
    result <- liftAff $ Loader.deleteSnapshotsByIds ids
    case result of
      Left err -> do
        log $ "[SnapshotMgmt] Delete error: " <> err
        H.modify_ _ { deletePhase = DeleteIdle }
      Right _results -> do
        log "[SnapshotMgmt] Delete complete"
        H.modify_ _ { deletePhase = DeleteDone, selectedForDelete = Set.empty }
        loadData  -- Refresh
        H.raise SnapshotsDeleted

  DismissResult -> do
    H.modify_ _ { createPhase = CreateIdle, deletePhase = DeleteIdle }

  GoToScene scene -> do
    H.raise (NavigateToScene scene)

-- =============================================================================
-- Data Loading
-- =============================================================================

loadData :: forall m. MonadAff m => H.HalogenM State Action () Output m Unit
loadData = do
  H.modify_ _ { loading = true }

  -- Load commits and snapshot details in parallel
  commitResult <- liftAff $ Loader.fetchGitLog 30 0
  snapshotResult <- liftAff $ Loader.fetchSnapshotDetails

  case commitResult of
    Left err -> log $ "[SnapshotMgmt] Error loading commits: " <> err
    Right r -> H.modify_ _
      { commits = r.commits
      , hasMoreCommits = r.hasMore
      , commitOffset = 0
      }

  case snapshotResult of
    Left err -> log $ "[SnapshotMgmt] Error loading snapshots: " <> err
    Right snaps -> H.modify_ _ { snapshots = snaps }

  H.modify_ _ { loading = false }
