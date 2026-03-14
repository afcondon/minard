-- | Project Management Scene
-- |
-- | Pure Halogen HTML component (no D3/canvas). Light theme, clean typographic layout.
-- | Project CRUD: list projects, add new ones, delete existing ones.
module CE2.Component.ProjectManagementViz
  ( component
  , Input
  , Output(..)
  , Query(..)
  , Slot
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import CE2.Data.Loader as Loader

-- =============================================================================
-- Types
-- =============================================================================

type Input =
  { projects :: Array Loader.ProjectInfo
  , dataReady :: Boolean
  }

data Output
  = ProjectAdded Loader.LoadResult
  | NavigateToProject Int
  | ProjectDeleted Int

data Query a = RefreshProjects (Array Loader.ProjectInfo) a

type Slot = H.Slot Query Output

-- | State machine for the add-project flow
data AddPhase
  = Idle
  | EnteringPath
  | Validating
  | ValidationResult Loader.PathValidation
  | LoadingProject
  | LoadSuccess Loader.LoadResult
  | LoadError String

derive instance eqAddPhase :: Eq AddPhase

type State =
  { projects :: Array Loader.ProjectInfo
  , dataReady :: Boolean
  , addPhase :: AddPhase
  , pathInput :: String
  , nameOverride :: String
  , confirmDeleteId :: Maybe Int
  }

data Action
  = Initialize
  | Receive Input
  | StartAddProject
  | CancelAdd
  | SetPathInput String
  | SetNameOverride String
  | DoValidate
  | DoLoad
  | DoDelete Int
  | ConfirmDelete Int
  | CancelDelete
  | ExploreProject Int

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
  { projects: input.projects
  , dataReady: input.dataReady
  , addPhase: if Array.null input.projects then EnteringPath else Idle
  , pathInput: ""
  , nameOverride: ""
  , confirmDeleteId: Nothing
  }

handleQuery :: forall m a. Query a -> H.HalogenM State Action () Output m (Maybe a)
handleQuery = case _ of
  RefreshProjects projects a -> do
    H.modify_ _ { projects = projects }
    pure (Just a)

-- =============================================================================
-- Render
-- =============================================================================

render :: forall m. MonadAff m => State -> H.ComponentHTML Action () m
render state =
  let projectCount = Array.length state.projects
      showForm = state.addPhase /= Idle || projectCount == 0
  in HH.div
    [ HP.style containerStyle ]
    [ HH.div
        [ HP.style "max-width: 1200px; width: 100%; margin: 0 auto; padding: 60px 24px 80px;" ]
        [ HH.h2
            [ HP.style sectionHeadingStyle ]
            [ HH.text "Projects" ]
        , if projectCount <= 1 then renderDemoState state showForm
          else renderPickerState state showForm
        ]
    ]

containerStyle :: String
containerStyle = "width: 100%; height: 100%; overflow-y: auto; background: #FAFAF8; "
  <> "font-family: -apple-system, 'Helvetica Neue', Helvetica, Arial, sans-serif; "
  <> "color: #333;"

-- | Fresh install: one project (the self-scan) or none
renderDemoState :: forall m. MonadAff m => State -> Boolean -> H.ComponentHTML Action () m
renderDemoState state showForm =
  let mProject = Array.head state.projects
  in HH.div_
    [ case mProject of
        Just project ->
          HH.div_
            [ HH.p
                [ HP.style sectionBodyStyle ]
                [ HH.text $ "Minard scanned its own codebase during setup \x2014 "
                    <> show project.stats.packageCount <> " packages, "
                    <> show project.stats.moduleCount <> " modules, "
                    <> show project.stats.declarationCount <> " declarations. "
                    <> "You can explore it right now to see how the tool works, or load your own PureScript project."
                ]
            , HH.div
                [ HP.style "display: flex; gap: 12px; margin-top: 16px; align-items: center;" ]
                [ HH.button
                    [ HE.onClick \_ -> ExploreProject 0
                    , HP.style "padding: 12px 28px; border: none; border-radius: 6px; cursor: pointer; font-size: 14px; font-weight: 600; background: #2D7D46; color: white; letter-spacing: 0.3px;"
                    , HP.disabled (not state.dataReady)
                    ]
                    [ HH.text "Explore This Codebase \x2192" ]
                , if not showForm
                    then HH.button
                      [ HE.onClick \_ -> StartAddProject
                      , HP.style "padding: 10px 20px; border: 1px solid #C0BDB4; border-radius: 6px; cursor: pointer; font-size: 13px; background: #fff; color: #555;"
                      ]
                      [ HH.text "Add Your Own Codebase" ]
                    else HH.text ""
                ]
            ]
        Nothing ->
          HH.p
            [ HP.style sectionBodyStyle ]
            [ HH.text "Point Minard at a built PureScript project. The loader will read your compiled output, resolve all dependencies, and populate the database. This typically takes 3-15 seconds." ]
    , if showForm
        then HH.div
          [ HP.style (cardStyle <> " margin-top: 20px;") ]
          [ renderAddForm state ]
        else HH.text ""
    ]

-- | Established install: multiple projects available
renderPickerState :: forall m. MonadAff m => State -> Boolean -> H.ComponentHTML Action () m
renderPickerState state showForm =
  HH.div_
    [ HH.p
        [ HP.style sectionBodyStyle ]
        [ HH.text "Choose a project to explore, or add another." ]
    , HH.div
        [ HP.style "display: grid; grid-template-columns: 1fr 1fr; gap: 12px; margin-top: 16px;" ]
        (map renderProjectCard state.projects)
    , if not showForm
        then HH.div
          [ HP.style "margin-top: 16px;" ]
          [ HH.button
              [ HE.onClick \_ -> StartAddProject
              , HP.style (buttonStyle <> " font-size: 13px;")
              ]
              [ HH.text "+ Add Another Project" ]
          ]
        else HH.div
          [ HP.style (cardStyle <> " margin-top: 20px;") ]
          [ renderAddForm state ]
    ]

renderProjectCard :: forall m. MonadAff m => Loader.ProjectInfo -> H.ComponentHTML Action () m
renderProjectCard project =
  HH.div
    [ HP.style "background: #fff; border: 1px solid #E0DDD4; border-radius: 6px; padding: 16px; display: flex; flex-direction: column; gap: 8px;" ]
    [ HH.div
        [ HP.style "font-size: 14px; font-weight: 600; color: #333;" ]
        [ HH.text project.name ]
    , HH.div
        [ HP.style "font-size: 11px; color: #888; font-family: 'SF Mono', 'Menlo', monospace;" ]
        [ HH.text project.repoPath ]
    , HH.div
        [ HP.style "font-size: 11px; color: #666;" ]
        [ HH.text $ show project.stats.packageCount <> " packages \x00B7 "
            <> show project.stats.moduleCount <> " modules \x00B7 "
            <> show project.stats.declarationCount <> " declarations"
        ]
    , HH.button
        [ HE.onClick \_ -> ExploreProject project.id
        , HP.style "align-self: flex-start; padding: 6px 16px; border: 1px solid #C0BDB4; border-radius: 4px; cursor: pointer; font-size: 12px; background: #F5F4F0; color: #333; margin-top: 4px;"
        ]
        [ HH.text "Explore \x2192" ]
    ]

-- =============================================================================
-- Shared Styles
-- =============================================================================

sectionHeadingStyle :: String
sectionHeadingStyle = "font-size: 18px; font-weight: 600; margin: 0 0 8px 0; letter-spacing: -0.3px; color: #333;"

sectionBodyStyle :: String
sectionBodyStyle = "font-size: 15px; color: #666; margin: 0 0 4px 0; line-height: 1.6; max-width: 800px;"

cardStyle :: String
cardStyle = "background: #fff; border: 1px solid #E0DDD4; border-radius: 6px; "
  <> "padding: 32px; box-shadow: 0 1px 3px rgba(0,0,0,0.06); margin-top: 16px;"

-- =============================================================================
-- Add Project Form
-- =============================================================================

renderAddForm :: forall m. MonadAff m => State -> H.ComponentHTML Action () m
renderAddForm state =
  HH.div_
    [ -- Path input
      HH.div
        [ HP.style "margin-bottom: 12px;" ]
        [ HH.label
            [ HP.style "display: block; font-size: 11px; font-weight: 500; color: #888; margin-bottom: 4px; text-transform: uppercase; letter-spacing: 0.3px;" ]
            [ HH.text "Project Path" ]
        , HH.div
            [ HP.style "display: flex; gap: 8px;" ]
            [ HH.input
                [ HP.type_ HP.InputText
                , HP.value state.pathInput
                , HP.placeholder "/path/to/your/purescript-project"
                , HE.onValueInput SetPathInput
                , HP.style inputStyle
                ]
            , case state.addPhase of
                Validating ->
                  HH.button
                    [ HP.style (buttonStyle <> " opacity: 0.6;")
                    , HP.disabled true
                    ]
                    [ HH.text "Checking..." ]
                LoadingProject ->
                  HH.button
                    [ HP.style (buttonStyle <> " opacity: 0.6;")
                    , HP.disabled true
                    ]
                    [ HH.text "Loading..." ]
                _ ->
                  HH.button
                    [ HE.onClick \_ -> DoValidate
                    , HP.style buttonStyle
                    , HP.disabled (state.pathInput == "")
                    ]
                    [ HH.text "Validate" ]
            ]
        ]

    -- Name override (optional, shown after validation succeeds)
    , case state.addPhase of
        ValidationResult v | v.valid ->
          HH.div
            [ HP.style "margin-bottom: 12px;" ]
            [ HH.label
                [ HP.style "display: block; font-size: 11px; font-weight: 500; color: #888; margin-bottom: 4px; text-transform: uppercase; letter-spacing: 0.3px;" ]
                [ HH.text "Project Name (optional)" ]
            , HH.input
                [ HP.type_ HP.InputText
                , HP.value state.nameOverride
                , HP.placeholder v.projectName
                , HE.onValueInput SetNameOverride
                , HP.style inputStyle
                ]
            ]
        _ -> HH.text ""

    -- Validation checklist
    , case state.addPhase of
        ValidationResult v -> renderChecklist v
        _ -> HH.text ""

    -- Load button / success / error
    , case state.addPhase of
        ValidationResult v | v.valid ->
          HH.div
            [ HP.style "margin-top: 16px;" ]
            [ HH.button
                [ HE.onClick \_ -> DoLoad
                , HP.style (buttonStyle <> " background: #2D7D46; color: white;")
                ]
                [ HH.text "Load Project" ]
            ]
        LoadingProject ->
          HH.div
            [ HP.style "margin-top: 16px; display: flex; align-items: center; gap: 8px;" ]
            [ HH.div
                [ HP.style "width: 200px; height: 4px; background: #E0DDD4; border-radius: 2px; overflow: hidden;" ]
                [ HH.div
                    [ HP.style "width: 60%; height: 100%; background: #4E79A7; border-radius: 2px; animation: pulse 1.5s ease-in-out infinite;" ]
                    []
                ]
            , HH.span
                [ HP.style "font-size: 12px; color: #888;" ]
                [ HH.text "Loading project..." ]
            ]
        LoadSuccess result ->
          HH.div
            [ HP.style "margin-top: 16px; padding: 16px; background: #F0F9F0; border: 1px solid #C3E6C3; border-radius: 4px;" ]
            [ HH.div
                [ HP.style "font-weight: 500; color: #2D7D46; margin-bottom: 8px;" ]
                [ HH.text "Project loaded successfully" ]
            , case result.elapsedMs of
                Just ms -> HH.p
                    [ HP.style "font-size: 11px; color: #666; margin: 0 0 12px 0;" ]
                    [ HH.text $ "Completed in " <> show (ms / 1000.0) <> "s" ]
                Nothing -> HH.text ""
            , HH.button
                [ HE.onClick \_ -> ExploreProject 0  -- Signal to navigate
                , HP.style "padding: 12px 32px; border: none; border-radius: 6px; cursor: pointer; font-size: 14px; font-weight: 600; background: #2D7D46; color: white; letter-spacing: 0.3px;"
                ]
                [ HH.text "Explore Your Code" ]
            ]
        LoadError err ->
          HH.div
            [ HP.style "margin-top: 16px; padding: 16px; background: #FDF0F0; border: 1px solid #E6C3C3; border-radius: 4px;" ]
            [ HH.div
                [ HP.style "font-weight: 500; color: #C0392B; margin-bottom: 4px;" ]
                [ HH.text "Load failed" ]
            , HH.p
                [ HP.style "font-size: 11px; color: #888; margin: 0; white-space: pre-wrap;" ]
                [ HH.text err ]
            , HH.button
                [ HE.onClick \_ -> DoLoad
                , HP.style (buttonStyle <> " margin-top: 8px;")
                ]
                [ HH.text "Retry" ]
            ]
        _ -> HH.text ""

    -- Cancel button (when not idle)
    , case state.addPhase of
        Idle -> HH.text ""
        LoadSuccess _ -> HH.text ""
        _ ->
          if not (Array.null state.projects) then
            HH.button
              [ HE.onClick \_ -> CancelAdd
              , HP.style "background: none; border: none; color: #999; cursor: pointer; font-size: 11px; margin-top: 8px; padding: 0;"
              ]
              [ HH.text "Cancel" ]
          else HH.text ""
    ]

inputStyle :: String
inputStyle = "flex: 1; padding: 8px 10px; border: 1px solid #D0CFC8; border-radius: 4px; "
  <> "font-size: 13px; font-family: 'SF Mono', 'Menlo', monospace; outline: none; "
  <> "background: #FAFAF8;"

buttonStyle :: String
buttonStyle = "padding: 8px 16px; border: 1px solid #C0BDB4; border-radius: 4px; "
  <> "cursor: pointer; font-size: 12px; background: #F5F4F0; color: #333;"

-- =============================================================================
-- Validation Checklist
-- =============================================================================

renderChecklist :: forall m. Loader.PathValidation -> H.ComponentHTML Action () m
renderChecklist v =
  HH.div
    [ HP.style "margin-top: 16px; padding: 12px 16px; background: #FAFAF8; border: 1px solid #E0DDD4; border-radius: 4px;" ]
    [ HH.div [ HP.style "font-size: 11px; font-weight: 500; color: #888; margin-bottom: 8px; text-transform: uppercase; letter-spacing: 0.3px;" ]
        [ HH.text "Prerequisites" ]
    , checkItem v.checks.directoryExists "Directory exists"
    , checkItem v.checks.spagoLockExists "spago.lock found"
    , checkItem v.checks.outputDirExists "output/ directory exists"
    , checkItem (v.checks.docsJsonCount > 0) ("docs.json files: " <> show v.checks.docsJsonCount)
    , checkItem v.checks.loaderBinaryExists "Loader binary found"
    -- Show issues with remediation messages
    , if Array.length v.issues > 0
        then HH.div
          [ HP.style "margin-top: 8px; padding-top: 8px; border-top: 1px solid #E0DDD4;" ]
          (map renderIssue v.issues)
        else HH.text ""
    ]

checkItem :: forall m w. Boolean -> String -> HH.HTML w m
checkItem ok label =
  HH.div
    [ HP.style "display: flex; align-items: center; gap: 6px; margin-bottom: 4px; font-size: 12px;" ]
    [ HH.span
        [ HP.style $ "font-size: 14px; " <> if ok then "color: #2D7D46;" else "color: #C0392B;" ]
        [ HH.text $ if ok then "\x2713" else "\x2717" ]
    , HH.span_ [ HH.text label ]
    ]

renderIssue :: forall m w. Loader.ValidationIssue -> HH.HTML w m
renderIssue issue =
  HH.div
    [ HP.style $ "font-size: 11px; margin-bottom: 4px; padding-left: 20px; "
        <> if issue.severity == "error" then "color: #C0392B;" else "color: #B8860B;"
    ]
    [ HH.text issue.message ]

-- =============================================================================
-- Action Handlers
-- =============================================================================

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  Initialize -> pure unit

  Receive input -> do
    state <- H.get
    -- If projects arrived and user hasn't started filling the form, switch to Idle
    let resetPhase = not (Array.null input.projects) && state.addPhase == EnteringPath && state.pathInput == ""
    H.modify_ _ { projects = input.projects, dataReady = input.dataReady
                 , addPhase = if resetPhase then Idle else state.addPhase }

  StartAddProject ->
    H.modify_ _ { addPhase = EnteringPath, pathInput = "", nameOverride = "" }

  CancelAdd ->
    H.modify_ _ { addPhase = Idle, pathInput = "", nameOverride = "" }

  SetPathInput s ->
    H.modify_ _ { pathInput = s }

  SetNameOverride s ->
    H.modify_ _ { nameOverride = s }

  DoValidate -> do
    state <- H.get
    H.modify_ _ { addPhase = Validating }
    result <- liftAff $ Loader.validateProjectPath state.pathInput
    case result of
      Left err -> do
        log $ "[ProjectMgmt] Validation error: " <> err
        H.modify_ _ { addPhase = LoadError ("Validation failed: " <> err) }
      Right validation ->
        H.modify_ _ { addPhase = ValidationResult validation }

  DoLoad -> do
    state <- H.get
    H.modify_ _ { addPhase = LoadingProject }
    let name = if state.nameOverride == "" then Nothing else Just state.nameOverride
    result <- liftAff $ Loader.loadProject
      { path: state.pathInput
      , name: name
      , label: Nothing
      }
    case result of
      Left err -> do
        log $ "[ProjectMgmt] Load error: " <> err
        H.modify_ _ { addPhase = LoadError err }
      Right loadResult ->
        if loadResult.success then do
          H.modify_ _ { addPhase = LoadSuccess loadResult }
          H.raise (ProjectAdded loadResult)
        else
          H.modify_ _ { addPhase = LoadError (fromMaybe "Unknown error" loadResult.error) }

  ConfirmDelete projectId ->
    H.modify_ _ { confirmDeleteId = Just projectId }

  CancelDelete ->
    H.modify_ _ { confirmDeleteId = Nothing }

  DoDelete projectId -> do
    H.modify_ _ { confirmDeleteId = Nothing }
    result <- liftAff $ Loader.deleteProject projectId
    case result of
      Left err ->
        log $ "[ProjectMgmt] Delete error: " <> err
      Right _ -> do
        H.modify_ \s -> s { projects = Array.filter (\p -> p.id /= projectId) s.projects }
        H.raise (ProjectDeleted projectId)

  ExploreProject projectId ->
    H.raise (NavigateToProject projectId)
