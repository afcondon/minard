-- | Commit-Module Grid Visualization
-- |
-- | Dot matrix showing which modules changed in each commit.
-- | Y-axis = commits (newest at top), X-axis = modules in a package.
-- | Surfaces co-change patterns, stable modules, and codebase evolution.
module CE2.Component.CommitModuleGridViz
  ( component
  , Input
  , Output(..)
  , Query(..)
  , Slot
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (class Foldable)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Map as Map
import Data.Set (Set)
import Data.Set as Set
import Foreign.Object as Object
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import CE2.Data.Loader as Loader
import CE2.Data.CoChange as CoChange
import CE2.Scene (Scene(..), shortModuleName) as Scene

-- =============================================================================
-- Types
-- =============================================================================

type Input = { packageName :: String }

data Output = NavigateToScene Scene.Scene

type Slot = H.Slot Query Output

data Query a = Noop a

-- =============================================================================
-- State
-- =============================================================================

type State =
  { packageName :: String
  , commits :: Array Loader.CommitFileEntry
  , allModules :: Array String
  , loading :: Boolean
  , error :: Maybe String
  , hoveredModule :: Maybe String
  , hoveredCommit :: Maybe String
  , commitCount :: Int
  , orderMode :: CoChange.OrderMode
  , showBars :: Boolean
  , colorByOp :: Boolean  -- color dots by add/modify/delete
  }

-- =============================================================================
-- Actions
-- =============================================================================

data Action
  = Initialize
  | HoverModule (Maybe String)
  | HoverCommit (Maybe String)
  | ClickModule String
  | LoadMore
  | SetOrderMode CoChange.OrderMode
  | ToggleBars
  | ToggleColorByOp

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
initialState input =
  { packageName: input.packageName
  , commits: []
  , allModules: []
  , loading: true
  , error: Nothing
  , hoveredModule: Nothing
  , hoveredCommit: Nothing
  , commitCount: 80
  , orderMode: CoChange.ByCosimilarity
  , showBars: true
  , colorByOp: true
  }

-- =============================================================================
-- Render
-- =============================================================================

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div [ HP.class_ (HH.ClassName "commit-module-grid") ]
    [ HH.div [ HP.class_ (HH.ClassName "cmg-inner") ]
        [ renderHeader state
        , renderControls state
        , if state.loading
            then HH.div [ HP.class_ (HH.ClassName "cmg-loading") ] [ HH.text "Loading commit history..." ]
            else case state.error of
              Just err -> HH.div [ HP.class_ (HH.ClassName "cmg-error") ] [ HH.text err ]
              Nothing ->
                if Array.null state.allModules
                  then HH.div [ HP.class_ (HH.ClassName "cmg-empty") ]
                    [ HH.text "No commit history found for this package." ]
                  else renderGrid state
        ]
    ]

renderHeader :: forall m. State -> H.ComponentHTML Action () m
renderHeader state =
  HH.div [ HP.class_ (HH.ClassName "cmg-header") ]
    [ HH.h2 [] [ HH.text $ state.packageName <> " Commit History" ]
    , HH.p [ HP.class_ (HH.ClassName "cmg-subtitle") ]
        [ HH.text $ show (Array.length state.commits) <> " commits, "
            <> show (Array.length state.allModules) <> " modules changed"
        ]
    ]

renderControls :: forall m. State -> H.ComponentHTML Action () m
renderControls state =
  HH.div [ HP.class_ (HH.ClassName "cmg-controls") ]
    [ HH.span [ HP.class_ (HH.ClassName "cmg-control-label") ] [ HH.text "Order:" ]
    , orderButton CoChange.Alphabetical "A-Z" state.orderMode
    , orderButton CoChange.ByFrequency "Frequency" state.orderMode
    , orderButton CoChange.ByCosimilarity "Co-change" state.orderMode
    , HH.span [ HP.class_ (HH.ClassName "cmg-control-sep") ] []
    , HH.button
        [ HP.class_ (HH.ClassName $ "cmg-control-btn" <> if state.showBars then " active" else "")
        , HE.onClick \_ -> ToggleBars
        ]
        [ HH.text "Bars" ]
    , HH.button
        [ HP.class_ (HH.ClassName $ "cmg-control-btn" <> if state.colorByOp then " active" else "")
        , HE.onClick \_ -> ToggleColorByOp
        ]
        [ HH.text "A/M/D" ]
    ]
  where
  orderButton mode label current =
    HH.button
      [ HP.class_ (HH.ClassName $ "cmg-control-btn" <> if mode == current then " active" else "")
      , HE.onClick \_ -> SetOrderMode mode
      ]
      [ HH.text label ]

renderGrid :: forall m. State -> H.ComponentHTML Action () m
renderGrid state =
  let
    modules = CoChange.reorderModules state.orderMode state.commits state.allModules
    moduleCount = Array.length modules
    freqs = CoChange.moduleFrequencies state.commits
    maxFreq = foldlMax 1 (Map.values freqs)
    -- CSS grid template: commit-info columns + one column per module + optional breadth column
    gridTemplate = "140px 70px repeat(" <> show moduleCount <> ", 14px) 40px"
  in
  HH.div [ HP.class_ (HH.ClassName "cmg-grid-wrapper") ]
    [ HH.div
        [ HP.class_ (HH.ClassName "cmg-grid")
        , HP.style $ "grid-template-columns: " <> gridTemplate <> ";"
        ]
        ( -- Frequency bars row (optional)
          ( if state.showBars
              then [ HH.div [ HP.class_ (HH.ClassName "cmg-corner cmg-bar-row") ] []
                   , HH.div [ HP.class_ (HH.ClassName "cmg-corner cmg-bar-row") ] []
                   ]
                   <> map (renderFrequencyBar freqs maxFreq state) modules
                   <> [ HH.div [ HP.class_ (HH.ClassName "cmg-corner cmg-bar-row") ] [] ]
              else []
          )
          -- Header row: empty cells for commit info + module name headers
          <> [ HH.div [ HP.class_ (HH.ClassName "cmg-corner") ] []
             , HH.div [ HP.class_ (HH.ClassName "cmg-corner") ] []
             ]
          <> map (renderModuleHeader state) modules
          <> [ HH.div [ HP.class_ (HH.ClassName "cmg-corner cmg-breadth-header") ]
                 [ HH.text "#" ] ]
          -- Data rows
          <> Array.concatMap (renderCommitRow state modules) state.commits
        )
    , renderLoadMore state
    ]

renderFrequencyBar :: forall m. Map.Map String Int -> Int -> State -> String -> H.ComponentHTML Action () m
renderFrequencyBar freqs maxFreq state modName =
  let
    count = fromMaybe 0 (Map.lookup modName freqs)
    pct = if maxFreq > 0 then (count * 100) / maxFreq else 0
    isHovered = state.hoveredModule == Just modName
    cls = "cmg-freq-bar" <> if isHovered then " hovered" else ""
  in HH.div
    [ HP.class_ (HH.ClassName cls)
    , HE.onMouseEnter \_ -> HoverModule (Just modName)
    , HE.onMouseLeave \_ -> HoverModule Nothing
    , HP.title $ Scene.shortModuleName modName <> ": " <> show count <> " commits"
    ]
    [ HH.div
        [ HP.class_ (HH.ClassName "cmg-freq-bar-fill")
        , HP.style $ "height: " <> show pct <> "%;"
        ]
        []
    ]

renderModuleHeader :: forall m. State -> String -> H.ComponentHTML Action () m
renderModuleHeader state modName =
  let isHovered = state.hoveredModule == Just modName
      cls = "cmg-module-header" <> if isHovered then " hovered" else ""
  in HH.div
    [ HP.class_ (HH.ClassName cls)
    , HE.onMouseEnter \_ -> HoverModule (Just modName)
    , HE.onMouseLeave \_ -> HoverModule Nothing
    , HE.onClick \_ -> ClickModule modName
    , HP.title modName
    ]
    [ HH.span [ HP.class_ (HH.ClassName "cmg-module-label") ]
        [ HH.text (Scene.shortModuleName modName) ]
    ]

renderCommitRow :: forall m. State -> Array String -> Loader.CommitFileEntry -> Array (H.ComponentHTML Action () m)
renderCommitRow state modules commit =
  let
    isRowHovered = state.hoveredCommit == Just commit.hash
    changedSet = Set.fromFoldable commit.modules
    breadth = Array.length commit.modules
    rowCls suffix = "cmg-row-cell" <> suffix <> if isRowHovered then " row-hovered" else ""
  in
  [ -- Commit message cell
    HH.div
      [ HP.class_ (HH.ClassName $ rowCls " cmg-commit-msg")
      , HE.onMouseEnter \_ -> HoverCommit (Just commit.hash)
      , HE.onMouseLeave \_ -> HoverCommit Nothing
      , HP.title commit.message
      ]
      [ HH.text commit.message ]
  , -- Time cell
    HH.div
      [ HP.class_ (HH.ClassName $ rowCls " cmg-commit-time")
      , HE.onMouseEnter \_ -> HoverCommit (Just commit.hash)
      , HE.onMouseLeave \_ -> HoverCommit Nothing
      ]
      [ HH.text commit.relativeDate ]
  ]
  <> map (renderDot state commit changedSet commit.moduleStatuses) modules
  <> [ -- Breadth cell
       HH.div
         [ HP.class_ (HH.ClassName $ rowCls " cmg-breadth")
         , HE.onMouseEnter \_ -> HoverCommit (Just commit.hash)
         , HE.onMouseLeave \_ -> HoverCommit Nothing
         ]
         [ HH.text (show breadth) ]
     ]

renderDot :: forall m. State -> Loader.CommitFileEntry -> Set String -> Object.Object String -> String -> H.ComponentHTML Action () m
renderDot state commit changedSet statuses modName =
  let
    isActive = Set.member modName changedSet
    isColHovered = state.hoveredModule == Just modName
    isRowHovered = state.hoveredCommit == Just commit.hash
    status = Object.lookup modName statuses
    statusCls = if state.colorByOp
      then case status of
        Just "A" -> " status-add"
        Just "D" -> " status-delete"
        Just "R" -> " status-add"
        _ -> ""
      else ""
    cls = "cmg-dot"
      <> (if isActive then " active" else "")
      <> (if isColHovered then " col-hovered" else "")
      <> (if isRowHovered then " row-hovered" else "")
    statusLabel = case status of
      Just "A" -> " (added)"
      Just "D" -> " (deleted)"
      Just "R" -> " (renamed)"
      Just "M" -> " (modified)"
      _ -> ""
  in HH.div
    [ HP.class_ (HH.ClassName cls)
    , HE.onMouseEnter \_ -> HoverCommit (Just commit.hash)
    , HE.onMouseLeave \_ -> HoverCommit Nothing
    , HP.title $ if isActive
        then modName <> statusLabel <> " in " <> commit.shortHash <> ": " <> commit.message
        else ""
    ]
    [ if isActive
        then HH.div [ HP.class_ (HH.ClassName $ "cmg-dot-fill" <> statusCls) ] []
        else HH.text ""
    ]

renderLoadMore :: forall m. State -> H.ComponentHTML Action () m
renderLoadMore state =
  HH.div [ HP.class_ (HH.ClassName "cmg-load-more") ]
    [ HH.button
        [ HP.class_ (HH.ClassName "btn-load-more")
        , HE.onClick \_ -> LoadMore
        , HP.disabled state.loading
        ]
        [ HH.text "Show more commits..." ]
    ]

-- =============================================================================
-- Actions
-- =============================================================================

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  Initialize -> loadData

  HoverModule mMod ->
    H.modify_ _ { hoveredModule = mMod }

  HoverCommit mHash ->
    H.modify_ _ { hoveredCommit = mHash }

  ClickModule modName -> do
    state <- H.get
    H.raise (NavigateToScene (Scene.ModuleSignatureMap state.packageName modName))

  LoadMore -> do
    state <- H.get
    H.modify_ _ { commitCount = state.commitCount + 80 }
    loadData

  SetOrderMode mode ->
    H.modify_ _ { orderMode = mode }

  ToggleBars ->
    H.modify_ \s -> s { showBars = not s.showBars }

  ToggleColorByOp ->
    H.modify_ \s -> s { colorByOp = not s.colorByOp }

-- =============================================================================
-- Data Loading
-- =============================================================================

loadData :: forall m. MonadAff m => H.HalogenM State Action () Output m Unit
loadData = do
  state <- H.get
  H.modify_ _ { loading = true, error = Nothing }
  result <- liftAff $ Loader.fetchCommitFiles state.commitCount state.packageName
  case result of
    Left err -> do
      log $ "[CommitModuleGrid] Error: " <> err
      H.modify_ _ { loading = false, error = Just err }
    Right r -> do
      log $ "[CommitModuleGrid] Loaded " <> show (Array.length r.commits) <> " commits, "
        <> show (Array.length r.allModules) <> " modules"
      H.modify_ _ { loading = false, commits = r.commits, allModules = r.allModules }

-- =============================================================================
-- Helpers
-- =============================================================================

-- | Maximum value from a foldable, with a default
foldlMax :: forall f. Foldable f => Int -> f Int -> Int
foldlMax def xs = foldlDefault (\acc x -> if x > acc then x else acc) def xs

-- Can't use Data.Foldable.foldl on Map.values directly, need this helper
foldlDefault :: forall f a. Foldable f => (a -> a -> a) -> a -> f a -> a
foldlDefault f init xs = Array.foldl f init (Array.fromFoldable xs)
