-- | Bubble Pack Beeswarm Visualization Component
-- |
-- | A Halogen component for the Powers of Ten intermediate view showing
-- | packages as circle-packed bubbles in a beeswarm layout. Each package
-- | is a large circle with its modules packed inside.
-- |
-- | Manages its own simulation handle and reacts to Input changes:
-- | - scope change → GUP (setScope)
-- | - theme/colorMode change → updateColors
-- | - data change → full re-render
module CE2.Component.BubblePackBeeswarmViz
  ( component
  , Input
  , InitialPosition
  , Output(..)
  , Query(..)
  , Slot
  ) where

import Prelude

import Data.Array as Array
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust, fromMaybe)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS

import CE2.Containers as C
import CE2.Data.Filter (filterNodesByScope, filterNodesByFocalPackage)
import CE2.Types (SimNode, Package, ViewTheme, ColorMode, BeeswarmScope)
import CE2.Viz.BubblePackBeeswarm as BubblePackBeeswarm

-- =============================================================================
-- Types
-- =============================================================================

-- | Position data for animated transitions (from beeswarm)
type InitialPosition = { name :: String, x :: Number, y :: Number, r :: Number }

-- | Input from parent - all configuration is external
type Input =
  { nodes :: Array SimNode
  , packages :: Array Package
  , scope :: BeeswarmScope
  , focalPackage :: Maybe String  -- When set, filter to show only this package's neighborhood
  , theme :: ViewTheme
  , colorMode :: ColorMode
  , initialPositions :: Maybe (Array InitialPosition)  -- For Beeswarm → SolarSwarm transition
  , moduleImports :: Map.Map String (Array String)     -- Module name -> modules it imports
  , moduleImportedBy :: Map.Map String (Array String)  -- Module name -> modules that import it
  }

-- | Output to parent
data Output
  = PackageClicked String  -- Package name
  | PackageHovered (Maybe String)
  | ModuleClicked String String  -- Package name, Module name
  | ModuleHovered String (Maybe String)  -- Package name, Maybe module name (Nothing = mouse left)

-- | Slot type for parent component
type Slot = H.Slot Query Output

-- | Queries from parent
data Query a
  = ForceRender a  -- Force a re-render

-- | Component state - only internal state, not copied from Input
-- | Uses lastInput for change detection, uses current input directly in handlers
type State =
  { handle :: Maybe BubblePackBeeswarm.BubblePackHandle  -- Internal: simulation handle
  , initialized :: Boolean                               -- Internal: first render done
  , actionListener :: Maybe (HS.Listener Action)         -- Internal: D3 callbacks -> Halogen
  , lastInput :: Input                                   -- For change detection only
  }

-- | Actions
data Action
  = Initialize
  | Receive Input
  | Finalize
  | HandlePackageClick String
  | HandlePackageHover (Maybe String)
  | HandleModuleClick String String  -- Package name, Module name
  | HandleModuleHover String (Maybe String)  -- Package name, Maybe module name

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
        , finalize = Just Finalize
        }
    }

initialState :: Input -> State
initialState input =
  { handle: Nothing
  , initialized: false
  , actionListener: Nothing
  , lastInput: input
  }

-- =============================================================================
-- Render
-- =============================================================================

render :: forall m. State -> H.ComponentHTML Action () m
render _state =
  HH.div
    [ HP.class_ (HH.ClassName "bubblepack-beeswarm-viz")
    , HP.style "position: relative; width: 100%; height: 100%;"
    ]
    [ HH.div
        [ HP.id C.bubblePackBeeswarmContainerId
        , HP.class_ (HH.ClassName "bubblepack-beeswarm")
        , HP.style "position: absolute; top: 0; left: 0; width: 100%; height: 100%;"
        ]
        []
    ]

-- =============================================================================
-- Action Handlers
-- =============================================================================

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  Initialize -> do
    state <- H.get
    let input = state.lastInput
    log $ "[BubblePackBeeswarmViz] Initializing, nodes=" <> show (Array.length input.nodes)
        <> ", packages=" <> show (Array.length input.packages)
        <> ", scope=" <> show input.scope
        <> ", hasHandle=" <> show (isJust state.handle)

    -- Set up subscription for D3 callbacks -> Halogen actions
    { emitter, listener } <- liftEffect HS.create
    void $ H.subscribe emitter
    H.modify_ _ { actionListener = Just listener }

    when (Array.length input.nodes > 0) do
      startVisualization input

  Receive input -> do
    state <- H.get
    let lastInput = state.lastInput

    -- Check what changed (compare to lastInput)
    let nodesChanged = Array.length input.nodes /= Array.length lastInput.nodes
        packagesChanged = Array.length input.packages /= Array.length lastInput.packages
        scopeChanged = input.scope /= lastInput.scope
        focalChanged = input.focalPackage /= lastInput.focalPackage
        themeChanged = input.theme /= lastInput.theme
        colorModeChanged = input.colorMode /= lastInput.colorMode

    -- Update lastInput for next comparison
    H.modify_ _ { lastInput = input }

    -- React to changes using input directly (not copied state)
    if nodesChanged || packagesChanged then do
      -- Full re-render needed
      log "[BubblePackBeeswarmViz] Data changed, re-rendering"
      startVisualization input
    else if scopeChanged || focalChanged then do
      -- Use GUP for scope or focal change
      log $ "[BubblePackBeeswarmViz] Scope/focal changed: scope=" <> show input.scope
          <> ", focal=" <> show input.focalPackage
      case state.handle of
        Just handle -> do
          -- Filter nodes based on scope and focal
          let filteredNodes = applyFilters input
              callbacks = makeCallbacks state.actionListener
          log $ "[BubblePackBeeswarmViz] Filtering to " <> show (Array.length filteredNodes) <> " nodes"
          liftEffect $ BubblePackBeeswarm.setScope handle callbacks filteredNodes
          -- Re-apply colors after GUP
          liftEffect $ BubblePackBeeswarm.updateColors
            C.bubblePackBeeswarmContainer
            input.theme
            input.colorMode
        Nothing -> do
          -- No handle - recover by re-initializing
          log "[BubblePackBeeswarmViz] WARNING: No handle for scope/focal change, re-initializing"
          startVisualization input
    else if themeChanged || colorModeChanged then do
      -- Just update colors in place
      log $ "[BubblePackBeeswarmViz] Theme/color changed, hasHandle=" <> show (isJust state.handle)
      liftEffect $ BubblePackBeeswarm.updateColors
        C.bubblePackBeeswarmContainer
        input.theme
        input.colorMode
    else
      pure unit

  Finalize -> do
    state <- H.get
    log $ "[BubblePackBeeswarmViz] Finalizing, hadHandle=" <> show (isJust state.handle)
    case state.handle of
      Just handle -> do
        log "[BubblePackBeeswarmViz] Stopping simulation"
        liftEffect handle.stop
      Nothing ->
        log "[BubblePackBeeswarmViz] No handle to stop"

  HandlePackageClick packageName -> do
    log $ "[BubblePackBeeswarmViz] Package clicked: " <> packageName
    H.raise (PackageClicked packageName)

  HandlePackageHover mPackageName -> do
    H.raise (PackageHovered mPackageName)

  HandleModuleClick packageName moduleName -> do
    log $ "[BubblePackBeeswarmViz] Module clicked: " <> packageName <> "/" <> moduleName
    H.raise (ModuleClicked packageName moduleName)

  HandleModuleHover packageName mModuleName -> do
    H.raise (ModuleHovered packageName mModuleName)

-- =============================================================================
-- Query Handlers
-- =============================================================================

handleQuery :: forall m a. MonadAff m => Query a -> H.HalogenM State Action () Output m (Maybe a)
handleQuery = case _ of
  ForceRender a -> do
    state <- H.get
    startVisualization state.lastInput
    pure (Just a)

-- =============================================================================
-- Internal Helpers
-- =============================================================================

-- | Apply filters to nodes
-- | When focalPackage is set, bypass scope filtering - the focal neighborhood defines the visible set
-- | Otherwise, filter by scope as normal
applyFilters :: Input -> Array SimNode
applyFilters input =
  case input.focalPackage of
    -- Focal package set: show the focal neighborhood (focal + deps + dependents)
    -- Scope filtering is bypassed - neighborhood overrides scope
    Just focalName -> filterNodesByFocalPackage focalName input.nodes input.packages
    -- No focal: filter by scope as normal
    Nothing -> filterNodesByScope input.scope input.nodes input.packages

-- | Start the visualization from scratch
-- | Takes input directly instead of reading from state
startVisualization :: forall m. MonadAff m => Input -> H.HalogenM State Action () Output m Unit
startVisualization input = do
  state <- H.get

  -- Stop existing simulation if any
  case state.handle of
    Just h -> liftEffect h.stop
    Nothing -> pure unit

  -- Filter nodes based on scope and focal
  let filteredNodes = applyFilters input

  let hasInitialPositions = isJust input.initialPositions
      positionCount = fromMaybe 0 (Array.length <$> input.initialPositions)

  log $ "[BubblePackBeeswarmViz] Starting with "
      <> show (Array.length filteredNodes) <> " filtered nodes"
      <> ", focal=" <> show input.focalPackage
      <> if hasInitialPositions then ", with " <> show positionCount <> " initial positions" else ""

  -- Build callbacks using the action listener
  let callbacks = makeCallbacks state.actionListener

  -- Render the bubblepack beeswarm with filtered nodes and optional positions
  handle <- liftEffect $ renderBubblePackBeeswarmWithPositions
    input.packages filteredNodes input.initialPositions
    input.moduleImports input.moduleImportedBy callbacks

  -- Store handle (initialPositions consumed from input, not stored in state)
  H.modify_ _ { handle = Just handle, initialized = true }
  log "[BubblePackBeeswarmViz] Handle stored successfully"

  -- Apply initial colors
  liftEffect $ BubblePackBeeswarm.updateColors
    C.bubblePackBeeswarmContainer
    input.theme
    input.colorMode

-- | Build callbacks that route D3 events to Halogen actions via the listener
makeCallbacks :: Maybe (HS.Listener Action) -> BubblePackBeeswarm.Callbacks
makeCallbacks mListener =
  { onPackageClick: \packageName -> case mListener of
      Just listener -> HS.notify listener (HandlePackageClick packageName)
      Nothing -> pure unit
  , onPackageHover: \mPackageName -> case mListener of
      Just listener -> HS.notify listener (HandlePackageHover mPackageName)
      Nothing -> pure unit
  , onModuleClick: \packageName moduleName -> case mListener of
      Just listener -> HS.notify listener (HandleModuleClick packageName moduleName)
      Nothing -> pure unit
  , onModuleHover: \packageName mModuleName -> case mListener of
      Just listener -> HS.notify listener (HandleModuleHover packageName mModuleName)
      Nothing -> pure unit
  }

-- | Render the bubblepack beeswarm visualization with optional initial positions
renderBubblePackBeeswarmWithPositions
  :: Array Package
  -> Array SimNode
  -> Maybe (Array InitialPosition)
  -> Map.Map String (Array String)  -- moduleImports
  -> Map.Map String (Array String)  -- moduleImportedBy
  -> BubblePackBeeswarm.Callbacks
  -> Effect BubblePackBeeswarm.BubblePackHandle
renderBubblePackBeeswarmWithPositions packages nodes mInitialPositions moduleImports moduleImportedBy callbacks = do
  -- Calculate max topo layer for X positioning spread
  let maxTopoLayer = Array.foldl (\acc n -> max acc n.topoLayer) 0 nodes
      config :: BubblePackBeeswarm.Config
      config =
        { containerSelector: C.bubblePackBeeswarmContainer
        , width: 1650.0
        , height: 900.0
        , packages: packages
        , maxTopoLayer: maxTopoLayer
        , moduleImports: moduleImports
        , moduleImportedBy: moduleImportedBy
        }
  case mInitialPositions of
    Just positions -> do
      log $ "[BubblePackBeeswarmViz] Rendering with " <> show (Array.length positions) <> " initial positions, maxTopoLayer=" <> show maxTopoLayer
      BubblePackBeeswarm.renderWithPositions config callbacks nodes positions
    Nothing ->
      BubblePackBeeswarm.render config callbacks nodes

