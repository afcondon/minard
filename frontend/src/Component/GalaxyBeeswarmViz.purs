-- | Galaxy Beeswarm Visualization Component
-- |
-- | A proper Halogen component for the package set beeswarm.
-- | Manages its own simulation handle and reacts to Input changes:
-- | - scope change → GUP (setScope)
-- | - theme/colorMode change → updateColors
-- | - packages change → full re-render
-- |
-- | This follows the pattern of BeeswarmViz but adds theming orthogonality.
module CE2.Component.GalaxyBeeswarmViz
  ( component
  , Input
  , InitialPosition
  , Output(..)
  , Query(..)
  , Slot
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Set as Set
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

import CE2.Containers as C
import CE2.Data.Filter (filterPackagesByScope)
import CE2.Data.Loader as Loader
import CE2.Types (ViewTheme, ColorMode(..), BeeswarmScope(..), CellContents(..), projectPackages)
import CE2.Viz.PackageSetBeeswarm as Beeswarm
import CE2.Viz.PackageSetTreemap as Treemap

-- =============================================================================
-- Types
-- =============================================================================

-- | Position data for animated transitions (from treemap)
type InitialPosition = { name :: String, x :: Number, y :: Number, r :: Number }

-- | Input from parent - all configuration is external
type Input =
  { packages :: Array Loader.PackageSetPackage
  , scope :: BeeswarmScope
  , theme :: ViewTheme
  , colorMode :: ColorMode
  , initialPositions :: Maybe (Array InitialPosition)  -- For Treemap → Beeswarm transition
  }

-- | Output to parent
data Output
  = PackageClicked String  -- Package name
  | PackageHovered (Maybe String)

-- | Slot type for parent component
type Slot = H.Slot Query Output

-- | Queries for parent component
data Query a
  = GetPositions (Array { name :: String, x :: Number, y :: Number, r :: Number } -> a)
  -- ^ Get current beeswarm node positions (for scene transitions)

-- | Component state - only internal state, not copied from Input
-- | Uses lastInput for change detection, uses current input directly in handlers
type State =
  { handle :: Maybe Beeswarm.BeeswarmHandle  -- Internal: simulation handle
  , initialized :: Boolean                    -- Internal: first render done
  , lastInput :: Input                        -- For change detection only
  }

-- | Actions
data Action
  = Initialize
  | Receive Input
  | Finalize

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

-- =============================================================================
-- Query Handlers
-- =============================================================================

handleQuery :: forall m a. MonadAff m => Query a -> H.HalogenM State Action () Output m (Maybe a)
handleQuery = case _ of
  GetPositions reply -> do
    state <- H.get
    case state.handle of
      Just handle -> do
        nodes <- liftEffect $ handle.simHandle.getNodes
        let positions = map (\n -> { name: n.pkg.name, x: n.x, y: n.y, r: n.r }) nodes
        pure $ Just $ reply positions
      Nothing -> do
        log "[GalaxyBeeswarmViz] GetPositions query but no handle available"
        pure $ Just $ reply []

initialState :: Input -> State
initialState input =
  { handle: Nothing
  , initialized: false
  , lastInput: input
  }

-- =============================================================================
-- Render
-- =============================================================================

render :: forall m. State -> H.ComponentHTML Action () m
render _state =
  HH.div
    [ HP.class_ (HH.ClassName "galaxy-beeswarm-viz")
    , HP.style "position: relative; width: 100%; height: 100%;"
    ]
    [ -- Treemap layer (below)
      HH.div
        [ HP.id C.galaxyBeeswarmTreemapContainerId
        , HP.class_ (HH.ClassName "galaxy-treemap")
        , HP.style "position: absolute; top: 0; left: 0; width: 100%; height: 100%; z-index: 1;"
        ]
        []
    -- Beeswarm layer (above)
    , HH.div
        [ HP.id C.galaxyBeeswarmContainerId
        , HP.class_ (HH.ClassName "galaxy-beeswarm")
        , HP.style "position: absolute; top: 0; left: 0; width: 100%; height: 100%; z-index: 2;"
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
    log $ "[GalaxyBeeswarmViz] Initializing, packages=" <> show (Array.length input.packages)
        <> ", hasHandle=" <> show (isJust state.handle)
    when (Array.length input.packages > 0) do
      startVisualization input

  Receive input -> do
    state <- H.get
    let lastInput = state.lastInput

    -- Check what changed (compare to lastInput)
    let packagesChanged = Array.length input.packages /= Array.length lastInput.packages
        scopeChanged = input.scope /= lastInput.scope
        themeChanged = input.theme /= lastInput.theme
        colorModeChanged = input.colorMode /= lastInput.colorMode

    -- Update lastInput for next comparison
    H.modify_ _ { lastInput = input }

    -- React to changes using input directly (not copied state)
    if packagesChanged then do
      -- Full re-render needed
      log "[GalaxyBeeswarmViz] Packages changed, re-rendering"
      startVisualization input
    else if scopeChanged then do
      -- Use GUP for scope change
      log $ "[GalaxyBeeswarmViz] Scope changed to " <> show input.scope
      case state.handle of
        Just handle -> do
          let filteredPkgs = filterPackagesByScope input.scope input.packages
          log $ "[GalaxyBeeswarmViz] Filtering to " <> show (Array.length filteredPkgs) <> " packages"
          liftEffect $ Beeswarm.setScope handle filteredPkgs
          -- Re-apply colors after GUP
          liftEffect $ Beeswarm.updateColors C.galaxyBeeswarmContainer input.theme input.colorMode
        Nothing -> do
          -- No handle - this shouldn't happen but recover by re-initializing
          log "[GalaxyBeeswarmViz] WARNING: No handle for scope change, re-initializing"
          startVisualization input
    else if themeChanged || colorModeChanged then do
      -- Just update colors in place
      log $ "[GalaxyBeeswarmViz] Theme/color changed, hasHandle=" <> show (isJust state.handle)
      liftEffect $ Treemap.updateColors C.galaxyBeeswarmTreemapContainer input.theme input.colorMode
      liftEffect $ Beeswarm.updateColors C.galaxyBeeswarmContainer input.theme input.colorMode
    else
      pure unit

  Finalize -> do
    state <- H.get
    log $ "[GalaxyBeeswarmViz] Finalizing, hadHandle=" <> show (isJust state.handle)
    case state.handle of
      Just handle -> do
        log "[GalaxyBeeswarmViz] Stopping simulation"
        liftEffect handle.stop
      Nothing ->
        log "[GalaxyBeeswarmViz] No handle to stop"
    -- Clean up containers to prevent ghosting on scene transition
    liftEffect $ Beeswarm.cleanup C.galaxyBeeswarmContainer
    liftEffect $ Treemap.cleanup C.galaxyBeeswarmTreemapContainer

-- | Start the visualization from scratch
-- | If initialPositions is provided (hero transition from treemap), uses those positions
-- | Takes input directly instead of reading from state
startVisualization :: forall m. MonadAff m => Input -> H.HalogenM State Action () Output m Unit
startVisualization input = do
  state <- H.get

  -- Stop existing simulation if any
  case state.handle of
    Just h -> liftEffect h.stop
    Nothing -> pure unit

  let hasInitialPositions = isJust input.initialPositions
      positionCount = fromMaybe 0 (Array.length <$> input.initialPositions)

  log $ "[GalaxyBeeswarmViz] Starting with " <> show (Array.length input.packages) <> " packages"
      <> if hasInitialPositions then ", with " <> show positionCount <> " initial positions (hero mode)" else ""

  -- Render treemap background
  -- During hero transition: render with CellEmpty (rectangles only, circles leave)
  -- Normal mode: render with CellCircle (full treemap)
  liftEffect $ renderTreemap input.theme input.packages hasInitialPositions

  -- Render beeswarm with filtered packages
  let filteredPkgs = filterPackagesByScope input.scope input.packages
  handle <- liftEffect $ renderBeeswarmWithPositions input.packages filteredPkgs input.colorMode input.initialPositions

  -- Store handle (initialPositions consumed from input, not stored in state)
  H.modify_ _ { handle = Just handle, initialized = true }
  log "[GalaxyBeeswarmViz] Handle stored successfully"

  -- Apply initial colors
  -- During hero transition: beeswarm starts with topo coloring (circles fly with their "plumage")
  -- Then treemap gets its colors too
  liftEffect $ Treemap.updateColors C.galaxyBeeswarmTreemapContainer input.theme input.colorMode
  if hasInitialPositions
    then liftEffect $ Beeswarm.updateColors C.galaxyBeeswarmContainer input.theme FullRegistryTopo
    else liftEffect $ Beeswarm.updateColors C.galaxyBeeswarmContainer input.theme input.colorMode

-- =============================================================================
-- Rendering Helpers
-- =============================================================================

-- | Render the treemap background for GalaxyBeeswarm scene
-- | Always uses CellText (text labels only) because circles live in the beeswarm layer
-- | The treemap is just a reference background showing package boundaries
renderTreemap :: ViewTheme -> Array Loader.PackageSetPackage -> Boolean -> Effect Unit
renderTreemap theme packages _heroMode = do
  let config :: Treemap.Config
      config =
        { containerSelector: C.galaxyBeeswarmTreemapContainer
        , width: 1650.0
        , height: 900.0
        , theme: theme
        , cellContents: CellText  -- Always text-only; circles are in beeswarm layer
        , projectPackages: Set.fromFoldable projectPackages
        , transitivePackages: Set.empty
        }
  Treemap.render config packages

-- | Render the beeswarm visualization with optional initial positions
-- | If positions are provided, uses them for hero transition animation
renderBeeswarmWithPositions
  :: Array Loader.PackageSetPackage
  -> Array Loader.PackageSetPackage
  -> ColorMode
  -> Maybe (Array InitialPosition)
  -> Effect Beeswarm.BeeswarmHandle
renderBeeswarmWithPositions allPackages filteredPackages colorMode mInitialPositions = do
  let maxLayer = Array.foldl (\acc pkg -> max acc pkg.topoLayer) 0 allPackages
      config :: Beeswarm.Config
      config =
        { containerSelector: C.galaxyBeeswarmContainer
        , width: 1650.0
        , height: 900.0
        , projectPackages: Set.fromFoldable projectPackages
        , maxTopoLayer: maxLayer
        , colorMode: colorMode
        , onPackageClick: Nothing  -- TODO: wire up click handler
        , enableHighlighting: true
        }

  case mInitialPositions of
    Just positions -> do
      -- Hero transition: render at captured positions
      log $ "[GalaxyBeeswarmViz] Rendering with " <> show (Array.length positions) <> " initial positions"
      Beeswarm.renderWithPositions config filteredPackages positions
    Nothing ->
      -- Normal render
      Beeswarm.render config filteredPackages

