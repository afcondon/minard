-- | Galaxy Treemap Visualization Component
-- |
-- | A proper Halogen component for the full registry treemap.
-- | Manages its own rendering lifecycle and reacts to Input changes:
-- | - packages changed → full re-render
-- | - infraLayerThreshold changed → full re-render (dependency maps change)
-- | - theme/colorMode changed → full re-render (HATS trees embed colors)
-- |
-- | This follows the GalaxyBeeswarmViz pattern, replacing the Effect-rendered
-- | approach that required manual listener subscriptions in SceneCoordinator.
module CE2.Component.GalaxyTreemapViz
  ( component
  , Input
  , Output(..)
  , Query(..)
  , Slot
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl)
import Data.Int (toNumber)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS

import CE2.Containers as C
import CE2.Data.Loader as Loader
import CE2.Types (ViewTheme, ColorMode(..), CellContents(..), PackageReachability, projectPackages)
import CE2.Viz.PackageSetTreemap as Treemap

-- =============================================================================
-- Types
-- =============================================================================

-- | Input from parent - all configuration is external
type Input =
  { packages :: Array Loader.PackageSetPackage
  , theme :: ViewTheme
  , colorMode :: ColorMode
  , infraLayerThreshold :: Int  -- Hide deps to packages with topoLayer < threshold (0 = show all, 2 = hide layers 0-1)
  , modules :: Array Loader.V2ModuleListItem  -- All modules across all packages
  , gitStatus :: Maybe Loader.GitStatusData   -- Git status for module coloring
  , reachabilityData :: Maybe PackageReachability  -- For reachability coloring in galaxy view
  , reachabilityPeek :: Boolean                   -- True while R key held (peek overlay)
  }

-- | Output to parent
data Output
  = CircleClicked String          -- Circle click → SolarSwarm neighborhood
  | RectClicked String            -- Rect click → PkgTreemap drill-in
  | PackageHovered (Maybe String) -- Future: coordinated hover

-- | Slot type for parent component
type Slot = H.Slot Query Output

-- | Queries from parent component
data Query a
  = GetPositions (Array { name :: String, x :: Number, y :: Number, r :: Number } -> a)
  -- ^ Get current treemap cell positions (for Treemap → Beeswarm hero transition)

-- | Component state - only internal state, not copied from Input
-- | Uses lastInput for change detection, uses current input directly in handlers
type State =
  { actionListener :: Maybe (HS.Listener Action)  -- Internal: D3 callbacks -> Halogen
  , lastInput :: Input                              -- For change detection only
  }

-- | Actions
data Action
  = Initialize
  | Receive Input
  | Finalize
  | HandleCircleClick String   -- Circle click → neighborhood view
  | HandleRectClick String     -- Rect click → package treemap view

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
    let input = state.lastInput
    if Array.length input.packages > 0
      then do
        let config = buildTreemapConfig input
            positions = Treemap.computeCellPositions config input.packages
        pure $ Just $ reply positions
      else do
        log "[GalaxyTreemapViz] GetPositions query but no packages available"
        pure $ Just $ reply []

initialState :: Input -> State
initialState input =
  { actionListener: Nothing
  , lastInput: input
  }

-- =============================================================================
-- Render
-- =============================================================================

render :: forall m. State -> H.ComponentHTML Action () m
render _state =
  HH.div
    [ HP.id C.galaxyTreemapContainerId
    , HP.class_ (HH.ClassName "galaxy-treemap")
    , HP.style "position: absolute; top: 0; left: 0; width: 100%; height: 100%;"
    ]
    []

-- =============================================================================
-- Action Handlers
-- =============================================================================

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  Initialize -> do
    state <- H.get
    let input = state.lastInput
    log $ "[GalaxyTreemapViz] Initializing, packages=" <> show (Array.length input.packages)

    -- Set up subscription for D3 callbacks -> Halogen actions
    { emitter, listener } <- liftEffect HS.create
    void $ H.subscribe emitter
    H.modify_ _ { actionListener = Just listener }

    when (Array.length input.packages > 0) do
      renderTreemap input

  Receive input -> do
    state <- H.get
    let lastInput = state.lastInput

    -- Check what changed (compare to lastInput)
    let packagesChanged = Array.length input.packages /= Array.length lastInput.packages
        themeChanged = input.theme /= lastInput.theme
        colorModeChanged = input.colorMode /= lastInput.colorMode
        thresholdChanged = input.infraLayerThreshold /= lastInput.infraLayerThreshold
        modulesChanged = Array.length input.modules /= Array.length lastInput.modules
        gitStatusChanged = (input.gitStatus /= Nothing) /= (lastInput.gitStatus /= Nothing)
                        || (input.gitStatus /= lastInput.gitStatus)
        reachabilityChanged = input.reachabilityData /= lastInput.reachabilityData
        peekChanged = input.reachabilityPeek /= lastInput.reachabilityPeek

    -- Update lastInput for next comparison
    H.modify_ _ { lastInput = input }

    -- Any change requires full re-render (HATS trees embed all config)
    when (packagesChanged || themeChanged || colorModeChanged || thresholdChanged || modulesChanged || gitStatusChanged || reachabilityChanged || peekChanged) do
      log $ "[GalaxyTreemapViz] Input changed ("
          <> (if packagesChanged then "packages " else "")
          <> (if themeChanged then "theme " else "")
          <> (if colorModeChanged then "colorMode " else "")
          <> (if thresholdChanged then "threshold " else "")
          <> (if modulesChanged then "modules " else "")
          <> (if gitStatusChanged then "gitStatus " else "")
          <> (if reachabilityChanged then "reachability " else "")
          <> "), re-rendering"
      renderTreemap input

  HandleCircleClick packageName -> do
    log $ "[GalaxyTreemapViz] Circle clicked: " <> packageName
    H.raise (CircleClicked packageName)

  HandleRectClick packageName -> do
    log $ "[GalaxyTreemapViz] Rect clicked: " <> packageName
    H.raise (RectClicked packageName)

  Finalize -> do
    log "[GalaxyTreemapViz] Finalizing"
    liftEffect $ Treemap.cleanup C.galaxyTreemapContainer

-- =============================================================================
-- Rendering Helpers
-- =============================================================================

-- | Render the treemap with current input configuration
renderTreemap :: forall m. MonadAff m => Input -> H.HalogenM State Action () Output m Unit
renderTreemap input = do
  state <- H.get

  -- Build click handlers using action listener
  let onCircleClick = makeClickHandler state.actionListener HandleCircleClick
      onRectClick = makeClickHandler state.actionListener HandleRectClick

  let config = buildTreemapConfigWithHandlers input onRectClick onCircleClick

  log $ "[GalaxyTreemapViz] Rendering treemap with " <> show (Array.length input.packages)
      <> " packages, threshold=" <> show input.infraLayerThreshold
  liftEffect $ Treemap.renderWithHighlighting config input.packages

-- | Group modules by package name for circle packing
buildModulesByPackage :: Array Loader.V2ModuleListItem -> Map.Map String (Array Treemap.ModuleCircleData)
buildModulesByPackage modules =
  foldl (\acc m ->
    let entry = { name: m.name, loc: toNumber (fromMaybe 0 m.loc) }
    in Map.alter
      (Just <<< Array.cons entry <<< fromMaybe [])
      m.package.name
      acc
  ) Map.empty modules

-- | Compute effective color mode: peek overrides to Reachability when R held and data available
effectiveColorMode :: Input -> ColorMode
effectiveColorMode input
  | input.reachabilityPeek && isJust input.reachabilityData = Reachability
  | otherwise = input.colorMode

-- | Build treemap config from input (without click handlers, for pure computations)
buildTreemapConfig :: Input -> Treemap.Config
buildTreemapConfig input =
  { containerSelector: C.galaxyTreemapContainer
  , width: 1650.0
  , height: 900.0
  , projectPackages: Set.fromFoldable projectPackages
  , transitivePackages: computeTransitivePackages input.packages
  , theme: input.theme
  , cellContents: CellModuleCircles
  , onRectClick: Nothing
  , onCircleClick: Nothing
  , infraLayerThreshold: input.infraLayerThreshold
  , modulesByPackage: buildModulesByPackage input.modules
  , gitStatus: input.gitStatus
  , colorMode: effectiveColorMode input
  , reachabilityData: input.reachabilityData
  }

-- | Build treemap config from input with click handlers
buildTreemapConfigWithHandlers :: Input -> Maybe (String -> Effect Unit) -> Maybe (String -> Effect Unit) -> Treemap.Config
buildTreemapConfigWithHandlers input onRectClick onCircleClick =
  { containerSelector: C.galaxyTreemapContainer
  , width: 1650.0
  , height: 900.0
  , projectPackages: Set.fromFoldable projectPackages
  , transitivePackages: computeTransitivePackages input.packages
  , theme: input.theme
  , cellContents: CellModuleCircles
  , onRectClick: onRectClick
  , onCircleClick: onCircleClick
  , infraLayerThreshold: input.infraLayerThreshold
  , modulesByPackage: buildModulesByPackage input.modules
  , gitStatus: input.gitStatus
  , colorMode: effectiveColorMode input
  , reachabilityData: input.reachabilityData
  }

-- | Build click handler that routes D3 events to Halogen actions via the listener
makeClickHandler :: Maybe (HS.Listener Action) -> (String -> Action) -> Maybe (String -> Effect Unit)
makeClickHandler mListener mkAction = case mListener of
  Just listener -> Just $ \packageName ->
    HS.notify listener (mkAction packageName)
  Nothing -> Nothing

-- =============================================================================
-- Transitive Package Computation
-- =============================================================================

-- | Compute transitive packages from project dependencies
-- | This is used to mark packages as "transitive" (indirect dependencies)
-- | in the treemap visualization for visual differentiation.
computeTransitivePackages :: Array Loader.PackageSetPackage -> Set String
computeTransitivePackages packages =
  let
    depMap :: Map.Map String (Array String)
    depMap = Map.fromFoldable $ packages <#> \p -> Tuple p.name p.depends

    projectPkgNames :: Set String
    projectPkgNames = Set.fromFoldable projectPackages

    go :: Set String -> Set String -> Set String
    go frontier visited
      | Set.isEmpty frontier = visited
      | otherwise =
          let
            newDeps = foldl (\acc name ->
              case Map.lookup name depMap of
                Nothing -> acc
                Just deps -> Set.union acc (Set.fromFoldable deps)
              ) Set.empty frontier
            unvisited = Set.difference newDeps visited
            newVisited = Set.union visited frontier
          in go unvisited newVisited
  in
    Set.difference (go projectPkgNames Set.empty) projectPkgNames
