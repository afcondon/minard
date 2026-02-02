-- | Circle Pack Visualization Component (HATS Version)
-- |
-- | Demonstrates:
-- | - Circle packing using psd3-layout's packSiblingsMap
-- | - Force simulation for package positioning
-- | - Nested template pattern: modules packed inside package bubbles
-- |
-- | Packages are shown as force-positioned bubbles with their
-- | modules circle-packed inside.
-- |
-- | Uses renderNodes: false pattern - simulation runs the physics,
-- | HATS handles all rendering on each tick.
module CE2.Component.CirclePackViz
  ( component
  , Input
  , Output(..)
  , Slot
  , Query(..)
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl)
import Data.Int (toNumber)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable as Nullable
import Data.Number (sqrt)
import Data.Set as Set
import Data.String as String
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS

-- PSD3 HATS Imports
import Hylograph.HATS (Tree, elem, staticStr, thunkedStr, thunkedNum, forEach, withBehaviors, onMouseEnter, onMouseLeave, onClick)
import Hylograph.HATS.InterpreterTick (rerender)
import Hylograph.Internal.Selection.Types (ElementType(..))

-- Simulation imports
import Hylograph.Simulation
  ( runSimulation
  , Engine(..)
  , SimulationEvent(..)
  , SimulationHandle
  , subscribe
  , setup
  , collide
  , manyBody
  , positionX
  , positionY
  , withStrength
  , withRadius
  , withX
  , withY
  , static
  , dynamic
  )
import Hylograph.ForceEngine.Simulation (SimulationNode)

-- Circle packing from psd3-layout
import DataViz.Layout.Hierarchy.Pack (packSiblingsMap, Circle)

import CE2.Color as Color
import CE2.Containers as C
import CE2.Data.Loader as Loader

-- =============================================================================
-- Types
-- =============================================================================

-- | Input from parent (ViewCoordinator)
type Input =
  { packages :: Array Loader.V2Package
  , modules :: Array Loader.V2ModuleListItem
  , imports :: Array Loader.V2ModuleImports
  , focalPackage :: String  -- For three-layer layout: deps | focal | dependents
  }

-- | Output to parent
data Output
  = PackageClicked Int
  | PackageHovered (Maybe Int)

-- | Slot type for parent component
type Slot = H.Slot Query Output

-- | Queries from parent
data Query a
  = StartSimulation a
  | StopSimulation a

-- | Callbacks for user interactions
type Callbacks =
  { onPackageClick :: Int -> Effect Unit  -- Package ID
  , onPackageHover :: Maybe Int -> Effect Unit  -- Package ID or Nothing
  }

-- | Module circle (packed within a package)
type ModuleCircle =
  { id :: Int
  , name :: String
  , x :: Number
  , y :: Number
  , r :: Number
  }

-- | Row type for extra fields on package nodes
type PackageRow =
  ( name :: String
  , source :: String
  , moduleCount :: Int
  , r :: Number
  , modules :: Array ModuleCircle
  , layer :: Int
  )

-- | Package node for force simulation
-- | layer: -1 = deps (left), 0 = focal (center), 1 = dependents (right)
type PackageNode = SimulationNode PackageRow

-- | Handle type for the simulation
type PackageSimulationHandle = SimulationHandle PackageRow

-- | Component state - only internal state, not copied from Input
-- | Uses lastInput for change detection, uses current input directly in handlers
type State =
  { nodes :: Array PackageNode               -- Internal: computed from input
  , simulationRunning :: Boolean             -- Internal: simulation state
  , actionListener :: Maybe (HS.Listener Action)  -- Internal: D3 callbacks -> Halogen
  , lastInput :: Input                       -- For change detection only
  , simulationHandle :: Maybe PackageSimulationHandle  -- Handle for getting node positions
  , callbacks :: Callbacks                   -- Cached callbacks for rendering
  }

-- | Actions
data Action
  = Initialize
  | Receive Input
  | Finalize
  | SimulationTick { alpha :: Number }
  | SimulationCompleted
  | HandlePackageClick Int       -- Internal: D3 click -> Halogen output
  | HandlePackageHover (Maybe Int)  -- Internal: D3 hover -> Halogen output

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

-- | Dummy callbacks for initial state (replaced during initialization)
dummyCallbacks :: Callbacks
dummyCallbacks =
  { onPackageClick: \_ -> pure unit
  , onPackageHover: \_ -> pure unit
  }

initialState :: Input -> State
initialState input =
  { nodes: []
  , simulationRunning: false
  , actionListener: Nothing
  , lastInput: input
  , simulationHandle: Nothing
  , callbacks: dummyCallbacks
  }

-- =============================================================================
-- Render
-- =============================================================================

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  let
    -- Count packages by layer
    depsCount = Array.length $ Array.filter (\n -> n.layer == (-1)) state.nodes
    deptsCount = Array.length $ Array.filter (\n -> n.layer == 1) state.nodes
    focalPackage = state.lastInput.focalPackage
    modeLabel = if focalPackage == ""
      then "Overview: " <> show (Array.length state.nodes) <> " packages"
      else "Neighborhood: " <> focalPackage
         <> " | deps: " <> show depsCount
         <> " | dependents: " <> show deptsCount
  in
    HH.div
      [ HP.class_ (HH.ClassName "circlepack-viz") ]
      [ HH.p_
          [ HH.text $ modeLabel
              <> " | " <> if state.simulationRunning then "Running" else "Converged"
          ]
      , HH.element (HH.ElemName "div")
          [ HP.id C.circlePackContainerId ]
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
    log $ "[CirclePackViz] Initializing for focal: " <> input.focalPackage
    -- Set up subscription for D3 callbacks -> Halogen actions
    { emitter, listener } <- liftEffect HS.create
    void $ H.subscribe emitter

    -- Build callbacks and store them
    let callbacks = makeCallbacks (Just listener)
    H.modify_ _
      { actionListener = Just listener
      , callbacks = callbacks
      }
    startVisualization input callbacks

  Receive input -> do
    state <- H.get
    let lastInput = state.lastInput

    -- Check what changed (compare to lastInput)
    let packagesChanged = Array.length input.packages /= Array.length lastInput.packages
        focalChanged = input.focalPackage /= lastInput.focalPackage

    -- Update lastInput for next comparison
    H.modify_ _ { lastInput = input }

    when (packagesChanged || focalChanged) do
      log $ "[CirclePackViz] Input changed, restarting for focal: " <> input.focalPackage
      startVisualization input state.callbacks

  Finalize -> do
    log "[CirclePackViz] Finalizing"

  SimulationTick _ -> do
    state <- H.get
    H.modify_ _ { simulationRunning = true }
    -- Re-render nodes with current positions
    case state.simulationHandle of
      Just handle -> liftEffect do
        currentNodes <- handle.getNodes
        renderNodesHATS currentNodes state.callbacks
      Nothing -> pure unit

  SimulationCompleted -> do
    log "[CirclePackViz] Simulation converged"
    state <- H.get
    H.modify_ _ { simulationRunning = false }
    -- Final render
    case state.simulationHandle of
      Just handle -> liftEffect do
        currentNodes <- handle.getNodes
        renderNodesHATS currentNodes state.callbacks
      Nothing -> pure unit

  HandlePackageClick pkgId -> do
    log $ "[CirclePackViz] Package clicked: " <> show pkgId
    H.raise (PackageClicked pkgId)

  HandlePackageHover maybePkgId -> do
    H.raise (PackageHovered maybePkgId)

-- | Start the visualization from scratch
-- | Takes input directly instead of reading from state
startVisualization :: forall m. MonadAff m => Input -> Callbacks -> H.HalogenM State Action () Output m Unit
startVisualization input callbacks = do
  state <- H.get
  let nodes = preparePackages input.packages input.modules input.imports input.focalPackage

  H.modify_ _
    { nodes = nodes
    , simulationRunning = true
    }

  log $ "[CirclePackViz] Prepared " <> show (Array.length nodes) <> " package nodes for focal: " <> input.focalPackage

  -- Get the listener for tick notifications
  let mListener = state.actionListener

  liftEffect do
    -- Render SVG container first
    renderSVGContainerHATS

    -- Start simulation
    handle <- startSimulation nodes callbacks mListener
    pure unit

  -- Store the handle so we can get node positions on tick
  -- Note: We need to capture the handle from the Effect, which requires restructuring
  -- For now, we render in the tick handler which gets the handle from state

-- | Build callbacks that route D3 events to Halogen actions via the listener
makeCallbacks :: Maybe (HS.Listener Action) -> Callbacks
makeCallbacks mListener =
  { onPackageClick: \pkgId -> case mListener of
      Just listener -> HS.notify listener (HandlePackageClick pkgId)
      Nothing -> pure unit
  , onPackageHover: \maybePkgId -> case mListener of
      Just listener -> HS.notify listener (HandlePackageHover maybePkgId)
      Nothing -> pure unit
  }

-- =============================================================================
-- Query Handlers
-- =============================================================================

handleQuery :: forall m a. MonadAff m => Query a -> H.HalogenM State Action () Output m (Maybe a)
handleQuery = case _ of
  StartSimulation a -> do
    state <- H.get
    when (Array.length state.lastInput.packages > 0) do
      startVisualization state.lastInput state.callbacks
    pure (Just a)

  StopSimulation a -> do
    pure (Just a)

-- =============================================================================
-- Circle Packing
-- =============================================================================

-- | Group modules by package ID
groupModulesByPackage :: Array Loader.V2ModuleListItem -> Map Int (Array Loader.V2ModuleListItem)
groupModulesByPackage modules =
  foldl addModule Map.empty modules
  where
  addModule acc mod =
    Map.alter (Just <<< Array.cons mod <<< fromMaybe []) mod.package.id acc

-- | Prepare all packages with circle packing and layer assignment
-- | Layer: -1 = deps (left), 0 = focal (center), 1 = dependents (right)
preparePackages :: Array Loader.V2Package -> Array Loader.V2ModuleListItem -> Array Loader.V2ModuleImports -> String -> Array PackageNode
preparePackages packages modules imports focalPackage =
  let
    modulesByPkg = groupModulesByPackage modules

    -- Build module name -> package name mapping
    moduleToPackage :: Map String String
    moduleToPackage = Map.fromFoldable $
      modules <#> \m -> Tuple m.name m.package.name

    -- Build module imports map
    importsMap :: Map Int (Array String)
    importsMap = Map.fromFoldable $
      imports <#> \mi -> Tuple mi.moduleId mi.imports

    -- Find deps: packages that focal imports
    focalModules = Array.filter (\m -> m.package.name == focalPackage) modules
    depPkgNames :: Set.Set String
    depPkgNames = foldl findDeps Set.empty focalModules
      where
      findDeps acc mod =
        let
          moduleImports = fromMaybe [] $ Map.lookup mod.id importsMap
          importedPkgs = Array.mapMaybe (\impName -> Map.lookup impName moduleToPackage) moduleImports
          externalDeps = Array.filter (_ /= focalPackage) importedPkgs
        in
          Set.union acc (Set.fromFoldable externalDeps)

    -- Find dependents: packages whose modules import the focal package
    dependentPkgNames :: Set.Set String
    dependentPkgNames = foldl findDependents Set.empty modules
      where
      findDependents acc mod
        | mod.package.name == focalPackage = acc
        | otherwise =
            let
              moduleImports = fromMaybe [] $ Map.lookup mod.id importsMap
              importedPkgs = Set.fromFoldable $ Array.mapMaybe (\impName -> Map.lookup impName moduleToPackage) moduleImports
            in
              if Set.member focalPackage importedPkgs
                then Set.insert mod.package.name acc
                else acc

    -- Assign layer to each package
    -- When focalPackage is empty, all packages get layer 0 (centered)
    getLayer :: String -> Int
    getLayer name
      | focalPackage == "" = 0  -- Overview mode: all centered
      | name == focalPackage = 0
      | Set.member name depPkgNames = -1
      | Set.member name dependentPkgNames = 1
      | otherwise = 0  -- Shouldn't happen with filtered data

    -- Filter to packages with modules, sorted by layer then module count
    packagesWithModules = packages
      # Array.filter (\p -> p.moduleCount > 0)
      # Array.sortBy (\a b ->
          let layerA = getLayer a.name
              layerB = getLayer b.name
          in case compare layerA layerB of
               EQ -> compare b.moduleCount a.moduleCount
               other -> other)
  in
    packagesWithModules # Array.mapWithIndex \idx pkg ->
      let pkgModules = fromMaybe [] $ Map.lookup pkg.id modulesByPkg
          layer = getLayer pkg.name
      in createPackedPackage idx pkg pkgModules layer

-- | Create a packed package from data
-- | layer: -1 = deps (left), 0 = focal (center), 1 = dependents (right)
createPackedPackage :: Int -> Loader.V2Package -> Array Loader.V2ModuleListItem -> Int -> PackageNode
createPackedPackage idx pkg modules layer =
  let
    -- Create circles for each module
    moduleCircles :: Array Circle
    moduleCircles = modules # map \m ->
      { x: 0.0
      , y: 0.0
      , r: 5.0 + sqrt (toNumber (String.length m.name)) * 1.5
      }

    -- Pack the modules
    packed = packSiblingsMap moduleCircles

    -- Convert to ModuleCircle with IDs
    packedModules :: Array ModuleCircle
    packedModules = Array.zipWith toModuleCircle modules packed.circles

    -- Initial position based on layer (spread horizontally)
    -- layer -1 (deps): left, layer 0 (focal): center, layer 1 (dependents): right
    layerX = toNumber layer * 350.0
    row = idx / 6  -- vertical spread within layer
    y = (toNumber row - 2.0) * 100.0

    -- Package radius with padding
    r = packed.radius + 10.0
  in
    { id: pkg.id
    , x: layerX
    , y
    , vx: 0.0
    , vy: 0.0
    , fx: Nullable.null
    , fy: Nullable.null
    , name: pkg.name
    , source: pkg.source
    , moduleCount: pkg.moduleCount
    , r
    , modules: packedModules
    , layer
    }
  where
  toModuleCircle :: Loader.V2ModuleListItem -> Circle -> ModuleCircle
  toModuleCircle m c = { id: m.id, name: m.name, x: c.x, y: c.y, r: c.r }

-- =============================================================================
-- Force Simulation
-- =============================================================================

startSimulation :: Array PackageNode -> Callbacks -> Maybe (HS.Listener Action) -> Effect PackageSimulationHandle
startSimulation nodes _callbacks mListener = do
  -- Three-layer layout: deps (left @ -350), focal (center @ 0), dependents (right @ 350)
  { handle, events } <- runSimulation
    { engine: D3
    , setup: setup "circlepack"
        [ manyBody "charge" # withStrength (static (-50.0))
        , collide "collision" # withRadius (dynamic \n -> n.r + 8.0)
        -- ForceX positions packages horizontally by layer
        , positionX "layerX" # withX (dynamic \n -> toNumber n.layer * 350.0) # withStrength (static 0.3)
        -- ForceY gently centers vertically with half-gap padding
        , positionY "centerY" # withY (static 0.0) # withStrength (static 0.05)
        ]
    , nodes: nodes
    , links: []
    , container: C.circlePackNodes
    , alphaMin: 0.001
    }

  -- Subscribe to events and notify Halogen
  _ <- subscribe events \event -> case event of
    Tick tickData -> case mListener of
      Just listener -> HS.notify listener (SimulationTick { alpha: tickData.alpha })
      Nothing -> pure unit
    Completed -> case mListener of
      Just listener -> HS.notify listener SimulationCompleted
      Nothing -> log "[CirclePackViz] Simulation converged"
    Started -> log "[CirclePackViz] Simulation started"
    Stopped -> pure unit

  pure handle

-- =============================================================================
-- HATS Rendering
-- =============================================================================

-- | Render SVG container using HATS
renderSVGContainerHATS :: Effect Unit
renderSVGContainerHATS = do
  let containerTree :: Tree
      containerTree =
        elem SVG
          [ staticStr "id" "circlepack-svg"
          , staticStr "viewBox" "-600 -400 1200 800"
          , staticStr "width" "100%"
          , staticStr "height" "700"
          , staticStr "style" "background: #1a1a2e; border-radius: 8px;"
          ]
          [ elem Group
              [ staticStr "id" C.circlePackNodesId
              , staticStr "class" "packages"
              ]
              []
          ]
  _ <- rerender C.circlePackContainer containerTree
  pure unit

-- | Render all package nodes using HATS
renderNodesHATS :: Array PackageNode -> Callbacks -> Effect Unit
renderNodesHATS nodes callbacks = do
  let nodesTree = createPackageNodesTree nodes callbacks
  _ <- rerender C.circlePackNodes nodesTree
  pure unit

-- | Create the nodes tree for HATS rendering
createPackageNodesTree :: Array PackageNode -> Callbacks -> Tree
createPackageNodesTree nodes callbacks =
  forEach "packages" Group nodes nodeKey (packageNodeHATS callbacks)
  where
  nodeKey :: PackageNode -> String
  nodeKey n = show n.id

-- | Package node template using HATS
-- | Uses layer-based coloring for three-layer layout
-- | Includes click/hover behaviors for navigation
packageNodeHATS :: Callbacks -> PackageNode -> Tree
packageNodeHATS callbacks node =
  withBehaviors
    [ onMouseEnter (callbacks.onPackageHover (Just node.id))
    , onMouseLeave (callbacks.onPackageHover Nothing)
    , onClick (callbacks.onPackageClick node.id)
    ]
  $ elem Group
      [ thunkedStr "transform" ("translate(" <> show node.x <> "," <> show node.y <> ")")
      , staticStr "class" "package-bubble"
      , staticStr "cursor" "pointer"
      ]
      ( [ -- Package enclosing circle - colored by layer
          elem Circle
            [ staticStr "cx" "0"
            , staticStr "cy" "0"
            , thunkedNum "r" node.r
            , thunkedStr "fill" (packageFillByLayer node.layer node.source)
            , staticStr "fill-opacity" "0.15"
            , thunkedStr "stroke" (packageStrokeByLayer node.layer node.source)
            , thunkedNum "stroke-width" (if node.layer == 0 then 3.0 else 2.0)
            , thunkedStr "stroke-opacity" (if node.layer == 0 then "0.9" else "0.5")
            ]
            []
        ]
        -- Module circles - nested
        <> map toModuleCircleHATS node.modules
        <>
        [ -- Package label
          elem Text
            [ thunkedNum "y" (node.r + 15.0)
            , staticStr "text-anchor" "middle"
            , thunkedStr "font-size" (if node.layer == 0 then "12" else "10")
            , thunkedStr "fill" (if node.layer == 0 then "#ffc040" else "#e0e0e0")
            , staticStr "font-family" "system-ui, sans-serif"
            , thunkedStr "textContent" (truncateName 25 node.name)
            ]
            []
        ]
      )

-- | Convert a module circle to HATS tree
toModuleCircleHATS :: ModuleCircle -> Tree
toModuleCircleHATS mod = elem Circle
  [ thunkedNum "cx" mod.x
  , thunkedNum "cy" mod.y
  , thunkedNum "r" mod.r
  , staticStr "fill" "rgba(255, 255, 255, 0.7)"
  , staticStr "stroke" "rgba(255, 255, 255, 0.9)"
  , staticStr "stroke-width" "0.5"
  , staticStr "class" "module-circle"
  ]
  []

-- | Package fill color - uses layer for three-layer layout coloring
-- | layer -1 (deps): blue tint, layer 0 (focal): gold/highlighted, layer 1 (dependents): green tint
packageFillByLayer :: Int -> String -> String
packageFillByLayer layer source = case layer of
  (-1) -> "#2a4a6a"  -- Deps: blue-gray
  0    -> case source of
           "workspace" -> "#3a5a8a"  -- Focal: brighter if workspace
           _ -> "#4a4a2a"  -- Focal: gold tint
  1    -> "#2a5a4a"  -- Dependents: green tint
  _    -> Color.packageColor source

-- | Package stroke color by layer
packageStrokeByLayer :: Int -> String -> String
packageStrokeByLayer layer _source = case layer of
  (-1) -> "#6a9acc"  -- Deps: lighter blue
  0    -> "#ffc040"  -- Focal: gold highlight
  1    -> "#6acc9a"  -- Dependents: lighter green
  _    -> "#9a9a9a"


truncateName :: Int -> String -> String
truncateName maxLen name =
  if String.length name > maxLen
    then String.take maxLen name <> "…"
    else name
