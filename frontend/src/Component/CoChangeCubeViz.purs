-- | Co-Change Cube Visualization Component
-- |
-- | 3D module×module×commit tensor using Canvas 2D + purescript-linear.
-- | Quaternion trackball camera (drag to rotate, scroll to zoom).
-- | Self-contained data loading (like CommitModuleGridViz).
module CE2.Component.CoChangeCubeViz
  ( component
  , Input
  , Output(..)
  , Query(..)
  , Slot
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Int (round, toNumber)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Linear.V2 (V2(..))
import Web.HTML.HTMLCanvasElement as HTMLCanvas
import Web.Event.Event as WE
import Web.UIEvent.MouseEvent as MouseEvent
import Web.UIEvent.WheelEvent as WheelEvent
import CE2.Data.Loader as Loader
import CE2.Data.CoChange as CoChange
import CE2.Scene (Scene(..)) as Scene
import CE2.Viz.CoChangeCube as Cube

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
  , ctx :: Maybe Cube.Context2D
  , camera :: Cube.Camera
  , viewport :: Cube.Viewport
  , dragging :: Maybe (V2 Number)
  , voxels :: Array Cube.VoxelSpec
  , sliceMode :: Cube.SliceMode
  , sliceIndex :: Int
  , maxSliceIndex :: Int
  , voxelCount :: Int
  , nModules :: Int
  , nCommits :: Int
  }

-- =============================================================================
-- Actions
-- =============================================================================

data Action
  = Initialize
  | StartDrag MouseEvent.MouseEvent
  | Drag MouseEvent.MouseEvent
  | EndDrag MouseEvent.MouseEvent
  | Wheel WheelEvent.WheelEvent
  | SetSliceMode Cube.SliceMode
  | StepSlice Int
  | ResetCamera

canvasRef :: H.RefLabel
canvasRef = H.RefLabel "cube-canvas"

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
  , ctx: Nothing
  , camera: Cube.defaultCamera
  , viewport: { width: 900.0, height: 700.0 }
  , dragging: Nothing
  , voxels: []
  , sliceMode: Cube.NoSlice
  , sliceIndex: 0
  , maxSliceIndex: 0
  , voxelCount: 0
  , nModules: 0
  , nCommits: 0
  }

-- =============================================================================
-- Render
-- =============================================================================

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div [ HP.class_ (HH.ClassName "co-change-cube") ]
    [ HH.div [ HP.class_ (HH.ClassName "ccc-inner") ]
        [ renderHeader state
        , renderControls state
        , if state.loading
            then HH.div [ HP.class_ (HH.ClassName "ccc-loading") ] [ HH.text "Loading commit history..." ]
            else case state.error of
              Just err -> HH.div [ HP.class_ (HH.ClassName "ccc-error") ] [ HH.text err ]
              Nothing -> renderCanvas state
        ]
    ]

renderHeader :: forall m. State -> H.ComponentHTML Action () m
renderHeader state =
  HH.div [ HP.class_ (HH.ClassName "ccc-header") ]
    [ HH.h2 [] [ HH.text $ state.packageName <> " Co-Change Cube" ]
    , HH.p [ HP.class_ (HH.ClassName "ccc-subtitle") ]
        [ HH.text $ show (Array.length state.commits) <> " commits, "
            <> show (Array.length state.allModules) <> " modules, "
            <> show state.voxelCount <> " co-change pairs"
        ]
    ]

renderControls :: forall m. State -> H.ComponentHTML Action () m
renderControls state =
  HH.div [ HP.class_ (HH.ClassName "ccc-controls") ]
    [ HH.span [ HP.class_ (HH.ClassName "ccc-control-label") ] [ HH.text "Slice:" ]
    , sliceButton Cube.NoSlice "All"
    , sliceButton Cube.CommitSlice "Commit"
    , sliceButton Cube.ModuleASlice "Module A"
    , sliceButton Cube.ModuleBSlice "Module B"
    , case state.sliceMode of
        Cube.NoSlice -> HH.text ""
        _ -> HH.span [ HP.class_ (HH.ClassName "ccc-slice-nav") ]
          [ HH.button
              [ HP.class_ (HH.ClassName "ccc-control-btn")
              , HE.onClick \_ -> StepSlice (-1)
              , HP.disabled (state.sliceIndex <= 0)
              ]
              [ HH.text "<" ]
          , HH.span [ HP.class_ (HH.ClassName "ccc-slice-index") ]
              [ HH.text $ show (state.sliceIndex + 1) <> "/" <> show (state.maxSliceIndex + 1) ]
          , HH.button
              [ HP.class_ (HH.ClassName "ccc-control-btn")
              , HE.onClick \_ -> StepSlice 1
              , HP.disabled (state.sliceIndex >= state.maxSliceIndex)
              ]
              [ HH.text ">" ]
          ]
    , HH.span [ HP.class_ (HH.ClassName "ccc-control-sep") ] []
    , HH.button
        [ HP.class_ (HH.ClassName "ccc-control-btn")
        , HE.onClick \_ -> ResetCamera
        ]
        [ HH.text "Reset View" ]
    ]
  where
  sliceButton mode label =
    HH.button
      [ HP.class_ (HH.ClassName $ "ccc-control-btn" <> if eqSliceMode state.sliceMode mode then " active" else "")
      , HE.onClick \_ -> SetSliceMode mode
      ]
      [ HH.text label ]

renderCanvas :: forall m. State -> H.ComponentHTML Action () m
renderCanvas state =
  HH.div [ HP.class_ (HH.ClassName "ccc-scene-wrapper") ]
    [ HH.canvas
        [ HP.ref canvasRef
        , HP.width (round state.viewport.width)
        , HP.height (round state.viewport.height)
        , HP.style "cursor: grab;"
        , HE.onMouseDown StartDrag
        , HE.onMouseMove Drag
        , HE.onMouseUp EndDrag
        , HE.onMouseLeave EndDrag
        , HE.onWheel Wheel
        ]
    ]

-- =============================================================================
-- Action Handlers
-- =============================================================================

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  Initialize -> do
    state <- H.get
    H.modify_ _ { loading = true, error = Nothing }
    result <- liftAff $ Loader.fetchCommitFiles 200 state.packageName
    case result of
      Left err -> do
        log $ "[CoChangeCube] Error: " <> err
        H.modify_ _ { loading = false, error = Just err }
      Right r -> do
        let modules = CoChange.reorderModules CoChange.ByCosimilarity r.commits r.allModules
            rawVoxels = CoChange.buildCubeVoxels r.commits modules
            coMatrix = buildPairCounts r.commits modules
            maxCount = Array.foldl max 1 (Array.fromFoldable (Map.values coMatrix))
            voxelSpecs = rawVoxels <#> \v ->
              let pairKey = pairKeyFromIndices v.moduleAIndex v.moduleBIndex
                  count = fromMaybe 1 (Map.lookup pairKey coMatrix)
                  heat = toNumber count / toNumber maxCount
              in { moduleAIndex: v.moduleAIndex
                 , moduleBIndex: v.moduleBIndex
                 , commitIndex: v.commitIndex
                 , color: heatColor heat
                 , opacity: 0.85
                 }

        log $ "[CoChangeCube] Loaded " <> show (Array.length r.commits) <> " commits, "
          <> show (Array.length modules) <> " modules, "
          <> show (Array.length voxelSpecs) <> " voxels"

        H.modify_ _
          { loading = false
          , commits = r.commits
          , allModules = modules
          , voxels = voxelSpecs
          , voxelCount = Array.length voxelSpecs
          , nModules = Array.length modules
          , nCommits = Array.length r.commits
          , maxSliceIndex = Array.length r.commits - 1
          }

        -- Get canvas context and do initial render
        mElem <- H.getRef canvasRef
        case mElem >>= HTMLCanvas.fromElement of
          Just canvas -> do
            ctx <- liftEffect $ Cube.getContext2D canvas
            H.modify_ _ { ctx = Just ctx }
            renderScene
          Nothing ->
            log "[CoChangeCube] Canvas not found"

  StartDrag event -> do
    let x = toNumber $ MouseEvent.clientX event
        y = toNumber $ MouseEvent.clientY event
    H.modify_ _ { dragging = Just (V2 x y) }

  Drag event -> do
    state <- H.get
    case state.dragging of
      Nothing -> pure unit
      Just (V2 lastX lastY) -> do
        let x = toNumber $ MouseEvent.clientX event
            y = toNumber $ MouseEvent.clientY event
            dx = x - lastX
            dy = y - lastY
            newCamera = Cube.rotateCamera 0.005 (V2 dx dy) state.camera
        H.modify_ _ { camera = newCamera, dragging = Just (V2 x y) }
        renderScene

  EndDrag _ ->
    H.modify_ _ { dragging = Nothing }

  Wheel event -> do
    -- Prevent page scroll when wheeling over the cube
    liftEffect $ WE.preventDefault (WheelEvent.toEvent event)
    state <- H.get
    let delta = WheelEvent.deltaY event
        factor = if delta > 0.0 then 1.05 else 0.95
        newCamera = Cube.zoomCamera factor state.camera
    H.modify_ _ { camera = newCamera }
    renderScene

  SetSliceMode mode -> do
    state <- H.get
    let maxIdx = case mode of
          Cube.CommitSlice -> Array.length state.commits - 1
          Cube.ModuleASlice -> Array.length state.allModules - 1
          Cube.ModuleBSlice -> Array.length state.allModules - 1
          Cube.NoSlice -> 0
    H.modify_ _ { sliceMode = mode, sliceIndex = 0, maxSliceIndex = maxIdx }
    renderScene

  StepSlice delta -> do
    state <- H.get
    let newIdx = max 0 (min (state.sliceIndex + delta) state.maxSliceIndex)
    H.modify_ _ { sliceIndex = newIdx }
    renderScene

  ResetCamera -> do
    H.modify_ _ { camera = Cube.defaultCamera }
    renderScene

-- | Re-render the 3D scene with current camera and slice state
renderScene :: forall m. MonadAff m => H.HalogenM State Action () Output m Unit
renderScene = do
  state <- H.get
  case state.ctx of
    Just ctx -> do
      let visible = Cube.filterBySlice state.sliceMode state.sliceIndex state.voxels
      liftEffect $ Cube.render ctx state.camera state.viewport
        { nModules: state.nModules, nCommits: state.nCommits, moduleLabels: state.allModules }
        visible
    Nothing -> pure unit

-- =============================================================================
-- Helpers
-- =============================================================================

eqSliceMode :: Cube.SliceMode -> Cube.SliceMode -> Boolean
eqSliceMode Cube.NoSlice Cube.NoSlice = true
eqSliceMode Cube.CommitSlice Cube.CommitSlice = true
eqSliceMode Cube.ModuleASlice Cube.ModuleASlice = true
eqSliceMode Cube.ModuleBSlice Cube.ModuleBSlice = true
eqSliceMode _ _ = false

buildPairCounts :: Array Loader.CommitFileEntry -> Array String -> Map.Map String Int
buildPairCounts commits modules =
  let
    moduleIndex = Map.fromFoldable $
      Array.mapWithIndex (\i m -> Tuple m i) modules
  in
    Array.foldl (\acc commit ->
      let indices = Array.sort $ Array.mapMaybe (\m -> Map.lookup m moduleIndex) commit.modules
      in Array.foldl (\acc' pair -> Map.alter (Just <<< (_ + 1) <<< fromMaybe 0) pair acc')
           acc (allPairKeys indices)
    ) Map.empty commits

allPairKeys :: Array Int -> Array String
allPairKeys indices = do
  i <- Array.range 0 (Array.length indices - 2)
  j <- Array.range (i + 1) (Array.length indices - 1)
  case Array.index indices i, Array.index indices j of
    Just a, Just b -> [ pairKeyFromIndices a b ]
    _, _ -> []

pairKeyFromIndices :: Int -> Int -> String
pairKeyFromIndices a b =
  let lo = min a b
      hi = max a b
  in show lo <> ":" <> show hi

heatColor :: Number -> String
heatColor t
  | t < 0.33 = "#4285f4"
  | t < 0.66 = "#f59e0b"
  | otherwise = "#ef4444"
