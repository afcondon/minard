-- | Canvas 2D Co-Change Cube
-- |
-- | 3D voxel rendering via Canvas 2D + purescript-linear quaternion camera.
-- | Follows the Lorenz attractor pattern: all projection and math in PureScript,
-- | Canvas FFI is just primitive draw calls.
module CE2.Viz.CoChangeCube
  ( -- Camera
    Camera
  , defaultCamera
  , rotateCamera
  , zoomCamera
    -- Viewport
  , Viewport
    -- Rendering
  , Context2D
  , getContext2D
  , render
    -- Types
  , VoxelSpec
  , SliceMode(..)
  , filterBySlice
  ) where

import Prelude

import Data.Array as Array
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Number (sqrt)
import Data.Traversable (for_)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, EffectFn4, EffectFn5, runEffectFn1, runEffectFn2, runEffectFn3, runEffectFn4, runEffectFn5)
import Linear.V2 (V2(..))
import Linear.V3 (V3(..))
import Linear.Quaternion (Quaternion(..), axisAngle, rotate, qmul)
import Web.HTML.HTMLCanvasElement (HTMLCanvasElement)

-- =============================================================================
-- Canvas FFI (uncurried EffectFn)
-- =============================================================================

foreign import data Context2D :: Type

foreign import getContext2DImpl :: EffectFn1 HTMLCanvasElement Context2D
foreign import clearRectImpl :: EffectFn5 Context2D Number Number Number Number Unit
foreign import setFillStyleImpl :: EffectFn2 Context2D String Unit
foreign import fillRectImpl :: EffectFn5 Context2D Number Number Number Number Unit
foreign import beginPathImpl :: EffectFn1 Context2D Unit
foreign import moveToImpl :: EffectFn3 Context2D Number Number Unit
foreign import lineToImpl :: EffectFn3 Context2D Number Number Unit
foreign import strokeImpl :: EffectFn1 Context2D Unit
foreign import setStrokeStyleImpl :: EffectFn2 Context2D String Unit
foreign import setLineWidthImpl :: EffectFn2 Context2D Number Unit
foreign import setGlobalAlphaImpl :: EffectFn2 Context2D Number Unit
foreign import fillCircleImpl :: EffectFn4 Context2D Number Number Number Unit
foreign import strokeRectImpl :: EffectFn5 Context2D Number Number Number Number Unit
foreign import fillTextImpl :: EffectFn4 Context2D String Number Number Unit
foreign import setFontImpl :: EffectFn2 Context2D String Unit
foreign import setTextAlignImpl :: EffectFn2 Context2D String Unit
foreign import setTextBaselineImpl :: EffectFn2 Context2D String Unit

getContext2D :: HTMLCanvasElement -> Effect Context2D
getContext2D = runEffectFn1 getContext2DImpl

clearRect :: Context2D -> Number -> Number -> Number -> Number -> Effect Unit
clearRect = runEffectFn5 clearRectImpl

setFillStyle :: Context2D -> String -> Effect Unit
setFillStyle = runEffectFn2 setFillStyleImpl

fillRect :: Context2D -> Number -> Number -> Number -> Number -> Effect Unit
fillRect = runEffectFn5 fillRectImpl

beginPath :: Context2D -> Effect Unit
beginPath = runEffectFn1 beginPathImpl

moveTo :: Context2D -> Number -> Number -> Effect Unit
moveTo = runEffectFn3 moveToImpl

lineTo :: Context2D -> Number -> Number -> Effect Unit
lineTo = runEffectFn3 lineToImpl

stroke :: Context2D -> Effect Unit
stroke = runEffectFn1 strokeImpl

setStrokeStyle :: Context2D -> String -> Effect Unit
setStrokeStyle = runEffectFn2 setStrokeStyleImpl

setLineWidth :: Context2D -> Number -> Effect Unit
setLineWidth = runEffectFn2 setLineWidthImpl

setGlobalAlpha :: Context2D -> Number -> Effect Unit
setGlobalAlpha = runEffectFn2 setGlobalAlphaImpl

fillCircle :: Context2D -> Number -> Number -> Number -> Effect Unit
fillCircle = runEffectFn4 fillCircleImpl

strokeRect :: Context2D -> Number -> Number -> Number -> Number -> Effect Unit
strokeRect = runEffectFn5 strokeRectImpl

fillText :: Context2D -> String -> Number -> Number -> Effect Unit
fillText = runEffectFn4 fillTextImpl

setFont :: Context2D -> String -> Effect Unit
setFont = runEffectFn2 setFontImpl

setTextAlign :: Context2D -> String -> Effect Unit
setTextAlign = runEffectFn2 setTextAlignImpl

setTextBaseline :: Context2D -> String -> Effect Unit
setTextBaseline = runEffectFn2 setTextBaselineImpl

-- =============================================================================
-- Types
-- =============================================================================

type Viewport = { width :: Number, height :: Number }

type Camera =
  { rotation :: Quaternion Number
  , distance :: Number  -- Fixed: controls perspective strength (large = mild)
  , zoom :: Number      -- Zoom level: scales projected coordinates
  }

type VoxelSpec =
  { moduleAIndex :: Int
  , moduleBIndex :: Int
  , commitIndex :: Int
  , color :: String
  , opacity :: Number
  }

data SliceMode
  = CommitSlice
  | ModuleASlice
  | ModuleBSlice
  | NoSlice

-- =============================================================================
-- Camera (quaternion trackball, following Lorenz.Camera)
-- =============================================================================

defaultCamera :: Camera
defaultCamera =
  { rotation: Quaternion 1.0 (V3 0.0 0.0 0.0)
  , distance: 6.0   -- Large distance = mild perspective (nearly orthographic)
  , zoom: 1.0
  }

rotateCamera :: Number -> V2 Number -> Camera -> Camera
rotateCamera sensitivity (V2 dx dy) camera =
  let
    angleX = dy * sensitivity
    angleY = dx * sensitivity
    rotY = axisAngle (V3 0.0 1.0 0.0) angleY
    rotX = axisAngle (V3 1.0 0.0 0.0) angleX
    newRotation = qmul rotX (qmul rotY camera.rotation)
  in
    camera { rotation = normalizeQ newRotation }

zoomCamera :: Number -> Camera -> Camera
zoomCamera factor camera =
  camera { zoom = max 0.3 (min 5.0 (camera.zoom * factor)) }

normalizeQ :: Quaternion Number -> Quaternion Number
normalizeQ (Quaternion w (V3 x y z)) =
  let len = sqrt (w * w + x * x + y * y + z * z)
  in if len > 0.0001
     then Quaternion (w / len) (V3 (x / len) (y / len) (z / len))
     else Quaternion 1.0 (V3 0.0 0.0 0.0)

-- =============================================================================
-- Projection (perspective, centered on unit cube origin)
-- =============================================================================

-- | Project a 3D point to 2D screen coordinates
-- | Uses fixed distance for mild perspective + zoom as post-projection scale
project :: Camera -> Viewport -> V3 Number -> V2 Number
project camera vp point =
  let
    -- Rotate world point into view space
    viewPoint = rotate camera.rotation point
    V3 vx vy vz = viewPoint
    -- Perspective: camera at (0, 0, -distance), fixed distance for stable perspective
    z' = vz + camera.distance
    perspScale = if z' > 0.1 then camera.distance / z' else 0.001
    -- Apply zoom as a separate scale (doesn't change perspective)
    s = perspScale * camera.zoom * 200.0
    -- Screen coordinates centered
    screenX = vx * s + vp.width / 2.0
    screenY = vp.height / 2.0 - vy * s
  in V2 screenX screenY

-- | Depth for sorting (higher = further from camera)
getDepth :: Camera -> V3 Number -> Number
getDepth camera point =
  let V3 _ _ vz = rotate camera.rotation point
  in vz

-- =============================================================================
-- Slice filtering (pure)
-- =============================================================================

filterBySlice :: SliceMode -> Int -> Array VoxelSpec -> Array VoxelSpec
filterBySlice mode idx voxels = case mode of
  NoSlice -> voxels
  CommitSlice -> Array.filter (\v -> v.commitIndex == idx) voxels
  ModuleASlice -> Array.filter (\v -> v.moduleAIndex == idx) voxels
  ModuleBSlice -> Array.filter (\v -> v.moduleBIndex == idx) voxels

-- =============================================================================
-- Rendering
-- =============================================================================

-- | Full render pass: clear, draw wireframe cube, draw voxels depth-sorted
render
  :: Context2D
  -> Camera
  -> Viewport
  -> { nModules :: Int, nCommits :: Int, moduleLabels :: Array String }
  -> Array VoxelSpec
  -> Effect Unit
render ctx camera vp meta voxels = do
  -- Clear
  setFillStyle ctx "#fafafa"
  fillRect ctx 0.0 0.0 vp.width vp.height

  -- Draw wireframe cube
  renderWireframe ctx camera vp

  -- Draw axis labels
  renderAxisLabels ctx camera vp

  -- Sort voxels by depth (far to near) and draw
  let
    nMod = max 1 meta.nModules
    nCom = max 1 meta.nCommits
    voxelSize = min 6.0 (180.0 / toNumber (max nMod nCom))

    withDepth = voxels <#> \v ->
      let pos = voxelPosition nMod nCom v
      in { voxel: v, pos: pos, depth: getDepth camera pos }

    sorted = Array.sortBy (comparing _.depth) withDepth

  for_ sorted \{ voxel, pos } -> do
    let V2 sx sy = project camera vp pos
        -- Size scales with perspective + zoom
        V3 _ _ vz = rotate camera.rotation pos
        z' = vz + camera.distance
        perspScale = if z' > 0.1 then camera.distance / z' else 0.001
        size = voxelSize * perspScale * camera.zoom

    setGlobalAlpha ctx voxel.opacity
    setFillStyle ctx voxel.color
    fillRect ctx (sx - size / 2.0) (sy - size / 2.0) size size

    -- Subtle border for depth cue
    setStrokeStyle ctx "rgba(0,0,0,0.15)"
    setLineWidth ctx 0.5
    strokeRect ctx (sx - size / 2.0) (sy - size / 2.0) size size

  setGlobalAlpha ctx 1.0

-- | Map voxel indices to 3D position in unit cube centered at origin
voxelPosition :: Int -> Int -> VoxelSpec -> V3 Number
voxelPosition nModules nCommits v =
  let
    -- Map to [-0.5, 0.5] so cube is centered at origin
    x = if nModules > 1
        then toNumber v.moduleAIndex / toNumber (nModules - 1) - 0.5
        else 0.0
    y = if nCommits > 1
        then toNumber v.commitIndex / toNumber (nCommits - 1) - 0.5
        else 0.0
    z = if nModules > 1
        then toNumber v.moduleBIndex / toNumber (nModules - 1) - 0.5
        else 0.0
  in V3 x y z

-- | Draw wireframe cube edges
renderWireframe :: Context2D -> Camera -> Viewport -> Effect Unit
renderWireframe ctx camera vp = do
  setStrokeStyle ctx "#cccccc"
  setLineWidth ctx 1.0
  setGlobalAlpha ctx 1.0

  -- 12 edges of a unit cube centered at origin
  let h = 0.5
      corners =
        [ V3 (-h) (-h) (-h), V3 h (-h) (-h), V3 h h (-h), V3 (-h) h (-h)
        , V3 (-h) (-h) h,    V3 h (-h) h,    V3 h h h,    V3 (-h) h h
        ]
      edges =
        [ {a: 0, b: 1}, {a: 1, b: 2}, {a: 2, b: 3}, {a: 3, b: 0}  -- front face
        , {a: 4, b: 5}, {a: 5, b: 6}, {a: 6, b: 7}, {a: 7, b: 4}  -- back face
        , {a: 0, b: 4}, {a: 1, b: 5}, {a: 2, b: 6}, {a: 3, b: 7}  -- connecting
        ]

  for_ edges \edge ->
    case Array.index corners edge.a, Array.index corners edge.b of
      Just ca, Just cb -> do
        let V2 x1 y1 = project camera vp ca
            V2 x2 y2 = project camera vp cb
        beginPath ctx
        moveTo ctx x1 y1
        lineTo ctx x2 y2
        stroke ctx
      _, _ -> pure unit

-- | Draw axis labels at the ends of the cube edges
renderAxisLabels :: Context2D -> Camera -> Viewport -> Effect Unit
renderAxisLabels ctx camera vp = do
  setFont ctx "11px 'Fira Code', monospace"
  setFillStyle ctx "#888888"
  setGlobalAlpha ctx 1.0

  -- Module A axis label (along X, at bottom-front)
  let V2 ax ay = project camera vp (V3 0.0 (-0.65) (-0.5))
  setTextAlign ctx "center"
  setTextBaseline ctx "middle"
  fillText ctx "Module A" ax ay

  -- Module B axis label (along Z, at bottom-right)
  let V2 bx by = project camera vp (V3 (-0.5) (-0.65) 0.0)
  fillText ctx "Module B" bx by

  -- Commit axis label (along Y, at left)
  let V2 cx cy = project camera vp (V3 (-0.65) 0.0 (-0.5))
  fillText ctx "Commits" cx cy
