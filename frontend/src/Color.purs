-- | Color utilities for Code Explorer visualizations
-- |
-- | Semantic color scheme:
-- | - Registry packages: Blue scale (topo depth interpolation)
-- | - Workspace/extra packages: Gold/orange scale (topo depth interpolation)
-- | - Modules: Darker shade of parent package
module CE2.Color
  ( packageColor
  , packageColorByDepth
  , moduleColor
  ) where

import Prelude

import Data.Int (round, toNumber)
import Data.Number (floor)

-- =============================================================================
-- Types
-- =============================================================================

-- | HSL color representation for smooth interpolation
type HSL = { h :: Number, s :: Number, l :: Number }

-- | Color scale defined by start and end HSL values
type ColorScale = { start :: HSL, end :: HSL }

-- =============================================================================
-- Color Scales
-- =============================================================================

-- | Registry packages: Blue family
-- | Layer 0 (foundation) = light blue, deepest layer = dark blue
registryScale :: ColorScale
registryScale =
  { start: { h: 210.0, s: 80.0, l: 75.0 }  -- Light blue (#93c5fd-ish)
  , end:   { h: 210.0, s: 70.0, l: 35.0 }  -- Dark blue (#1e40af-ish)
  }

-- | Workspace/extra packages: Gold/orange family
-- | Layer 0 = light gold, deepest layer = deep orange
workspaceScale :: ColorScale
workspaceScale =
  { start: { h: 40.0, s: 90.0, l: 70.0 }   -- Light gold (#fcd34d-ish)
  , end:   { h: 25.0, s: 95.0, l: 45.0 }   -- Deep orange (#ea580c-ish)
  }

-- =============================================================================
-- Public API
-- =============================================================================

-- | Get package color based on source type and topological depth
-- |
-- | source: "registry", "workspace", or "extra"
-- | layer: topological layer (0 = no dependencies)
-- | maxLayers: total number of layers (for normalization)
packageColorByDepth :: String -> Int -> Int -> String
packageColorByDepth source layer maxLayers =
  let
    t = if maxLayers <= 1
          then 0.0
          else toNumber layer / toNumber (maxLayers - 1)
    scale = case source of
      "workspace" -> workspaceScale
      "extra" -> workspaceScale  -- Same as workspace for now
      _ -> registryScale         -- Registry packages
  in
    interpolateColor scale t

-- | Simple package color without depth (fallback)
packageColor :: String -> String
packageColor source = case source of
  "workspace" -> "#4a9eff"
  "extra" -> "#ff9f43"
  _ -> "#6c757d"

-- | Module color: darker shade of parent package color
-- | Takes the package color and darkens it
moduleColor :: String -> String
moduleColor packageHex =
  -- Simple approach: just return a darker variant
  -- In a full implementation, we'd parse the hex and adjust lightness
  packageHex  -- For now, same as package - can enhance later

-- =============================================================================
-- Color Interpolation
-- =============================================================================

-- | Interpolate between two HSL colors
-- | t: 0.0 = start, 1.0 = end
interpolateColor :: ColorScale -> Number -> String
interpolateColor scale t =
  let
    h = lerp scale.start.h scale.end.h t
    s = lerp scale.start.s scale.end.s t
    l = lerp scale.start.l scale.end.l t
  in
    hslToHex h s l

-- | Linear interpolation
lerp :: Number -> Number -> Number -> Number
lerp a b t = a + (b - a) * (clampNum 0.0 1.0 t)

-- | Clamp a value between min and max
clampNum :: Number -> Number -> Number -> Number
clampNum minVal maxVal x
  | x < minVal = minVal
  | x > maxVal = maxVal
  | otherwise = x

-- =============================================================================
-- HSL to Hex Conversion
-- =============================================================================

-- | Convert HSL to hex color string
-- | h: 0-360, s: 0-100, l: 0-100
hslToHex :: Number -> Number -> Number -> String
hslToHex h s l =
  let
    { r, g, b } = hslToRgb h s l
  in
    "#" <> toHex r <> toHex g <> toHex b

-- | Convert HSL to RGB (0-255)
hslToRgb :: Number -> Number -> Number -> { r :: Int, g :: Int, b :: Int }
hslToRgb h s l =
  let
    s' = s / 100.0
    l' = l / 100.0
    c = (1.0 - abs (2.0 * l' - 1.0)) * s'
    x = c * (1.0 - abs (mod' (h / 60.0) 2.0 - 1.0))
    m = l' - c / 2.0
    { r', g', b' } =
      if h < 60.0 then { r': c, g': x, b': 0.0 }
      else if h < 120.0 then { r': x, g': c, b': 0.0 }
      else if h < 180.0 then { r': 0.0, g': c, b': x }
      else if h < 240.0 then { r': 0.0, g': x, b': c }
      else if h < 300.0 then { r': x, g': 0.0, b': c }
      else { r': c, g': 0.0, b': x }
  in
    { r: round ((r' + m) * 255.0)
    , g: round ((g' + m) * 255.0)
    , b: round ((b' + m) * 255.0)
    }

-- | Modulo for Number (fmod)
mod' :: Number -> Number -> Number
mod' x y = x - y * floor (x / y)

-- | Absolute value
abs :: Number -> Number
abs x = if x < 0.0 then -x else x

-- | Convert 0-255 int to 2-digit hex
toHex :: Int -> String
toHex n =
  let
    n' = clampInt 0 255 n
    hi = n' / 16
    lo = n' `mod` 16
  in
    hexDigit hi <> hexDigit lo

clampInt :: Int -> Int -> Int -> Int
clampInt minVal maxVal x
  | x < minVal = minVal
  | x > maxVal = maxVal
  | otherwise = x

hexDigit :: Int -> String
hexDigit n = case n of
  0 -> "0"
  1 -> "1"
  2 -> "2"
  3 -> "3"
  4 -> "4"
  5 -> "5"
  6 -> "6"
  7 -> "7"
  8 -> "8"
  9 -> "9"
  10 -> "a"
  11 -> "b"
  12 -> "c"
  13 -> "d"
  14 -> "e"
  15 -> "f"
  _ -> "0"
