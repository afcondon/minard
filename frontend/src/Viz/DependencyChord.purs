-- | Dependency Chord Diagram (HATS Version)
-- |
-- | Renders a chord diagram showing dependencies between entities.
-- | Works with modules, packages, or any other entity type.
-- |
-- | Uses DataViz.Layout.Chord for layout computation.
module CE2.Viz.DependencyChord
  ( render
  , updateColors
  , Config
  , ChordHandle
  , RenderResult(..)
  , maxEntities
  ) where

import Prelude

import Data.Array as Array
import Data.Array ((!!))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (cos, sin, pi)
import Effect (Effect)
import Effect.Class.Console (log)

-- PSD3 HATS Imports
import Hylograph.HATS (Tree, elem, staticStr, staticNum)
import Hylograph.HATS.InterpreterTick (rerender)
import Hylograph.Internal.Selection.Types (ElementType(..))

-- Layout
import DataViz.Layout.Chord (layout, ChordLayout, ChordGroup, Chord)

import CE2.Types (ViewTheme, ColorMode)
import CE2.Viz.DependencyMatrix (DependencyData)

-- =============================================================================
-- Types
-- =============================================================================

-- | Maximum entities before we refuse to render
-- | 150 is manageable for chord diagrams
maxEntities :: Int
maxEntities = 150

type Config =
  { containerSelector :: String
  , width :: Number
  , height :: Number
  , innerRadius :: Number   -- Inner radius of the chord diagram
  , outerRadius :: Number   -- Outer radius (for arcs)
  }

type ChordHandle =
  { config :: Config
  , layout :: ChordLayout
  }

data RenderResult
  = Success ChordHandle
  | Overloaded Int  -- Number of entities that was requested

-- =============================================================================
-- Public API
-- =============================================================================

-- | Render a chord diagram from dependency data
render :: Config -> DependencyData -> Effect RenderResult
render config depData = do
  let n = Array.length depData.names

  log $ "[DependencyChord] Rendering with " <> show n
      <> " entities, " <> show depData.totalConnections <> " connections"

  -- Check upper bound
  if n > maxEntities
    then do
      log $ "[DependencyChord] OVERLOAD: " <> show n <> " entities exceeds max " <> show maxEntities
      renderOverloadMessage config n
      pure $ Overloaded n
    else do
      -- Compute layout with padding between groups
      let chordLayout = layout depData.matrix

      log $ "[DependencyChord] Layout: " <> show (Array.length chordLayout.groups) <> " groups, "
          <> show (Array.length chordLayout.chords) <> " chords"

      -- Render SVG
      renderChordDiagram config depData.names chordLayout

      pure $ Success { config, layout: chordLayout }

-- | Update colors (placeholder for now)
updateColors :: String -> ViewTheme -> ColorMode -> Effect Unit
updateColors containerSelector _theme _colorMode = do
  log $ "[DependencyChord] updateColors: " <> containerSelector
  pure unit

-- =============================================================================
-- SVG Rendering (HATS)
-- =============================================================================

-- | Render an overload message when too many entities
renderOverloadMessage :: Config -> Int -> Effect Unit
renderOverloadMessage config n = do
  let
    cx = config.width / 2.0
    cy = config.height / 2.0
  _ <- rerender config.containerSelector (buildOverloadTree config.width config.height cx cy n)
  pure unit

buildOverloadTree :: Number -> Number -> Number -> Number -> Int -> Tree
buildOverloadTree w h cx cy n =
  elem SVG
    [ staticStr "id" "chord-svg"
    , staticStr "viewBox" $ "0 0 " <> show w <> " " <> show h
    , staticStr "width" "100%"
    , staticStr "height" "100%"
    , staticStr "style" "background: #1a2744;"
    ]
    [ elem Text
        [ staticNum "x" cx
        , staticNum "y" (cy - 20.0)
        , staticStr "text-anchor" "middle"
        , staticStr "fill" "#ff6b6b"
        , staticStr "font-size" "24px"
        , staticStr "font-family" "system-ui, sans-serif"
        , staticStr "textContent" "⚠ Chord Overload"
        ] []
    , elem Text
        [ staticNum "x" cx
        , staticNum "y" (cy + 20.0)
        , staticStr "text-anchor" "middle"
        , staticStr "fill" "#aaa"
        , staticStr "font-size" "16px"
        , staticStr "font-family" "system-ui, sans-serif"
        , staticStr "textContent" $ show n <> " entities is too many for a readable chord diagram"
        ] []
    , elem Text
        [ staticNum "x" cx
        , staticNum "y" (cy + 60.0)
        , staticStr "text-anchor" "middle"
        , staticStr "fill" "#888"
        , staticStr "font-size" "14px"
        , staticStr "font-family" "system-ui, sans-serif"
        , staticStr "textContent" $ "Maximum supported: " <> show maxEntities <> " entities"
        ] []
    ]

renderChordDiagram :: Config -> Array String -> ChordLayout -> Effect Unit
renderChordDiagram config names chordLayout = do
  _ <- rerender config.containerSelector (buildChordTree config names chordLayout)
  pure unit

buildChordTree :: Config -> Array String -> ChordLayout -> Tree
buildChordTree config names chordLayout =
  let
    cx = config.width / 2.0
    cy = config.height / 2.0
    viewBox = "0 0 " <> show config.width <> " " <> show config.height
  in
    elem SVG
      [ staticStr "id" "chord-svg"
      , staticStr "viewBox" viewBox
      , staticStr "width" "100%"
      , staticStr "height" "100%"
      , staticStr "style" "background: transparent;"
      ]
      [ -- Main group centered
        elem Group
          [ staticStr "transform" $ "translate(" <> show cx <> "," <> show cy <> ")"
          ]
          ( -- Chords (ribbons) - render first so arcs are on top
            [ elem Group [ staticStr "class" "chords" ]
              (chordLayout.chords <#> renderChord config)
            ]
            -- Arcs (groups)
            <> [ elem Group [ staticStr "class" "arcs" ]
                 (Array.mapWithIndex (renderArc config names) chordLayout.groups)
               ]
            -- Labels
            <> [ elem Group [ staticStr "class" "labels" ]
                 (Array.mapWithIndex (renderLabel config names) chordLayout.groups)
               ]
          )
      ]

-- | Render a single arc (group)
renderArc :: Config -> Array String -> Int -> ChordGroup -> Tree
renderArc config _names idx group =
  let
    arcPath = describeArc config.innerRadius config.outerRadius group.startAngle group.endAngle
    color = groupColor idx
  in
    elem Path
      [ staticStr "class" "arc"
      , staticStr "d" arcPath
      , staticStr "fill" color
      , staticStr "stroke" "#fff"
      , staticNum "stroke-width" 1.0
      , staticStr "data-index" (show idx)
      ] []

-- | Render a chord (ribbon)
renderChord :: Config -> Chord -> Tree
renderChord config chord =
  let
    ribbonPath = describeRibbon config.innerRadius chord.source chord.target
    color = groupColor chord.source.index
  in
    elem Path
      [ staticStr "class" "chord"
      , staticStr "d" ribbonPath
      , staticStr "fill" color
      , staticNum "fill-opacity" 0.6
      , staticStr "stroke" color
      , staticNum "stroke-width" 0.5
      ] []

-- | Render a label for a group
renderLabel :: Config -> Array String -> Int -> ChordGroup -> Tree
renderLabel config names idx group =
  let
    -- Position label at the middle of the arc, outside the outer radius
    midAngle = (group.startAngle + group.endAngle) / 2.0
    labelRadius = config.outerRadius + 15.0
    x = labelRadius * sin midAngle
    y = -labelRadius * cos midAngle
    name = fromMaybe ("Entity " <> show idx) $ names !! idx
    -- Rotate text to follow the arc
    rotation = (midAngle * 180.0 / pi) - 90.0
    -- Flip text on the left side for readability
    flipText = midAngle > pi
    finalRotation = if flipText then rotation + 180.0 else rotation
    anchor = if flipText then "end" else "start"
  in
    elem Text
      [ staticStr "class" "chord-label"
      , staticNum "x" x
      , staticNum "y" y
      , staticStr "transform" $ "rotate(" <> show finalRotation <> "," <> show x <> "," <> show y <> ")"
      , staticStr "text-anchor" anchor
      , staticStr "dominant-baseline" "middle"
      , staticStr "fill" "#ddd"
      , staticStr "font-size" "10px"
      , staticStr "font-family" "system-ui, sans-serif"
      , staticStr "textContent" (shortenName name)
      ] []

-- =============================================================================
-- Path Generators
-- =============================================================================

-- | Generate SVG path for an arc
describeArc :: Number -> Number -> Number -> Number -> String
describeArc innerR outerR startAngle endAngle =
  let
    -- Convert angles: D3/SVG convention (0 = 12 o'clock, clockwise)
    -- But our angles are already in that convention from the layout
    startX1 = outerR * sin startAngle
    startY1 = -outerR * cos startAngle
    endX1 = outerR * sin endAngle
    endY1 = -outerR * cos endAngle
    startX2 = innerR * sin endAngle
    startY2 = -innerR * cos endAngle
    endX2 = innerR * sin startAngle
    endY2 = -innerR * cos startAngle

    largeArc = if (endAngle - startAngle) > pi then "1" else "0"
  in
    "M " <> show startX1 <> " " <> show startY1 <>
    " A " <> show outerR <> " " <> show outerR <> " 0 " <> largeArc <> " 1 " <> show endX1 <> " " <> show endY1 <>
    " L " <> show startX2 <> " " <> show startY2 <>
    " A " <> show innerR <> " " <> show innerR <> " 0 " <> largeArc <> " 0 " <> show endX2 <> " " <> show endY2 <>
    " Z"

-- | Generate SVG path for a ribbon (chord)
describeRibbon :: Number -> ChordGroup -> ChordGroup -> String
describeRibbon radius source target =
  let
    -- Source arc points
    sx1 = radius * sin source.startAngle
    sy1 = -radius * cos source.startAngle
    sx2 = radius * sin source.endAngle
    sy2 = -radius * cos source.endAngle

    -- Target arc points
    tx1 = radius * sin target.startAngle
    ty1 = -radius * cos target.startAngle
    tx2 = radius * sin target.endAngle
    ty2 = -radius * cos target.endAngle

    -- Use quadratic bezier curves through center for the ribbon
  in
    "M " <> show sx1 <> " " <> show sy1 <>
    " Q 0 0 " <> show tx1 <> " " <> show ty1 <>
    " A " <> show radius <> " " <> show radius <> " 0 0 1 " <> show tx2 <> " " <> show ty2 <>
    " Q 0 0 " <> show sx2 <> " " <> show sy2 <>
    " A " <> show radius <> " " <> show radius <> " 0 0 1 " <> show sx1 <> " " <> show sy1 <>
    " Z"

-- =============================================================================
-- Helpers
-- =============================================================================

-- | Color for a group by index
groupColor :: Int -> String
groupColor idx =
  let colors =
        [ "#4e79a7", "#f28e2b", "#e15759", "#76b7b2", "#59a14f"
        , "#edc948", "#b07aa1", "#ff9da7", "#9c755f", "#bab0ac"
        ]
  in fromMaybe "#888" $ colors !! (idx `mod` Array.length colors)

-- | Shorten a module/entity name for display
shortenName :: String -> String
shortenName name =
  -- For now, just return the name as-is
  -- Could implement truncation or last-segment extraction
  name
