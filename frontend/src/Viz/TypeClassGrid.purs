-- | Type Class Grid Visualization
-- |
-- | Displays all type classes in a package set as a grid of cards.
-- | Each card shows:
-- | - Class name
-- | - Module name
-- | - Method count (as mini donut arcs)
-- | - Instance count (center number)
-- |
-- | Uses HATS for declarative rendering.
module CE2.Viz.TypeClassGrid
  ( Config
  , render
  , cleanup
  ) where

import Prelude

import Data.Array as Array
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Number (pi, cos, sin)
import Data.String.Pattern (Pattern(..))
import Data.String.Common (split) as StringCommon
import Effect (Effect)
import Data.Functor (void)
import Data.Int (floor) as Int

-- HATS imports
import Hylograph.HATS (Tree, elem, staticStr, thunkedStr)
import Hylograph.HATS.InterpreterTick (rerender, clearContainer)
import Hylograph.Internal.Element.Types (ElementType(..))

-- Local imports
import CE2.Data.Loader (TypeClassStats, TypeClassInfo)
import CE2.Types (ViewTheme(..), themeColors)

-- =============================================================================
-- Types
-- =============================================================================

type Config =
  { containerSelector :: String
  , width :: Number
  , height :: Number
  , theme :: ViewTheme
  }

-- =============================================================================
-- Public API
-- =============================================================================

-- | Render the type class grid
render :: Config -> TypeClassStats -> Effect Unit
render config stats = do
  let tree = renderGrid config stats
  void $ rerender config.containerSelector tree

-- | Clean up the visualization
cleanup :: Config -> Effect Unit
cleanup config = clearContainer config.containerSelector

-- =============================================================================
-- Grid Rendering
-- =============================================================================

-- | Render the full grid
renderGrid :: Config -> TypeClassStats -> Tree
renderGrid config stats =
  elem SVG
    [ staticStr "viewBox" $ "0 0 " <> show config.width <> " " <> show config.height
    , staticStr "width" "100%"
    , staticStr "height" "100%"
    , staticStr "class" "type-class-grid"
    ]
    [ renderBackground config
    , renderHeader config stats
    , renderCards config stats.typeClasses
    ]

-- | Background rect
renderBackground :: Config -> Tree
renderBackground config =
  let colors = themeColors config.theme
  in elem Rect
    [ staticStr "x" "0"
    , staticStr "y" "0"
    , thunkedStr "width" (show config.width)
    , thunkedStr "height" (show config.height)
    , staticStr "fill" colors.background
    ]
    []

-- | Header with summary stats
renderHeader :: Config -> TypeClassStats -> Tree
renderHeader config stats =
  let colors = themeColors config.theme
  in elem Group
    [ staticStr "class" "header"
    , staticStr "transform" "translate(20, 30)"
    ]
    [ elem Text
        [ staticStr "font-size" "16"
        , staticStr "font-weight" "bold"
        , staticStr "fill" colors.text
        , thunkedStr "textContent" $ show stats.count <> " Type Classes"
        ]
        []
    , elem Text
        [ staticStr "y" "20"
        , staticStr "font-size" "11"
        , staticStr "fill" colors.textMuted
        , thunkedStr "textContent" $ show stats.summary.totalMethods <> " methods, "
              <> show stats.summary.totalInstances <> " instances"
        ]
        []
    ]

-- | Render all type class cards in a grid
renderCards :: Config -> Array TypeClassInfo -> Tree
renderCards config classes =
  let
    -- Grid layout parameters
    cardWidth = 120.0
    cardHeight = 100.0
    padding = 10.0
    startY = 70.0
    startX = 20.0

    -- Calculate columns based on width
    cols = max 1.0 $ (config.width - startX * 2.0) / (cardWidth + padding)

    -- Position each card
    positionedCards = Array.mapWithIndex (positionCard cols cardWidth cardHeight padding startX startY) classes
  in
    elem Group
      [ staticStr "class" "cards" ]
      positionedCards

-- | Position a single card in the grid
positionCard :: Number -> Number -> Number -> Number -> Number -> Number -> Int -> TypeClassInfo -> Tree
positionCard cols cardW cardH padding startX startY idx tc =
  let
    colsInt = max 1 $ Int.floor cols
    col = toNumber (idx `mod` colsInt)
    row = toNumber (idx / colsInt)
    x = startX + col * (cardW + padding)
    y = startY + row * (cardH + padding)
  in
    renderCard cardW cardH x y tc

-- | Render a single type class card
-- | Donut centered with instance count inside, class name below
renderCard :: Number -> Number -> Number -> Number -> TypeClassInfo -> Tree
renderCard w h x y tc =
  let
    -- Donut centered in upper portion, leaving room for text below
    donutCx = w / 2.0
    donutCy = h / 2.0 - 10.0
    donutRadius = 40.0  -- 3x bigger than before (was 15)
  in
    elem Group
      [ staticStr "class" "type-class-card"
      , thunkedStr "transform" $ "translate(" <> show x <> "," <> show y <> ")"
      ]
      [ -- Donut with instance count in center
        renderDonut donutCx donutCy donutRadius tc.methodCount tc.instanceCount 12
      -- Class name (bottom)
      , elem Text
          [ thunkedStr "x" (show (w / 2.0))
          , thunkedStr "y" (show (h - 18.0))
          , staticStr "text-anchor" "middle"
          , staticStr "font-size" "10"
          , staticStr "font-weight" "600"
          , staticStr "fill" "#e2e8f0"
          , thunkedStr "textContent" tc.name
          ]
          []
      -- Module name (smaller, below class name)
      , elem Text
          [ thunkedStr "x" (show (w / 2.0))
          , thunkedStr "y" (show (h - 6.0))
          , staticStr "text-anchor" "middle"
          , staticStr "font-size" "7"
          , staticStr "fill" "#64748b"
          , thunkedStr "textContent" $ truncateModule tc.moduleName
          ]
          []
      ]

-- | Color based on instance count
instanceCountColor :: Int -> String
instanceCountColor n
  | n >= 100 = "#22c55e"   -- Green - heavily used
  | n >= 20 = "#eab308"    -- Yellow - moderate
  | n >= 1 = "#94a3b8"     -- Gray - light use
  | otherwise = "#475569"  -- Dim - no instances

-- | Truncate long module names
truncateModule :: String -> String
truncateModule s =
  let parts = StringCommon.split (Pattern ".") s
      len = Array.length parts
  in if len <= 2
     then s
     else "..." <> (Array.intercalate "." $ Array.drop (len - 2) parts)

-- =============================================================================
-- Donut Chart (methods as arcs, instances in center)
-- =============================================================================

-- | Render a donut showing method count as arcs, instance count in center
renderDonut :: Number -> Number -> Number -> Int -> Int -> Int -> Tree
renderDonut cx cy radius methodCount instanceCount maxMethods =
  let
    -- Clamp to max displayable
    displayCount = min methodCount maxMethods

    -- If no methods, show empty ring
    arcs = if displayCount <= 0
           then [ renderEmptyRing cx cy radius ]
           else Array.mapWithIndex (renderMethodArc cx cy radius displayCount) (Array.range 0 (displayCount - 1))

    -- Center text showing instance count
    centerText = elem Text
      [ thunkedStr "x" (show cx)
      , thunkedStr "y" (show (cy + 6.0))
      , staticStr "text-anchor" "middle"
      , staticStr "font-size" "16"
      , staticStr "font-weight" "bold"
      , staticStr "fill" $ instanceCountColor instanceCount
      , thunkedStr "textContent" $ show instanceCount
      ]
      []
  in
    elem Group
      [ staticStr "class" "donut" ]
      (Array.snoc arcs centerText)

-- | Empty ring for classes with 0 methods
renderEmptyRing :: Number -> Number -> Number -> Tree
renderEmptyRing cx cy r =
  elem Circle
    [ thunkedStr "cx" (show cx)
    , thunkedStr "cy" (show cy)
    , thunkedStr "r" (show (r - 6.0))
    , staticStr "fill" "none"
    , staticStr "stroke" "#334155"
    , staticStr "stroke-width" "8"
    ]
    []

-- | Render a single method arc
renderMethodArc :: Number -> Number -> Number -> Int -> Int -> Int -> Tree
renderMethodArc cx cy radius total idx _ =
  let
    -- Arc parameters - thicker arcs for larger donut
    gap = 0.08  -- Gap between arcs in radians
    arcLength = (2.0 * pi - toNumber total * gap) / toNumber total
    startAngle = toNumber idx * (arcLength + gap) - pi / 2.0
    endAngle = startAngle + arcLength

    innerR = radius - 10.0
    outerR = radius - 2.0

    -- Arc path
    x1 = cx + outerR * cos startAngle
    y1 = cy + outerR * sin startAngle
    x2 = cx + outerR * cos endAngle
    y2 = cy + outerR * sin endAngle
    x3 = cx + innerR * cos endAngle
    y3 = cy + innerR * sin endAngle
    x4 = cx + innerR * cos startAngle
    y4 = cy + innerR * sin startAngle

    largeArc = if arcLength > pi then "1" else "0"

    d = "M " <> show x1 <> " " <> show y1
      <> " A " <> show outerR <> " " <> show outerR <> " 0 " <> largeArc <> " 1 " <> show x2 <> " " <> show y2
      <> " L " <> show x3 <> " " <> show y3
      <> " A " <> show innerR <> " " <> show innerR <> " 0 " <> largeArc <> " 0 " <> show x4 <> " " <> show y4
      <> " Z"
  in
    elem Path
      [ thunkedStr "d" d
      , staticStr "fill" "#f59e0b"
      , staticStr "opacity" "0.8"
      ]
      []
