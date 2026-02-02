-- | Dependency Adjacency Matrix (HATS Version)
-- |
-- | Renders an adjacency matrix showing dependencies between entities.
-- | Works with modules, packages, or any other entity type.
-- |
-- | Uses DataViz.Layout.Adjacency for layout computation.
module CE2.Viz.DependencyAdjacency
  ( render
  , updateColors
  , Config
  , AdjacencyHandle
  , RenderResult(..)
  , maxEntities
  ) where

import Prelude

import Data.Array as Array
import Effect (Effect)
import Effect.Class.Console (log)

-- PSD3 HATS Imports
import Hylograph.HATS (Tree, elem, staticStr, staticNum)
import Hylograph.HATS.InterpreterTick (rerender)
import Hylograph.Internal.Selection.Types (ElementType(..))

-- Layout
import DataViz.Layout.Adjacency (layout, MatrixLayout, MatrixCell, MatrixLabel)
import DataViz.Layout.Adjacency.Layout (defaultConfig)

import CE2.Types (ViewTheme, ColorMode)
import CE2.Viz.DependencyMatrix (DependencyData)

-- =============================================================================
-- Types
-- =============================================================================

-- | Maximum entities before we refuse to render
-- | 100 entities = 10,000 cells which is manageable
maxEntities :: Int
maxEntities = 100

type Config =
  { containerSelector :: String
  , width :: Number
  , height :: Number
  , cellSize :: Number       -- Size of each cell
  , labelWidth :: Number     -- Space for row labels
  , labelHeight :: Number    -- Space for column labels
  , matrixMode :: Boolean    -- Green-on-black Matrix theme
  }

type AdjacencyHandle =
  { config :: Config
  , layout :: MatrixLayout
  }

data RenderResult
  = Success AdjacencyHandle
  | Overloaded Int  -- Number of entities that was requested

-- =============================================================================
-- Public API
-- =============================================================================

-- | Render an adjacency matrix from dependency data
render :: Config -> DependencyData -> Effect RenderResult
render config depData = do
  let n = Array.length depData.names

  log $ "[DependencyAdjacency] Rendering with " <> show n
      <> " entities, " <> show depData.totalConnections <> " connections"

  -- Check upper bound
  if n > maxEntities
    then do
      log $ "[DependencyAdjacency] OVERLOAD: " <> show n <> " entities exceeds max " <> show maxEntities
      renderOverloadMessage config n
      pure $ Overloaded n
    else do
      -- Compute layout
      let matrixLayout = layout { matrix: depData.matrix, names: depData.names }

      log $ "[DependencyAdjacency] Layout: " <> show (Array.length matrixLayout.cells) <> " cells"

      -- Render SVG
      renderAdjacencyMatrix config depData.names matrixLayout

      pure $ Success { config, layout: matrixLayout }

-- | Update colors (placeholder for now)
updateColors :: String -> ViewTheme -> ColorMode -> Effect Unit
updateColors containerSelector _theme _colorMode = do
  log $ "[DependencyAdjacency] updateColors: " <> containerSelector
  pure unit

-- =============================================================================
-- SVG Rendering (HATS)
-- =============================================================================

-- | Render an overload message when too many entities
renderOverloadMessage :: Config -> Int -> Effect Unit
renderOverloadMessage config n = do
  _ <- rerender config.containerSelector (buildOverloadTree n)
  pure unit

buildOverloadTree :: Int -> Tree
buildOverloadTree n =
  elem SVG
    [ staticStr "id" "adjacency-svg"
    , staticStr "viewBox" "0 0 600 400"
    , staticStr "width" "100%"
    , staticStr "height" "100%"
    , staticStr "style" "background: #1a2744;"
    ]
    [ elem Text
        [ staticNum "x" 300.0
        , staticNum "y" 180.0
        , staticStr "text-anchor" "middle"
        , staticStr "fill" "#ff6b6b"
        , staticStr "font-size" "24px"
        , staticStr "font-family" "system-ui, sans-serif"
        , staticStr "textContent" "⚠ Matrix Overload"
        ] []
    , elem Text
        [ staticNum "x" 300.0
        , staticNum "y" 220.0
        , staticStr "text-anchor" "middle"
        , staticStr "fill" "#aaa"
        , staticStr "font-size" "16px"
        , staticStr "font-family" "system-ui, sans-serif"
        , staticStr "textContent" $ show n <> " entities would create " <> show (n * n) <> " cells"
        ] []
    , elem Text
        [ staticNum "x" 300.0
        , staticNum "y" 260.0
        , staticStr "text-anchor" "middle"
        , staticStr "fill" "#888"
        , staticStr "font-size" "14px"
        , staticStr "font-family" "system-ui, sans-serif"
        , staticStr "textContent" $ "Maximum supported: " <> show maxEntities <> " entities (" <> show (maxEntities * maxEntities) <> " cells)"
        ] []
    ]

renderAdjacencyMatrix :: Config -> Array String -> MatrixLayout -> Effect Unit
renderAdjacencyMatrix config _names matrixLayout = do
  _ <- rerender config.containerSelector (buildMatrixTree config matrixLayout)
  pure unit

buildMatrixTree :: Config -> MatrixLayout -> Tree
buildMatrixTree config matrixLayout =
  let
    -- Add some padding
    padding = 20.0
    totalWidth = matrixLayout.totalWidth + padding * 2.0
    totalHeight = matrixLayout.totalHeight + padding * 2.0
    viewBox = "0 0 " <> show totalWidth <> " " <> show totalHeight

    -- Grid offset matches the layout's defaultConfig (labelWidth=100, labelHeight=80, labelMargin=8)
    gridOffsetX = defaultConfig.labelWidth + defaultConfig.labelMargin
    gridOffsetY = defaultConfig.labelHeight + defaultConfig.labelMargin

    -- Matrix-mode colors (green on black) vs default (blue on dark)
    bgColor = if config.matrixMode then "#000000" else "#1a2744"
    strokeColor = if config.matrixMode then "#003300" else "#2a3754"
    labelColor = if config.matrixMode then "#00ff00" else "#ddd"
  in
    elem SVG
      [ staticStr "id" "adjacency-svg"
      , staticStr "viewBox" viewBox
      , staticStr "width" "100%"
      , staticStr "height" "100%"
      , staticStr "style" "background: transparent;"
      ]
      [ -- Main group with padding offset
        elem Group
          [ staticStr "transform" $ "translate(" <> show padding <> "," <> show padding <> ")"
          ]
          ( -- Grid background (positioned to match layout's cell positions)
            [ elem Rect
                [ staticStr "class" "grid-bg"
                , staticNum "x" gridOffsetX
                , staticNum "y" gridOffsetY
                , staticNum "width" matrixLayout.gridWidth
                , staticNum "height" matrixLayout.gridHeight
                , staticStr "fill" bgColor
                , staticStr "stroke" strokeColor
                ] []
            ]
            -- Cells
            <> [ elem Group [ staticStr "class" "cells" ]
                 (matrixLayout.cells <#> renderCell config)
               ]
            -- Row labels
            <> [ elem Group [ staticStr "class" "row-labels" ]
                 (matrixLayout.rowLabels <#> renderRowLabel labelColor)
               ]
            -- Column labels
            <> [ elem Group [ staticStr "class" "col-labels" ]
                 (matrixLayout.colLabels <#> renderColLabel labelColor)
               ]
          )
      ]

-- | Render a single cell
renderCell :: Config -> MatrixCell -> Tree
renderCell config cell =
  let
    -- Color based on value and mode
    fillColor = if cell.value > 0.0
      then if config.matrixMode then "#00ff00" else "#4a9eff"
      else "transparent"
    fillOpacity = if cell.value > 0.0 then 0.8 else 0.0
    strokeColor = if config.matrixMode then "#003300" else "#2a3754"
  in
    elem Rect
      [ staticStr "class" "matrix-cell"
      , staticNum "x" cell.position.x
      , staticNum "y" cell.position.y
      , staticNum "width" (cell.position.width - 1.0)  -- Small gap between cells
      , staticNum "height" (cell.position.height - 1.0)
      , staticStr "fill" fillColor
      , staticNum "fill-opacity" fillOpacity
      , staticStr "stroke" strokeColor
      , staticNum "stroke-width" 0.5
      , staticStr "data-row" cell.rowName
      , staticStr "data-col" cell.colName
      ] []

-- | Render a row label
renderRowLabel :: String -> MatrixLabel -> Tree
renderRowLabel color label =
  elem Text
    [ staticStr "class" "row-label"
    , staticNum "x" label.position.x
    , staticNum "y" label.position.y
    , staticStr "text-anchor" label.position.anchor
    , staticStr "dominant-baseline" "middle"
    , staticStr "fill" color
    , staticStr "font-size" "9px"
    , staticStr "font-family" "system-ui, sans-serif"
    , staticStr "textContent" label.displayName
    ] []

-- | Render a column label
renderColLabel :: String -> MatrixLabel -> Tree
renderColLabel color label =
  elem Text
    [ staticStr "class" "col-label"
    , staticNum "x" label.position.x
    , staticNum "y" label.position.y
    , staticStr "text-anchor" label.position.anchor
    , staticStr "dominant-baseline" "middle"
    , staticStr "transform" $ "rotate(" <> show label.position.rotation <> "," <> show label.position.x <> "," <> show label.position.y <> ")"
    , staticStr "fill" color
    , staticStr "font-size" "9px"
    , staticStr "font-family" "system-ui, sans-serif"
    , staticStr "textContent" label.displayName
    ] []
