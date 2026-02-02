-- | Module Treemap Visualization (HATS Version)
-- |
-- | Visualizes modules within a single package as a treemap.
-- | - Rectangles represent modules, area proportional to LOC
-- | - Paperwhite theme (clean, light background)
-- | - Click module to explore (future enhancement)
module CE2.Viz.ModuleTreemap
  ( Config
  , render
  , renderWithImports
  , cleanup
  ) where

import Prelude

import Data.Array as Array
import Data.Int (toNumber, floor)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as String
import Effect (Effect)

-- PSD3 HATS Imports
import Hylograph.HATS (Tree, elem, staticStr, thunkedStr, thunkedNum, forEach, withBehaviors, onCoordinatedHighlight, onClick)
import Hylograph.HATS.InterpreterTick (rerender, clearContainer)
import Hylograph.Internal.Selection.Types (ElementType(..))
import Hylograph.Internal.Behavior.Types (HighlightClass(..))

-- Layout
import DataViz.Layout.Hierarchy.Types (ValuedNode(..))
import DataViz.Layout.Hierarchy.Treemap (TreemapNode(..), treemap, defaultTreemapConfig, squarify, phi)

import CE2.Data.Loader (V2ModuleListItem, V2ModuleImports)

-- For looking up modules
import Data.Foldable (foldl)
import Data.Map (Map)
import Data.Map as Map
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))

-- =============================================================================
-- Types
-- =============================================================================

-- | Configuration for module treemap
type Config =
  { containerSelector :: String
  , width :: Number
  , height :: Number
  , packageName :: String
  , onModuleClick :: Maybe (String -> String -> Effect Unit)  -- packageName -> moduleName -> Effect
  }

-- | Module with computed treemap position
type PositionedModule =
  { mod :: V2ModuleListItem
  , x :: Number
  , y :: Number
  , width :: Number
  , height :: Number
  }

-- | Render data bound to each module element
type ModuleRenderData =
  { name :: String
  , shortName :: String  -- Just the module name without package
  , loc :: Int
  , declarationCount :: Int
  , x :: Number
  , y :: Number
  , width :: Number
  , height :: Number
  , imports :: Array String      -- Module names this module imports (for hover highlighting)
  , importedBy :: Array String   -- Module names that import this module (reverse deps)
  }

-- =============================================================================
-- Public API
-- =============================================================================

-- | Render the module treemap for a single package (no hover highlighting)
render :: Config -> Array V2ModuleListItem -> Effect Unit
render config modules = do
  -- Clear existing content
  clearContainer config.containerSelector

  -- Filter modules to the package (should already be filtered, but double-check)
  let pkgModules = Array.filter (\m -> m.package.name == config.packageName) modules

  -- Compute treemap layout and render (no import data = no highlighting)
  let positioned = computeModulePositions config pkgModules
      renderData = positioned <#> toRenderDataWithoutImports
  renderModuleTreemapSVG config renderData false

-- | Render the module treemap with hover highlighting enabled
-- | Requires import data to show relationships on hover
renderWithImports :: Config -> Array V2ModuleListItem -> Array V2ModuleImports -> Effect Unit
renderWithImports config modules imports = do
  -- Clear existing content
  clearContainer config.containerSelector

  -- Filter modules to the package
  let pkgModules = Array.filter (\m -> m.package.name == config.packageName) modules
      pkgModuleNames = Set.fromFoldable $ map _.name pkgModules

  -- Build import maps
  let importMap = buildImportMap imports pkgModuleNames
      importedByMap = buildImportedByMap imports pkgModuleNames

  -- Compute treemap layout and render with import data
  let positioned = computeModulePositions config pkgModules
      renderData = positioned <#> toRenderDataWithImports importMap importedByMap
  renderModuleTreemapSVG config renderData true

-- | Clean up
cleanup :: String -> Effect Unit
cleanup selector = clearContainer selector

-- =============================================================================
-- Treemap Layout
-- =============================================================================

-- | Build hierarchy and compute positions
-- | Uses flat hierarchy: root -> modules as leaves (no grouping)
computeModulePositions :: Config -> Array V2ModuleListItem -> Array PositionedModule
computeModulePositions config modules =
  let
    -- Create leaf nodes for each module
    moduleLeaves :: Array (ValuedNode V2ModuleListItem)
    moduleLeaves = modules <#> \m ->
      VNode
        { data_: m
        , depth: 1
        , height: 0
        , value: toNumber $ max 1 $ fromMaybe m.declarationCount m.loc  -- Use LOC, fall back to declaration count
        , children: []
        , parent: Nothing
        }

    -- Calculate total value
    totalValue = foldl (\acc (VNode n) -> acc + n.value) 0.0 moduleLeaves

    -- Create dummy root module data
    rootData :: V2ModuleListItem
    rootData =
      { id: 0
      , name: config.packageName
      , path: Nothing
      , loc: Nothing
      , package: { id: 0, name: config.packageName, version: "", source: "workspace" }
      , namespacePath: Nothing
      , declarationCount: 0
      }

    -- Create root node containing all modules
    root :: ValuedNode V2ModuleListItem
    root = VNode
      { data_: rootData
      , depth: 0
      , height: 1
      , value: totalValue
      , children: moduleLeaves
      , parent: Nothing
      }

    -- Configure and run treemap
    treemapConfig = defaultTreemapConfig
      { size = { width: config.width, height: config.height }
      , paddingInner = 3.0
      , paddingOuter = 2.0
      , tile = squarify phi
      }

    TNode layoutRoot = treemap treemapConfig root
  in
    -- Extract positioned modules from children
    layoutRoot.children <#> \(TNode child) ->
      { mod: child.data_
      , x: child.x0
      , y: child.y0
      , width: child.x1 - child.x0
      , height: child.y1 - child.y0
      }

-- =============================================================================
-- Import Map Building
-- =============================================================================

-- | Build a map from module name to the modules it imports
buildImportMap :: Array V2ModuleImports -> Set String -> Map String (Array String)
buildImportMap imports pkgModuleNames =
  Map.fromFoldable $ imports
    # Array.filter (\imp -> Set.member imp.moduleName pkgModuleNames)
    <#> \imp -> Tuple imp.moduleName (Array.filter (\i -> Set.member i pkgModuleNames) imp.imports)

-- | Build a reverse map: module name to modules that import it
buildImportedByMap :: Array V2ModuleImports -> Set String -> Map String (Array String)
buildImportedByMap imports pkgModuleNames =
  let
    -- For each import relationship, record the reverse
    pairs :: Array (Tuple String String)
    pairs = imports
      # Array.filter (\imp -> Set.member imp.moduleName pkgModuleNames)
      # Array.concatMap (\imp ->
          imp.imports
            # Array.filter (\i -> Set.member i pkgModuleNames)
            <#> \imported -> Tuple imported imp.moduleName
        )
    -- Group by imported module
  in foldl (\acc (Tuple imported importer) ->
      Map.alter (Just <<< Array.cons importer <<< fromMaybe []) imported acc
    ) Map.empty pairs

-- | Convert positioned module to render data (without imports)
toRenderDataWithoutImports :: PositionedModule -> ModuleRenderData
toRenderDataWithoutImports pm =
  { name: pm.mod.name
  , shortName: shortModuleName pm.mod.name
  , loc: fromMaybe pm.mod.declarationCount pm.mod.loc  -- Fall back to declaration count (matches sizing logic)
  , declarationCount: pm.mod.declarationCount
  , x: pm.x
  , y: pm.y
  , width: pm.width
  , height: pm.height
  , imports: []
  , importedBy: []
  }

-- | Convert positioned module to render data (with imports for hover)
toRenderDataWithImports :: Map String (Array String) -> Map String (Array String) -> PositionedModule -> ModuleRenderData
toRenderDataWithImports importMap importedByMap pm =
  { name: pm.mod.name
  , shortName: shortModuleName pm.mod.name
  , loc: fromMaybe pm.mod.declarationCount pm.mod.loc  -- Fall back to declaration count (matches sizing logic)
  , declarationCount: pm.mod.declarationCount
  , x: pm.x
  , y: pm.y
  , width: pm.width
  , height: pm.height
  , imports: fromMaybe [] $ Map.lookup pm.mod.name importMap
  , importedBy: fromMaybe [] $ Map.lookup pm.mod.name importedByMap
  }

-- =============================================================================
-- SVG Rendering (Paperwhite Theme) - HATS
-- =============================================================================

-- | Render the SVG structure
renderModuleTreemapSVG :: Config -> Array ModuleRenderData -> Boolean -> Effect Unit
renderModuleTreemapSVG config renderData enableHighlighting = do
  let svgTree = buildModuleTreemapTree config renderData enableHighlighting
  _ <- rerender config.containerSelector svgTree
  pure unit

-- | Extract short module name (after last dot)
shortModuleName :: String -> String
shortModuleName name =
  case Array.last (String.split (String.Pattern ".") name) of
    Just short -> short
    Nothing -> name

-- | Build the SVG tree structure
buildModuleTreemapTree :: Config -> Array ModuleRenderData -> Boolean -> Tree
buildModuleTreemapTree config renderData enableHighlighting =
  elem SVG
    [ staticStr "id" "module-treemap"
    , staticStr "viewBox" $ "0 0 " <> show config.width <> " " <> show config.height
    , staticStr "width" "100%"
    , staticStr "height" "100%"
    , staticStr "preserveAspectRatio" "xMidYMid meet"
    , staticStr "style" "background: #fafafa; display: block; border-radius: 8px;"
    ]
    [ -- Modules as treemap cells
      if enableHighlighting
        then forEach "modules" Group renderData _.name (moduleCellWithHighlighting config)
        else forEach "modules" Group renderData _.name (moduleCell config)
    ]

-- | Render a single module cell (paperwhite style)
moduleCell :: Config -> ModuleRenderData -> Tree
moduleCell config m =
  withBehaviors
    (case config.onModuleClick of
       Nothing -> []
       Just onClickHandler -> [ onClick (onClickHandler config.packageName m.name) ])
  $ elem Group
      [ thunkedStr "transform" ("translate(" <> show m.x <> "," <> show m.y <> ")")
      , staticStr "class" "treemap-module"
      , staticStr "cursor" "pointer"
      ]
      [ -- Module rectangle (paperwhite)
        elem Rect
          [ staticStr "x" "0"
          , staticStr "y" "0"
          , thunkedNum "width" m.width
          , thunkedNum "height" m.height
          , staticStr "fill" "#fafafa"
          , staticStr "stroke" "rgba(0, 0, 0, 0.25)"
          , staticStr "stroke-width" "1"
          , staticStr "rx" "2"
          ]
          []

      -- Module label (if cell is big enough)
      , elem Text
          [ thunkedNum "x" (m.width / 2.0)
          , thunkedNum "y" (m.height / 2.0)
          , staticStr "text-anchor" "middle"
          , staticStr "dominant-baseline" "middle"
          , thunkedStr "font-size" (if m.width > 80.0 && m.height > 30.0 then "11" else "8")
          , staticStr "fill" "#333"
          , staticStr "font-family" "system-ui, sans-serif"
          , thunkedStr "textContent" (truncateName (m.width / 8.0) m.shortName)
          , thunkedStr "opacity" (if m.width > 40.0 && m.height > 20.0 then "1" else "0.5")
          ]
          []

      -- LOC indicator in corner (if cell is big enough)
      , elem Text
          [ thunkedNum "x" (m.width - 4.0)
          , thunkedNum "y" (m.height - 4.0)
          , staticStr "text-anchor" "end"
          , staticStr "font-size" "8"
          , staticStr "fill" "#888"
          , staticStr "font-family" "system-ui, sans-serif"
          , thunkedStr "textContent" (if m.width > 60.0 && m.height > 40.0 then show m.loc <> " LOC" else "")
          ]
          []
      ]

-- | Render a single module cell with coordinated highlighting behavior
-- | Hover highlights: imports (modules this imports) and importedBy (modules that import this)
moduleCellWithHighlighting :: Config -> ModuleRenderData -> Tree
moduleCellWithHighlighting config m =
  withBehaviors
    ( [ onCoordinatedHighlight
          { identify: m.name
          , classify: \hoveredId ->
              if m.name == hoveredId then Primary
              else if Array.elem hoveredId m.imports then Related      -- I import the hovered module
              else if Array.elem hoveredId m.importedBy then Related   -- The hovered module imports me
              else Dimmed
          , group: Nothing  -- Global coordination
          }
      ]
      <> case config.onModuleClick of
           Nothing -> []
           Just onClickHandler -> [ onClick (onClickHandler config.packageName m.name) ]
    )
  $ elem Group
      [ thunkedStr "transform" ("translate(" <> show m.x <> "," <> show m.y <> ")")
      , staticStr "class" "treemap-module"
      , staticStr "cursor" "pointer"
      ]
      [ -- Module rectangle with highlighting support
        elem Rect
          [ staticStr "x" "0"
          , staticStr "y" "0"
          , thunkedNum "width" m.width
          , thunkedNum "height" m.height
          , staticStr "fill" "#fafafa"
          , staticStr "stroke" "rgba(0, 0, 0, 0.25)"
          , staticStr "stroke-width" "1"
          , staticStr "rx" "2"
          , staticStr "class" "module-rect"  -- For CSS transitions
          ]
          []

      -- Module label (if cell is big enough)
      , elem Text
          [ thunkedNum "x" (m.width / 2.0)
          , thunkedNum "y" (m.height / 2.0)
          , staticStr "text-anchor" "middle"
          , staticStr "dominant-baseline" "middle"
          , thunkedStr "font-size" (if m.width > 80.0 && m.height > 30.0 then "11" else "8")
          , staticStr "fill" "#333"
          , staticStr "font-family" "system-ui, sans-serif"
          , thunkedStr "textContent" (truncateName (m.width / 8.0) m.shortName)
          , thunkedStr "opacity" (if m.width > 40.0 && m.height > 20.0 then "1" else "0.5")
          ]
          []

      -- LOC indicator in corner (if cell is big enough)
      , elem Text
          [ thunkedNum "x" (m.width - 4.0)
          , thunkedNum "y" (m.height - 4.0)
          , staticStr "text-anchor" "end"
          , staticStr "font-size" "8"
          , staticStr "fill" "#888"
          , staticStr "font-family" "system-ui, sans-serif"
          , thunkedStr "textContent" (if m.width > 60.0 && m.height > 40.0 then show m.loc <> " LOC" else "")
          ]
          []
      ]

-- | Truncate name to fit width
truncateName :: Number -> String -> String
truncateName maxChars name =
  let maxLen = max 3 (floor maxChars)
  in if String.length name > maxLen
     then String.take maxLen name <> "…"
     else name
