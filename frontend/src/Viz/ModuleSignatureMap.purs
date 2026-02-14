-- | Module Signature Map — Pure Data Preparation
-- |
-- | Category-lane layout of a module's type signatures.
-- | Each declaration cell is sized to its rendered SVG bounding box.
-- | SVG rendering is via hylograph-sigil (layoutXxx + emit).
module CE2.Viz.ModuleSignatureMap
  ( Config
  , Lane
  , MeasuredCell
  , prepareCells
  , groupIntoLanes
  , kindBackground
  , kindBorder
  , kindAccent
  , cellPad
  , insertSVGsIntoCells
  ) where

import Prelude
import Prim hiding (Constraint, Row)

import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as Set
import Effect (Effect)
import Web.DOM (Element)

import CE2.Data.Loader (V2Declaration, V2ChildDeclaration)
import CE2.Viz.TypeSignature as TS
import CE2.Viz.TypeSignature.TypeAST (parseToRenderType, extractCtorRenderTypes, collectTypeVars, RenderType)

-- =============================================================================
-- Types
-- =============================================================================

type Config =
  { containerSelector :: String
  , moduleName :: String
  , onDeclarationClick :: Maybe (String -> String -> String -> Effect Unit)
  , packageName :: String
  }

type MeasuredCell =
  { name :: String
  , kind :: String
  , sig :: String
  , ast :: Maybe RenderType       -- Parsed AST for HTML rendering (values, type synonyms)
  , svg :: Maybe Element          -- Full-size SVG (data types, classes only)
  , cellWidth :: Number
  , cellHeight :: Number
  , onClick :: Effect Unit
  }

type Lane =
  { key :: String
  , label :: String
  , accent :: String
  , cells :: Array MeasuredCell
  , column :: Boolean  -- true = vertical column, false = flex-wrap shelf
  }

-- =============================================================================
-- Lane definitions
-- =============================================================================

type LaneDef = { key :: String, label :: String, kinds :: Array String, column :: Boolean }

lanes :: Array LaneDef
lanes =
  [ { key: "structural", label: "Data Types",  kinds: ["data", "newtype"],  column: false }
  , { key: "classes",    label: "Classes",      kinds: ["type_class"],      column: false }
  , { key: "types",      label: "Type Aliases", kinds: ["type_synonym"],    column: true }
  , { key: "values",     label: "Values",       kinds: ["value"],           column: true }
  , { key: "foreign",    label: "Foreign",      kinds: ["foreign", "alias"], column: true }
  ]

-- =============================================================================
-- Kind color maps
-- =============================================================================

kindBackground :: String -> String
kindBackground = case _ of
  "value"        -> "#f0f4fa"
  "data"         -> "#eef6ee"
  "newtype"      -> "#eef8f7"
  "type_class"   -> "#fdf5ed"
  "type_synonym" -> "#fdf9ec"
  "foreign"      -> "#fdf0f0"
  "alias"        -> "#f5f0f8"
  _              -> "#f5f5f5"

kindBorder :: String -> String
kindBorder = case _ of
  "value"        -> "#c8d6e8"
  "data"         -> "#c0dcc0"
  "newtype"      -> "#b8dcd8"
  "type_class"   -> "#e8d4b8"
  "type_synonym" -> "#e8e0b0"
  "foreign"      -> "#e0c0c0"
  "alias"        -> "#d0c0d8"
  _              -> "#ddd"

kindAccent :: String -> String
kindAccent = case _ of
  "value"        -> "#4e79a7"
  "data"         -> "#59a14f"
  "newtype"      -> "#76b7b2"
  "type_class"   -> "#f28e2b"
  "type_synonym" -> "#edc948"
  "foreign"      -> "#e15759"
  "alias"        -> "#b07aa1"
  _              -> "#999"

-- =============================================================================
-- Cell sizing constants
-- =============================================================================

cellPad :: Number
cellPad = 8.0

minCellW :: Number
minCellW = 120.0

minCellH :: Number
minCellH = 40.0

-- =============================================================================
-- Pre-render and measure cells
-- =============================================================================

-- | Pre-render all declarations as SVG elements and measure their dimensions.
prepareCells :: Config -> Array V2Declaration -> Effect (Array MeasuredCell)
prepareCells config declarations = do
  Array.foldM (\acc decl -> do
    cell <- prepareOneCell config decl
    pure (Array.snoc acc cell)
  ) [] declarations

prepareOneCell :: Config -> V2Declaration -> Effect MeasuredCell
prepareOneCell config decl = do
  let
    sig = fromMaybe "" decl.typeSignature
    clickHandler = case config.onDeclarationClick of
      Just handler -> handler config.packageName config.moduleName decl.name
      Nothing -> pure unit

  result <- renderDeclaration decl sig

  let
    -- For values and type synonyms (and other non-SVG kinds), store parsed AST for HTML rendering
    ast = case result.svg of
      Just _ -> Nothing  -- ADT/ClassDef have SVG, no AST needed
      Nothing -> decl.typeSignature >>= parseToRenderType
    svg = result.svg
    cellWidth = case svg of
      Just _ -> min maxCellW (max minCellW (result.width + cellPad * 2.0))
      Nothing -> minCellW
    cellHeight = case svg of
      Just _ -> max minCellH (result.height + cellPad * 2.0)
      Nothing -> minCellH

  pure
    { name: decl.name
    , kind: decl.kind
    , sig: sig
    , ast: ast
    , svg: svg
    , cellWidth: cellWidth
    , cellHeight: cellHeight
    , onClick: clickHandler
    }
  where
    maxCellW = 1160.0  -- containerW - 40, assuming ~1200

-- | Render a declaration to an SVG element based on its kind.
-- | Uses hylograph-sigil layout + emit — dimensions come from the layout step.
renderDeclaration :: V2Declaration -> String -> Effect { svg :: Maybe Element, width :: Number, height :: Number }
renderDeclaration decl _sig
  | decl.kind == "data" || decl.kind == "newtype" = do
      let
        typeParams = if Array.null decl.children
          then decl.typeArguments
          else inferTypeParams decl.children
        constructors = decl.children
          # Array.filter (\c -> c.kind == "constructor" || c.kind == "" || c.kind == "")
          # map (\c -> { name: c.name, args: extractCtorRenderTypes (fromMaybe "" c.typeSignature) })
        keyword = case decl.dataDeclType of
          Just "newtype" -> Just "newtype"
          _ -> Nothing
      rendered <- TS.renderADTSVG decl.name typeParams constructors keyword
      pure { svg: Just rendered.svg, width: rendered.width, height: rendered.height }

  | decl.kind == "type_class" = do
      let
        methods = decl.children
          # Array.filter (\c -> c.kind == "class_member" || c.kind == "" || c.kind == "")
          # map (\c -> { name: c.name, ast: c.typeSignature >>= parseToRenderType })
        superclasses = decl.superclasses
          # map (\sc -> { name: sc.name
                        , methods: sc.methods # map (\m -> { name: m.name, ast: m.typeSignature >>= parseToRenderType })
                        })
        typeParams = decl.typeArguments
      rendered <- TS.renderClassDefSVG decl.name typeParams superclasses methods
      pure { svg: Just rendered.svg, width: rendered.width, height: rendered.height }

  -- Values, type synonyms, and other kinds: skip SVG, use HTML rendering via AST
  | otherwise = pure { svg: Nothing, width: 0.0, height: 0.0 }

-- | Infer type parameters from constructor children signatures
inferTypeParams :: Array V2ChildDeclaration -> Array String
inferTypeParams children =
  let
    allVars = Array.foldl (\acc child ->
      case child.typeSignature >>= parseToRenderType of
        Just rt -> Set.union acc (collectTypeVars rt)
        Nothing -> acc
    ) Set.empty children
  in Set.toUnfoldable allVars

-- =============================================================================
-- Group cells into lanes
-- =============================================================================

-- | Group measured cells into lanes, filtering empty lanes.
groupIntoLanes :: Array MeasuredCell -> Array Lane
groupIntoLanes cells = Array.mapMaybe mkLane lanes
  where
    mkLane :: LaneDef -> Maybe Lane
    mkLane laneDef =
      let laneCells = Array.filter (\c -> Array.elem c.kind laneDef.kinds) cells
      in if Array.null laneCells then Nothing
         else Just
           { key: laneDef.key
           , label: laneDef.label
           , accent: kindAccent (fromMaybe "" (Array.head laneDef.kinds))
           , cells: laneCells
           , column: laneDef.column
           }

-- =============================================================================
-- Post-render: insert SVGs into cell divs
-- =============================================================================

-- | Insert pre-rendered SVGs into their cell divs after Halogen render.
-- | Only used for ADT/ClassDef cells that still use SVG.
insertSVGsIntoCells :: Array MeasuredCell -> Effect Unit
insertSVGsIntoCells cells =
  Array.foldM (\_ cell -> do
    case cell.svg of
      Just svgEl -> TS.insertSVGIntoCell ("sig-cell-" <> cell.name) svgEl cell.cellWidth cellPad
      Nothing -> pure unit
  ) unit cells
