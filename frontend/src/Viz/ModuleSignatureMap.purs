-- | Module Signature Map — Pure Data Preparation
-- |
-- | Category-lane layout of a module's type signatures.
-- | Each declaration cell is sized to its rendered SVG bounding box.
-- | All layout logic is pure PureScript; SVG rendering is via TypeSignature FFI.
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

import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable (toMaybe)
import Data.Set as Set
import Effect (Effect)
import Web.DOM (Element)

import CE2.Data.Loader (V2Declaration, V2ChildDeclaration)
import CE2.Viz.TypeSignature as TS
import CE2.Viz.TypeSignature.TypeAST (parseAndExport, parseToRenderType, extractCtorArgs, collectTypeVars)

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
  , svg :: Maybe Element
  , cellWidth :: Number
  , cellHeight :: Number
  , onClick :: Effect Unit
  }

type Lane =
  { label :: String
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
    , svg: svg
    , cellWidth: cellWidth
    , cellHeight: cellHeight
    , onClick: clickHandler
    }
  where
    maxCellW = 1160.0  -- containerW - 40, assuming ~1200

-- | Render a declaration to an SVG element based on its kind.
renderDeclaration :: V2Declaration -> String -> Effect { svg :: Maybe Element, width :: Number, height :: Number }
renderDeclaration decl sig
  | (decl.kind == "data" || decl.kind == "newtype") && not (Array.null decl.children) = do
      let
        typeParams = inferTypeParams decl.children
        constructors = decl.children
          # Array.filter (\c -> c.kind == "constructor" || c.kind == "" || c.kind == "")
          # map (\c -> { name: c.name, args: extractCtorArgs (fromMaybe "" c.typeSignature) })
      nullableEl <- TS.renderADTElement decl.name typeParams constructors parseAndExport
      case toMaybe nullableEl of
        Just el -> do
          dims <- TS.measureSVGElement el
          pure { svg: Just el, width: dims.width, height: dims.height }
        Nothing -> pure { svg: Nothing, width: 0.0, height: 0.0 }

  | decl.kind == "type_class" && not (Array.null decl.children) = do
      let
        methods = decl.children
          # Array.filter (\c -> c.kind == "class_member" || c.kind == "" || c.kind == "")
          # map (\c -> { name: c.name, sig: fromMaybe "" c.typeSignature })
      nullableEl <- TS.renderClassDefElement decl.name [] [] methods parseAndExport
      case toMaybe nullableEl of
        Just el -> do
          dims <- TS.measureSVGElement el
          pure { svg: Just el, width: dims.width, height: dims.height }
        Nothing -> pure { svg: Nothing, width: 0.0, height: 0.0 }

  | otherwise = do
      let
        ast = case decl.typeSignature of
          Just s  -> parseAndExport s
          Nothing -> parseAndExport ""
        -- For type synonyms and childless data types, extract type vars from the
        -- signature body so they render as styled pills in the header.
        showTypeParams = decl.kind == "type_synonym" || decl.kind == "data" || decl.kind == "newtype"
        typeVars = if showTypeParams then
          case decl.typeSignature >>= parseToRenderType of
            Just rt -> Set.toUnfoldable (collectTypeVars rt)
            Nothing -> []
          else []
      nullableEl <- if Array.null typeVars
        then TS.renderSignatureElement decl.name sig ast
        else TS.renderSignatureWithParamsElement decl.name sig ast typeVars
      case toMaybe nullableEl of
        Just el -> do
          dims <- TS.measureSVGElement el
          pure { svg: Just el, width: dims.width, height: dims.height }
        Nothing -> pure { svg: Nothing, width: 0.0, height: 0.0 }

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
           { label: laneDef.label
           , accent: kindAccent (fromMaybe "" (Array.head laneDef.kinds))
           , cells: laneCells
           , column: laneDef.column
           }

-- =============================================================================
-- Post-render: insert SVGs into cell divs
-- =============================================================================

-- | Insert pre-rendered SVGs into their cell divs after Halogen render.
insertSVGsIntoCells :: Array MeasuredCell -> Effect Unit
insertSVGsIntoCells cells =
  Array.foldM (\_ cell ->
    case cell.svg of
      Just svgEl -> TS.insertSVGIntoCell ("sig-cell-" <> cell.name) svgEl cell.cellWidth cellPad
      Nothing -> pure unit
  ) unit cells
