-- | Module Signature Map — Pure Data Preparation
-- |
-- | Category-lane layout of a module's type signatures.
-- | Each declaration is prepared as structured rendering data
-- | for HTML output via sigil's HTML renderers.
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
  ) where

import Prelude
import Prim hiding (Constraint, Row)

import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as Set
import Effect (Effect)

import CE2.Data.Loader (V2Declaration, V2ChildDeclaration)
import CE2.Viz.TypeSignature.TypeAST (parseToRenderType, extractCtorRenderTypes, collectTypeVars, RenderType)
import Sigil.Types (SuperclassInfo)

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
  , dataDecl :: Maybe             -- Structured data for ADT HTML rendering
      { typeParams :: Array String
      , constructors :: Array { name :: String, args :: Array RenderType }
      , keyword :: Maybe String
      }
  , classDecl :: Maybe            -- Structured data for class HTML rendering
      { typeParams :: Array String
      , superclasses :: Array SuperclassInfo
      , methods :: Array { name :: String, ast :: Maybe RenderType }
      , instances :: Array { name :: String, sig :: Maybe String }
      }
  , typeSynonym :: Maybe          -- Structured data for type synonym HTML rendering
      { typeParams :: Array String
      , body :: RenderType
      }
  , foreignImport :: Boolean      -- true if this is a foreign import
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
  , { key: "foreign",    label: "Foreign",      kinds: ["foreign"],         column: true }
  , { key: "operators",  label: "Operators",    kinds: ["alias"],           column: true }
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

-- | Prepare all declarations as structured rendering data for HTML output.
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

    result = prepareDeclaration decl

  pure
    { name: decl.name
    , kind: decl.kind
    , sig: sig
    , ast: result.ast
    , dataDecl: result.dataDecl
    , classDecl: result.classDecl
    , typeSynonym: result.typeSynonym
    , foreignImport: result.foreignImport
    , cellWidth: minCellW
    , cellHeight: minCellH
    , onClick: clickHandler
    }

-- | Prepare a declaration's rendering data based on its kind.
type PreparedDecl =
  { ast :: Maybe RenderType
  , dataDecl :: Maybe { typeParams :: Array String, constructors :: Array { name :: String, args :: Array RenderType }, keyword :: Maybe String }
  , classDecl :: Maybe { typeParams :: Array String, superclasses :: Array SuperclassInfo, methods :: Array { name :: String, ast :: Maybe RenderType }, instances :: Array { name :: String, sig :: Maybe String } }
  , typeSynonym :: Maybe { typeParams :: Array String, body :: RenderType }
  , foreignImport :: Boolean
  }

emptyPrep :: PreparedDecl
emptyPrep = { ast: Nothing, dataDecl: Nothing, classDecl: Nothing, typeSynonym: Nothing, foreignImport: false }

prepareDeclaration :: V2Declaration -> PreparedDecl
prepareDeclaration decl
  | decl.kind == "data" || decl.kind == "newtype" =
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
      in emptyPrep { dataDecl = Just { typeParams, constructors, keyword } }

  | decl.kind == "type_class" =
      let
        methods = decl.children
          # Array.filter (\c -> c.kind == "class_member" || c.kind == "" || c.kind == "")
          # map (\c -> { name: c.name, ast: c.typeSignature >>= parseToRenderType })
        instances = decl.children
          # Array.filter (\c -> c.kind == "instance")
          # map (\c -> { name: c.name, sig: c.typeSignature })
        superclasses = decl.superclasses
          # map (\sc -> { name: sc.name
                        , methods: sc.methods # map (\m -> { name: m.name, ast: m.typeSignature >>= parseToRenderType })
                        })
        typeParams = decl.typeArguments
      in emptyPrep { classDecl = Just { typeParams, superclasses, methods, instances } }

  | decl.kind == "type_synonym" =
      case decl.typeSignature >>= parseToRenderType of
        Just body -> emptyPrep { typeSynonym = Just { typeParams: decl.typeArguments, body } }
        Nothing -> emptyPrep { ast = decl.typeSignature >>= parseToRenderType }

  | decl.kind == "foreign" =
      emptyPrep { ast = decl.typeSignature >>= parseToRenderType, foreignImport = true }

  | decl.kind == "alias" =
      emptyPrep { ast = decl.typeSignature >>= parseToRenderType }

  | otherwise =
      emptyPrep { ast = decl.typeSignature >>= parseToRenderType }

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

