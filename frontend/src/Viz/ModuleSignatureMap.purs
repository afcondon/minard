-- | Module Signature Map
-- |
-- | Full-screen treemap of a module's type signatures, sized by visual
-- | complexity of each signature (measured rendered SVG area).
-- | Complex types get proportionally more space.
-- |
-- | Exploratory visualization — does not replace any existing view.
module CE2.Viz.ModuleSignatureMap
  ( Config
  , SignatureMapHandle
  , render
  ) where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)

import CE2.Data.Loader (V2Declaration, V2ChildDeclaration)
import CE2.Viz.TypeSignature.TypeAST (JsResult, parseAndExport)

-- =============================================================================
-- Types
-- =============================================================================

-- | Configuration for the signature map
type Config =
  { containerSelector :: String
  , moduleName :: String
  , onDeclarationClick :: Maybe (String -> String -> String -> Effect Unit)
  , packageName :: String
  }

-- | Handle for cleanup
type SignatureMapHandle = { cleanup :: Effect Unit }

-- | Cell data passed to JS renderer
type SignatureCell =
  { name :: String
  , kind :: String
  , sig :: String           -- raw signature string (empty if none)
  , ast :: JsResult         -- parsed AST (ok/err)
  , children :: Array ChildInfo
  , onClick :: Effect Unit
  }

-- | Simplified child info for data constructors / class members
type ChildInfo =
  { name :: String
  , kind :: String
  , sig :: String
  }

-- =============================================================================
-- Public API
-- =============================================================================

-- | Render the module signature map
render :: Config -> Array V2Declaration -> Effect SignatureMapHandle
render config declarations = do
  let cells = prepareSignatureCells config declarations
  renderSignatureMapImpl config.containerSelector config.moduleName cells parseAndExport

-- =============================================================================
-- Cell Preparation
-- =============================================================================

-- | Convert declarations to signature cells with parsed ASTs
prepareSignatureCells :: Config -> Array V2Declaration -> Array SignatureCell
prepareSignatureCells config declarations =
  declarations <#> \decl ->
    let
      sig = fromMaybe "" decl.typeSignature
      ast = case decl.typeSignature of
        Just s  -> parseAndExport s
        Nothing -> parseAndExport ""  -- will fail gracefully
      children = decl.children <#> childToInfo
      clickHandler = case config.onDeclarationClick of
        Just handler -> handler config.packageName config.moduleName decl.name
        Nothing -> pure unit
    in
      { name: decl.name
      , kind: decl.kind
      , sig: sig
      , ast: ast
      , children: children
      , onClick: clickHandler
      }

-- | Extract simplified child info
childToInfo :: V2ChildDeclaration -> ChildInfo
childToInfo child =
  { name: child.name
  , kind: child.kind
  , sig: fromMaybe "" child.typeSignature
  }

-- =============================================================================
-- FFI
-- =============================================================================

foreign import renderSignatureMapImpl
  :: String             -- container selector
  -> String             -- module name
  -> Array SignatureCell
  -> (String -> JsResult) -- parse function for renderADT/renderClassDef
  -> Effect SignatureMapHandle
