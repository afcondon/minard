-- | Type signature rendering — thin wrapper over sigil Html module.
-- |
-- | Preserves the original API surface for backward compatibility while
-- | delegating all rendering to the standalone library.
module CE2.Viz.SignatureTree
  ( renderSignature
  , renderSiglet
  , renderLabeledSiglet
  , renderDataDecl
  , renderClassDecl
  , renderTypeSynonym
  , renderForeignImport
  , renderSignatureInto
  , renderSigletInto
  , renderLabeledSigletInto
  , renderDataDeclInto
  , renderClassDeclInto
  , renderTypeSynonymInto
  , renderForeignImportInto
  , appendHtmlInto
  ) where

import Prelude

import Data.Maybe (Maybe)
import Effect (Effect)

import Sigil.Html as Sigil
import Sigil.Types (RenderType, SuperclassInfo)

-- =============================================================================
-- Pure renderers (return HTML strings, no DOM access)
-- =============================================================================

-- | Render a full-size signature as an HTML string.
renderSignature
  :: { name :: String, sig :: String, ast :: RenderType, typeParams :: Array String, className :: Maybe String }
  -> String
renderSignature { name, ast, typeParams, className } =
  Sigil.renderSignature { name, ast, typeParams, className }

-- | Render a siglet (compact inline signature) as an HTML string.
renderSiglet :: { ast :: RenderType } -> String
renderSiglet = Sigil.renderSiglet

-- | Render a labeled siglet (dots with rotated identifier labels) as an HTML string.
renderLabeledSiglet :: { ast :: RenderType } -> String
renderLabeledSiglet = Sigil.renderLabeledSiglet

-- | Render a data/newtype declaration as an HTML string.
renderDataDecl
  :: { name :: String
     , typeParams :: Array String
     , constructors :: Array { name :: String, args :: Array RenderType }
     , keyword :: Maybe String
     }
  -> String
renderDataDecl = Sigil.renderDataDecl

-- | Render a type class definition as an HTML string.
renderClassDecl
  :: { name :: String
     , typeParams :: Array String
     , superclasses :: Array SuperclassInfo
     , methods :: Array { name :: String, ast :: Maybe RenderType }
     }
  -> String
renderClassDecl = Sigil.renderClassDecl

-- | Render a type synonym as an HTML string.
renderTypeSynonym
  :: { name :: String
     , typeParams :: Array String
     , body :: RenderType
     }
  -> String
renderTypeSynonym = Sigil.renderTypeSynonym

-- | Render a foreign import as an HTML string.
renderForeignImport
  :: { name :: String
     , ast :: RenderType
     }
  -> String
renderForeignImport = Sigil.renderForeignImport

-- =============================================================================
-- Into renderers (querySelector + innerHTML, for backward compat)
-- =============================================================================

-- | Render a full-size signature into a container element.
renderSignatureInto
  :: String
  -> { name :: String, sig :: String, ast :: RenderType, typeParams :: Array String, className :: Maybe String }
  -> Effect Unit
renderSignatureInto selector { name, ast, typeParams, className } =
  Sigil.renderSignatureInto selector { name, ast, typeParams, className }

-- | Render a siglet (compact inline signature) into a container element.
renderSigletInto :: String -> { ast :: RenderType } -> Effect Unit
renderSigletInto = Sigil.renderSigletInto

-- | Render a labeled siglet (dots with rotated identifier labels) into a container element.
renderLabeledSigletInto :: String -> { ast :: RenderType } -> Effect Unit
renderLabeledSigletInto = Sigil.renderLabeledSigletInto

-- | Render a data/newtype declaration into a container element.
renderDataDeclInto
  :: String
  -> { name :: String
     , typeParams :: Array String
     , constructors :: Array { name :: String, args :: Array RenderType }
     , keyword :: Maybe String
     }
  -> Effect Unit
renderDataDeclInto = Sigil.renderDataDeclInto

-- | Render a type class definition into a container element.
renderClassDeclInto
  :: String
  -> { name :: String
     , typeParams :: Array String
     , superclasses :: Array SuperclassInfo
     , methods :: Array { name :: String, ast :: Maybe RenderType }
     }
  -> Effect Unit
renderClassDeclInto = Sigil.renderClassDeclInto

-- | Render a type synonym into a container element.
renderTypeSynonymInto
  :: String
  -> { name :: String
     , typeParams :: Array String
     , body :: RenderType
     }
  -> Effect Unit
renderTypeSynonymInto = Sigil.renderTypeSynonymInto

-- | Render a foreign import into a container element.
renderForeignImportInto
  :: String
  -> { name :: String
     , ast :: RenderType
     }
  -> Effect Unit
renderForeignImportInto = Sigil.renderForeignImportInto

-- | Append HTML to an element (does not replace existing content).
foreign import appendHtmlInto :: String -> String -> Effect Unit
