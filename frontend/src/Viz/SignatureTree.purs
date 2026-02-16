-- | Type signature rendering — thin wrapper over sigil Html module.
-- |
-- | Preserves the original API surface for backward compatibility while
-- | delegating all rendering to the standalone library.
module CE2.Viz.SignatureTree
  ( renderSignatureInto
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

-- | Render a full-size signature into a container element.
renderSignatureInto
  :: String
  -> { name :: String, sig :: String, ast :: RenderType, typeParams :: Array String, className :: Maybe String }
  -> Effect Unit
renderSignatureInto selector { name, ast, typeParams, className } =
  Sigil.renderSignatureInto selector { name, ast, typeParams, className }

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
