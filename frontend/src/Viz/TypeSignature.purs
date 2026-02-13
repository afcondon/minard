-- | Type Signature SVG Visualization
-- |
-- | Parses a type signature string via the CST parser and renders it as an SVG
-- | using the TypeSigRenderer (loaded as a global script).
module CE2.Viz.TypeSignature
  ( renderInto
  , renderIntoWithKind
  , injectSparklines
  , SparklineCell
  , renderSignatureElement
  , renderSignatureWithParamsElement
  , renderADTElement
  , renderClassDefElement
  , measureSVGElement
  , insertSVGIntoCell
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable)
import Data.Set as Set
import Effect (Effect)
import Web.DOM (Element)
import CE2.Viz.TypeSignature.TypeAST (JsResult, parseAndExport, parseToRenderType, collectTypeVars)

-- | Parse a type signature and render the SVG into a container element.
-- | If sig is Nothing, does nothing (container left empty).
-- | If parse fails, shows fallback plain text.
renderInto :: String -> String -> Maybe String -> Effect Unit
renderInto containerId declName = case _ of
  Nothing -> pure unit
  Just sig -> renderIntoFFI containerId declName sig (parseAndExport sig) []

-- | Like renderInto but extracts type params for type synonyms/data types.
renderIntoWithKind :: String -> String -> String -> Maybe String -> Effect Unit
renderIntoWithKind containerId declName kind = case _ of
  Nothing -> pure unit
  Just sig ->
    let
      ast = parseAndExport sig
      typeVars = if kind == "type_synonym" || kind == "data" || kind == "newtype" then
        case parseToRenderType sig of
          Just rt -> Set.toUnfoldable (collectTypeVars rt)
          Nothing -> []
        else []
    in renderIntoFFI containerId declName sig ast typeVars

foreign import renderIntoFFI :: String -> String -> String -> JsResult -> Array String -> Effect Unit

-- | A sparkline cell: parsed AST + treemap position
type SparklineCell =
  { ast :: JsResult
  , x :: Number
  , y :: Number
  , width :: Number
  , height :: Number
  }

-- | Inject sparkline type signature visualizations into treemap cells.
-- | Appends a pointer-events:none overlay group to the treemap SVG.
foreign import injectSparklines :: String -> Array SparklineCell -> Effect Unit

-- =============================================================================
-- Element-returning FFI wrappers (for ModuleSignatureMap)
-- =============================================================================

-- | Returns a detached SVG element for a value/type synonym signature.
-- | Calls TypeSigRenderer.renderSignature with the pre-parsed AST.
foreign import renderSignatureElement :: String -> String -> JsResult -> Effect (Nullable Element)

-- | Like renderSignatureElement but with explicit type params rendered as styled pills.
-- | Used for type synonyms and data types where params aren't in a forall.
foreign import renderSignatureWithParamsElement
  :: String -> String -> JsResult -> Array String -> Effect (Nullable Element)

-- | Returns a detached SVG element for a data type (ADT tree rendering).
foreign import renderADTElement
  :: String                                          -- name
  -> Array String                                    -- type params
  -> Array { name :: String, args :: Array String }  -- constructors
  -> (String -> JsResult)                            -- parse function
  -> Effect (Nullable Element)

-- | Returns a detached SVG element for a type class definition.
foreign import renderClassDefElement
  :: String                                          -- name
  -> Array String                                    -- type params
  -> Array String                                    -- superclasses
  -> Array { name :: String, sig :: String }         -- methods
  -> (String -> JsResult)                            -- parse function
  -> Effect (Nullable Element)

-- | Measure an SVG element's width/height attributes.
foreign import measureSVGElement :: Element -> Effect { width :: Number, height :: Number }

-- | Insert an SVG element into a container div by ID, with padding and optional scale-down.
foreign import insertSVGIntoCell :: String -> Element -> Number -> Number -> Effect Unit
