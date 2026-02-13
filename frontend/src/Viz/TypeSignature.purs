-- | Type Signature SVG Visualization
-- |
-- | Parses a type signature string via the CST parser and renders it as an SVG
-- | using the TypeSigRenderer (loaded as a global script).
module CE2.Viz.TypeSignature (renderInto, injectSparklines, SparklineCell) where

import Prelude
import Data.Maybe (Maybe(..))
import Effect (Effect)
import CE2.Viz.TypeSignature.TypeAST (JsResult, parseAndExport)

-- | Parse a type signature and render the SVG into a container element.
-- | If sig is Nothing, does nothing (container left empty).
-- | If parse fails, shows fallback plain text.
renderInto :: String -> String -> Maybe String -> Effect Unit
renderInto containerId declName = case _ of
  Nothing -> pure unit
  Just sig -> renderIntoFFI containerId declName sig (parseAndExport sig)

foreign import renderIntoFFI :: String -> String -> String -> JsResult -> Effect Unit

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
