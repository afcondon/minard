-- | Type Signature SVG Visualization
-- |
-- | Parses type signatures and renders them as SVG using hylograph-sigil.
-- | Replaces the old JS-based TypeSigRenderer global.
module CE2.Viz.TypeSignature
  ( renderInto
  , renderIntoWithKind
  , injectSparklines
  , SparklineCell
  , renderSignatureSVG
  , renderADTSVG
  , renderClassDefSVG
  , insertSVGIntoCell
  , RenderedSVG
  ) where

import Prelude
import Prim hiding (Constraint, Row)

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toMaybe)
import Data.Set as Set
import Effect (Effect)
import Web.DOM (Element)

import Hylograph.Sigil (layoutSignature, layoutADT, layoutClassDef, layoutSparkline, emit, emitNode)
import Hylograph.Sigil.Types (RenderType, SuperclassInfo)
import CE2.Viz.TypeSignature.TypeAST (parseToRenderType, collectTypeVars)

-- =============================================================================
-- Types
-- =============================================================================

type RenderedSVG = { svg :: Element, width :: Number, height :: Number }

-- | A sparkline cell: parsed RenderType + treemap position
type SparklineCell =
  { ast :: Maybe RenderType
  , x :: Number
  , y :: Number
  , width :: Number
  , height :: Number
  }

-- =============================================================================
-- FFI helpers
-- =============================================================================

foreign import replaceContainerContent :: String -> Element -> Effect Unit
foreign import showFallbackText :: String -> String -> Effect Unit
foreign import insertSVGIntoCell :: String -> Element -> Number -> Number -> Effect Unit

-- Sparkline DOM helpers
foreign import querySvgInContainer :: String -> Effect (Nullable Element)
foreign import removeOldSparklines :: Element -> Effect Unit
foreign import createSparklineGroup :: Effect Element
foreign import setSvgAttr :: Element -> String -> String -> Effect Unit
foreign import appendSvgChild :: Element -> Element -> Effect Unit

-- =============================================================================
-- Rendering functions (sigil-based)
-- =============================================================================

-- | Render a value/type-synonym signature to SVG element + dimensions.
renderSignatureSVG
  :: String -> String -> RenderType -> Array String -> Maybe String
  -> Effect RenderedSVG
renderSignatureSVG name sig ast typeParams className = do
  let result = layoutSignature { name, sig, ast, typeParams, className }
  el <- emit result.layout result.dimensions
  pure { svg: el, width: result.dimensions.width, height: result.dimensions.height }

-- | Render an ADT to SVG element + dimensions.
renderADTSVG
  :: String -> Array String -> Array { name :: String, args :: Array RenderType }
  -> Effect RenderedSVG
renderADTSVG name typeParams constructors = do
  let result = layoutADT { name, typeParams, constructors }
  el <- emit result.layout result.dimensions
  pure { svg: el, width: result.dimensions.width, height: result.dimensions.height }

-- | Render a class definition to SVG element + dimensions.
renderClassDefSVG
  :: String -> Array String -> Array SuperclassInfo
  -> Array { name :: String, ast :: Maybe RenderType }
  -> Effect RenderedSVG
renderClassDefSVG name typeParams superclasses methods = do
  let result = layoutClassDef { name, typeParams, superclasses, methods }
  el <- emit result.layout result.dimensions
  pure { svg: el, width: result.dimensions.width, height: result.dimensions.height }

-- =============================================================================
-- Container rendering (parse + layout + emit + insert into DOM)
-- =============================================================================

-- | Parse a type signature and render the SVG into a container element.
-- | If sig is Nothing, does nothing (container left empty).
-- | If parse fails, shows fallback plain text.
renderInto :: String -> String -> Maybe String -> Effect Unit
renderInto containerId declName = case _ of
  Nothing -> pure unit
  Just sig -> case parseToRenderType sig of
    Just ast -> do
      rendered <- renderSignatureSVG declName sig ast [] Nothing
      replaceContainerContent containerId rendered.svg
    Nothing ->
      showFallbackText containerId (declName <> " :: " <> sig)

-- | Like renderInto but extracts type params for type synonyms/data types.
renderIntoWithKind :: String -> String -> String -> Maybe String -> Effect Unit
renderIntoWithKind containerId declName kind = case _ of
  Nothing -> pure unit
  Just sig -> case parseToRenderType sig of
    Just ast -> do
      let typeVars = if kind == "type_synonym" || kind == "data" || kind == "newtype"
            then Set.toUnfoldable (collectTypeVars ast)
            else []
      rendered <- renderSignatureSVG declName sig ast typeVars Nothing
      replaceContainerContent containerId rendered.svg
    Nothing ->
      showFallbackText containerId (declName <> " :: " <> sig)

-- =============================================================================
-- Sparkline injection
-- =============================================================================

-- | Inject sparkline type signature visualizations into treemap cells.
-- | Appends a pointer-events:none overlay group to the treemap SVG.
injectSparklines :: String -> Array SparklineCell -> Effect Unit
injectSparklines containerSelector cells = do
  nullableSvg <- querySvgInContainer containerSelector
  case toMaybe nullableSvg of
    Nothing -> pure unit
    Just svg -> do
      removeOldSparklines svg
      group <- createSparklineGroup
      Array.foldM (\_ cell -> renderSparklineCell group cell) unit cells
      appendSvgChild svg group

renderSparklineCell :: Element -> SparklineCell -> Effect Unit
renderSparklineCell group cell = case cell.ast of
  Nothing -> pure unit
  Just ast -> do
    let minW = 60.0
        minH = 40.0
        pad = 4.0
        stripH = 3.0
        labelH = 14.0
    when (cell.width >= minW && cell.height >= minH) do
      let availW = cell.width - pad * 2.0
          availH = cell.height - stripH - labelH - pad
      case layoutSparkline { ast, maxWidth: availW, maxHeight: availH } of
        Nothing -> pure unit
        Just result -> do
          sparkEl <- emitNode result.layout
          let offsetX = cell.x + pad + (availW - result.scaledWidth) / 2.0
              offsetY = cell.y + stripH + pad + (availH - result.scaledHeight) / 2.0
          setSvgAttr sparkEl "transform" ("translate(" <> show offsetX <> "," <> show offsetY <> ")")
          appendSvgChild group sparkEl
