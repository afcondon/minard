-- | Layer Diagram Panel Component
-- |
-- | Standalone Halogen component showing the internal call hierarchy of a module.
-- | Declarations are arranged in layers: leaf functions at the bottom,
-- | functions that call others stacked above. Edges show call relationships.
-- | Pure SVG rendering, self-contained computation.
module CE2.Component.LayerDiagramPanel
  ( component
  , Input
  , Output(..)
  , Query(..)
  , Slot
  ) where

import Prelude

import Data.Array as Array
import Data.Int as Int
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (min) as Num
import Data.String.CodeUnits as SCU
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import CE2.Data.Loader as Loader
import CE2.Util.SVG (svgElem, sa)
import CE2.Viz.DeclarationLayerDiagram as LayerDiagram

-- =============================================================================
-- Types
-- =============================================================================

type Input =
  { moduleName :: String
  , declarations :: Array Loader.V2Declaration
  , functionCalls :: Map.Map Int (Array Loader.V2FunctionCall)
  }

data Output
  = DeclarationClicked String  -- declaration name
  | DeclarationHovered (Maybe String)

data Query a = NoQuery a

type Slot = H.Slot Query Output

type State =
  { lastInput :: Input
  , layerLayout :: Maybe LayerDiagram.LayerLayout
  , hoveredNode :: Maybe String
  }

data Action
  = Initialize
  | Receive Input
  | NodeHovered (Maybe String)
  | NodeClicked String

-- =============================================================================
-- Component
-- =============================================================================

component :: forall m. MonadAff m => H.Component Query Input Output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , receive = Just <<< Receive
        , initialize = Just Initialize
        }
    }

initialState :: Input -> State
initialState input =
  { lastInput: input
  , layerLayout: Nothing
  , hoveredNode: Nothing
  }

-- =============================================================================
-- Render
-- =============================================================================

render :: forall m. State -> H.ComponentHTML Action () m
render state = case state.layerLayout of
  Nothing ->
    HH.div [ HP.style "padding: 24px; color: #999; font-size: 12px; text-align: center;" ]
      [ HH.text "No internal call hierarchy \x2014 declarations do not call each other" ]
  Just layout
    | Array.null layout.nodes ->
        HH.div [ HP.style "padding: 24px; color: #999; font-size: 12px; text-align: center;" ]
          [ HH.text "No internal call hierarchy \x2014 declarations do not call each other" ]
    | otherwise ->
        HH.div [ HP.style "padding: 8px;" ]
          [ HH.div [ HP.style "font-size: 11px; color: #888; margin-bottom: 6px;" ]
              [ HH.text $ "Call hierarchy \x2014 " <> show (Array.length layout.nodes) <> " declarations, "
                  <> show layout.maxLayer <> " layers" ]
          , svgElem "svg"
              [ sa "viewBox" ("0 0 " <> show layout.width <> " " <> show layout.height)
              , sa "width" "100%"
              , sa "preserveAspectRatio" "xMidYMid meet"
              , HP.style "display: block; border: 1px solid #d5d0c4; border-radius: 4px; background: #f0ede6;"
              ]
              ( renderBands layout
              <> (layout.edges <#> renderEdge state layout)
              <> (layout.nodes <#> renderNode state)
              <> (layout.nodes <#> renderLabel state)
              )
          ]

-- =============================================================================
-- SVG Rendering
-- =============================================================================

renderBands :: forall w i. LayerDiagram.LayerLayout -> Array (HH.HTML w i)
renderBands layout =
  Array.concatMap (\l ->
    let
      y = 30.0 + Int.toNumber (layout.maxLayer - l.layer) * 60.0
      isEven = l.layer `mod` 2 == 0
    in [ svgElem "rect"
           [ sa "x" "0", sa "y" (show y)
           , sa "width" (show layout.width), sa "height" "60"
           , sa "fill" (if isEven then "#eae7df" else "#f0ede6")
           ] []
       , svgElem "text"
           [ sa "x" "4", sa "y" (show (y + 12.0))
           , sa "font-size" "8", sa "fill" "#b8b0a0"
           , sa "font-family" "system-ui, sans-serif"
           ] [ HH.text $ "L" <> show l.layer ]
       ]
  ) layout.layers

renderEdge :: forall m. State -> LayerDiagram.LayerLayout -> LayerDiagram.LayerEdge -> H.ComponentHTML Action () m
renderEdge state _layout edge =
  let
    isHoverActive = state.hoveredNode /= Nothing
    isConnected = case state.hoveredNode of
      Nothing -> true
      Just hovered -> edge.fromName == hovered || edge.toName == hovered
    opacity = if isConnected then (if isHoverActive then "0.7" else "0.2") else "0.04"
    width = if isConnected && isHoverActive then "1.5" else "0.8"
    isViolation = edge.crossesLayers > 1
    color = if isViolation then "#c05a4e" else "#94a3b8"
    midY = (edge.fromY + edge.toY) / 2.0
    pathD = "M" <> show edge.fromX <> "," <> show edge.fromY
         <> " C" <> show edge.fromX <> "," <> show midY
         <> " " <> show edge.toX <> "," <> show midY
         <> " " <> show edge.toX <> "," <> show edge.toY
  in
  if isViolation then
    svgElem "line"
      [ sa "x1" (show edge.fromX), sa "y1" (show edge.fromY)
      , sa "x2" (show edge.toX), sa "y2" (show edge.toY)
      , sa "stroke" color, sa "stroke-width" width
      , sa "stroke-opacity" opacity, sa "stroke-dasharray" "4,2"
      , HP.style "transition: stroke-opacity 150ms ease;"
      ] []
  else
    svgElem "path"
      [ sa "d" pathD
      , sa "stroke" color, sa "stroke-width" width
      , sa "stroke-opacity" opacity, sa "fill" "none"
      , HP.style "transition: stroke-opacity 150ms ease;"
      ] []

renderNode :: forall m. State -> LayerDiagram.LayerNode -> H.ComponentHTML Action () m
renderNode state node =
  let
    isHovered = state.hoveredNode == Just node.name
    isConnected = case state.hoveredNode of
      Nothing -> true
      Just hovered -> hovered == node.name || nodeConnected hovered node.name state.layerLayout
    r = if isHovered then show (node.r + 2.0) else show node.r
    opacity = if isConnected then "1" else "0.2"
    fillColor = kindColor node.kind node.effectful
  in svgElem "circle"
    [ sa "cx" (show node.x), sa "cy" (show node.y)
    , sa "r" r
    , sa "fill" fillColor
    , sa "stroke" "#fff", sa "stroke-width" "0.8"
    , sa "opacity" opacity
    , sa "cursor" "pointer"
    , HE.onMouseEnter \_ -> NodeHovered (Just node.name)
    , HE.onMouseLeave \_ -> NodeHovered Nothing
    , HE.onClick \_ -> NodeClicked node.name
    ] []

renderLabel :: forall m. State -> LayerDiagram.LayerNode -> H.ComponentHTML Action () m
renderLabel state node =
  let
    isConnected = case state.hoveredNode of
      Nothing -> true
      Just hovered -> hovered == node.name || nodeConnected hovered node.name state.layerLayout
    opacity = if isConnected then "1" else "0.15"
    declLine = Array.findMap (\d ->
      if d.name == node.name then d.sourceSpan >>= \s -> Array.head s.start
      else Nothing
    ) state.lastInput.declarations
    lineTag = case declLine of
      Just l | l > 0 -> ":" <> show l
      _ -> ""
    baseName = if SCU.length node.name > 18 then SCU.take 17 node.name <> "\x2026" else node.name
    label = baseName <> lineTag
    labelY = node.y + node.r + 12.0
    labelColor = if node.effectful then "#d97706" else "#2563eb"
  in svgElem "text"
    [ sa "x" (show node.x), sa "y" (show labelY)
    , sa "text-anchor" "start"
    , sa "font-size" "8px"
    , sa "font-family" "system-ui, sans-serif"
    , sa "fill" labelColor
    , sa "opacity" opacity
    , sa "pointer-events" "none"
    , sa "transform" ("rotate(-45," <> show node.x <> "," <> show labelY <> ")")
    ]
    [ HH.text label ]

-- =============================================================================
-- Helpers
-- =============================================================================

kindColor :: String -> Boolean -> String
kindColor kind effectful
  | effectful = "#d97706"
  | otherwise = case kind of
      "value" -> "#3b82f6"
      "data" -> "#10b981"
      "newtype" -> "#10b981"
      "type_synonym" -> "#8b5cf6"
      "type_class" -> "#f59e0b"
      _ -> "#6b7280"

nodeConnected :: String -> String -> Maybe LayerDiagram.LayerLayout -> Boolean
nodeConnected a b = case _ of
  Nothing -> false
  Just layout -> Array.any (\e ->
    (e.fromName == a && e.toName == b) || (e.fromName == b && e.toName == a)
    ) layout.edges

computeLayout :: Input -> Maybe LayerDiagram.LayerLayout
computeLayout input =
  let layoutInput = { moduleName: input.moduleName
        , declarations: input.declarations
        , functionCalls: input.functionCalls
        , layoutWidth: 900.0
        }
      layout = LayerDiagram.computeLayout layoutInput
  in if Array.null layout.nodes then Nothing else Just layout

-- =============================================================================
-- Action Handlers
-- =============================================================================

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  Initialize -> do
    state <- H.get
    H.modify_ _ { layerLayout = computeLayout state.lastInput }

  Receive input -> do
    state <- H.get
    let changed = input.moduleName /= state.lastInput.moduleName
              || Array.length input.declarations /= Array.length state.lastInput.declarations
    H.modify_ _ { lastInput = input }
    when changed do
      H.modify_ _ { layerLayout = computeLayout input, hoveredNode = Nothing }

  NodeHovered mName -> do
    H.modify_ _ { hoveredNode = mName }
    H.raise (DeclarationHovered mName)

  NodeClicked name -> do
    H.raise (DeclarationClicked name)
