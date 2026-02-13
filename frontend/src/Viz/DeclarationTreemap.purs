-- | Declaration Treemap - Treemap of declarations within a single module
-- |
-- | Renders declarations as treemap cells sized by LOC (from sourceSpan),
-- | with kind-colored strips, packed child circles (constructors, members,
-- | synthetic arity args), and coordinated dependency highlighting.
-- | Designed for the left panel of the ModuleOverview split view.
module CE2.Viz.DeclarationTreemap
  ( Config
  , render
  , cleanup
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl)
import Data.Int (toNumber, floor)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (sqrt, min) as Number
import Data.String as String
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)

-- PSD3 HATS Imports
import Hylograph.HATS (Tree, elem, staticStr, thunkedStr, thunkedNum, forEach, withBehaviors, onCoordinatedHighlightWithTooltip, onClick)
import Hylograph.HATS.InterpreterTick (rerender, clearContainer)
import Hylograph.Internal.Selection.Types (ElementType(..))
import Hylograph.Internal.Behavior.Types (HighlightClass(..), TooltipTrigger(..))

-- Layout
import DataViz.Layout.Hierarchy.Types (ValuedNode(..))
import DataViz.Layout.Hierarchy.Treemap (TreemapNode(..), treemap, defaultTreemapConfig, squarify, phi)

import CE2.Data.Loader (V2Declaration, V2FunctionCall)
import CE2.Viz.ModuleTreemapEnriched (ChildCircle, kindColor, childCircleElem, packChildren, syntheticArityChildren)
import CE2.Viz.TypeSignature as TypeSignature
import CE2.Viz.TypeSignature.TypeAST (parseAndExport)

-- =============================================================================
-- Types
-- =============================================================================

-- | Configuration for declaration treemap
type Config =
  { containerSelector :: String
  , width :: Number
  , height :: Number
  , packageName :: String
  , moduleName :: String
  , onDeclarationClick :: Maybe (String -> String -> String -> Effect Unit)  -- pkg -> mod -> decl
  , focusedDeclaration :: Maybe String  -- When Just, dimmed background mode with one cell highlighted
  }

-- | Declaration with computed treemap position and packed children
type PositionedDeclaration =
  { decl :: V2Declaration
  , x :: Number
  , y :: Number
  , width :: Number
  , height :: Number
  , calls :: Array String      -- fullNames this decl calls
  , calledBy :: Array String   -- fullNames that call this decl
  , packedChildren :: Array ChildCircle  -- Packed child circles (constructors, members, args)
  , packRadius :: Number                 -- Enclosing radius of the child pack
  }

-- =============================================================================
-- Public API
-- =============================================================================

-- | Render the declaration treemap into the container
render
  :: Config
  -> Array V2Declaration
  -> Map Int (Array V2FunctionCall)
  -> Effect Unit
render config declarations callsMap = do
  clearContainer config.containerSelector

  when (Array.length declarations > 0) do
    -- Build dependency maps scoped to this module
    let { callsTo, calledByMap } = buildSingleModuleDependencyMaps config.moduleName declarations callsMap

    log $ "[DeclarationTreemap] Module: " <> config.moduleName
        <> ", declarations: " <> show (Array.length declarations)
        <> ", callsTo: " <> show (Map.size callsTo)
        <> ", calledBy: " <> show (Map.size calledByMap)

    -- Compute treemap layout
    let positioned = computeDeclarationPositions config declarations callsTo calledByMap

    -- Build position map for dependency links
    let posMap = buildPositionMap positioned

    -- Render SVG
    let svgTree = buildTreemapSVG config positioned posMap
    _ <- rerender config.containerSelector svgTree

    -- Inject sparkline type signature visualizations into treemap cells
    let sparkCells = Array.mapMaybe (\pd ->
          case pd.decl.typeSignature of
            Just sig -> Just
              { ast: parseAndExport sig
              , x: pd.x
              , y: pd.y
              , width: pd.width
              , height: pd.height
              }
            Nothing -> Nothing
        ) positioned
    TypeSignature.injectSparklines config.containerSelector sparkCells

-- | Clean up
cleanup :: String -> Effect Unit
cleanup selector = clearContainer selector

-- =============================================================================
-- Dependency Maps (scoped to single module)
-- =============================================================================

-- | Build call maps for declarations within a single module
buildSingleModuleDependencyMaps
  :: String  -- module name
  -> Array V2Declaration
  -> Map Int (Array V2FunctionCall)
  -> { callsTo :: Map String (Array String), calledByMap :: Map String (Array String) }
buildSingleModuleDependencyMaps moduleName _declarations callsMap =
  let
    allCalls :: Array { caller :: String, callee :: String }
    allCalls = Array.concatMap processCalls (Array.fromFoldable (Map.values callsMap))

    processCalls :: Array V2FunctionCall -> Array { caller :: String, callee :: String }
    processCalls calls = calls <#> \c ->
      { caller: moduleName <> "." <> c.callerName
      , callee: c.calleeModule <> "." <> c.calleeName
      }

    callsTo = foldl (\acc { caller, callee } ->
        Map.alter (Just <<< Array.cons callee <<< fromMaybe []) caller acc
      ) Map.empty allCalls

    calledByMap = foldl (\acc { caller, callee } ->
        Map.alter (Just <<< Array.cons caller <<< fromMaybe []) callee acc
      ) Map.empty allCalls
  in
    { callsTo, calledByMap }

-- =============================================================================
-- Treemap Layout
-- =============================================================================

-- | Compute LOC for a declaration from its sourceSpan
declarationLoc :: V2Declaration -> Int
declarationLoc decl = case decl.sourceSpan of
  Just span -> case { start: Array.index span.start 0, end_: Array.index span.end 0 } of
    { start: Just startLine, end_: Just endLine } -> max 1 (endLine - startLine + 1)
    _ -> fallbackSize decl
  Nothing -> fallbackSize decl
  where
    fallbackSize d = max 1 (Array.length d.children + 1)

-- | Compute treemap positions for declarations, with packed children
computeDeclarationPositions
  :: Config
  -> Array V2Declaration
  -> Map String (Array String)  -- callsTo
  -> Map String (Array String)  -- calledBy
  -> Array PositionedDeclaration
computeDeclarationPositions config declarations callsTo calledByMap =
  let
    declLeaves :: Array (ValuedNode V2Declaration)
    declLeaves = declarations <#> \d ->
      VNode
        { data_: d
        , depth: 1
        , height: 0
        , value: toNumber (declarationLoc d)
        , children: []
        , parent: Nothing
        }

    totalValue = foldl (\acc (VNode n) -> acc + n.value) 0.0 declLeaves

    rootData :: V2Declaration
    rootData =
      { id: 0
      , name: config.moduleName
      , kind: "module"
      , typeSignature: Nothing
      , comments: Nothing
      , dataDeclType: Nothing
      , sourceSpan: Nothing
      , sourceCode: Nothing
      , children: []
      }

    root :: ValuedNode V2Declaration
    root = VNode
      { data_: rootData
      , depth: 0
      , height: 1
      , value: totalValue
      , children: declLeaves
      , parent: Nothing
      }

    treemapConfig = defaultTreemapConfig
      { size = { width: config.width, height: config.height }
      , paddingInner = 3.0
      , paddingOuter = 2.0
      , tile = squarify phi
      }

    TNode layoutRoot = treemap treemapConfig root
  in
    layoutRoot.children <#> \(TNode child) ->
      let
        fullName = config.moduleName <> "." <> child.data_.name
        cellWidth = child.x1 - child.x0
        cellHeight = child.y1 - child.y0

        -- Pack children (real or synthetic) into this cell
        effectiveChildren = if Array.length child.data_.children > 0
          then child.data_.children
          else case child.data_.kind of
            "value" -> syntheticArityChildren child.data_.typeSignature
            _ -> []

        { children: packed, packRadius } =
          packChildren effectiveChildren config.moduleName child.data_.name child.data_.kind

        -- Fallback: if no children at all, create a single "self" circle
        -- so every cell has some visual content
        selfCircleFallback :: Array ChildCircle
        selfCircleFallback =
          [ { name: child.data_.name
            , fullName: fullName
            , kind: child.data_.kind
            , typeSignature: child.data_.typeSignature
            , x: 0.0
            , y: 0.0
            , r: 5.0
            } ]

        finalChildren = if Array.null packed then selfCircleFallback else packed
        finalPackRadius = if Array.null packed then 5.0 else packRadius

        -- Scale children to fit within the cell, leaving room for strip + label
        stripHeight = 3.0
        labelSpace = 14.0  -- bottom label area
        availW = cellWidth * 0.85
        availH = (cellHeight - stripHeight - labelSpace) * 0.85
        fitRadius = Number.min (availW / 2.0) (availH / 2.0)
        scaleFactor = if finalPackRadius > 0.0 && fitRadius > 0.0
                      then min (fitRadius / finalPackRadius) 3.0  -- cap upscale
                      else 1.0

        scaledChildren = finalChildren <#> \c -> c
          { x = c.x * scaleFactor
          , y = c.y * scaleFactor
          , r = c.r * scaleFactor
          }
      in
        { decl: child.data_
        , x: child.x0
        , y: child.y0
        , width: cellWidth
        , height: cellHeight
        , calls: fromMaybe [] $ Map.lookup fullName callsTo
        , calledBy: fromMaybe [] $ Map.lookup fullName calledByMap
        , packedChildren: scaledChildren
        , packRadius: finalPackRadius * scaleFactor
        }

-- =============================================================================
-- Position Map (for dependency links)
-- =============================================================================

type AbsolutePosition = { x :: Number, y :: Number }

buildPositionMap :: Array PositionedDeclaration -> Map String AbsolutePosition
buildPositionMap positioned =
  Map.fromFoldable $ positioned <#> \pd ->
    Tuple (pd.decl.name) { x: pd.x + pd.width / 2.0, y: pd.y + pd.height / 2.0 }

-- =============================================================================
-- SVG Rendering (HATS)
-- =============================================================================

buildTreemapSVG :: Config -> Array PositionedDeclaration -> Map String AbsolutePosition -> Tree
buildTreemapSVG config positioned posMap =
  let
    isFocusedMode = case config.focusedDeclaration of
      Just _ -> true
      Nothing -> false

    children =
      [ forEach "decls" Group positioned (\pd -> pd.decl.name) (declarationCell config)
      ] <>
        -- Skip dependency links in focused mode (the neighborhood overlay shows them)
        if isFocusedMode then []
        else [ renderDependencyLinks config positioned posMap ]
  in
    elem SVG
      [ staticStr "id" "declaration-treemap"
      , staticStr "viewBox" $ "0 0 " <> show config.width <> " " <> show config.height
      , staticStr "width" "100%"
      , staticStr "height" "100%"
      , staticStr "preserveAspectRatio" "xMidYMid meet"
      , staticStr "style" "background: transparent; display: block; border-radius: 4px;"
      ]
      children

-- | Render a single declaration cell with packed child circles
declarationCell :: Config -> PositionedDeclaration -> Tree
declarationCell config pd =
  let
    fullName = config.moduleName <> "." <> pd.decl.name
    tooltipText = buildTooltip pd
    clickBehavior = case config.onDeclarationClick of
      Nothing -> []
      Just handler -> [ onClick (handler config.packageName config.moduleName pd.decl.name) ]

    cellWidth = pd.width
    cellHeight = pd.height
    stripHeight = 3.0

    -- Center point for child circles (offset down to account for strip)
    centerX = cellWidth / 2.0
    centerY = stripHeight + (cellHeight - stripHeight) / 2.0

    hasChildren = not (Array.null pd.packedChildren)

    -- In focused mode, use static highlight classes instead of coordinated highlighting
    behaviors = case config.focusedDeclaration of
      Just _ ->
        -- Static mode: focused cell is Primary, all others Dimmed
        clickBehavior
      Nothing ->
        -- Interactive mode: coordinated highlighting with tooltip
        [ onCoordinatedHighlightWithTooltip
            { identify: fullName
            , classify: \hoveredId ->
                if fullName == hoveredId then Primary
                else if Array.elem hoveredId pd.calls then Related
                else if Array.elem hoveredId pd.calledBy then Related
                else Dimmed
            , group: Just "declarations"
            , tooltip: Just { content: tooltipText, showWhen: OnHover }
            }
        ] <> clickBehavior

    -- Static highlight class for focused mode
    highlightClass = case config.focusedDeclaration of
      Just focusName
        | focusName == pd.decl.name -> " highlight-primary"
        | otherwise -> " highlight-dimmed"
      Nothing -> ""
  in
    withBehaviors behaviors
    $ elem Group
        [ thunkedStr "transform" ("translate(" <> show pd.x <> "," <> show pd.y <> ")")
        , staticStr "class" ("treemap-declaration" <> highlightClass)
        , staticStr "cursor" "pointer"
        ]
        ( [ -- Background rect with muted kind tint
            elem Rect
              [ staticStr "x" "0"
              , staticStr "y" "0"
              , thunkedNum "width" cellWidth
              , thunkedNum "height" cellHeight
              , thunkedStr "fill" (kindFill pd.decl.kind)
              , staticStr "stroke" "rgba(255, 255, 255, 0.15)"
              , staticStr "stroke-width" "1"
              , staticStr "rx" "2"
              , staticStr "class" "declaration-rect"
              ]
              []
          -- Kind indicator strip at top
          , elem Rect
              [ staticStr "x" "0"
              , staticStr "y" "0"
              , thunkedNum "width" cellWidth
              , thunkedNum "height" stripHeight
              , thunkedStr "fill" (kindColor pd.decl.kind)
              , staticStr "rx" "2"
              , staticStr "class" "declaration-strip"
              ]
              []
          -- Packed child circles (constructors, members, args)
          , elem Group
              [ thunkedStr "transform"
                  ("translate(" <> show centerX <> "," <> show centerY <> ")")
              , staticStr "class" "declaration-children"
              , staticStr "pointer-events" "none"
              ]
              (map (childCircleElem pd.decl.kind) pd.packedChildren)
          -- Declaration name label (at bottom if children present, centered if not)
          , elem Text
              [ thunkedNum "x" (cellWidth / 2.0)
              , thunkedNum "y" (if hasChildren then cellHeight - 5.0 else cellHeight / 2.0 + 2.0)
              , staticStr "text-anchor" "middle"
              , staticStr "dominant-baseline" (if hasChildren then "auto" else "central")
              , thunkedStr "font-size" (labelFontSize cellWidth cellHeight)
              , staticStr "fill" "rgba(255, 255, 255, 0.9)"
              , staticStr "font-family" "system-ui, sans-serif"
              , staticStr "font-weight" "500"
              , staticStr "pointer-events" "none"
              , thunkedStr "textContent" (truncateLabel cellWidth pd.decl.name)
              , thunkedStr "opacity" (if cellWidth > 25.0 && cellHeight > 18.0 then "1" else "0")
              ]
              []
          -- Kind abbreviation in top-right
          , elem Text
              [ thunkedNum "x" (cellWidth - 3.0)
              , thunkedNum "y" (stripHeight + 9.0)
              , staticStr "text-anchor" "end"
              , staticStr "font-size" "6"
              , thunkedStr "fill" (kindColor pd.decl.kind)
              , staticStr "font-family" "system-ui, sans-serif"
              , staticStr "font-weight" "600"
              , staticStr "pointer-events" "none"
              , thunkedStr "textContent"
                  (if cellWidth > 30.0 && cellHeight > 20.0
                   then kindAbbrev pd.decl.kind
                   else "")
              ]
              []
          ]
        )

-- =============================================================================
-- Dependency Links
-- =============================================================================

renderDependencyLinks :: Config -> Array PositionedDeclaration -> Map String AbsolutePosition -> Tree
renderDependencyLinks config positioned posMap =
  let
    links = Array.concatMap (outgoingLinks config posMap) positioned
  in
    elem Group
      [ staticStr "class" "dependency-links"
      , staticStr "pointer-events" "none"
      ]
      links

outgoingLinks :: Config -> Map String AbsolutePosition -> PositionedDeclaration -> Array Tree
outgoingLinks config posMap pd =
  let
    fromFullName = config.moduleName <> "." <> pd.decl.name
    fromPos = { x: pd.x + pd.width / 2.0, y: pd.y + pd.height / 2.0 }
  in
    pd.calls # Array.mapMaybe \calleeFullName ->
      let calleeName = case Array.last (String.split (String.Pattern ".") calleeFullName) of
                          Just n -> n
                          Nothing -> calleeFullName
      in Map.lookup calleeName posMap <#> \toPos ->
        linkPath fromPos toPos fromFullName calleeFullName

linkPath :: AbsolutePosition -> AbsolutePosition -> String -> String -> Tree
linkPath from to fromName toName =
  let
    dx = to.x - from.x
    dy = to.y - from.y
    dist = Number.sqrt (dx * dx + dy * dy)

    curvature = Number.min 0.3 (dist / 400.0) * 30.0
    perpX = if dist > 0.0 then -dy / dist * curvature else 0.0
    perpY = if dist > 0.0 then dx / dist * curvature else 0.0
    midX = (from.x + to.x) / 2.0
    midY = (from.y + to.y) / 2.0
    cx = midX + perpX
    cy = midY + perpY

    pathD = "M " <> show from.x <> " " <> show from.y
         <> " Q " <> show cx <> " " <> show cy
         <> " " <> show to.x <> " " <> show to.y

    linkId = fromName <> "→" <> toName
  in
    withBehaviors
      [ onCoordinatedHighlightWithTooltip
          { identify: linkId
          , classify: \hoveredId ->
              if hoveredId == fromName || hoveredId == toName then Related
              else Dimmed
          , group: Just "declarations"
          , tooltip: Nothing
          }
      ]
    $ elem Path
        [ thunkedStr "d" pathD
        , staticStr "fill" "none"
        , staticStr "stroke" "#f59e0b"
        , staticStr "stroke-width" "0.75"
        , staticStr "stroke-opacity" "0.3"
        , staticStr "class" "dependency-link"
        ]
        []

-- =============================================================================
-- Utilities
-- =============================================================================

-- | Muted tint of kind color for cell backgrounds
kindFill :: String -> String
kindFill = case _ of
  "value"        -> "rgba(78, 121, 167, 0.12)"
  "data"         -> "rgba(89, 161, 79, 0.12)"
  "newtype"      -> "rgba(118, 183, 178, 0.12)"
  "type_class"   -> "rgba(242, 142, 43, 0.12)"
  "type_synonym" -> "rgba(237, 201, 72, 0.12)"
  "foreign"      -> "rgba(225, 87, 89, 0.12)"
  "alias"        -> "rgba(176, 122, 161, 0.12)"
  _              -> "rgba(255, 255, 255, 0.06)"

-- | Kind abbreviation for cell labels
kindAbbrev :: String -> String
kindAbbrev = case _ of
  "value"        -> "val"
  "data"         -> "dat"
  "newtype"      -> "nt"
  "type_class"   -> "cls"
  "type_synonym" -> "typ"
  "foreign"      -> "ffi"
  "alias"        -> "als"
  _              -> ""

-- | Font size based on cell dimensions
labelFontSize :: Number -> Number -> String
labelFontSize w h =
  let area = w * h
  in if area > 4000.0 then "11"
     else if area > 2000.0 then "9"
     else if area > 800.0 then "7"
     else "6"

-- | Truncate label to fit cell width
truncateLabel :: Number -> String -> String
truncateLabel cellWidth name =
  let maxLen = max 3 (floor (cellWidth / 6.5))
  in if String.length name > maxLen
     then String.take maxLen name <> "…"
     else name

-- | Tooltip for a declaration cell
buildTooltip :: PositionedDeclaration -> String
buildTooltip pd =
  let
    kindStr = case pd.decl.kind of
      "value" -> "value"
      "data" -> "data"
      "newtype" -> "newtype"
      "type_class" -> "class"
      "type_synonym" -> "type"
      "foreign" -> "foreign"
      _ -> pd.decl.kind

    header = pd.decl.name <> " :: " <> kindStr

    typeSig = case pd.decl.typeSignature of
      Just sig -> "\n" <> sig
      Nothing -> ""

    childInfo = if Array.length pd.decl.children > 0
      then "\n(" <> show (Array.length pd.decl.children) <> " children)"
      else ""

    depInfo =
      let nCalls = Array.length pd.calls
          nCalledBy = Array.length pd.calledBy
      in if nCalls > 0 || nCalledBy > 0
         then "\n→ " <> show nCalls <> " calls, ← " <> show nCalledBy <> " callers"
         else ""
  in
    header <> typeSig <> childInfo <> depInfo
