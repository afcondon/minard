-- | Concerns Panel Component
-- |
-- | Standalone Halogen component showing concern clusters in a module.
-- | Analyzes case expression branches to identify groups of declarations
-- | that share state fields (concerns). Renders as a force-directed graph
-- | with group circles. Pure SVG, self-contained data fetching.
module CE2.Component.ConcernsPanel
  ( component
  , Input
  , Output(..)
  , Query(..)
  , Slot
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Int as Int
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number as Number
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import CE2.Data.Loader as Loader
import CE2.Data.SubDeclarationAnalysis as SDA
import CE2.Util.SVG (svgElem, sa)

-- =============================================================================
-- Types
-- =============================================================================

type Input =
  { moduleName :: String
  }

data Output
  = DeclarationClicked String

data Query a = NoQuery a

type Slot = H.Slot Query Output

data LoadState
  = Loading
  | NoData String
  | Loaded ConcernLayout

type State =
  { lastInput :: Input
  , loadState :: LoadState
  , hoveredNode :: Maybe String
  }

data Action
  = Initialize
  | Receive Input
  | NodeHovered (Maybe String)
  | NodeClicked String

-- | Pre-computed layout for rendering
type ConcernLayout =
  { groups :: Array GroupInfo
  , nodes :: Array NodeInfo
  , edges :: Array EdgeInfo
  , viewBox :: { x :: Number, y :: Number, w :: Number, h :: Number }
  }

type GroupInfo =
  { index :: Int
  , functionName :: String
  , branchCount :: Int
  , center :: { x :: Number, y :: Number }
  , radius :: Number
  }

type NodeInfo =
  { name :: String
  , x :: Number
  , y :: Number
  , group :: Int
  }

type EdgeInfo =
  { from :: String
  , to :: String
  , fromX :: Number
  , fromY :: Number
  , toX :: Number
  , toY :: Number
  , sameGroup :: Boolean
  , group :: Int
  }

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
  , loadState: Loading
  , hoveredNode: Nothing
  }

-- =============================================================================
-- Render
-- =============================================================================

render :: forall m. State -> H.ComponentHTML Action () m
render state = case state.loadState of
  Loading ->
    HH.div [ HP.style "padding: 24px; color: #999; font-size: 12px; text-align: center;" ]
      [ HH.text "Analyzing module source..." ]
  NoData msg ->
    HH.div [ HP.style "padding: 24px; color: #999; font-size: 12px; text-align: center;" ]
      [ HH.text msg ]
  Loaded layout ->
    renderConcernGraph state layout

renderConcernGraph :: forall m. State -> ConcernLayout -> H.ComponentHTML Action () m
renderConcernGraph state layout =
  HH.div [ HP.style "padding: 8px;" ]
    [ HH.div [ HP.style "font-size: 11px; color: #888; margin-bottom: 6px;" ]
        [ HH.text $ "Declarations grouped by shared sub-expressions. "
            <> show (Array.length layout.groups) <> " concern groups." ]
    , svgElem "svg"
        [ sa "viewBox" (show layout.viewBox.x <> " " <> show layout.viewBox.y <> " " <> show layout.viewBox.w <> " " <> show layout.viewBox.h)
        , sa "width" "100%"
        , sa "preserveAspectRatio" "xMidYMid meet"
        , HP.style "display: block; border: 1px solid #d5d0c4; border-radius: 4px; background: #f0ede6; max-height: 500px;"
        ]
        ( -- Group background circles
          (layout.groups <#> renderGroupBg)
          -- Edges
          <> (layout.edges <#> renderEdge state layout)
          -- Nodes
          <> (layout.nodes <#> renderNode state layout)
          -- Group labels
          <> (layout.groups <#> renderGroupLabel)
        )
    ]

renderGroupBg :: forall w i. GroupInfo -> HH.HTML w i
renderGroupBg g =
  svgElem "circle"
    [ sa "cx" (show g.center.x), sa "cy" (show g.center.y)
    , sa "r" (show g.radius)
    , sa "fill" (blockColor g.index), sa "fill-opacity" "0.08"
    , sa "stroke" (blockColor g.index), sa "stroke-opacity" "0.2"
    , sa "stroke-width" "1.5"
    ] []

renderGroupLabel :: forall w i. GroupInfo -> HH.HTML w i
renderGroupLabel g =
  svgElem "text"
    [ sa "x" (show g.center.x), sa "y" (show (g.center.y - g.radius - 14.0))
    , sa "text-anchor" "middle", sa "font-size" "9px"
    , sa "font-weight" "600"
    , sa "fill" (blockColor g.index), sa "font-family" "system-ui, sans-serif"
    ]
    [ HH.text $ g.functionName <> " (" <> show g.branchCount <> ")" ]

renderEdge :: forall m. State -> ConcernLayout -> EdgeInfo -> H.ComponentHTML Action () m
renderEdge state _layout edge =
  let
    isConnected = case state.hoveredNode of
      Nothing -> true
      Just hovered -> edge.from == hovered || edge.to == hovered
    opacity = if isConnected then "0.2" else "0.03"
  in
  if edge.sameGroup then
    let midX = (edge.fromX + edge.toX) / 2.0
        midY = (edge.fromY + edge.toY) / 2.0
        -- Find group center for curve control point
        mGroup = Array.find (\g -> g.index == edge.group) _layout.groups
        cpx = case mGroup of
          Just g -> midX * 0.4 + g.center.x * 0.6
          Nothing -> midX
        cpy = case mGroup of
          Just g -> midY * 0.4 + g.center.y * 0.6
          Nothing -> midY
        d = "M" <> show edge.fromX <> "," <> show edge.fromY
          <> " Q" <> show cpx <> "," <> show cpy
          <> " " <> show edge.toX <> "," <> show edge.toY
    in
    svgElem "path"
      [ sa "d" d, sa "fill" "none"
      , sa "stroke" (blockColor edge.group)
      , sa "stroke-width" "0.5", sa "stroke-opacity" opacity
      , HP.style "transition: stroke-opacity 150ms ease;"
      ] []
  else
    svgElem "line"
      [ sa "x1" (show edge.fromX), sa "y1" (show edge.fromY)
      , sa "x2" (show edge.toX), sa "y2" (show edge.toY)
      , sa "stroke" (blockColor edge.group)
      , sa "stroke-width" "1.0", sa "stroke-opacity" opacity
      , HP.style "transition: stroke-opacity 150ms ease;"
      ] []

renderNode :: forall m. State -> ConcernLayout -> NodeInfo -> H.ComponentHTML Action () m
renderNode state _layout node =
  let
    isHovered = state.hoveredNode == Just node.name
    isConnected = case state.hoveredNode of
      Nothing -> true
      Just hovered -> hovered == node.name || nodesConnected hovered node.name _layout.edges
    opacity = if isConnected then "1" else "0.2"
    r = if isHovered then "5" else "3.5"
  in
  svgElem "g" [ sa "cursor" "pointer" ]
    [ svgElem "circle"
        [ sa "cx" (show node.x), sa "cy" (show node.y), sa "r" r
        , sa "fill" (blockColor node.group)
        , sa "stroke" "#fff", sa "stroke-width" "0.8"
        , sa "opacity" opacity
        , HE.onMouseEnter \_ -> NodeHovered (Just node.name)
        , HE.onMouseLeave \_ -> NodeHovered Nothing
        , HE.onClick \_ -> NodeClicked node.name
        , HP.style "transition: opacity 150ms ease;"
        ] []
    , svgElem "text"
        [ sa "x" (show node.x), sa "y" (show (node.y - 8.0))
        , sa "text-anchor" "middle", sa "font-size" "7px"
        , sa "fill" "#555", sa "font-family" "system-ui, sans-serif"
        , sa "opacity" (if isHovered then "1" else "0")
        , sa "pointer-events" "none"
        ]
        [ HH.text node.name ]
    ]

-- =============================================================================
-- Layout Computation
-- =============================================================================

computeLayout :: SDA.SubDeclAnalysis -> Maybe ConcernLayout
computeLayout analysis =
  if Array.null analysis.caseExpressions then Nothing
  else
    let
      caseExprs = analysis.caseExpressions
      nGroups = Array.length caseExprs
      width = 900.0
      height = 600.0
      centerX = width / 2.0
      centerY = height / 2.0

      -- Build graph from branches
      { declarations, internalCalls } = SDA.branchesToDeclGraph analysis.allBranches
      declNames = Set.fromFoldable $ declarations <#> _.name
      graphEdges = foldl (\acc call ->
        if Set.member call.callerName declNames && Set.member call.calleeName declNames
        then
          Map.alter (Just <<< Set.insert call.calleeName <<< fromMaybe Set.empty) call.callerName
            (Map.alter (Just <<< Set.insert call.callerName <<< fromMaybe Set.empty) call.calleeName acc)
        else acc
      ) Map.empty internalCalls
      graph = { nodes: Set.toUnfoldable declNames :: Array String, edges: graphEdges }

      -- Branch → group index
      branchGroup = foldl (\acc (Tuple gi ce) ->
        foldl (\a branch -> Map.insert branch.name gi a) acc ce.branches
      ) Map.empty (mapWithIndex Tuple caseExprs)

      -- Cross-group edge weights
      crossGroupWeights = foldl (\acc name ->
        let targets = fromMaybe Set.empty (Map.lookup name graphEdges)
        in foldl (\a tgt ->
          if name < tgt then
            case Map.lookup name branchGroup, Map.lookup tgt branchGroup of
              Just gi, Just gj | gi /= gj ->
                let key = Tuple (min gi gj) (max gi gj)
                in Map.alter (Just <<< (_ + 1) <<< fromMaybe 0) key a
              _, _ -> a
          else a
        ) acc (Set.toUnfoldable targets :: Array String)
      ) (Map.empty :: Map.Map (Tuple Int Int) Int) graph.nodes

      -- Group radii
      groupRadii = mapWithIndex (\_ ce ->
        max 20.0 (Number.sqrt (Int.toNumber (Array.length ce.branches)) * 12.0)
      ) caseExprs

      -- Force layout
      mainRadius = min (width * 0.32) (height * 0.32)
      initialPositions = mapWithIndex (\i _ce ->
        let angle = 2.0 * Number.pi * Int.toNumber i / Int.toNumber (max nGroups 1) - Number.pi / 2.0
        in { x: centerX + mainRadius * Number.cos angle
           , y: centerY + mainRadius * Number.sin angle
           , vx: 0.0, vy: 0.0
           }
      ) caseExprs

      groupCenters = map (\p -> { x: p.x, y: p.y }) $
        forceLayoutGroups { width, height, centerX, centerY }
          crossGroupWeights groupRadii initialPositions 200

      -- Groups
      groups = Array.mapMaybe (\(Tuple gi ce) ->
        case groupCenters Array.!! gi of
          Nothing -> Nothing
          Just center ->
            let r = max 20.0 (Number.sqrt (Int.toNumber (Array.length ce.branches)) * 12.0)
            in Just { index: gi, functionName: ce.functionName, branchCount: Array.length ce.branches, center, radius: r }
      ) (mapWithIndex Tuple caseExprs)

      -- Nodes
      nodePositions = foldl (\acc (Tuple gi ce) ->
        let
          center = fromMaybe { x: centerX, y: centerY } (groupCenters Array.!! gi)
          n = Array.length ce.branches
          r = max 20.0 (Number.sqrt (Int.toNumber n) * 12.0)
        in foldl (\a (Tuple ni branch) ->
          let
            angle = 2.0 * Number.pi * Int.toNumber ni / Int.toNumber (max n 1) - Number.pi / 2.0
            x = center.x + r * Number.cos angle
            y = center.y + r * Number.sin angle
          in Map.insert branch.name { name: branch.name, x, y, group: gi } a
        ) acc (mapWithIndex Tuple ce.branches)
      ) Map.empty (mapWithIndex Tuple caseExprs)

      nodes = Array.fromFoldable $ Map.values nodePositions

      -- Edges
      edges = Array.concatMap (\name ->
        let targets = fromMaybe Set.empty (Map.lookup name graphEdges)
        in Array.mapMaybe (\tgt ->
          if name < tgt then
            case Map.lookup name nodePositions, Map.lookup tgt nodePositions of
              Just p1, Just p2 ->
                Just { from: name, to: tgt, fromX: p1.x, fromY: p1.y, toX: p2.x, toY: p2.y
                     , sameGroup: p1.group == p2.group, group: p1.group }
              _, _ -> Nothing
          else Nothing
        ) (Set.toUnfoldable targets :: Array String)
      ) graph.nodes

      -- Bounding box
      groupExtents = groups <#> \g ->
        let r = g.radius + 16.0
        in { minX: g.center.x - r, maxX: g.center.x + r, minY: g.center.y - r - 10.0, maxY: g.center.y + r }
      bbox = foldl (\acc e ->
        { minX: min acc.minX e.minX, maxX: max acc.maxX e.maxX
        , minY: min acc.minY e.minY, maxY: max acc.maxY e.maxY }
      ) { minX: width, maxX: 0.0, minY: height, maxY: 0.0 } groupExtents
      pad = 20.0
      viewBox = { x: bbox.minX - pad, y: bbox.minY - pad
                , w: max 100.0 (bbox.maxX - bbox.minX + pad * 2.0)
                , h: max 100.0 (bbox.maxY - bbox.minY + pad * 2.0) }

    in Just { groups, nodes, edges, viewBox }

-- =============================================================================
-- Force Layout
-- =============================================================================

type ForceNode = { x :: Number, y :: Number, vx :: Number, vy :: Number }

forceLayoutGroups
  :: { width :: Number, height :: Number, centerX :: Number, centerY :: Number }
  -> Map.Map (Tuple Int Int) Int
  -> Array Number
  -> Array ForceNode
  -> Int
  -> Array ForceNode
forceLayoutGroups bounds weights radii initial iterations =
  let
    n = Array.length initial
    alpha0 = 1.0
    decay = alpha0 / Int.toNumber iterations
    step :: Number -> Array ForceNode -> Array ForceNode
    step alpha nodes =
      let
        repelled = mapWithIndex (\i ni ->
          foldl (\acc j ->
            if i == j then acc
            else case nodes Array.!! j of
              Nothing -> acc
              Just nj ->
                let dx = ni.x - nj.x
                    dy = ni.y - nj.y
                    dist = max 1.0 (Number.sqrt (dx * dx + dy * dy))
                    minDist = (fromMaybe 30.0 (radii Array.!! i)) + (fromMaybe 30.0 (radii Array.!! j)) + 60.0
                    force = if dist < minDist then (minDist - dist) * 1.0 * alpha else 1500.0 * alpha / (dist * dist)
                in { x: acc.x + dx / dist * force, y: acc.y + dy / dist * force, vx: 0.0, vy: 0.0 }
          ) ni (Array.range 0 (n - 1))
        ) nodes
        attracted = mapWithIndex (\i ni ->
          foldl (\acc (Tuple (Tuple gi gj) w) ->
            if i == gi then case nodes Array.!! gj of
              Nothing -> acc
              Just nj ->
                let dx = nj.x - ni.x
                    dy = nj.y - ni.y
                    dist = max 1.0 (Number.sqrt (dx * dx + dy * dy))
                    force = Int.toNumber w * 0.3 * alpha
                in { x: acc.x + dx / dist * force, y: acc.y + dy / dist * force, vx: 0.0, vy: 0.0 }
            else if i == gj then case nodes Array.!! gi of
              Nothing -> acc
              Just nj ->
                let dx = nj.x - ni.x
                    dy = nj.y - ni.y
                    dist = max 1.0 (Number.sqrt (dx * dx + dy * dy))
                    force = Int.toNumber w * 0.3 * alpha
                in { x: acc.x + dx / dist * force, y: acc.y + dy / dist * force, vx: 0.0, vy: 0.0 }
            else acc
          ) ni (Map.toUnfoldable weights :: Array (Tuple (Tuple Int Int) Int))
        ) repelled
        centered = attracted <#> \ni ->
          { x: ni.x + (bounds.centerX - ni.x) * 0.02 * alpha
          , y: ni.y + (bounds.centerY - ni.y) * 0.02 * alpha
          , vx: 0.0, vy: 0.0
          }
      in centered
    go :: Int -> Number -> Array ForceNode -> Array ForceNode
    go iter alpha nodes =
      if iter >= iterations then nodes
      else go (iter + 1) (alpha - decay) (step (max 0.0 alpha) nodes)
  in go 0 alpha0 initial

-- =============================================================================
-- Helpers
-- =============================================================================

blockColors :: Array String
blockColors =
  [ "#4e9a6d", "#c05a4e", "#5a8ec0", "#c09a4e", "#8e5ac0"
  , "#c04e8e", "#4ec0c0", "#a0a040", "#e07048", "#4888c0"
  ]

blockColor :: Int -> String
blockColor i = fromMaybe "#888" (blockColors Array.!! (i `mod` Array.length blockColors))

nodesConnected :: String -> String -> Array EdgeInfo -> Boolean
nodesConnected a b edges = Array.any (\e ->
  (e.from == a && e.to == b) || (e.from == b && e.to == a)
  ) edges

-- =============================================================================
-- Action Handlers
-- =============================================================================

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  Initialize -> do
    state <- H.get
    fetchAndAnalyze state.lastInput

  Receive input -> do
    state <- H.get
    let changed = input.moduleName /= state.lastInput.moduleName
    H.modify_ _ { lastInput = input }
    when changed do
      H.modify_ _ { loadState = Loading, hoveredNode = Nothing }
      fetchAndAnalyze input

  NodeHovered mName ->
    H.modify_ _ { hoveredNode = mName }

  NodeClicked name ->
    H.raise (DeclarationClicked name)

fetchAndAnalyze :: forall m. MonadAff m => Input -> H.HalogenM State Action () Output m Unit
fetchAndAnalyze input = do
  result <- liftAff $ Loader.fetchModuleSource input.moduleName
  case result of
    Left err -> do
      log $ "[ConcernsPanel] Source unavailable: " <> err
      H.modify_ _ { loadState = NoData "Module source not available" }
    Right src -> do
      let analysis = SDA.analyzeModuleSource src.source
      log $ "[ConcernsPanel] " <> input.moduleName <> ": "
          <> show (Array.length analysis.allBranches) <> " branches, "
          <> show (Array.length analysis.caseExpressions) <> " case expressions"
      case computeLayout analysis of
        Just layout -> H.modify_ _ { loadState = Loaded layout }
        Nothing -> H.modify_ _ { loadState = NoData "No case expressions found \x2014 concern clustering requires pattern-matching branches" }
