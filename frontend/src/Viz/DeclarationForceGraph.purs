-- | Declaration Force Graph - Force-directed graph of declarations within a module
-- |
-- | Replaces the declaration treemap in the ModuleOverview left panel.
-- | Shows intra-module call structure: which functions call which,
-- | which are orphans, and how tightly coupled the module's internals are.
-- | Structural declarations (data, newtypes, type classes) are pinned.
-- | Node size encodes LOC.
module CE2.Viz.DeclarationForceGraph
  ( Config
  , ForceGraphHandle
  , render
  , cleanup
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl)
import Data.Int (toNumber) as Int
import Data.Int (floor) as IntFloor
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable as Nullable
import Data.Number (sqrt, min, max, cos, sin) as Num
import Data.String as String
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)

-- HATS
import Hylograph.HATS (Tree, elem, staticStr, thunkedStr, thunkedNum, forEach, withBehaviors, onCoordinatedHighlightWithTooltip, onClick)
import Hylograph.HATS.InterpreterTick (rerender, clearContainer)
import Hylograph.Internal.Selection.Types (ElementType(..))
import Hylograph.Internal.Behavior.Types (HighlightClass(..), TooltipTrigger(..))

-- Simulation
import Hylograph.Simulation
  ( runSimulation
  , Engine(..)
  , SimulationEvent(..)
  , subscribe
  , setup
  , collide
  , manyBody
  , link
  , positionX
  , positionY
  , withStrength
  , withRadius
  , withDistance
  , withX
  , withY
  , static
  , dynamic
  )
import Hylograph.ForceEngine.Simulation (SimulationNode)

import CE2.Data.Loader (V2Declaration, V2FunctionCall)
import CE2.Viz.ModuleTreemapEnriched (kindColor)

-- =============================================================================
-- Types
-- =============================================================================

-- | Configuration for the declaration force graph
type Config =
  { containerSelector :: String
  , width :: Number
  , height :: Number
  , packageName :: String
  , moduleName :: String
  , onDeclarationClick :: Maybe (String -> String -> String -> Effect Unit)
  }

-- | Handle for cleanup
type ForceGraphHandle = { stop :: Effect Unit }

-- | Extended simulation node for declarations
type DeclNodeRow =
  ( name :: String
  , fullName :: String
  , kind :: String
  , typeSignature :: Maybe String
  , loc :: Number
  , isStructural :: Boolean
  , childCount :: Int
  , callCount :: Int
  , calledByCount :: Int
  , r :: Number
  , color :: String
  )

type DeclNode = SimulationNode DeclNodeRow

-- | Call edge between declarations
type DeclEdge = { source :: Int, target :: Int, callCount :: Int }

-- | Neighbor map: fullName -> Set of connected fullNames
type NeighborMap = Map String (Array String)

-- =============================================================================
-- Public API
-- =============================================================================

-- | Render the declaration force graph into the container
render
  :: Config
  -> Array V2Declaration
  -> Map Int (Array V2FunctionCall)
  -> Effect ForceGraphHandle
render config declarations callsMap = do
  log $ "[DeclarationForceGraph] Module: " <> config.moduleName
      <> ", " <> show (Array.length declarations) <> " declarations"

  if Array.length declarations == 0 then do
    renderEmptyState config
    pure { stop: pure unit }
  else do
    -- Build intra-module call data
    let { callsTo, calledByMap, intraEdges } =
          buildIntraModuleCalls config.moduleName declarations callsMap

    -- Prepare nodes
    let nodes = prepareNodes config declarations callsTo calledByMap

    -- Build neighbor map (fullName -> all connected fullNames) for highlighting
    let neighborMap = buildNeighborMap config.moduleName callsTo calledByMap

    -- Prepare link edges (using node indices)
    let edges = prepareEdges nodes intraEdges

    log $ "[DeclarationForceGraph] " <> show (Array.length nodes) <> " nodes, "
        <> show (Array.length edges) <> " edges"

    -- Start simulation
    startSimulation config nodes edges neighborMap

-- | Clean up the force graph
cleanup :: String -> Effect Unit
cleanup selector = clearContainer selector

-- =============================================================================
-- Call Graph Extraction
-- =============================================================================

-- | Build intra-module call maps and edge list
buildIntraModuleCalls
  :: String
  -> Array V2Declaration
  -> Map Int (Array V2FunctionCall)
  -> { callsTo :: Map String (Array String)
     , calledByMap :: Map String (Array String)
     , intraEdges :: Array { from :: String, to :: String, count :: Int }
     }
buildIntraModuleCalls moduleName _declarations callsMap =
  let
    allCalls :: Array V2FunctionCall
    allCalls = Array.concatMap identity (Array.fromFoldable (Map.values callsMap))

    -- Only intra-module calls
    intraCalls = Array.filter (\c -> not c.isCrossModule || c.calleeModule == moduleName) allCalls

    intraEdges :: Array { from :: String, to :: String, count :: Int }
    intraEdges = intraCalls <#> \c ->
      { from: c.callerName, to: c.calleeName, count: c.callCount }

    callsTo = foldl (\acc e ->
        Map.alter (Just <<< Array.cons e.to <<< fromMaybe []) e.from acc
      ) Map.empty intraEdges

    calledByMap = foldl (\acc e ->
        Map.alter (Just <<< Array.cons e.from <<< fromMaybe []) e.to acc
      ) Map.empty intraEdges
  in
    { callsTo, calledByMap, intraEdges }

-- | Build a map from each fullName to all its connected neighbors (both directions)
buildNeighborMap :: String -> Map String (Array String) -> Map String (Array String) -> NeighborMap
buildNeighborMap moduleName callsTo calledByMap =
  let
    addPrefix n = moduleName <> "." <> n
    allNames = Array.nub $
      (Array.fromFoldable (Map.keys callsTo)) <> (Array.fromFoldable (Map.keys calledByMap))
  in
    foldl (\acc name ->
      let
        callees = fromMaybe [] (Map.lookup name callsTo) <#> addPrefix
        callers = fromMaybe [] (Map.lookup name calledByMap) <#> addPrefix
        neighbors = Array.nub (callees <> callers)
      in
        Map.insert (addPrefix name) neighbors acc
    ) Map.empty allNames

-- =============================================================================
-- Node Preparation
-- =============================================================================

-- | Compute LOC for a declaration from its sourceSpan
declarationLoc :: V2Declaration -> Int
declarationLoc decl = case decl.sourceSpan of
  Just span -> case { start: Array.index span.start 0, end_: Array.index span.end 0 } of
    { start: Just startLine, end_: Just endLine } -> max 1 (endLine - startLine + 1)
    _ -> max 1 (Array.length decl.children + 1)
  Nothing -> max 1 (Array.length decl.children + 1)

-- | Whether a declaration kind is "structural" (data, newtype, class, type synonym)
isStructuralKind :: String -> Boolean
isStructuralKind = case _ of
  "data"         -> true
  "newtype"      -> true
  "type_class"   -> true
  "type_synonym" -> true
  _              -> false

-- | Prepare simulation nodes from declarations
prepareNodes
  :: Config
  -> Array V2Declaration
  -> Map String (Array String)
  -> Map String (Array String)
  -> Array DeclNode
prepareNodes config declarations callsTo calledByMap =
  let
    centerX = config.width / 2.0
    centerY = config.height / 2.0
    n = Array.length declarations

    -- Count structural declarations for pinning layout
    structuralDecls = Array.filter (\d -> isStructuralKind d.kind) declarations
    nStructural = Array.length structuralDecls
  in
    Array.mapWithIndex (\idx decl ->
      let
        loc = Int.toNumber (declarationLoc decl)
        r = Num.max 8.0 (Num.min 40.0 (Num.sqrt loc * 2.5))
        structural = isStructuralKind decl.kind
        calls = fromMaybe [] (Map.lookup decl.name callsTo)
        callers = fromMaybe [] (Map.lookup decl.name calledByMap)

        -- Initial position: spread in a circle
        angle = 2.0 * 3.14159265 * Int.toNumber idx / Int.toNumber (max 1 n)
        initDist = Num.min (config.width * 0.3) (config.height * 0.3)

        -- Pin structural declarations in an arc at the top
        pinned = structural && nStructural > 0
        structIdx = fromMaybe 0 (Array.findIndex (\d -> d.name == decl.name) structuralDecls)
        pinAngle = 3.14159265 + 3.14159265 * (Int.toNumber structIdx + 0.5) / Int.toNumber (max 1 nStructural)
        pinDist = Num.min (config.width * 0.35) (config.height * 0.35)
        pinX = centerX + pinDist * Num.cos pinAngle
        pinY = centerY + pinDist * Num.sin pinAngle * 0.6
      in
        { id: idx
        , x: if pinned then pinX else centerX + initDist * Num.cos angle
        , y: if pinned then pinY else centerY + initDist * Num.sin angle
        , vx: 0.0
        , vy: 0.0
        , fx: if pinned then Nullable.notNull pinX else Nullable.null
        , fy: if pinned then Nullable.notNull pinY else Nullable.null
        , name: decl.name
        , fullName: config.moduleName <> "." <> decl.name
        , kind: decl.kind
        , typeSignature: decl.typeSignature
        , loc: loc
        , isStructural: structural
        , childCount: Array.length decl.children
        , callCount: Array.length calls
        , calledByCount: Array.length callers
        , r: r
        , color: kindColor decl.kind
        }
    ) declarations

-- | Build edge list for the simulation (indices into nodes array)
prepareEdges :: Array DeclNode -> Array { from :: String, to :: String, count :: Int } -> Array DeclEdge
prepareEdges nodes intraEdges =
  let
    nameToIdx :: Map String Int
    nameToIdx = Map.fromFoldable $ Array.mapWithIndex (\idx n -> Tuple n.name idx) nodes
  in
    Array.mapMaybe (\e -> do
      srcIdx <- Map.lookup e.from nameToIdx
      tgtIdx <- Map.lookup e.to nameToIdx
      if srcIdx == tgtIdx then Nothing
      else Just { source: srcIdx, target: tgtIdx, callCount: e.count }
    ) intraEdges

-- =============================================================================
-- Simulation
-- =============================================================================

-- | Start force simulation and render
startSimulation :: Config -> Array DeclNode -> Array DeclEdge -> NeighborMap -> Effect ForceGraphHandle
startSimulation config nodes edges neighborMap = do
  let centerX = config.width / 2.0
      centerY = config.height / 2.0
      nodesGroupId = config.containerSelector <> " > svg > #decl-force-nodes"

  -- Render SVG container
  let containerTree = elem SVG
        [ staticStr "viewBox" $ "0 0 " <> show config.width <> " " <> show config.height
        , staticStr "width" "100%"
        , staticStr "height" "100%"
        , staticStr "preserveAspectRatio" "xMidYMid meet"
        , staticStr "style" "display: block; border-radius: 4px;"
        ]
        [ elem Group [ staticStr "id" "decl-force-links" ] []
        , elem Group [ staticStr "id" "decl-force-nodes" ] []
        ]
  _ <- rerender config.containerSelector containerTree

  -- Convert edges to simulation link format
  let simLinks = edges <#> \e -> { source: e.source, target: e.target }

  -- Configure forces
  let hasLinks = Array.length edges > 0
      forces =
        [ manyBody "charge" # withStrength (static (-120.0))
        , collide "collision" # withRadius (dynamic \n -> n.r + 3.0) # withStrength (static 0.8)
        , positionX "cx" # withX (static centerX) # withStrength (static 0.03)
        , positionY "cy" # withY (static centerY) # withStrength (static 0.03)
        ] <> (if hasLinks
              then [ link "calls" # withDistance (static 60.0) # withStrength (static 0.3) ]
              else [])

  { handle, events } <- runSimulation
    { engine: D3
    , setup: setup "decl-force" forces
    , nodes: nodes
    , links: simLinks
    , container: nodesGroupId
    , alphaMin: 0.01
    }

  -- Initial render
  initialNodes <- handle.getNodes
  renderFullHATS config initialNodes edges neighborMap

  -- Subscribe to tick events
  _ <- subscribe events \event -> case event of
    Tick _ -> do
      currentNodes <- handle.getNodes
      renderFullHATS config currentNodes edges neighborMap
    Completed -> do
      log "[DeclarationForceGraph] Simulation completed"
      finalNodes <- handle.getNodes
      renderFullHATS config finalNodes edges neighborMap
    Started -> log "[DeclarationForceGraph] Simulation started"
    Stopped -> pure unit

  pure { stop: handle.stop }

-- =============================================================================
-- HATS Rendering
-- =============================================================================

-- | Full render: links + nodes
renderFullHATS :: Config -> Array DeclNode -> Array DeclEdge -> NeighborMap -> Effect Unit
renderFullHATS config nodes edges neighborMap = do
  let linkTree = buildLinksTree nodes edges
  _ <- rerender (config.containerSelector <> " > svg > #decl-force-links") linkTree
  let nodeTree = buildNodesTree config nodes neighborMap
  _ <- rerender (config.containerSelector <> " > svg > #decl-force-nodes") nodeTree
  pure unit

-- | Build the nodes HATS tree
buildNodesTree :: Config -> Array DeclNode -> NeighborMap -> Tree
buildNodesTree config nodes neighborMap =
  forEach "decl-nodes" Group nodes (\n -> n.fullName) (declNodeElem config neighborMap)

-- | Render a single declaration node
declNodeElem :: Config -> NeighborMap -> DeclNode -> Tree
declNodeElem config neighborMap node =
  let
    tooltipText = buildNodeTooltip node
    fullName = node.fullName

    -- Get this node's neighbors for highlight classification
    neighbors = fromMaybe [] (Map.lookup fullName neighborMap)

    clickBehavior = case config.onDeclarationClick of
      Nothing -> []
      Just handler -> [ onClick (handler config.packageName config.moduleName node.name) ]

    behaviors =
      [ onCoordinatedHighlightWithTooltip
          { identify: fullName
          , classify: \hoveredId ->
              if fullName == hoveredId then Primary
              else if Array.elem hoveredId neighbors then Related
              else Dimmed
          , group: Just "decl-force"
          , tooltip: Just { content: tooltipText, showWhen: OnHover }
          }
      ] <> clickBehavior

    strokeColor = if node.isStructural then "rgba(0,0,0,0.3)" else "rgba(0,0,0,0.15)"
    strokeWidth = if node.isStructural then "2" else "1"
    fillOpacity = if node.isStructural then "0.75" else "0.85"

    labelY = node.r + 12.0
    fontSize = if node.r > 20.0 then "10" else if node.r > 12.0 then "9" else "8"

    maxLen = max 3 (IntFloor.floor (node.r * 2.0 / 5.5))
    label = if String.length node.name > maxLen
            then String.take maxLen node.name <> "..."
            else node.name
  in
    withBehaviors behaviors
    $ elem Group
        [ thunkedStr "transform" ("translate(" <> show node.x <> "," <> show node.y <> ")")
        , staticStr "class" "decl-force-node"
        , staticStr "cursor" "pointer"
        ]
        [ -- Node circle
          elem Circle
            [ staticStr "cx" "0"
            , staticStr "cy" "0"
            , thunkedNum "r" node.r
            , thunkedStr "fill" node.color
            , staticStr "fill-opacity" fillOpacity
            , staticStr "stroke" strokeColor
            , staticStr "stroke-width" strokeWidth
            ]
            []
        -- Kind abbreviation inside large nodes
        , if node.r >= 16.0
          then elem Text
            [ staticStr "x" "0"
            , staticStr "y" "1"
            , staticStr "text-anchor" "middle"
            , staticStr "dominant-baseline" "central"
            , staticStr "font-size" "8"
            , staticStr "fill" "rgba(255,255,255,0.7)"
            , staticStr "font-family" "system-ui, sans-serif"
            , staticStr "font-weight" "600"
            , staticStr "pointer-events" "none"
            , thunkedStr "textContent" (kindAbbrev node.kind)
            ]
            []
          else elem Group [] []
        -- Name label below
        , elem Text
            [ staticStr "x" "0"
            , thunkedNum "y" labelY
            , staticStr "text-anchor" "middle"
            , staticStr "font-size" fontSize
            , staticStr "fill" "#333"
            , staticStr "font-family" "system-ui, sans-serif"
            , staticStr "font-weight" "500"
            , staticStr "pointer-events" "none"
            , thunkedStr "textContent" label
            ]
            []
        ]

-- | Build links tree
buildLinksTree :: Array DeclNode -> Array DeclEdge -> Tree
buildLinksTree nodes edges =
  let
    linkElems = Array.mapMaybe (\edge -> do
      src <- Array.index nodes edge.source
      tgt <- Array.index nodes edge.target
      Just (linkElem src tgt edge.callCount)
    ) edges
  in
    elem Group
      [ staticStr "class" "decl-force-links"
      , staticStr "pointer-events" "none"
      ]
      linkElems

-- | Render a single link between two nodes
linkElem :: DeclNode -> DeclNode -> Int -> Tree
linkElem src tgt callCount =
  let
    dx = tgt.x - src.x
    dy = tgt.y - src.y
    dist = Num.sqrt (dx * dx + dy * dy)

    curvature = Num.min 0.3 (dist / 400.0) * 25.0
    perpX = if dist > 0.0 then -dy / dist * curvature else 0.0
    perpY = if dist > 0.0 then dx / dist * curvature else 0.0
    mx = (src.x + tgt.x) / 2.0 + perpX
    my = (src.y + tgt.y) / 2.0 + perpY

    pathD = "M " <> show src.x <> " " <> show src.y
         <> " Q " <> show mx <> " " <> show my
         <> " " <> show tgt.x <> " " <> show tgt.y

    sw = Num.min 3.0 (0.75 + Int.toNumber callCount * 0.5)
  in
    withBehaviors
      [ onCoordinatedHighlightWithTooltip
          { identify: src.fullName <> "->" <> tgt.fullName
          , classify: \hoveredId ->
              if hoveredId == src.fullName || hoveredId == tgt.fullName then Related
              else Dimmed
          , group: Just "decl-force"
          , tooltip: Nothing
          }
      ]
    $ elem Path
        [ thunkedStr "d" pathD
        , staticStr "fill" "none"
        , staticStr "stroke" "#94a3b8"
        , thunkedNum "stroke-width" sw
        , staticStr "stroke-opacity" "0.4"
        , staticStr "class" "decl-force-link"
        ]
        []

-- =============================================================================
-- Empty State
-- =============================================================================

renderEmptyState :: Config -> Effect Unit
renderEmptyState config = do
  let tree = elem SVG
        [ staticStr "viewBox" $ "0 0 " <> show config.width <> " " <> show config.height
        , staticStr "width" "100%"
        , staticStr "height" "100%"
        , staticStr "preserveAspectRatio" "xMidYMid meet"
        , staticStr "style" "display: block;"
        ]
        [ elem Text
            [ thunkedNum "x" (config.width / 2.0)
            , thunkedNum "y" (config.height / 2.0)
            , staticStr "text-anchor" "middle"
            , staticStr "font-size" "12"
            , staticStr "fill" "#999"
            , staticStr "font-family" "system-ui, sans-serif"
            , staticStr "textContent" "No declarations"
            ]
            []
        ]
  _ <- rerender config.containerSelector tree
  pure unit

-- =============================================================================
-- Utilities
-- =============================================================================

-- | Build tooltip text for a declaration node
buildNodeTooltip :: DeclNode -> String
buildNodeTooltip node =
  let
    kindStr = case node.kind of
      "value" -> "value"
      "data" -> "data"
      "newtype" -> "newtype"
      "type_class" -> "class"
      "type_synonym" -> "type"
      "foreign" -> "foreign"
      _ -> node.kind

    header = node.name <> " :: " <> kindStr

    typeSig = case node.typeSignature of
      Just sig -> "\n" <> sig
      Nothing -> ""

    childInfo = if node.childCount > 0
      then "\n(" <> show node.childCount <> " children)"
      else ""

    depInfo =
      if node.callCount > 0 || node.calledByCount > 0
      then "\n-> " <> show node.callCount <> " calls, <- " <> show node.calledByCount <> " callers"
      else "\n(orphan)"
  in
    header <> typeSig <> childInfo <> depInfo

-- | Kind abbreviation
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
