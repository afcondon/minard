-- | Cutpoints Panel Component
-- |
-- | Shows bridge edges and articulation points in a module's internal call graph.
-- | Bridges are "cut points" where the module could be split. Articulation points
-- | are "tangled hubs" embedded in cycles. Self-contained: computes graph
-- | decomposition from declarations and function calls.
module CE2.Component.CutpointsPanel
  ( component
  , Input
  , Output(..)
  , Query(..)
  , Slot
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl, foldMap)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Set as Set
import Data.String.CodeUnits as SCU
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import CE2.Data.Decomposition as Dec
import CE2.Data.Loader as Loader

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

data Query a = NoQuery a

type Slot = H.Slot Query Output

type State =
  { lastInput :: Input
  , declGraph :: Maybe (Dec.SimpleGraph String)
  , declDecomp :: Maybe Dec.DecompInfo
  }

data Action
  = Initialize
  | Receive Input
  | ClickDecl String

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
  , declGraph: Nothing
  , declDecomp: Nothing
  }

-- =============================================================================
-- Render
-- =============================================================================

render :: forall m. State -> H.ComponentHTML Action () m
render state = case state.declDecomp, state.declGraph of
  Just info, Just graph ->
    let
      bridgeList = Set.toUnfoldable info.bridgeSet :: Array (Tuple String String)
      apCount = Set.size info.aps
      bridgeCount = Array.length bridgeList
    in
    if bridgeCount == 0 && apCount == 0
      then HH.div [ HP.style "padding: 24px; color: #999; font-size: 12px; text-align: center;" ]
        [ HH.text "No cut points found \x2014 the internal call graph is fully connected" ]
      else HH.div [ HP.style "padding: 12px 16px;" ]
        ( -- Bridges
          (if bridgeCount > 0
            then [ HH.div [ HP.style "font-size: 12px; font-weight: 600; color: #8b6914; margin-bottom: 8px;" ]
                     [ HH.text $ show bridgeCount <> " bridge" <> (if bridgeCount > 1 then "s" else "") <> " \x2014 cut points where this module could split" ]
                 ]
              <> (Array.take 12 bridgeList <#> \(Tuple from to) ->
                renderBridgeCard from to info graph
              )
            else [])
          -- Tangled hubs
          <> renderTangledHubs info
        )
  _, _ ->
    HH.div [ HP.style "padding: 24px; color: #999; font-size: 12px; text-align: center;" ]
      [ HH.text "Computing call graph..." ]

renderBridgeCard :: forall m. String -> String -> Dec.DecompInfo -> Dec.SimpleGraph String -> H.ComponentHTML Action () m
renderBridgeCard from to info graph =
  let
    sideA = reachableWithout from to graph
    sideB = reachableWithout to from graph
    sideACount = Set.size sideA
    sideBCount = Set.size sideB
    fromIsAP = Set.member from info.aps
    toIsAP = Set.member to info.aps
  in
  HH.div [ HP.style "display: flex; align-items: baseline; gap: 6px; padding: 4px 10px; margin-bottom: 3px; background: #f5f2eb; border-radius: 4px; border-left: 3px solid #d4a017; font-size: 11px; line-height: 1.5;" ]
    [ HH.span [ HP.style "font-weight: 600; color: #2563eb; cursor: pointer;", HE.onClick \_ -> ClickDecl from ] [ HH.text from ]
    , HH.span [ HP.style "color: #999;" ] [ HH.text "\x2194" ]
    , HH.span [ HP.style "font-weight: 600; color: #2563eb; cursor: pointer;", HE.onClick \_ -> ClickDecl to ] [ HH.text to ]
    , HH.span [ HP.style "color: #888;" ]
        [ HH.text $ show sideACount <> " | " <> show sideBCount
            <> (if fromIsAP || toIsAP then " \x00B7 " <> (if fromIsAP then from else to) <> " is a hub" else "")
        ]
    ]

renderTangledHubs :: forall m. Dec.DecompInfo -> Array (H.ComponentHTML Action () m)
renderTangledHubs info =
  let
    apList = Array.sort (Set.toUnfoldable info.aps :: Array String)
    tangledAPs = Array.filter (\name ->
      case Map.lookup name info.nodeBlock of
        Just blockIdx ->
          case Array.find (\b -> b.index == blockIdx) info.blocks of
            Just block -> not block.isBridge && Set.size block.nodes > 2
            Nothing -> false
        Nothing -> false
    ) apList
  in
  if Array.length tangledAPs == 0 then []
  else
    [ HH.div [ HP.style "font-size: 12px; font-weight: 600; color: #c62828; margin: 12px 0 6px;" ]
        [ HH.text $ show (Array.length tangledAPs) <> " tangled hub" <> (if Array.length tangledAPs > 1 then "s" else "") <> " \x2014 embedded in cycles, harder to extract" ]
    , HH.div [ HP.style "font-size: 11px; color: #555; line-height: 1.6; padding: 6px 8px; background: #f5f2eb; border-radius: 3px;" ]
        (tangledAPs <#> \name ->
          HH.div [ HP.style "padding: 1px 0;" ]
            [ HH.span [ HP.style "font-weight: 500; color: #c62828; cursor: pointer;", HE.onClick \_ -> ClickDecl name ] [ HH.text name ]
            , HH.span [ HP.style "color: #888;" ] [ HH.text " \x2014 removing this breaks cycles in its cluster" ]
            ]
        )
    ]

-- =============================================================================
-- Graph Helpers
-- =============================================================================

-- | Find all nodes reachable from `start` without crossing the edge to `excluded`
reachableWithout :: String -> String -> Dec.SimpleGraph String -> Set String
reachableWithout start excluded graph =
  go (Set.singleton start) (Set.singleton start)
  where
  go frontier visited =
    let
      newNeighbors = foldl (\acc node ->
        let neighbors = fromMaybe Set.empty (Map.lookup node graph.edges)
            filtered = Set.filter (\n -> n /= excluded && not (Set.member n visited)) neighbors
        in Set.union acc filtered
      ) Set.empty frontier
    in
    if Set.isEmpty newNeighbors then visited
    else go newNeighbors (Set.union visited newNeighbors)

buildCallGraph :: Input -> { graph :: Dec.SimpleGraph String, decomp :: Dec.DecompInfo }
buildCallGraph input =
  let
    allCalls = foldMap identity input.functionCalls
    internalCalls = Array.filter (\c ->
      not c.isCrossModule && c.calleeModule == input.moduleName && c.callerName /= c.calleeName
    ) allCalls
    exportedNames = Set.fromFoldable $ input.declarations <#> _.name
    callNames = foldl (\acc c -> Set.insert c.callerName (Set.insert c.calleeName acc)) Set.empty internalCalls
    declNames = Set.filter (not <<< isCompilerGenerated) (Set.union exportedNames callNames)
    edges = foldl (\acc call ->
      if Set.member call.callerName declNames && Set.member call.calleeName declNames
      then
        Map.alter (Just <<< Set.insert call.calleeName <<< fromMaybe Set.empty) call.callerName
          (Map.alter (Just <<< Set.insert call.callerName <<< fromMaybe Set.empty) call.calleeName acc)
      else acc
    ) Map.empty internalCalls
    graph = { nodes: Set.toUnfoldable declNames :: Array String, edges }
    decomp = Dec.analyzeGraph graph
  in { graph, decomp }

isCompilerGenerated :: String -> Boolean
isCompilerGenerated name =
  SCU.take 7 name == "discard" || SCU.take 4 name == "bind" || SCU.take 2 name == "$$"

-- =============================================================================
-- Action Handlers
-- =============================================================================

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  Initialize -> do
    state <- H.get
    let { graph, decomp } = buildCallGraph state.lastInput
    H.modify_ _ { declGraph = Just graph, declDecomp = Just decomp }

  Receive input -> do
    state <- H.get
    let changed = input.moduleName /= state.lastInput.moduleName
              || Array.length input.declarations /= Array.length state.lastInput.declarations
    H.modify_ _ { lastInput = input }
    when changed do
      let { graph, decomp } = buildCallGraph input
      H.modify_ _ { declGraph = Just graph, declDecomp = Just decomp }

  ClickDecl name -> do
    H.raise (DeclarationClicked name)
