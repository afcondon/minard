-- | Declaration Usage Graph Component
-- |
-- | Pure Halogen HTML component showing cross-module usage for a declaration.
-- | Horizontal flexbox layout: caller hops | FOCUS | callee hops.
-- | Self-contained: fetches its own data on Initialize/Receive.
module CE2.Component.DeclarationUsageGraph
  ( component
  , Input
  , Output(..)
  , Query(..)
  , Slot
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as String
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import CE2.Data.Loader as Loader

-- =============================================================================
-- Types
-- =============================================================================

type Input =
  { packageName :: String
  , moduleName :: String
  , declarationName :: String
  }

data Output
  = NodeClicked String String String  -- pkg, mod, decl

type Slot = H.Slot Query Output

data Query a = NoQuery a

data LoadState
  = Loading
  | Loaded Loader.DeclarationUsage
  | Empty  -- No cross-module usage found

type State =
  { input :: Input
  , loadState :: LoadState
  }

data Action
  = Initialize
  | Receive Input
  | ClickNode String String  -- moduleName, declName

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
  { input
  , loadState: Loading
  }

-- =============================================================================
-- Render
-- =============================================================================

render :: forall m. MonadAff m => State -> H.ComponentHTML Action () m
render state = case state.loadState of
  Loading ->
    HH.div
      [ HP.style "display: flex; align-items: center; justify-content: center; width: 100%; height: 100%; color: #999; font-family: 'Courier New', Courier, monospace; font-size: 11px;" ]
      [ HH.text "Loading usage graph..." ]

  Empty ->
    HH.div
      [ HP.style "display: flex; flex-direction: column; align-items: center; justify-content: center; width: 100%; height: 100%; color: #aaa; font-family: 'Courier New', Courier, monospace;" ]
      [ HH.div
          [ HP.style "font-size: 12px; margin-bottom: 8px;" ]
          [ HH.text "No cross-module usage found" ]
      , HH.div
          [ HP.style "font-size: 9px; opacity: 0.6; max-width: 200px; text-align: center; line-height: 1.4;" ]
          [ HH.text "Only cross-module value-level calls are tracked (extracted from CoreFN)" ]
      ]

  Loaded usage ->
    renderUsageGraph state.input usage

-- | Render the full usage graph with callers | focus | callees
renderUsageGraph :: forall m. Input -> Loader.DeclarationUsage -> H.ComponentHTML Action () m
renderUsageGraph input usage =
  HH.div
    [ HP.style "display: flex; align-items: stretch; width: 100%; height: 100%; font-family: 'Courier New', Courier, monospace; overflow-x: auto; overflow-y: auto;" ]
    ( callerColumns <> [ focusColumn ] <> calleeColumns )
  where
  -- Group callers by hop, reverse order (hop 3, 2, 1)
  callersByHop = groupByHop usage.callers
  callerHops = Array.reverse $ Array.sort $ Array.fromFoldable $ Map.keys callersByHop
  callerColumns = callerHops <#> \hop ->
    renderHopColumn hop (fromMaybe [] $ Map.lookup hop callersByHop) "caller"

  -- Group callees by hop, ascending (hop 1, 2, 3)
  calleesByHop = groupByHop usage.callees
  calleeHops = Array.sort $ Array.fromFoldable $ Map.keys calleesByHop
  calleeColumns = calleeHops <#> \hop ->
    renderHopColumn hop (fromMaybe [] $ Map.lookup hop calleesByHop) "callee"

  -- Focus column (the declaration itself)
  focusColumn :: H.ComponentHTML Action () m
  focusColumn =
    HH.div
      [ HP.style "display: flex; flex-direction: column; align-items: center; justify-content: center; min-width: 120px; padding: 12px 16px; background: rgba(0,0,0,0.06); border-left: 2px solid #ccc; border-right: 2px solid #ccc;" ]
      [ HH.div
          [ HP.style "font-size: 9px; color: #999; text-transform: uppercase; margin-bottom: 6px; letter-spacing: 0.5px;" ]
          [ HH.text "focus" ]
      , HH.div
          [ HP.style "font-size: 13px; font-weight: bold; color: #222; text-align: center; word-break: break-word;" ]
          [ HH.text input.declarationName ]
      , HH.div
          [ HP.style "font-size: 9px; color: #888; margin-top: 4px; text-align: center;" ]
          [ HH.text $ shortModuleName input.moduleName ]
      ]

  -- Render a single hop column (callers or callees at a given depth)
  renderHopColumn :: Int -> Array Loader.UsageNode -> String -> H.ComponentHTML Action () m
  renderHopColumn hop nodes direction =
    let
      -- Group nodes by module
      byModule = groupByModule nodes
      moduleNames = Array.sort $ Array.fromFoldable $ Map.keys byModule
      borderSide = if direction == "caller" then "border-right" else "border-left"
      opacity = case hop of
        1 -> "1.0"
        2 -> "0.7"
        _ -> "0.5"
    in
    HH.div
      [ HP.style $ "display: flex; flex-direction: column; min-width: 100px; padding: 8px; "
          <> borderSide <> ": 1px solid #e0e0e0; opacity: " <> opacity <> ";"
      ]
      [ -- Hop label
        HH.div
          [ HP.style "font-size: 8px; color: #bbb; text-align: center; margin-bottom: 6px; text-transform: uppercase; letter-spacing: 0.5px;" ]
          [ HH.text $ (if direction == "caller" then "callers" else "callees") <> " +" <> show hop ]
      -- Module groups
      , HH.div
          [ HP.style "display: flex; flex-direction: column; gap: 6px; overflow-y: auto;" ]
          (moduleNames <#> \modName ->
            renderModuleGroup modName (fromMaybe [] $ Map.lookup modName byModule)
          )
      ]

  -- Render a group of nodes from the same module
  renderModuleGroup :: String -> Array Loader.UsageNode -> H.ComponentHTML Action () m
  renderModuleGroup modName nodes =
    HH.div
      [ HP.style "background: rgba(0,0,0,0.03); border-radius: 4px; padding: 4px 6px;" ]
      [ -- Module label
        HH.div
          [ HP.style "font-size: 8px; color: #999; margin-bottom: 2px; font-weight: 600;" ]
          [ HH.text $ shortModuleName modName ]
      -- Declaration nodes
      , HH.div
          [ HP.style "display: flex; flex-direction: column; gap: 1px;" ]
          (nodes <#> \node ->
            HH.div
              [ HP.style "font-size: 10px; color: #4e79a7; cursor: pointer; padding: 1px 4px; border-radius: 2px; white-space: nowrap; overflow: hidden; text-overflow: ellipsis;"
              , HE.onClick \_ -> ClickNode node.moduleName node.declName
              , HP.title (node.moduleName <> "." <> node.declName)
              ]
              [ HH.text node.declName ]
          )
      ]

-- =============================================================================
-- Helpers
-- =============================================================================

-- | Group usage nodes by hop number
groupByHop :: Array Loader.UsageNode -> Map.Map Int (Array Loader.UsageNode)
groupByHop nodes =
  Array.foldl (\acc node ->
    Map.alter (Just <<< Array.cons node <<< fromMaybe []) node.hop acc
  ) Map.empty nodes

-- | Group usage nodes by module name
groupByModule :: Array Loader.UsageNode -> Map.Map String (Array Loader.UsageNode)
groupByModule nodes =
  Array.foldl (\acc node ->
    Map.alter (Just <<< Array.cons node <<< fromMaybe []) node.moduleName acc
  ) Map.empty nodes

-- | Extract the last segment of a dotted module name
shortModuleName :: String -> String
shortModuleName name =
  case Array.last (String.split (String.Pattern ".") name) of
    Just short -> short
    Nothing -> name

-- =============================================================================
-- Action Handlers
-- =============================================================================

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  Initialize -> do
    state <- H.get
    fetchUsage state.input

  Receive input -> do
    state <- H.get
    let changed = input.declarationName /= state.input.declarationName
              || input.moduleName /= state.input.moduleName
    H.modify_ _ { input = input }
    when changed do
      H.modify_ _ { loadState = Loading }
      fetchUsage input

  ClickNode moduleName declName -> do
    state <- H.get
    -- Find the package for this module — use input's package as fallback
    -- (cross-module calls may be in the same package or different ones)
    H.raise (NodeClicked state.input.packageName moduleName declName)

-- | Fetch usage data from the API
fetchUsage :: forall m. MonadAff m => Input -> H.HalogenM State Action () Output m Unit
fetchUsage input = do
  log $ "[DeclarationUsageGraph] Fetching usage for " <> input.moduleName <> "." <> input.declarationName
  result <- liftAff $ Loader.fetchDeclarationUsage input.moduleName input.declarationName
  case result of
    Right usage ->
      if usage.callerCount == 0 && usage.calleeCount == 0
        then H.modify_ _ { loadState = Empty }
        else H.modify_ _ { loadState = Loaded usage }
    Left err -> do
      log $ "[DeclarationUsageGraph] Error: " <> err
      H.modify_ _ { loadState = Empty }
