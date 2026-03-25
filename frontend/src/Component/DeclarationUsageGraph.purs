-- | Declaration Usage Graph Component
-- |
-- | Pure Halogen HTML component showing cross-module usage for a declaration.
-- | Three-pane centered layout: callers (left, scrollable) | FOCUS (centered) | callees (right, scrollable).
-- | Self-contained: fetches its own data on Initialize/Receive.
-- | Renders compact Sigil type signatures (siglets) for each declaration.
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
import Halogen.HTML.Core (PropName(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import CE2.Data.Loader as Loader
import CE2.Viz.ModuleTreemapEnriched (kindColor)
import CE2.Viz.SignatureTree as SigTree
import CE2.Viz.TypeSignature.TypeAST (parseToRenderType)

-- =============================================================================
-- Types
-- =============================================================================

type Input =
  { packageName :: String
  , moduleName :: String
  , declarationName :: String
  , focusTypeSignature :: Maybe String
  }

data Output
  = NodeClicked String String String  -- pkg, mod, decl
  | ModuleClicked String              -- navigate to module structure
  | OpenFocusInEditor                 -- open focus decl in VS Code
  | ViewModuleSignatures String       -- navigate to module signature map
  | ViewPackage String                -- navigate to package treemap

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
  | ClickModule String       -- moduleName
  | ClickOpenInEditor
  | ClickViewSignatures
  | ClickViewPackage

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

-- | Render the full usage graph: callers | focus | callees
renderUsageGraph :: forall m. Input -> Loader.DeclarationUsage -> H.ComponentHTML Action () m
renderUsageGraph input usage =
  HH.div
    [ HP.style "display: flex; align-items: stretch; width: 100%; height: 100%; font-family: 'Courier New', Courier, monospace;" ]
    [ -- Left pane: callers (scrolls horizontally, content right-aligned)
      HH.div
        [ HP.style "flex: 1; overflow-x: auto; overflow-y: auto; display: flex; justify-content: flex-end;" ]
        [ HH.div
            [ HP.style "display: flex; align-items: stretch;" ]
            callerColumns
        ]

    -- Center: focus column
    , focusColumn

    -- Right pane: callees (scrolls horizontally)
    , HH.div
        [ HP.style "flex: 1; overflow-x: auto; overflow-y: auto; display: flex; justify-content: flex-start;" ]
        [ HH.div
            [ HP.style "display: flex; align-items: stretch;" ]
            calleeColumns
        ]
    ]
  where
  callersByHop = groupByHop usage.callers
  callerHops = Array.reverse $ Array.sort $ Array.fromFoldable $ Map.keys callersByHop
  callerColumns = callerHops <#> \hop ->
    renderHopColumn hop (fromMaybe [] $ Map.lookup hop callersByHop) "caller"

  calleesByHop = groupByHop usage.callees
  calleeHops = Array.sort $ Array.fromFoldable $ Map.keys calleesByHop
  calleeColumns = calleeHops <#> \hop ->
    renderHopColumn hop (fromMaybe [] $ Map.lookup hop calleesByHop) "callee"

  -- Focus column — visually anchored center with full Sigil signature
  -- Prefer API-provided focusTypeSignature, fall back to input from parent
  focusColumn :: H.ComponentHTML Action () m
  focusColumn =
    let
      focusSig = case usage.focusTypeSignature of
        Just s -> Just s
        Nothing -> input.focusTypeSignature
      mSigHtml = do
        sig <- focusSig
        ast <- parseToRenderType sig
        pure $ SigTree.renderSignature { name: input.declarationName, sig, ast, typeParams: [], className: Nothing }
    in
    HH.div
      [ HP.style "display: flex; flex-direction: column; align-items: center; justify-content: center; min-width: 200px; max-width: 360px; padding: 20px 24px; background: #f8f6f0; border-left: 2px solid #c8c0a8; border-right: 2px solid #c8c0a8; flex-shrink: 0;" ]
      [ HH.div
          [ HP.style "font-size: 11px; color: #888; text-align: center; margin-bottom: 8px;" ]
          [ HH.text $ shortModuleName input.moduleName ]
      , case mSigHtml of
          Just html ->
            HH.div
              [ HP.style "margin-bottom: 10px;"
              , HP.prop (PropName "innerHTML") html
              ]
              []
          Nothing ->
            HH.div
              [ HP.style "font-size: 18px; font-weight: 700; color: #222; text-align: center; word-break: break-word; margin-bottom: 6px;" ]
              [ HH.text input.declarationName ]
      , HH.div
          [ HP.style "font-size: 10px; color: #aaa; margin-bottom: 16px;" ]
          [ HH.text $ show (Array.length usage.callers) <> " callers \x00B7 " <> show (Array.length usage.callees) <> " callees" ]
      -- Navigation links
      , HH.div
          [ HP.style "border: 1px solid #d8d0bc; border-radius: 4px; padding: 8px 12px; margin-top: 4px; background: rgba(255,255,255,0.5); display: flex; flex-direction: column; gap: 4px; font-size: 11px;" ]
          [ navLink "Open in editor" ClickOpenInEditor
          , navLink "Module structure" (ClickModule input.moduleName)
          , navLink "Signature map" ClickViewSignatures
          , navLink "Package treemap" ClickViewPackage
          ]
      ]

  navLink :: String -> Action -> H.ComponentHTML Action () m
  navLink label action =
    HH.div
      [ HP.style "color: #2a5a8a; cursor: pointer; text-align: left; padding: 3px 4px; font-weight: 600; border-radius: 2px; transition: background 100ms ease;"
      , HE.onClick \_ -> action
      ]
      [ HH.text "\x2192 "
      , HH.text label
      ]

  -- Render a single hop column
  renderHopColumn :: Int -> Array Loader.UsageNode -> String -> H.ComponentHTML Action () m
  renderHopColumn hop nodes direction =
    let
      byModule = groupByModule nodes
      moduleNames = Array.sort $ Array.fromFoldable $ Map.keys byModule
      borderSide = if direction == "caller" then "border-right" else "border-left"
      bgAlpha = case hop of
        1 -> "0.00"
        2 -> "0.02"
        _ -> "0.04"
    in
    HH.div
      [ HP.style $ "display: flex; flex-direction: column; min-width: 140px; max-width: 280px; padding: 12px 10px; "
          <> borderSide <> ": 1px solid #e8e4d8; background: rgba(0,0,0," <> bgAlpha <> ");"
      ]
      [ HH.div
          [ HP.style "display: flex; flex-direction: column; gap: 8px; overflow-y: auto;" ]
          (moduleNames <#> \modName ->
            renderModuleGroup modName (fromMaybe [] $ Map.lookup modName byModule)
          )
      ]

  -- Render a group of nodes from the same module
  renderModuleGroup :: String -> Array Loader.UsageNode -> H.ComponentHTML Action () m
  renderModuleGroup modName nodes =
    HH.div
      [ HP.style "border-radius: 4px; padding: 6px 8px; background: rgba(0,0,0,0.04); border-left: 3px solid #c8c0a8;" ]
      [ -- Module label (clickable)
        HH.div
          [ HP.style "font-size: 9px; color: #8b7355; margin-bottom: 4px; font-weight: 700; letter-spacing: 0.3px; cursor: pointer;"
          , HE.onClick \_ -> ClickModule modName
          , HP.title modName
          ]
          [ HH.text $ shortModuleName modName ]
      -- Declaration nodes
      , HH.div
          [ HP.style "display: flex; flex-direction: column; gap: 4px;" ]
          (nodes <#> renderNode)
      ]

  -- Render a single declaration node with kind dot, name, purity tint, and optional siglet
  renderNode :: Loader.UsageNode -> H.ComponentHTML Action () m
  renderNode node =
    let purityBg = if isEffectful node then "rgba(232, 150, 12, 0.08)" else "rgba(59, 130, 196, 0.06)"
    in
    HH.div
      [ HP.style $ "cursor: pointer; padding: 3px 6px; border-radius: 3px; transition: background 100ms ease; background: " <> purityBg <> ";"
      , HE.onClick \_ -> ClickNode node.moduleName node.declName
      , HP.title (node.moduleName <> "." <> node.declName)
      ]
      [ -- Name row with kind dot
        HH.div
          [ HP.style "display: flex; align-items: center; gap: 5px;" ]
          [ HH.span
              [ HP.style $ "display: inline-block; width: 6px; height: 6px; border-radius: 50%; flex-shrink: 0; background: " <> kindColor node.kind <> ";" ]
              []
          , HH.span
              [ HP.style "font-size: 11px; color: #333; font-weight: 600; white-space: nowrap; overflow: hidden; text-overflow: ellipsis;" ]
              [ HH.text node.declName ]
          ]
      -- Siglet (compact type signature) if available
      , case renderSigletHtml node of
          Just html ->
            HH.div
              [ HP.style "margin-left: 11px; margin-top: 2px; font-size: 10px; line-height: 1.4;"
              , HP.prop (PropName "innerHTML") html
              ]
              []
          Nothing -> HH.text ""
      ]

-- | Check if a declaration is effectful based on its type signature
-- | Heuristic: presence of Effect, Aff, or MonadEffect/MonadAff in the signature
isEffectful :: Loader.UsageNode -> Boolean
isEffectful node = case node.typeSignature of
  Just sig -> String.contains (String.Pattern "Effect") sig
           || String.contains (String.Pattern "Aff") sig
  Nothing -> false

-- | Try to render a compact siglet for a usage node
renderSigletHtml :: Loader.UsageNode -> Maybe String
renderSigletHtml node = do
  sig <- node.typeSignature
  ast <- parseToRenderType sig
  pure $ SigTree.renderSiglet { ast }

-- =============================================================================
-- Helpers
-- =============================================================================

groupByHop :: Array Loader.UsageNode -> Map.Map Int (Array Loader.UsageNode)
groupByHop nodes =
  Array.foldl (\acc node ->
    Map.alter (Just <<< Array.cons node <<< fromMaybe []) node.hop acc
  ) Map.empty nodes

groupByModule :: Array Loader.UsageNode -> Map.Map String (Array Loader.UsageNode)
groupByModule nodes =
  Array.foldl (\acc node ->
    Map.alter (Just <<< Array.cons node <<< fromMaybe []) node.moduleName acc
  ) Map.empty nodes

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
    H.raise (NodeClicked state.input.packageName moduleName declName)

  ClickModule moduleName -> do
    H.raise (ModuleClicked moduleName)

  ClickOpenInEditor -> do
    H.raise OpenFocusInEditor

  ClickViewSignatures -> do
    state <- H.get
    H.raise (ViewModuleSignatures state.input.moduleName)

  ClickViewPackage -> do
    state <- H.get
    H.raise (ViewPackage state.input.packageName)

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
