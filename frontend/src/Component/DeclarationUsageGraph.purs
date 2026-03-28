-- | Declaration Usage Graph Component
-- |
-- | Three-pane centered layout: callers (left) | module spine (center) | callees (right).
-- | The center column shows all declarations from the focused module as a scrollable
-- | spine — full Sigil signature for the focused declaration, compact siglets for siblings.
-- | Clicking a sibling refocuses the graph. Self-contained: fetches its own usage data.
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
import Data.Set as Set
import Data.String as String
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core (PropName(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import CE2.Data.Loader as Loader
import CE2.Viz.DOMHelpers (scrollChildIntoView)
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
  , declarations :: Array Loader.V2Declaration
  , moduleCalls :: Array Loader.V2FunctionCall
  , allCalls :: Map.Map Int (Array Loader.V2FunctionCall)
  , moduleNameToId :: Map.Map String Int
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
  | Empty

-- | "module.decl" key for node identity
type NodeKey = String

type State =
  { input :: Input
  , loadState :: LoadState
  , hoveredSibling :: Maybe String
  , hoveredNode :: Maybe NodeKey
  , callsFrom :: Map.Map NodeKey (Set.Set NodeKey)  -- caller → set of callees
  , callsTo :: Map.Map NodeKey (Set.Set NodeKey)    -- callee → set of callers
  }

data Action
  = Initialize
  | Receive Input
  | ClickNode String String  -- moduleName, declName (callers/callees)
  | ClickSibling String      -- declName (same module)
  | HoverSibling (Maybe String) -- declName or Nothing on leave
  | HoverNode (Maybe NodeKey)   -- hover any caller/callee node
  | ClickModule String
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
  , hoveredSibling: Nothing
  , hoveredNode: Nothing
  , callsFrom: Map.empty
  , callsTo: Map.empty
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
    renderUsageGraph state.input usage (computeHighlightSet state usage)

-- | Render the full usage graph: callers | module spine | callees
-- | `highlightSet` contains "module.decl" keys that should stay bright when a sibling is hovered
renderUsageGraph :: forall m. Input -> Loader.DeclarationUsage -> Maybe (Set.Set String) -> H.ComponentHTML Action () m
renderUsageGraph input usage highlightSet =
  HH.div
    [ HP.style "width: 100%; height: 100%; overflow-x: auto; overflow-y: auto;" ]
    [ HH.div
        [ HP.style "display: flex; align-items: stretch; min-width: min-content; margin: 0 auto; font-family: 'Courier New', Courier, monospace; height: 100%; justify-content: center;" ]
        [ -- Left pane: callers
          HH.div
            [ HP.style "display: flex; align-items: stretch; justify-content: flex-end; flex-shrink: 0;" ]
            callerColumns

        -- Center: module spine (or single focus for cross-package)
        , if Array.null input.declarations
            then focusColumnSingle
            else focusColumnSpine

        -- Right pane: callees
        , HH.div
            [ HP.style "display: flex; align-items: stretch; justify-content: flex-start; flex-shrink: 0;" ]
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

  -- -------------------------------------------------------------------------
  -- Focus column: single declaration (fallback for cross-package)
  -- -------------------------------------------------------------------------
  focusColumnSingle :: H.ComponentHTML Action () m
  focusColumnSingle =
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
      , navBox
      ]

  -- -------------------------------------------------------------------------
  -- Focus column: module spine (full module with focused + siblings)
  -- -------------------------------------------------------------------------
  focusColumnSpine :: H.ComponentHTML Action () m
  focusColumnSpine =
    let sorted = sortByKind input.declarations
    in
    HH.div
      [ HP.style "display: flex; flex-direction: column; min-width: 220px; max-width: 380px; background: #f8f6f0; border-left: 2px solid #c8c0a8; border-right: 2px solid #c8c0a8; flex-shrink: 0;" ]
      [ -- Module name header (fixed)
        HH.div
          [ HP.style "font-size: 11px; color: #888; text-align: center; padding: 10px 16px 6px; flex-shrink: 0; border-bottom: 1px solid #e8e4d8;" ]
          [ HH.text $ shortModuleName input.moduleName
          , HH.span [ HP.style "color: #bbb; margin-left: 6px;" ] [ HH.text $ "(" <> show (Array.length sorted) <> ")" ]
          ]
      -- Scrollable spine
      , HH.div
          [ HP.style "flex: 1; overflow-y: auto; padding: 6px 12px;"
          , HP.id "usage-graph-spine"
          ]
          (sorted <#> renderSpineEntry)
      -- Caller/callee count + nav (fixed footer)
      , HH.div
          [ HP.style "flex-shrink: 0; border-top: 1px solid #e8e4d8; padding: 8px 16px;" ]
          [ HH.div
              [ HP.style "font-size: 10px; color: #aaa; text-align: center; margin-bottom: 8px;" ]
              [ HH.text $ show (Array.length usage.callers) <> " callers \x00B7 " <> show (Array.length usage.callees) <> " callees" ]
          , navBox
          ]
      ]

  -- Render a spine entry: full sigil for focused, siglet for siblings
  renderSpineEntry :: Loader.V2Declaration -> H.ComponentHTML Action () m
  renderSpineEntry decl =
    if decl.name == input.declarationName
      then renderFocusedEntry decl
      else renderSiblingEntry decl

  -- Focused declaration: full Sigil signature, highlighted
  renderFocusedEntry :: Loader.V2Declaration -> H.ComponentHTML Action () m
  renderFocusedEntry decl =
    let
      focusSig = case usage.focusTypeSignature of
        Just s -> Just s
        Nothing -> decl.typeSignature
      mSigHtml = do
        sig <- focusSig
        ast <- parseToRenderType sig
        pure $ SigTree.renderSignature { name: decl.name, sig, ast, typeParams: decl.typeArguments, className: Nothing }
    in
    HH.div
      [ HP.style "padding: 10px 8px; margin: 3px 0; background: rgba(78, 121, 167, 0.08); border-radius: 4px; border-left: 3px solid #4e79a7;"
      , HP.id "spine-focus"
      ]
      [ case mSigHtml of
          Just html ->
            HH.div
              [ HP.prop (PropName "innerHTML") html ]
              []
          Nothing ->
            HH.div
              [ HP.style "font-size: 14px; font-weight: 700; color: #222;" ]
              [ HH.text decl.name ]
      ]

  -- Sibling declaration: compact siglet, clickable
  renderSiblingEntry :: Loader.V2Declaration -> H.ComponentHTML Action () m
  renderSiblingEntry decl =
    let
      mSigletHtml = do
        sig <- decl.typeSignature
        ast <- parseToRenderType sig
        pure $ SigTree.renderSiglet { ast }
      purityBg = if isEffectfulSig decl.typeSignature then "rgba(232, 150, 12, 0.06)" else "rgba(59, 130, 196, 0.04)"
    in
    HH.div
      [ HP.style $ "cursor: pointer; padding: 4px 8px; margin: 1px 0; border-radius: 3px; transition: background 100ms ease; background: " <> purityBg <> ";"
      , HE.onClick \_ -> ClickSibling decl.name
      , HE.onMouseEnter \_ -> HoverSibling (Just decl.name)
      , HE.onMouseLeave \_ -> HoverSibling Nothing
      , HP.title decl.name
      ]
      [ HH.div
          [ HP.style "display: flex; align-items: center; gap: 5px;" ]
          [ HH.span
              [ HP.style $ "display: inline-block; width: 6px; height: 6px; border-radius: 50%; flex-shrink: 0; background: " <> kindColor decl.kind <> ";" ]
              []
          , HH.span
              [ HP.style "font-size: 11px; color: #555; font-weight: 600; white-space: nowrap; overflow: hidden; text-overflow: ellipsis;" ]
              [ HH.text decl.name ]
          ]
      , case mSigletHtml of
          Just html ->
            HH.div
              [ HP.style "margin-left: 11px; margin-top: 2px; font-size: 10px; line-height: 1.4;"
              , HP.prop (PropName "innerHTML") html
              ]
              []
          Nothing -> HH.text ""
      ]

  -- Navigation box (shared between single and spine modes)
  navBox :: H.ComponentHTML Action () m
  navBox =
    HH.div
      [ HP.style "text-align: center; font-size: 11px;" ]
      [ HH.span
          [ HP.style "color: #2a5a8a; cursor: pointer; font-weight: 600;"
          , HE.onClick \_ -> ClickOpenInEditor
          ]
          [ HH.text "\x2192 Open in editor" ]
      ]

  -- -------------------------------------------------------------------------
  -- Caller/callee columns (unchanged)
  -- -------------------------------------------------------------------------

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

  renderModuleGroup :: String -> Array Loader.UsageNode -> H.ComponentHTML Action () m
  renderModuleGroup modName nodes =
    HH.div
      [ HP.style "border-radius: 4px; padding: 6px 8px; background: rgba(0,0,0,0.04); border-left: 3px solid #c8c0a8;" ]
      [ HH.div
          [ HP.style "font-size: 9px; color: #8b7355; margin-bottom: 4px; font-weight: 700; letter-spacing: 0.3px; cursor: pointer;"
          , HE.onClick \_ -> ClickModule modName
          , HP.title modName
          ]
          [ HH.text $ shortModuleName modName ]
      , HH.div
          [ HP.style "display: flex; flex-direction: column; gap: 4px;" ]
          (nodes <#> renderNode)
      ]

  renderNode :: Loader.UsageNode -> H.ComponentHTML Action () m
  renderNode node =
    let purityBg = if isEffectful node then "rgba(232, 150, 12, 0.08)" else "rgba(59, 130, 196, 0.06)"
        nodeKey = node.moduleName <> "." <> node.declName
        dimmed = case highlightSet of
          Just hs -> not (Set.member nodeKey hs)
          Nothing -> false
        opacity = if dimmed then "opacity: 0.2; " else ""
    in
    HH.div
      [ HP.style $ "cursor: pointer; padding: 3px 6px; border-radius: 3px; transition: opacity 150ms ease, background 100ms ease; background: " <> purityBg <> "; " <> opacity
      , HE.onClick \_ -> ClickNode node.moduleName node.declName
      , HE.onMouseEnter \_ -> HoverNode (Just (node.moduleName <> "." <> node.declName))
      , HE.onMouseLeave \_ -> HoverNode Nothing
      , HP.title (node.moduleName <> "." <> node.declName)
      ]
      [ HH.div
          [ HP.style "display: flex; align-items: center; gap: 5px;" ]
          [ HH.span
              [ HP.style $ "display: inline-block; width: 6px; height: 6px; border-radius: 50%; flex-shrink: 0; background: " <> kindColor node.kind <> ";" ]
              []
          , HH.span
              [ HP.style "font-size: 11px; color: #333; font-weight: 600; white-space: nowrap; overflow: hidden; text-overflow: ellipsis;" ]
              [ HH.text node.declName ]
          ]
      , case renderSigletHtml node of
          Just html ->
            HH.div
              [ HP.style "margin-left: 11px; margin-top: 2px; font-size: 10px; line-height: 1.4;"
              , HP.prop (PropName "innerHTML") html
              ]
              []
          Nothing -> HH.text ""
      ]

-- =============================================================================
-- Hover Highlighting
-- =============================================================================

-- | Compute highlight set from either sibling hover or node hover.
-- | Returns Nothing when nothing is hovered (all nodes render normally).
-- | Returns Just (set of keys that should stay bright) when hovering.
computeHighlightSet :: State -> Loader.DeclarationUsage -> Maybe (Set.Set NodeKey)
computeHighlightSet state _usage =
  case state.hoveredSibling of
    Just siblingName -> Just (siblingConnections state siblingName)
    Nothing -> case state.hoveredNode of
      Just nodeKey -> Just (nodeConnections state nodeKey)
      Nothing -> Nothing

-- | Connections for a hovered sibling (uses moduleCalls for the focused module)
siblingConnections :: State -> String -> Set.Set NodeKey
siblingConnections state siblingName =
  let calls = state.input.moduleCalls
      moduleName = state.input.moduleName
      callees = calls
        # Array.filter (\c -> c.callerName == siblingName)
        # map (\c -> c.calleeModule <> "." <> c.calleeName)
      internalCallers = calls
        # Array.filter (\c -> c.calleeName == siblingName && not c.isCrossModule)
        # map (\c -> moduleName <> "." <> c.callerName)
      selfKey = moduleName <> "." <> siblingName
  in Set.fromFoldable (callees <> internalCallers <> [selfKey])

-- | Connections for a hovered caller/callee node (uses prebuilt call index)
nodeConnections :: State -> NodeKey -> Set.Set NodeKey
nodeConnections state nodeKey =
  let outgoing = fromMaybe Set.empty (Map.lookup nodeKey state.callsFrom)
      incoming = fromMaybe Set.empty (Map.lookup nodeKey state.callsTo)
  in Set.insert nodeKey (Set.union outgoing incoming)

-- | Build bidirectional call index from the loaded allCalls data.
-- | callsFrom: "mod.decl" → set of "mod.decl" it calls
-- | callsTo: "mod.decl" → set of "mod.decl" that call it
buildCallIndex
  :: Map.Map Int (Array Loader.V2FunctionCall)
  -> Map.Map String Int
  -> { callsFrom :: Map.Map NodeKey (Set.Set NodeKey)
     , callsTo :: Map.Map NodeKey (Set.Set NodeKey)
     }
buildCallIndex allCalls moduleNameToId =
  let
    -- Build reverse map: id → module name
    nameEntries :: Array (Tuple String Int)
    nameEntries = Map.toUnfoldable moduleNameToId
    idToName :: Map.Map Int String
    idToName = Map.fromFoldable $ map (\(Tuple k v) -> Tuple v k) nameEntries
    -- Process all calls
    callEntries :: Array (Tuple Int (Array Loader.V2FunctionCall))
    callEntries = Map.toUnfoldable allCalls
    allEntries = Array.concatMap (\(Tuple modId calls) ->
      case Map.lookup modId idToName of
        Just modName -> calls <#> \c ->
          { from: modName <> "." <> c.callerName
          , to: c.calleeModule <> "." <> c.calleeName
          }
        Nothing -> []
      ) callEntries
    -- Build forward index
    callsFrom = Array.foldl (\acc e ->
      Map.alter (Just <<< Set.insert e.to <<< fromMaybe Set.empty) e.from acc
    ) Map.empty allEntries
    -- Build reverse index
    callsTo = Array.foldl (\acc e ->
      Map.alter (Just <<< Set.insert e.from <<< fromMaybe Set.empty) e.to acc
    ) Map.empty allEntries
  in { callsFrom, callsTo }

-- =============================================================================
-- Helpers
-- =============================================================================

isEffectful :: Loader.UsageNode -> Boolean
isEffectful node = isEffectfulSig node.typeSignature

isEffectfulSig :: Maybe String -> Boolean
isEffectfulSig = case _ of
  Just sig -> String.contains (String.Pattern "Effect") sig
           || String.contains (String.Pattern "Aff") sig
  Nothing -> false

renderSigletHtml :: Loader.UsageNode -> Maybe String
renderSigletHtml node = do
  sig <- node.typeSignature
  ast <- parseToRenderType sig
  pure $ SigTree.renderSiglet { ast }

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

kindOrder :: String -> Int
kindOrder = case _ of
  "data"         -> 0
  "newtype"      -> 1
  "type_class"   -> 2
  "type_synonym" -> 3
  "value"        -> 4
  "foreign"      -> 5
  _              -> 6

sortByKind :: Array Loader.V2Declaration -> Array Loader.V2Declaration
sortByKind = Array.sortWith (\d -> Tuple (kindOrder d.kind) d.name)

-- =============================================================================
-- Action Handlers
-- =============================================================================

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  Initialize -> do
    state <- H.get
    -- Build bidirectional call index from allCalls data
    let idx = buildCallIndex state.input.allCalls state.input.moduleNameToId
    H.modify_ _ { callsFrom = idx.callsFrom, callsTo = idx.callsTo }
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

  ClickSibling declName -> do
    state <- H.get
    H.modify_ _ { hoveredSibling = Nothing }
    H.raise (NodeClicked state.input.packageName state.input.moduleName declName)

  HoverSibling mName -> do
    H.modify_ _ { hoveredSibling = mName }

  HoverNode mKey -> do
    H.modify_ _ { hoveredNode = mKey }

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
    Right usage -> do
      if usage.callerCount == 0 && usage.calleeCount == 0
        then H.modify_ _ { loadState = Empty }
        else do
          H.modify_ _ { loadState = Loaded usage }
          -- Scroll focused declaration into view
          liftEffect $ scrollChildIntoView "usage-graph-spine" "#spine-focus"
    Left err -> do
      log $ "[DeclarationUsageGraph] Error: " <> err
      H.modify_ _ { loadState = Empty }
