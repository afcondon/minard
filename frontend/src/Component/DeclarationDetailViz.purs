-- | Declaration Detail Visualization Component
-- |
-- | Source-code-first layout: shows the full module source with the focused
-- | declaration highlighted, syntax-colored, and clickable identifiers.
-- | Compact usage summary (callers/callees) at the bottom.
module CE2.Component.DeclarationDetailViz
  ( component
  , Input
  , Output(..)
  , Query(..)
  , Slot
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as String
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))

import CE2.Data.Loader as Loader
import CE2.Component.DeclarationUsageGraph as DeclarationUsageGraph
import CE2.Viz.SourceCode as SourceCode
import CE2.Viz.ModuleTreemapEnriched (kindColor)

-- =============================================================================
-- Types
-- =============================================================================

type Input =
  { packageName :: String
  , moduleName :: String
  , declarationName :: String
  , declarations :: Array Loader.V2Declaration
  , knownDeclarations :: Array SourceCode.KnownDeclaration
  }

data Output
  = BackToModuleOverview
  | DeclarationClicked String String String

type Slot = H.Slot Query Output

data Query a = NoQuery a

type ChildSlots =
  ( declarationUsageGraph :: DeclarationUsageGraph.Slot Unit
  )

_declarationUsageGraph :: Proxy "declarationUsageGraph"
_declarationUsageGraph = Proxy

type State =
  { initialized :: Boolean
  , lastInput :: Input
  , moduleSource :: Maybe String
  , sourceLoading :: Boolean
  }

data Action
  = Initialize
  | Receive Input
  | HandleUsageGraphOutput DeclarationUsageGraph.Output
  | ReceiveSource (Either String Loader.ModuleSource)
  | ClickedIdentifier String String String
  | ClickedModuleName

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
  { initialized: false
  , lastInput: input
  , moduleSource: Nothing
  , sourceLoading: false
  }

-- =============================================================================
-- Render
-- =============================================================================

render :: forall m. MonadAff m => State -> H.ComponentHTML Action ChildSlots m
render state =
  let
    input = state.lastInput
    mDecl = Array.find (\d -> d.name == input.declarationName) input.declarations
    shortModuleName = lastSegment input.moduleName
  in
  HH.div
    [ HP.class_ (HH.ClassName "declaration-detail-viz")
    , HP.style "display: flex; flex-direction: column; width: 100%; height: 100%; overflow: hidden;"
    ]
    [ -- Mini breadcrumb bar
      HH.div
        [ HP.style "padding: 8px 16px; border-bottom: 1px solid #e0e0e0; font-family: var(--font-mono); font-size: 11px; display: flex; align-items: center; gap: 6px; flex-shrink: 0; background: #faf9f7;" ]
        [ HH.span
            [ HP.style "color: #888;" ]
            [ HH.text input.packageName ]
        , HH.span [ HP.style "color: #ccc;" ] [ HH.text " › " ]
        , HH.span
            [ HP.style "color: #0E4C8A; cursor: pointer; text-decoration: underline; text-underline-offset: 2px; text-decoration-color: rgba(14,76,138,0.3);"
            , HE.onClick \_ -> ClickedModuleName
            ]
            [ HH.text shortModuleName ]
        , HH.span [ HP.style "color: #ccc;" ] [ HH.text " › " ]
        , HH.span
            [ HP.style "font-weight: bold; color: #222;" ]
            [ HH.text input.declarationName ]
        , case mDecl of
            Just decl ->
              HH.span
                [ HP.style $ "font-size: 9px; padding: 1px 6px; border-radius: 3px; margin-left: 4px; background: " <> kindColor decl.kind <> "; color: white; font-weight: 600;" ]
                [ HH.text (declKindLabel decl.kind) ]
            Nothing -> HH.text ""
        ]

    -- Source code panel (dominant, scrollable)
    , HH.div
        [ HP.style "flex: 1; overflow-y: auto; padding: 0; min-height: 0;"
        , HP.id "ps-source-scroll-container"
        ]
        [ renderSourcePanel state ]

    -- Usage summary (compact, at bottom)
    , HH.div
        [ HP.style "flex-shrink: 0; border-top: 1px solid #e0e0e0; max-height: 35%; overflow: hidden;"
        ]
        [ HH.slot _declarationUsageGraph unit DeclarationUsageGraph.component
            { packageName: input.packageName
            , moduleName: input.moduleName
            , declarationName: input.declarationName
            }
            HandleUsageGraphOutput
        ]
    ]

-- | Render the source code panel
renderSourcePanel :: forall m. State -> H.ComponentHTML Action ChildSlots m
renderSourcePanel state =
  let input = state.lastInput
  in case state.moduleSource of
    Just source ->
      let
        mDecl = Array.find (\d -> d.name == input.declarationName) input.declarations
        focusRange = mDecl >>= \decl -> decl.sourceSpan <#> \span ->
          { startLine: fromMaybe 1 (Array.index span.start 0)
          , endLine: fromMaybe 1 (Array.index span.end 0)
          }
        focusKind = fromMaybe "value" (mDecl <#> _.kind)
      in
        HH.div
          [ HP.style "padding: 0;" ]
          (SourceCode.renderSource source input.knownDeclarations focusRange focusKind ClickedIdentifier)

    Nothing ->
      if state.sourceLoading
        then
          HH.div
            [ HP.style "padding: 24px; text-align: center; color: #999; font-family: var(--font-mono); font-size: 12px;" ]
            [ HH.text "Loading source..." ]
        else
          -- Fallback: show per-declaration source code if available from DB
          renderFallbackSource state

-- | Fallback when full module source isn't available
renderFallbackSource :: forall m. State -> H.ComponentHTML Action ChildSlots m
renderFallbackSource state =
  let
    input = state.lastInput
    mDecl = Array.find (\d -> d.name == input.declarationName) input.declarations
  in case mDecl of
    Just decl -> case decl.sourceCode of
      Just src | String.length src > 0 ->
        HH.div
          [ HP.style "padding: 16px;" ]
          [ HH.div
              [ HP.style "font-size: 10px; color: #999; text-transform: uppercase; margin-bottom: 4px; font-weight: 600; font-family: var(--font-mono);" ]
              [ HH.text "Source (declaration only)" ]
          , HH.pre
              [ HP.style "margin: 0; font-size: 13px; color: #333; white-space: pre-wrap; line-height: 1.7; background: #faf9f7; padding: 12px; border-radius: 4px; border: 1px solid #e8e6e0; font-family: var(--font-mono); tab-size: 2;" ]
              [ HH.code_ [ HH.text src ] ]
          ]
      _ -> renderSourceUnavailable decl
    Nothing ->
      HH.div
        [ HP.style "padding: 24px; color: #999; font-family: var(--font-mono);" ]
        [ HH.text $ "Declaration not found: " <> input.declarationName ]

-- | Show location info when no source available
renderSourceUnavailable :: forall m. Loader.V2Declaration -> H.ComponentHTML Action ChildSlots m
renderSourceUnavailable decl =
  case decl.sourceSpan of
    Just span ->
      HH.div
        [ HP.style "margin: 24px; padding: 12px 16px; background: #f5f5f5; border-radius: 4px; border: 1px solid #e0e0e0; font-family: var(--font-mono);" ]
        [ HH.span
            [ HP.style "font-size: 11px; color: #888;" ]
            [ HH.text $ "Defined in " <> span.name <> " (lines " <> show span.start <> "–" <> show span.end <> ")" ]
        ]
    Nothing ->
      HH.div
        [ HP.style "margin: 24px; padding: 16px; background: #f5f5f5; border-radius: 4px; border: 1px dashed #ccc; text-align: center; font-family: var(--font-mono);" ]
        [ HH.span
            [ HP.style "font-size: 11px; color: #aaa; font-style: italic;" ]
            [ HH.text "Source code not available" ]
        ]

-- =============================================================================
-- Action Handlers
-- =============================================================================

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action ChildSlots Output m Unit
handleAction = case _ of
  Initialize -> do
    state <- H.get
    let input = state.lastInput
    log $ "[DeclarationDetailViz] Initializing: " <> input.declarationName
        <> " in " <> input.moduleName

    H.modify_ _ { initialized = true, sourceLoading = true }

    -- Fetch full module source
    void $ H.fork do
      result <- liftAff $ Loader.fetchModuleSource input.moduleName
      handleAction (ReceiveSource result)

  Receive input -> do
    state <- H.get
    let moduleChanged = input.moduleName /= state.lastInput.moduleName
        declChanged = input.declarationName /= state.lastInput.declarationName
    H.modify_ _ { lastInput = input }

    -- Re-fetch source if module changed
    when (moduleChanged && state.initialized) do
      H.modify_ _ { sourceLoading = true, moduleSource = Nothing }
      void $ H.fork do
        result <- liftAff $ Loader.fetchModuleSource input.moduleName
        handleAction (ReceiveSource result)

    -- Scroll to focused declaration when just the decl changed
    when (declChanged && not moduleChanged) do
      pure unit  -- Scroll handled by browser via anchor or future JS FFI

  ReceiveSource result -> do
    case result of
      Right ms -> do
        log $ "[DeclarationDetailViz] Source loaded: " <> show (String.length ms.source) <> " chars from " <> ms.path
        H.modify_ _ { moduleSource = Just ms.source, sourceLoading = false }
      Left err -> do
        log $ "[DeclarationDetailViz] Source unavailable: " <> err
        H.modify_ _ { sourceLoading = false }

  HandleUsageGraphOutput output -> case output of
    DeclarationUsageGraph.NodeClicked pkgName modName declName -> do
      log $ "[DeclarationDetailViz] Usage graph node clicked: " <> modName <> "." <> declName
      H.raise (DeclarationClicked pkgName modName declName)

  ClickedIdentifier pkgName modName declName -> do
    log $ "[DeclarationDetailViz] Identifier clicked: " <> declName
    H.raise (DeclarationClicked pkgName modName declName)

  ClickedModuleName -> do
    log "[DeclarationDetailViz] Module name clicked → back to overview"
    H.raise BackToModuleOverview

-- =============================================================================
-- Utilities
-- =============================================================================

declKindLabel :: String -> String
declKindLabel = case _ of
  "value"        -> "val"
  "data"         -> "data"
  "newtype"      -> "newtype"
  "type_class"   -> "class"
  "type_synonym" -> "type"
  "foreign"      -> "foreign"
  "alias"        -> "alias"
  _              -> ""

-- | Extract the last segment of a module name (e.g., "CE2.Viz.Foo" → "Foo")
lastSegment :: String -> String
lastSegment name =
  case Array.last (String.split (String.Pattern ".") name) of
    Just s -> s
    Nothing -> name
