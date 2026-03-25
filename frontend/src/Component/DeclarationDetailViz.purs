-- | Declaration Detail Visualization Component
-- |
-- | Usage-graph-first layout: shows the cross-module caller/callee graph
-- | centered on the focused declaration. Source code viewing is delegated
-- | to VS Code via "Open in editor".
module CE2.Component.DeclarationDetailViz
  ( component
  , Input
  , Output(..)
  , Query(..)
  , Slot
  ) where

import Prelude

import Data.Array as Array
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String as String
import Effect (Effect)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Data.Either (Either(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))

import CE2.Data.Loader as Loader
import CE2.Component.DeclarationUsageGraph as DeclarationUsageGraph
import CE2.Viz.ModuleTreemapEnriched (kindColor)

foreign import openUri :: String -> Effect Unit

-- =============================================================================
-- Types
-- =============================================================================

type Input =
  { packageName :: String
  , moduleName :: String
  , declarationName :: String
  , declarations :: Array Loader.V2Declaration
  , moduleCalls :: Array Loader.V2FunctionCall
  , allCalls :: Map.Map Int (Array Loader.V2FunctionCall)
  , moduleNameToId :: Map.Map String Int
  }

data Output
  = BackToModuleOverview
  | DeclarationClicked String String String
  | NavigateToModule String           -- moduleName → module structure
  | NavigateToModuleSignatures String -- moduleName → signature map
  | NavigateToPackage String          -- packageName → package treemap

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
  }

data Action
  = Initialize
  | Receive Input
  | HandleUsageGraphOutput DeclarationUsageGraph.Output
  | ClickedModuleName
  | OpenInEditor

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
        , HH.span [ HP.style "color: #ccc;" ] [ HH.text " \x203A " ]
        , HH.span
            [ HP.style "color: #0E4C8A; cursor: pointer; text-decoration: underline; text-underline-offset: 2px; text-decoration-color: rgba(14,76,138,0.3);"
            , HE.onClick \_ -> ClickedModuleName
            ]
            [ HH.text shortModuleName ]
        , HH.span [ HP.style "color: #ccc;" ] [ HH.text " \x203A " ]
        , HH.span
            [ HP.style "font-weight: bold; color: #222;" ]
            [ HH.text input.declarationName ]
        , case mDecl of
            Just decl ->
              HH.span
                [ HP.style $ "font-size: 9px; padding: 1px 6px; border-radius: 3px; margin-left: 4px; background: " <> kindColor decl.kind <> "; color: white; font-weight: 600;" ]
                [ HH.text (declKindLabel decl.kind) ]
            Nothing -> HH.text ""
        , HH.span [ HP.style "flex: 1;" ] []
        , HH.span
            [ HP.style "font-size: 10px; color: #999; cursor: pointer; transition: color 150ms ease;"
            , HE.onClick \_ -> OpenInEditor
            ]
            [ HH.text "Open in editor" ]
        ]

    -- Usage graph (full area)
    , HH.div
        [ HP.style "flex: 1; min-height: 0; overflow: hidden;" ]
        [ HH.slot _declarationUsageGraph unit DeclarationUsageGraph.component
            { packageName: input.packageName
            , moduleName: input.moduleName
            , declarationName: input.declarationName
            , focusTypeSignature: mDecl >>= _.typeSignature
            , declarations: input.declarations
            , moduleCalls: input.moduleCalls
            , allCalls: input.allCalls
            , moduleNameToId: input.moduleNameToId
            }
            HandleUsageGraphOutput
        ]
    ]

-- =============================================================================
-- Action Handlers
-- =============================================================================

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action ChildSlots Output m Unit
handleAction = case _ of
  Initialize -> do
    H.modify_ _ { initialized = true }

  Receive input -> do
    H.modify_ _ { lastInput = input }

  HandleUsageGraphOutput output -> case output of
    DeclarationUsageGraph.NodeClicked pkgName modName declName -> do
      log $ "[DeclarationDetailViz] Usage graph node clicked: " <> modName <> "." <> declName
      H.raise (DeclarationClicked pkgName modName declName)
    DeclarationUsageGraph.ModuleClicked modName -> do
      log $ "[DeclarationDetailViz] Module clicked: " <> modName
      H.raise (NavigateToModule modName)
    DeclarationUsageGraph.OpenFocusInEditor -> do
      handleAction OpenInEditor
    DeclarationUsageGraph.ViewModuleSignatures modName -> do
      log $ "[DeclarationDetailViz] View signatures: " <> modName
      H.raise (NavigateToModuleSignatures modName)
    DeclarationUsageGraph.ViewPackage pkgName -> do
      log $ "[DeclarationDetailViz] View package: " <> pkgName
      H.raise (NavigateToPackage pkgName)

  ClickedModuleName -> do
    log "[DeclarationDetailViz] Module name clicked -> back to overview"
    H.raise BackToModuleOverview

  OpenInEditor -> do
    state <- H.get
    log $ "[DeclarationDetailViz] Opening in VS Code: " <> state.lastInput.moduleName
    result <- liftAff $ Loader.fetchSourceLocation state.lastInput.moduleName
    case result of
      Right loc -> do
        log $ "[DeclarationDetailViz] Resolved path: " <> loc.filePath
        liftEffect $ openUri ("vscode://file/" <> loc.filePath)
      Left err ->
        log $ "[DeclarationDetailViz] Could not resolve path: " <> err

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

-- | Extract the last segment of a module name (e.g., "CE2.Viz.Foo" -> "Foo")
lastSegment :: String -> String
lastSegment name =
  case Array.last (String.split (String.Pattern ".") name) of
    Just s -> s
    Nothing -> name
