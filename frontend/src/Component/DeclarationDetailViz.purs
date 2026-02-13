-- | Declaration Detail Visualization Component
-- |
-- | Split-panel view: interactive declaration treemap (left) + declaration detail (right).
-- | Shows a single declaration "unfolded" with SVG type signature, doc comments,
-- | and child declarations. Clicking a treemap rect navigates to that declaration.
module CE2.Component.DeclarationDetailViz
  ( component
  , Input
  , Output(..)
  , Query(..)
  , Slot
  ) where

import Prelude

import Data.Array as Array
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.String as String
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS

import CE2.Containers as C
import CE2.Data.Loader as Loader
import CE2.Viz.DeclarationTreemap as DeclarationTreemap
import CE2.Viz.TypeSignature as TypeSignature
import CE2.Viz.ModuleTreemapEnriched (kindColor, childKindColor)

import Hylograph.HATS.InterpreterTick (clearContainer)

-- =============================================================================
-- Types
-- =============================================================================

type Input =
  { packageName :: String
  , moduleName :: String
  , declarationName :: String
  , declarations :: Array Loader.V2Declaration  -- All module declarations (for treemap)
  , functionCalls :: Map Int (Array Loader.V2FunctionCall)
  }

data Output
  = BackToModuleOverview
  | DeclarationClicked String String String  -- Navigate to a different declaration

type Slot = H.Slot Query Output

data Query a = NoQuery a

type State =
  { initialized :: Boolean
  , actionListener :: Maybe (HS.Listener Action)
  , lastInput :: Input
  }

data Action
  = Initialize
  | Receive Input
  | HandleDeclarationClick String String String
  | HandleBackClick

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
  , actionListener: Nothing
  , lastInput: input
  }

-- =============================================================================
-- Render
-- =============================================================================

render :: forall m. MonadAff m => State -> H.ComponentHTML Action () m
render state =
  let
    input = state.lastInput
    mDecl = Array.find (\d -> d.name == input.declarationName) input.declarations
  in
  HH.div
    [ HP.class_ (HH.ClassName "declaration-detail-viz")
    , HP.style "display: flex; width: 100%; height: 100%;"
    ]
    [ -- Left panel: Interactive declaration treemap
      HH.div
        [ HP.style "width: 38%; height: 100%; position: relative; border-right: 1px solid #e0e0e0;"
        ]
        [ HH.div
            [ HP.id C.declarationDetailTreemapContainerId
            , HP.style "width: 100%; height: 100%;"
            ]
            []
        ]
    -- Right panel: Declaration detail
    , HH.div
        [ HP.style "width: 62%; height: 100%; overflow-y: auto; padding: 16px 24px; font-family: 'Courier New', Courier, monospace;"
        ]
        [ case mDecl of
            Just decl -> renderDeclarationDetail input decl
            Nothing ->
              HH.div
                [ HP.style "color: #999; padding: 24px;" ]
                [ HH.text $ "Declaration not found: " <> input.declarationName ]
        ]
    ]

-- | Render the full declaration detail in the right panel
renderDeclarationDetail :: forall m. Input -> Loader.V2Declaration -> H.ComponentHTML Action () m
renderDeclarationDetail input decl =
  HH.div []
    [ -- Header: name + kind badge + dot
      HH.div
        [ HP.style "margin-bottom: 16px; padding-bottom: 12px; border-bottom: 2px solid #333;" ]
        [ HH.div
            [ HP.style "display: flex; align-items: center; gap: 10px;" ]
            [ HH.span
                [ HP.style $ "width: 14px; height: 14px; border-radius: 50%; background: " <> kindColor decl.kind <> "; flex-shrink: 0;" ]
                []
            , HH.span
                [ HP.style "font-size: 20px; font-weight: bold; color: #222;" ]
                [ HH.text decl.name ]
            , HH.span
                [ HP.style $ "font-size: 10px; padding: 2px 8px; border-radius: 3px; background: " <> kindColor decl.kind <> "; color: white; font-weight: 600;" ]
                [ HH.text (declKindLabel decl.kind) ]
            ]
        , HH.div
            [ HP.style "font-size: 11px; color: #888; margin-top: 4px;" ]
            [ HH.text $ input.moduleName <> " (" <> input.packageName <> ")" ]
        ]

    -- Type signature (SVG visualization)
    , HH.div
        [ HP.id C.typeSigContainerId
        , HP.style "margin-bottom: 16px; min-height: 36px; overflow-x: auto;"
        ]
        []

    -- Doc comments
    , case decl.comments of
        Just comments | String.length comments > 0 ->
          HH.div
            [ HP.style "margin-bottom: 16px;" ]
            [ HH.div
                [ HP.style "font-size: 10px; color: #999; text-transform: uppercase; margin-bottom: 4px; font-weight: 600;" ]
                [ HH.text "Documentation" ]
            , HH.pre
                [ HP.style "margin: 0; font-size: 12px; color: #444; white-space: pre-wrap; line-height: 1.5; background: #faf9f7; padding: 12px; border-radius: 4px; border: 1px solid #e8e6e0;" ]
                [ HH.text comments ]
            ]
        _ -> HH.text ""

    -- Children (constructors, class members, instances)
    , if Array.length decl.children > 0
      then HH.div
        [ HP.style "margin-bottom: 16px;" ]
        [ HH.div
            [ HP.style "font-size: 10px; color: #999; text-transform: uppercase; margin-bottom: 8px; font-weight: 600;" ]
            [ HH.text $ childKindLabel decl.kind <> " (" <> show (Array.length decl.children) <> ")" ]
        , HH.div [] (map (renderChildDeclaration decl.kind) decl.children)
        ]
      else HH.text ""

    -- Source code (if available from DB) or placeholder
    , case decl.sourceCode of
        Just src | String.length src > 0 ->
          HH.div
            [ HP.style "margin-top: 16px;" ]
            [ HH.div
                [ HP.style "font-size: 10px; color: #999; text-transform: uppercase; margin-bottom: 4px; font-weight: 600;" ]
                [ HH.text "Source" ]
            , HH.pre
                [ HP.style "margin: 0; font-size: 12px; color: #333; white-space: pre-wrap; line-height: 1.5; background: #f8f8f8; padding: 12px; border-radius: 4px; border: 1px solid #e0e0e0; overflow-x: auto; font-family: 'Courier New', Courier, monospace; tab-size: 2;" ]
                [ HH.code_ [ HH.text src ] ]
            ]
        _ ->
          -- Show source span location if available, otherwise placeholder
          case decl.sourceSpan of
            Just span ->
              HH.div
                [ HP.style "margin-top: 24px; padding: 12px 16px; background: #f5f5f5; border-radius: 4px; border: 1px solid #e0e0e0;" ]
                [ HH.span
                    [ HP.style "font-size: 11px; color: #888;" ]
                    [ HH.text $ "Defined in " <> span.name <> " (lines " <> show span.start <> "–" <> show span.end <> ")" ]
                ]
            Nothing ->
              HH.div
                [ HP.style "margin-top: 24px; padding: 16px; background: #f5f5f5; border-radius: 4px; border: 1px dashed #ccc; text-align: center;" ]
                [ HH.span
                    [ HP.style "font-size: 11px; color: #aaa; font-style: italic;" ]
                    [ HH.text "Source code not available" ]
                ]
    ]

-- | Render a child declaration (constructor, class member, instance)
renderChildDeclaration :: forall m. String -> Loader.V2ChildDeclaration -> H.ComponentHTML Action () m
renderChildDeclaration parentKind child =
  HH.div
    [ HP.style "padding: 6px 12px; margin-bottom: 2px; border-radius: 3px; background: rgba(0,0,0,0.02);" ]
    [ HH.div
        [ HP.style "display: flex; align-items: center; gap: 8px;" ]
        [ HH.span
            [ HP.style $ "width: 8px; height: 8px; border-radius: 50%; background: " <> childKindColor parentKind child.kind <> "; flex-shrink: 0;" ]
            []
        , HH.span
            [ HP.style "font-weight: bold; font-size: 12px; color: #333;" ]
            [ HH.text child.name ]
        , HH.span
            [ HP.style "font-size: 9px; color: #aaa;" ]
            [ HH.text child.kind ]
        ]
    , case child.typeSignature of
        Just sig ->
          HH.div
            [ HP.style "font-size: 11px; color: #0E4C8A; margin-top: 2px; margin-left: 16px; white-space: pre-wrap;" ]
            [ HH.text $ ":: " <> sig ]
        Nothing -> HH.text ""
    ]

-- =============================================================================
-- Action Handlers
-- =============================================================================

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  Initialize -> do
    state <- H.get
    let input = state.lastInput
    log $ "[DeclarationDetailViz] Initializing: " <> input.declarationName
        <> " in " <> input.moduleName

    { emitter, listener } <- liftEffect HS.create
    void $ H.subscribe emitter
    H.modify_ _ { actionListener = Just listener, initialized = true }

    renderTreemap input listener

    -- Render type signature visualization
    let mDecl = Array.find (\d -> d.name == input.declarationName) input.declarations
    case mDecl of
      Just decl -> liftEffect $ TypeSignature.renderInto C.typeSigContainerId decl.name decl.typeSignature
      Nothing -> pure unit

  Receive input -> do
    state <- H.get
    let changed = input.declarationName /= state.lastInput.declarationName
              || input.moduleName /= state.lastInput.moduleName
    H.modify_ _ { lastInput = input }
    when (changed && state.initialized) do
      case state.actionListener of
        Just listener -> do
          renderTreemap input listener
          -- Render type signature visualization
          let mDecl = Array.find (\d -> d.name == input.declarationName) input.declarations
          case mDecl of
            Just decl -> liftEffect $ TypeSignature.renderInto C.typeSigContainerId decl.name decl.typeSignature
            Nothing -> pure unit
        Nothing -> pure unit

  HandleDeclarationClick pkgName modName declName -> do
    log $ "[DeclarationDetailViz] Declaration clicked: " <> declName
    H.raise (DeclarationClicked pkgName modName declName)

  HandleBackClick -> do
    H.raise BackToModuleOverview

-- | Render the interactive declaration treemap in the left panel
renderTreemap :: forall m. MonadAff m => Input -> HS.Listener Action -> H.HalogenM State Action () Output m Unit
renderTreemap input listener = do
  let onDeclClick :: String -> String -> String -> Effect Unit
      onDeclClick pkg mod_ decl_ = HS.notify listener (HandleDeclarationClick pkg mod_ decl_)

  liftEffect do
    clearContainer C.declarationDetailTreemapContainer

    DeclarationTreemap.render
      { containerSelector: C.declarationDetailTreemapContainer
      , width: 600.0
      , height: 900.0
      , packageName: input.packageName
      , moduleName: input.moduleName
      , onDeclarationClick: Just onDeclClick
      , focusedDeclaration: Just input.declarationName
      }
      input.declarations
      input.functionCalls

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

childKindLabel :: String -> String
childKindLabel = case _ of
  "data"       -> "Constructors"
  "newtype"    -> "Constructors"
  "type_class" -> "Members"
  _            -> "Children"
