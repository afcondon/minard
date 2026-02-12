-- | Module Overview Visualization Component
-- |
-- | Split-panel view: declaration treemap (left) + declaration listing (right).
-- | Shows all declarations in a module at a glance ("folded code" view).
module CE2.Component.ModuleOverviewViz
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
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS

import CE2.Containers as C
import CE2.Data.Loader as Loader
import CE2.Viz.ModuleTreemapEnriched (kindColor)
import CE2.Viz.DeclarationTreemap as DeclarationTreemap

-- =============================================================================
-- Types
-- =============================================================================

type Input =
  { packageName :: String
  , moduleName :: String
  , declarations :: Array Loader.V2Declaration
  , functionCalls :: Map Int (Array Loader.V2FunctionCall)
  }

data Output
  = DeclarationClicked String String String  -- pkg, mod, decl
  | DeclarationHovered (Maybe String)        -- declaration name

type Slot = H.Slot Query Output

data Query a = NoQuery a

type State =
  { initialized :: Boolean
  , actionListener :: Maybe (HS.Listener Action)
  , lastInput :: Input
  , hoveredDeclaration :: Maybe String
  }

data Action
  = Initialize
  | Receive Input
  | HandleDeclarationClick String String String  -- pkg, mod, decl
  | SetHoveredDeclaration (Maybe String)

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
  , hoveredDeclaration: Nothing
  }

-- =============================================================================
-- Render
-- =============================================================================

render :: forall m. MonadAff m => State -> H.ComponentHTML Action () m
render state =
  let
    input = state.lastInput
    sorted = sortDeclarations input.declarations
  in
  HH.div
    [ HP.class_ (HH.ClassName "module-overview-viz")
    , HP.style "display: flex; width: 100%; height: 100%;"
    ]
    [ -- Left panel: Declaration treemap
      HH.div
        [ HP.style "width: 38%; height: 100%; position: relative; border-right: 1px solid rgba(255,255,255,0.1);"
        ]
        [ HH.div
            [ HP.id C.declarationTreemapContainerId
            , HP.style "width: 100%; height: 100%;"
            ]
            []
        ]
    -- Right panel: Declaration listing
    , HH.div
        [ HP.style "width: 62%; height: 100%; overflow-y: auto; padding: 16px 24px; font-family: 'Courier New', Courier, monospace;"
        ]
        [ -- Module header
          HH.div
            [ HP.style "margin-bottom: 16px; padding-bottom: 12px; border-bottom: 2px solid #333;" ]
            [ HH.div
                [ HP.style "font-size: 18px; font-weight: bold; color: #333;" ]
                [ HH.text input.moduleName ]
            , HH.div
                [ HP.style "font-size: 11px; color: #888; margin-top: 4px;" ]
                [ HH.text $ show (Array.length input.declarations) <> " declarations" ]
            ]
        -- Declaration entries
        , HH.div [] (map (renderDeclarationEntry state) sorted)
        ]
    ]

-- | Render a single declaration entry in the right panel
renderDeclarationEntry :: forall m. State -> Loader.V2Declaration -> H.ComponentHTML Action () m
renderDeclarationEntry state decl =
  let
    isHovered = state.hoveredDeclaration == Just decl.name
    bgColor = if isHovered then kindColorLight decl.kind else "transparent"
    kindLabel = declKindLabel decl.kind
  in
  HH.div
    [ HP.id ("decl-" <> decl.name)
    , HP.style $ "padding: 8px 12px; margin-bottom: 2px; border-radius: 4px; cursor: pointer; "
        <> "background: " <> bgColor <> "; transition: background 0.15s ease;"
    , HE.onClick \_ -> HandleDeclarationClick state.lastInput.packageName state.lastInput.moduleName decl.name
    , HE.onMouseEnter \_ -> SetHoveredDeclaration (Just decl.name)
    , HE.onMouseLeave \_ -> SetHoveredDeclaration Nothing
    ]
    [ -- First line: dot + name + kind
      HH.div
        [ HP.style "display: flex; align-items: center; gap: 8px;" ]
        [ -- Kind color dot
          HH.span
            [ HP.style $ "width: 10px; height: 10px; border-radius: 50%; background: " <> kindColor decl.kind <> "; flex-shrink: 0;" ]
            []
        -- Name
        , HH.span
            [ HP.style "font-weight: bold; font-size: 13px; color: #222;" ]
            [ HH.text decl.name ]
        -- Kind label
        , HH.span
            [ HP.style "font-size: 10px; color: #999; margin-left: 4px;" ]
            [ HH.text kindLabel ]
        -- Child count
        , if Array.length decl.children > 0
          then HH.span
            [ HP.style "font-size: 10px; color: #aaa;" ]
            [ HH.text $ "(" <> show (Array.length decl.children) <> " " <> childKindLabel decl.kind <> ")" ]
          else HH.text ""
        ]
    -- Type signature
    , case decl.typeSignature of
        Just sig ->
          HH.div
            [ HP.style "font-size: 11px; color: #0E4C8A; margin-top: 3px; margin-left: 18px; white-space: pre-wrap; word-break: break-all;" ]
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
    log $ "[ModuleOverviewViz] Initializing: " <> input.moduleName
        <> ", " <> show (Array.length input.declarations) <> " declarations"

    { emitter, listener } <- liftEffect HS.create
    void $ H.subscribe emitter
    H.modify_ _ { actionListener = Just listener, initialized = true }

    renderDeclarationTreemap input

  Receive input -> do
    state <- H.get
    let changed = input.moduleName /= state.lastInput.moduleName
              || Array.length input.declarations /= Array.length state.lastInput.declarations
    H.modify_ _ { lastInput = input }
    when (changed && state.initialized) do
      renderDeclarationTreemap input

  HandleDeclarationClick pkgName modName declName -> do
    log $ "[ModuleOverviewViz] Declaration clicked: " <> declName
    H.raise (DeclarationClicked pkgName modName declName)

  SetHoveredDeclaration mName -> do
    H.modify_ _ { hoveredDeclaration = mName }
    H.raise (DeclarationHovered mName)

-- | Render the declaration treemap into the left panel container
renderDeclarationTreemap :: forall m. MonadAff m => Input -> H.HalogenM State Action () Output m Unit
renderDeclarationTreemap input = do
  state <- H.get
  let onDeclClick = makeDeclarationClickCallback state.actionListener
  liftEffect $ DeclarationTreemap.render
    { containerSelector: C.declarationTreemapContainer
    , width: 600.0
    , height: 900.0
    , packageName: input.packageName
    , moduleName: input.moduleName
    , onDeclarationClick: Just onDeclClick
    , focusedDeclaration: Nothing
    }
    input.declarations
    input.functionCalls

-- | Create a declaration click callback that notifies the Halogen listener
makeDeclarationClickCallback :: Maybe (HS.Listener Action) -> String -> String -> String -> Effect Unit
makeDeclarationClickCallback mListener pkgName modName declName = case mListener of
  Just listener -> HS.notify listener (HandleDeclarationClick pkgName modName declName)
  Nothing -> log $ "[ModuleOverviewViz] No listener for decl click: " <> pkgName <> "/" <> modName <> "/" <> declName

-- =============================================================================
-- Utilities
-- =============================================================================

-- | Sort declarations: data/newtype first, then type_class, then value, then foreign/alias
sortDeclarations :: Array Loader.V2Declaration -> Array Loader.V2Declaration
sortDeclarations = Array.sortWith \d -> kindSortOrder d.kind
  where
    kindSortOrder :: String -> Int
    kindSortOrder = case _ of
      "data"         -> 0
      "newtype"      -> 1
      "type_class"   -> 2
      "type_synonym" -> 3
      "value"        -> 4
      "foreign"      -> 5
      "alias"        -> 6
      _              -> 7

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
  "data"       -> "constructors"
  "newtype"    -> "constructors"
  "type_class" -> "members"
  _            -> "children"

-- | Light tint of kind color for hover background
kindColorLight :: String -> String
kindColorLight = case _ of
  "value"        -> "rgba(78,121,167,0.08)"
  "data"         -> "rgba(89,161,79,0.08)"
  "newtype"      -> "rgba(118,183,178,0.08)"
  "type_class"   -> "rgba(242,142,43,0.08)"
  "type_synonym" -> "rgba(237,201,72,0.08)"
  "foreign"      -> "rgba(225,87,89,0.08)"
  "alias"        -> "rgba(176,122,161,0.08)"
  _              -> "rgba(0,0,0,0.04)"
