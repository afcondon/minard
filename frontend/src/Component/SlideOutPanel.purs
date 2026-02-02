-- | Slide-Out Panel Component
-- |
-- | A bidirectional navigation surface that slides in from the right:
-- | - Viz → Panel: Click module/package → panel shows source/documentation
-- | - Panel → Viz: Click declaration in docs → viz navigates to that location
-- |
-- | Content modes:
-- | - Source code view (module source)
-- | - Pursuit documentation (iframe or parsed)
-- | - Search results
module CE2.Component.SlideOutPanel
  ( component
  , Input
  , Output(..)
  , Query(..)
  , Slot
  , PanelContent(..)
  , Declaration
  , SearchResult
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

-- =============================================================================
-- Types
-- =============================================================================

-- | Content to display in the panel
data PanelContent
  = NoContent
  | ModuleLoading
      { moduleName :: String
      , packageName :: String
      }
  | ModuleDeclarations
      { moduleName :: String
      , packageName :: String
      , packageVersion :: Maybe String
      , pursuitUrl :: Maybe String
      , declarations :: Array Declaration
      }
  | ModuleDocumentation
      { moduleName :: String
      , packageName :: String
      , pursuitUrl :: Maybe String
      }
  | SearchResults
      { query :: String
      , results :: Array SearchResult
      }

derive instance eqPanelContent :: Eq PanelContent

-- | A declaration to display
type Declaration =
  { name :: String
  , kind :: String  -- "value", "data", "newtype", "type_synonym", "type_class", "foreign"
  , typeSignature :: Maybe String
  , comments :: Maybe String
  }

-- | A search result item
type SearchResult =
  { name :: String
  , packageName :: String
  , moduleName :: String
  , kind :: String  -- "function", "type", "class", etc.
  }

-- | Input from parent
type Input =
  { initiallyOpen :: Boolean
  }

-- | Output to parent
data Output
  = PanelClosed
  | NavigateToModule String String  -- packageName, moduleName
  | NavigateToPackage String        -- packageName

-- | Queries from parent
data Query a
  = Open PanelContent a
  | Close a
  | SetContent PanelContent a
  | IsOpen (Boolean -> a)

-- | Slot type
type Slot = H.Slot Query Output

-- | Component state
type State =
  { isOpen :: Boolean
  , content :: PanelContent
  , isAnimating :: Boolean  -- During slide animation
  }

-- | Actions
data Action
  = Initialize
  | ClosePanel
  | NavigateToSearchResult { packageName :: String, moduleName :: String }  -- Click in search results

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
        , handleQuery = handleQuery
        , initialize = Just Initialize
        }
    }

initialState :: Input -> State
initialState input =
  { isOpen: input.initiallyOpen
  , content: NoContent
  , isAnimating: false
  }

-- =============================================================================
-- Render
-- =============================================================================

-- =============================================================================
-- House Style Colors (Data Viz Theme)
-- =============================================================================

-- | Paperwhite theme colors for module-level content
paperwhiteBg :: String
paperwhiteBg = "#FAFAFA"

paperwhiteHeaderBg :: String
paperwhiteHeaderBg = "#F0F0F0"

paperwhiteBorder :: String
paperwhiteBorder = "#E0E0E0"

blueprintAccent :: String
blueprintAccent = "#0E4C8A"

textPrimary :: String
textPrimary = "#1A1A1A"

textSecondary :: String
textSecondary = "#666666"

textMuted :: String
textMuted = "#999999"

-- =============================================================================
-- Render
-- =============================================================================

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div
    [ HP.class_ (HH.ClassName panelClass)
    , HP.style panelStyle
    ]
    [ -- Panel header with close button
      renderHeader state

      -- Panel content
    , HH.div
        [ HP.class_ (HH.ClassName "panel-content")
        , HP.style $ "flex: 1; overflow-y: auto; padding: 1rem; "
            <> "font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;"
        ]
        [ renderContent state.content ]
    ]
  where
  panelClass = "slide-out-panel" <> if state.isOpen then " open" else ""

  -- CSS for the sliding panel - paperwhite house style
  panelStyle =
    "position: fixed; "
    <> "top: 0; "
    <> "right: 0; "
    <> "width: 450px; "
    <> "height: 100vh; "
    <> "background: " <> paperwhiteBg <> "; "
    <> "color: " <> textPrimary <> "; "
    <> "border-left: 1px solid " <> paperwhiteBorder <> "; "
    <> "box-shadow: -2px 0 8px rgba(0, 0, 0, 0.08); "
    <> "transform: translateX(" <> if state.isOpen then "0" else "100%" <> "); "
    <> "transition: transform 0.3s ease-out; "
    <> "z-index: 1000; "
    <> "display: flex; "
    <> "flex-direction: column; "

-- | Render panel header with title and close button
renderHeader :: forall m. State -> H.ComponentHTML Action () m
renderHeader state =
  HH.div
    [ HP.class_ (HH.ClassName "panel-header")
    , HP.style $ "display: flex; justify-content: space-between; align-items: center; "
        <> "padding: 0.75rem 1rem; "
        <> "border-bottom: 1px solid " <> paperwhiteBorder <> "; "
        <> "background: " <> paperwhiteHeaderBg <> "; "
    ]
    [ HH.h3
        [ HP.style $ "margin: 0; font-size: 14px; font-weight: 600; color: " <> textPrimary <> ";" ]
        [ HH.text $ contentTitle state.content ]
    , HH.button
        [ HE.onClick \_ -> ClosePanel
        , HP.class_ (HH.ClassName "close-button")
        , HP.style $ "background: none; border: none; color: " <> textMuted <> "; "
            <> "font-size: 20px; cursor: pointer; padding: 0 4px; "
            <> "line-height: 1; transition: color 0.15s;"
        ]
        [ HH.text "×" ]
    ]

-- | Get title for current content
contentTitle :: PanelContent -> String
contentTitle = case _ of
  NoContent -> "Panel"
  ModuleLoading { moduleName } -> moduleName <> " ..."
  ModuleDeclarations { moduleName } -> moduleName
  ModuleDocumentation { moduleName } -> moduleName <> " - Docs"
  SearchResults { query } -> "Search: " <> query

-- | Render content based on type
renderContent :: forall m. PanelContent -> H.ComponentHTML Action () m
renderContent = case _ of
  NoContent ->
    HH.div
      [ HP.class_ (HH.ClassName "no-content")
      , HP.style $ "color: " <> textMuted <> "; text-align: center; padding: 2rem;"
      ]
      [ HH.p_ [ HH.text "Click a module to view its source or documentation." ]
      ]

  ModuleLoading { moduleName, packageName } ->
    HH.div
      [ HP.class_ (HH.ClassName "loading-view") ]
      [ HH.div
          [ HP.style $ "margin-bottom: 1rem; padding-bottom: 0.5rem; border-bottom: 1px solid " <> paperwhiteBorder <> ";" ]
          [ HH.div
              [ HP.style $ "font-size: 12px; color: " <> textSecondary <> ";" ]
              [ HH.text $ "Package: " <> packageName ]
          ]
      , HH.div
          [ HP.style $ "color: " <> textMuted <> "; text-align: center; padding: 2rem;" ]
          [ HH.text $ "Loading " <> moduleName <> "..." ]
      ]

  ModuleDeclarations { packageName, packageVersion, pursuitUrl, declarations } ->
    HH.div
      [ HP.class_ (HH.ClassName "declarations-view") ]
      [ -- Module header
        HH.div
          [ HP.style $ "margin-bottom: 1rem; padding-bottom: 0.5rem; border-bottom: 1px solid " <> paperwhiteBorder <> ";" ]
          [ HH.div
              [ HP.style "display: flex; justify-content: space-between; align-items: center;" ]
              [ HH.div
                  [ HP.style $ "font-size: 12px; color: " <> textSecondary <> ";" ]
                  [ HH.text $ "Package: " <> packageName <> fromMaybe "" (map (\v -> " @ " <> v) packageVersion) ]
              , -- Pursuit link
                case pursuitUrl of
                  Nothing -> HH.text ""
                  Just url ->
                    HH.a
                      [ HP.href url
                      , HP.target "_blank"
                      , HP.style $ "font-size: 11px; color: " <> blueprintAccent <> "; text-decoration: none; "
                          <> "padding: 2px 8px; border: 1px solid " <> blueprintAccent <> "33; border-radius: 4px;"
                      ]
                      [ HH.text "Pursuit →" ]
              ]
          , HH.div
              [ HP.style $ "font-size: 11px; color: " <> textMuted <> "; margin-top: 4px;" ]
              [ HH.text $ show (Array.length declarations) <> " declarations" ]
          ]
      , -- Declarations list
        if Array.null declarations
          then HH.div
            [ HP.style $ "color: " <> textMuted <> "; text-align: center; padding: 1rem;" ]
            [ HH.text "No declarations found" ]
          else HH.div_ (map renderDeclaration declarations)
      ]

  ModuleDocumentation { moduleName, packageName, pursuitUrl } ->
    HH.div
      [ HP.class_ (HH.ClassName "docs-view") ]
      [ -- Module info
        HH.div
          [ HP.style "margin-bottom: 1rem;" ]
          [ HH.div
              [ HP.style $ "font-size: 12px; color: " <> textSecondary <> "; margin-bottom: 0.5rem;" ]
              [ HH.text $ "Package: " <> packageName ]
          , HH.div
              [ HP.style $ "font-size: 16px; font-weight: 500; color: " <> blueprintAccent <> ";" ]
              [ HH.text moduleName ]
          ]
      , -- Pursuit link or iframe
        case pursuitUrl of
          Nothing ->
            HH.div
              [ HP.style $ "color: " <> textMuted <> ";" ]
              [ HH.text "Documentation not available" ]
          Just url ->
            HH.div_
              [ HH.a
                  [ HP.href url
                  , HP.target "_blank"
                  , HP.style $ "color: " <> blueprintAccent <> "; text-decoration: none;"
                  ]
                  [ HH.text "View on Pursuit →" ]
              -- Future: iframe embed with link interception
              ]
      ]

  SearchResults { query, results } ->
    HH.div
      [ HP.class_ (HH.ClassName "search-results") ]
      [ HH.div
          [ HP.style $ "margin-bottom: 1rem; color: " <> textSecondary <> "; font-size: 12px;" ]
          [ HH.text $ show (Array.length results) <> " results for \"" <> query <> "\"" ]
      , HH.div_
          (map renderSearchResult results)
      ]

-- | Render a single declaration
renderDeclaration :: forall m. Declaration -> H.ComponentHTML Action () m
renderDeclaration decl =
  HH.div
    [ HP.class_ (HH.ClassName "declaration")
    , HP.style $ "padding: 0.75rem; margin-bottom: 0.5rem; "
        <> "background: #FFFFFF; border-radius: 4px; "
        <> "border: 1px solid " <> paperwhiteBorder <> "; "
        <> "border-left: 3px solid " <> kindColor decl.kind <> ";"
    ]
    [ -- Declaration name and kind badge
      HH.div
        [ HP.style "display: flex; align-items: center; gap: 8px; margin-bottom: 4px;" ]
        [ HH.span
            [ HP.style $ "font-weight: 600; color: " <> textPrimary <> ";" ]
            [ HH.text decl.name ]
        , HH.span
            [ HP.style $ "font-size: 10px; padding: 2px 6px; border-radius: 3px; "
                <> "background: " <> kindColor decl.kind <> "18; "
                <> "color: " <> kindColor decl.kind <> ";"
            ]
            [ HH.text $ kindLabel decl.kind ]
        ]
    , -- Type signature (if present)
      case decl.typeSignature of
        Nothing -> HH.text ""
        Just sig ->
          HH.pre
            [ HP.style $ "margin: 4px 0 0 0; font-family: 'JetBrains Mono', 'Fira Code', monospace; "
                <> "font-size: 11px; color: " <> blueprintAccent <> "; "
                <> "white-space: pre-wrap; word-wrap: break-word; "
                <> "background: #F5F5F5; padding: 6px 8px; border-radius: 3px;"
            ]
            [ HH.code_ [ HH.text sig ] ]
    , -- Doc comment (if present)
      case decl.comments of
        Nothing -> HH.text ""
        Just "" -> HH.text ""
        Just comment ->
          HH.div
            [ HP.style $ "font-size: 11px; color: " <> textSecondary <> "; margin-top: 8px; line-height: 1.5;" ]
            [ HH.text comment ]
    ]
  where
  -- Color based on declaration kind (adjusted for light theme)
  kindColor :: String -> String
  kindColor = case _ of
    "value" -> "#2E7D32"      -- Green (darker for light bg)
    "data" -> "#F57C00"       -- Orange
    "newtype" -> "#F57C00"    -- Orange (same as data)
    "type_synonym" -> "#7B1FA2" -- Purple
    "type_class" -> "#0E4C8A" -- Blueprint blue
    "foreign" -> "#D84315"    -- Deep orange
    _ -> "#757575"            -- Gray

  -- Label based on declaration kind
  kindLabel :: String -> String
  kindLabel = case _ of
    "value" -> "val"
    "data" -> "data"
    "newtype" -> "newtype"
    "type_synonym" -> "type"
    "type_class" -> "class"
    "foreign" -> "foreign"
    _ -> "?"

-- | Render a single search result
renderSearchResult :: forall m. SearchResult -> H.ComponentHTML Action () m
renderSearchResult result =
  HH.div
    [ HP.class_ (HH.ClassName "search-result")
    , HP.style $ "padding: 0.75rem; margin-bottom: 0.5rem; "
        <> "background: #FFFFFF; border-radius: 4px; "
        <> "border: 1px solid " <> paperwhiteBorder <> "; "
        <> "cursor: pointer; transition: border-color 0.15s;"
    , HE.onClick \_ -> NavigateToSearchResult { packageName: result.packageName, moduleName: result.moduleName }
    ]
    [ HH.div
        [ HP.style $ "font-weight: 600; color: " <> textPrimary <> ";" ]
        [ HH.text result.name ]
    , HH.div
        [ HP.style $ "font-size: 11px; color: " <> textMuted <> "; margin-top: 2px;" ]
        [ HH.text $ result.packageName <> " / " <> result.moduleName
        , HH.span
            [ HP.style $ "margin-left: 8px; color: " <> blueprintAccent <> ";" ]
            [ HH.text result.kind ]
        ]
    ]

-- =============================================================================
-- Action Handlers
-- =============================================================================

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  Initialize -> do
    log "[SlideOutPanel] Initialized"

  ClosePanel -> do
    log "[SlideOutPanel] Closing"
    H.modify_ _ { isOpen = false }
    H.raise PanelClosed

  NavigateToSearchResult { packageName, moduleName } -> do
    log $ "[SlideOutPanel] Navigating to: " <> packageName <> "/" <> moduleName
    H.raise (NavigateToModule packageName moduleName)

-- =============================================================================
-- Query Handlers
-- =============================================================================

handleQuery :: forall m a. MonadAff m => Query a -> H.HalogenM State Action () Output m (Maybe a)
handleQuery = case _ of
  Open content a -> do
    log $ "[SlideOutPanel] Opening with content: " <> contentTitle content
    H.modify_ _ { isOpen = true, content = content }
    pure (Just a)

  Close a -> do
    log "[SlideOutPanel] Closing via query"
    H.modify_ _ { isOpen = false }
    H.raise PanelClosed
    pure (Just a)

  SetContent content a -> do
    log $ "[SlideOutPanel] Setting content: " <> contentTitle content
    H.modify_ _ { content = content }
    pure (Just a)

  IsOpen reply -> do
    state <- H.get
    pure (Just (reply state.isOpen))
