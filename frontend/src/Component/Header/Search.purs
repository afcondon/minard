-- | Header search typeahead.
-- | Renders the search input and dropdown results overlay.
module CE2.Component.Header.Search (SearchState, SearchActions, render) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as String
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import CE2.Data.Loader as Loader

type SearchState =
  { query :: String
  , results :: Array Loader.UnifiedSearchResult
  , selectedIndex :: Int
  , open :: Boolean
  }

type SearchActions i =
  { onInput :: String -> i
  , onKeyDown :: KeyboardEvent -> i
  , onDismiss :: i
  , onConfirmIndex :: Int -> i
  }

render :: forall w i. SearchState -> SearchActions i -> HH.HTML w i
render state actions =
  HH.div
    [ HP.class_ (HH.ClassName "header-search-wrapper") ]
    [ HH.input
        [ HP.type_ HP.InputText
        , HP.class_ (HH.ClassName "module-search-input")
        , HP.placeholder "search..."
        , HP.value state.query
        , HE.onValueInput actions.onInput
        , HE.onKeyDown actions.onKeyDown
        , HE.onBlur \_ -> actions.onDismiss
        ]
    , if state.open && Array.length state.results > 0
        then renderDropdown state actions
        else HH.text ""
    ]

renderDropdown :: forall w i. SearchState -> SearchActions i -> HH.HTML w i
renderDropdown state actions =
  let
    -- Results are pre-sorted by the coordinator (packages > modules > declarations)
    -- Cap at 8 visible to avoid hidden scrolling
    visible = Array.take 8 state.results
    total = Array.length state.results
  in
  HH.div
    [ HP.class_ (HH.ClassName "module-search-dropdown") ]
    ( Array.mapWithIndex renderResult visible
      <> if total > 8
           then [ HH.div
                    [ HP.style "padding: 4px 10px; font-size: 9px; color: #999; text-align: center;" ]
                    [ HH.text $ show (total - 8) <> " more\x2026" ]
                ]
           else []
    )
  where
  renderResult :: Int -> Loader.UnifiedSearchResult -> HH.HTML w i
  renderResult idx result =
    let
      isSelected = idx == state.selectedIndex
      entityIcon = case result.entityType of
        "package" -> "pkg"
        "module" -> "mod"
        _ -> fromMaybe "val" (result.kind <#> kindAbbrev)
      contextText = case result.entityType of
        "package" -> result.packageVersion
        "module" -> result.packageName
        "declaration" -> fromMaybe "" result.moduleName <> " / " <> result.packageName
        _ -> ""
      typeSigSnippet = case result.typeSignature of
        Just sig -> " :: " <> String.take 50 sig
        Nothing -> ""
    in
      HH.div
        [ HP.classes
            [ HH.ClassName "module-search-result"
            , HH.ClassName (if isSelected then "module-search-result--selected" else "")
            ]
        , HE.onMouseDown \_ -> actions.onConfirmIndex idx
        ]
        [ HH.div
            [ HP.style "display: flex; align-items: baseline; gap: 6px;" ]
            [ HH.span
                [ HP.style $ "font-size: 8px; padding: 1px 3px; border-radius: 2px; background: " <> entityColor result.entityType <> "; color: #fff;" ]
                [ HH.text entityIcon ]
            , HH.span
                [ HP.style "font-weight: bold;" ]
                [ HH.text result.name ]
            , HH.span
                [ HP.style "opacity: 0.5; font-size: 10px;" ]
                [ HH.text contextText ]
            ]
        , if typeSigSnippet /= ""
            then HH.div
              [ HP.style "font-size: 9px; opacity: 0.4; margin-top: 1px; white-space: nowrap; overflow: hidden; text-overflow: ellipsis;" ]
              [ HH.text typeSigSnippet ]
            else HH.text ""
        ]

  kindAbbrev :: String -> String
  kindAbbrev = case _ of
    "value" -> "val"
    "data" -> "dat"
    "newtype" -> "new"
    "type_synonym" -> "syn"
    "type_class" -> "cls"
    "foreign" -> "ffi"
    other -> String.take 3 other

  entityColor :: String -> String
  entityColor = case _ of
    "package" -> "#7c3aed"
    "module" -> "#0891b2"
    "declaration" -> "#4e79a7"
    _ -> "#666"

