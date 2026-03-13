-- | Header breadcrumb trail.
-- | Renders clickable path segments for the current scene hierarchy.
module CE2.Component.Header.Breadcrumbs (render) where

import Prelude

import Data.Array as Array
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import CE2.Scene (Scene, BreadcrumbSegment, sceneBreadcrumbs)

render :: forall w i. (Scene -> i) -> Scene -> HH.HTML w i
render onNavigate scene =
  let
    crumbs = sceneBreadcrumbs scene
    lastIdx = Array.length crumbs - 1
  in
    HH.span
      [ HP.style "display: inline-flex; align-items: center;" ]
      (Array.concat (Array.mapWithIndex (renderSegment lastIdx) crumbs))
  where
  renderSegment :: Int -> Int -> BreadcrumbSegment -> Array (HH.HTML w i)
  renderSegment lastIdx idx seg =
    let
      isFinal = idx == lastIdx
      separator = if idx > 0
        then [ HH.span
                 [ HP.style "margin: 0 6px; opacity: 0.5;" ]
                 [ HH.text "\x203A" ] ]
        else []
      kindPrefix = if seg.kind == ""
        then []
        else [ HH.span
                 [ HP.style "opacity: 0.45; font-weight: normal;" ]
                 [ HH.text (seg.kind <> " ") ] ]
      label =
        if isFinal
          then
            HH.span
              [ HP.style "font-weight: bold;" ]
              (kindPrefix <> [ HH.text seg.label ])
          else
            HH.span
              [ HE.onClick \_ -> onNavigate seg.scene
              , HP.style "cursor: pointer; text-decoration: underline; text-underline-offset: 2px; text-decoration-color: rgba(0,0,0,0.3);"
              ]
              (kindPrefix <> [ HH.text seg.label ])
    in separator <> [label]
