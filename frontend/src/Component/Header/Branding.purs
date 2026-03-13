-- | Header branding element.
-- | Renders "MINARD" + subtitle as a clickable home link.
-- | Double-height to anchor the two-tier header bar.
module CE2.Component.Header.Branding (render) where

import Prelude

import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

render :: forall w i. i -> HH.HTML w i
render onHome =
  HH.div
    [ HE.onClick \_ -> onHome
    , HP.style $ "display: flex; flex-direction: column; justify-content: center; "
        <> "cursor: pointer; user-select: none; padding-right: 12px; "
        <> "border-right: 1px solid rgba(0,0,0,0.15); margin-right: 12px;"
    ]
    [ HH.span
        [ HP.style $ "font-weight: bold; font-size: 14px; letter-spacing: 2px; "
            <> "text-transform: uppercase; line-height: 1.1;"
        ]
        [ HH.text "MINARD" ]
    , HH.span
        [ HP.style "font-size: 8px; opacity: 0.5; letter-spacing: 0.5px; line-height: 1.1;" ]
        [ HH.text "code cartography" ]
    ]
