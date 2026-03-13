-- | Header branding element.
-- | Renders "MINARD" as a clickable home link.
module CE2.Component.Header.Branding (render) where

import Prelude

import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

render :: forall w i. i -> HH.HTML w i
render onHome =
  HH.span
    [ HE.onClick \_ -> onHome
    , HP.style $ "font-weight: bold; font-size: 12px; letter-spacing: 1px; "
        <> "text-transform: uppercase; margin-right: 8px; cursor: pointer;"
    ]
    [ HH.text "MINARD" ]
