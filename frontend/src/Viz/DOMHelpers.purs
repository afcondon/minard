module CE2.Viz.DOMHelpers (scrollElementIntoView, setInnerHTML) where

import Prelude (Unit)
import Effect (Effect)

foreign import scrollElementIntoView :: String -> Effect Unit
foreign import setInnerHTML :: String -> String -> Effect Unit
