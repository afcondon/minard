module CE2.Viz.DOMHelpers (scrollElementIntoView) where

import Prelude (Unit)
import Effect (Effect)

foreign import scrollElementIntoView :: String -> Effect Unit
