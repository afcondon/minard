module CE2.Viz.DOMHelpers (scrollElementIntoView, scrollChildIntoView, setInnerHTML, setDocumentTitle) where

import Prelude (Unit)
import Effect (Effect)

foreign import scrollElementIntoView :: String -> Effect Unit
foreign import scrollChildIntoView :: String -> String -> Effect Unit
foreign import setInnerHTML :: String -> String -> Effect Unit
foreign import setDocumentTitle :: String -> Effect Unit
