-- | Shared SVG helpers for Halogen components.
-- |
-- | Provides `svgElem` and `sa` for inline SVG rendering without
-- | importing Namespace/ElemName/AttrName in every component.
module CE2.Util.SVG
  ( svgNS
  , svgElem
  , sa
  ) where

import Halogen.HTML as HH
import Halogen.HTML.Core (AttrName(..), ElemName(..), Namespace(..))
import Halogen.HTML.Properties as HP

svgNS :: Namespace
svgNS = Namespace "http://www.w3.org/2000/svg"

svgElem :: forall r w i. String -> Array (HH.IProp r i) -> Array (HH.HTML w i) -> HH.HTML w i
svgElem name = HH.elementNS svgNS (ElemName name)

sa :: forall r i. String -> String -> HH.IProp r i
sa name val = HP.attr (AttrName name) val
