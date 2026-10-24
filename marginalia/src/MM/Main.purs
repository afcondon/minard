module MM.Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import MM.App as App
import Web.DOM.ParentNode (QuerySelector(..))

main :: Effect Unit
main = HA.runHalogenAff do
  _ <- HA.awaitBody
  mEl <- HA.selectElement (QuerySelector "#app")
  case mEl of
    Nothing -> liftEffect $ log "[MM] #app element not found"
    Just el -> void $ runUI App.component unit el
