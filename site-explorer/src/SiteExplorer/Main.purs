module SiteExplorer.Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Web.DOM.ParentNode (QuerySelector(..))
import SiteExplorer.App as App
import SiteExplorer.Data (loadRouteData)

main :: Effect Unit
main = HA.runHalogenAff do
  -- Wait for body
  body <- HA.awaitBody

  -- Try to mount to #app, fallback to body
  maybeApp <- HA.selectElement (QuerySelector "#app")
  let mountPoint = case maybeApp of
        Just el -> el
        Nothing -> body

  -- Load data
  { routes, baseUrl } <- liftEffect loadRouteData

  -- Run the UI with input
  _ <- runUI App.component { routes, baseUrl } mountPoint
  pure unit
