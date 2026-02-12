-- | Entry point for Type Signature Visualizer
-- |
-- | Sets up window.TypeSigViz.parse() for the JS renderer to call,
-- | then triggers the JS app initialization.
module TypeSigViz.Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import TypeSigViz.TypeAST (parseAndExport, JsResult)

main :: Effect Unit
main = do
  log "[TypeSigViz] Initializing with CST parser"
  exportParser parseAndExport
  log "[TypeSigViz] Parser exported, triggering app init"
  triggerInit

foreign import exportParser :: (String -> JsResult) -> Effect Unit
foreign import triggerInit :: Effect Unit
