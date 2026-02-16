-- | Signature Test Page
-- |
-- | Renders every test signature in two sections:
-- |   1. Sigils (full HATS semantic HTML)
-- |   2. Siglets (compact dot notation)
module CE2.SigTest.Main where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)

import CE2.Viz.SignatureTree as SigTree
import CE2.Viz.TypeSignature.TypeAST (parseToRenderType, elideAST)
import CE2.SigTest.Signatures (signatures)

foreign import createRow :: String -> Int -> String -> String -> String -> String -> Boolean -> Effect Unit

main :: Effect Unit
main = do
  log "[SigTest] Rendering signature gallery"
  -- Pass 1: Sigils
  Array.foldM (\_ s -> renderSigil s) unit signatures
  -- Pass 2: Siglets
  Array.foldM (\_ s -> renderSiglet s) unit signatures
  log "[SigTest] Done"

renderSigil :: { idx :: Int, category :: String, name :: String, sig :: String } -> Effect Unit
renderSigil s = do
  let containerId = "sigil-" <> show s.idx
  case parseToRenderType s.sig of
    Just ast -> do
      createRow "sigil-table" s.idx s.name s.category s.sig containerId true
      SigTree.renderSignatureInto ("#" <> containerId)
        { name: s.name, sig: s.sig, ast, typeParams: [], className: Nothing }
    Nothing -> do
      createRow "sigil-table" s.idx s.name s.category s.sig containerId false
      log $ "[SigTest] Parse failed: #" <> show s.idx <> " " <> s.name

renderSiglet :: { idx :: Int, category :: String, name :: String, sig :: String } -> Effect Unit
renderSiglet s = do
  let containerId = "siglet-" <> show s.idx
  case parseToRenderType s.sig of
    Just ast -> do
      createRow "siglet-table" s.idx s.name s.category s.sig containerId true
      SigTree.renderSigletInto ("#" <> containerId)
        { ast: elideAST ast, maxWidth: 400.0 }
    Nothing -> do
      createRow "siglet-table" s.idx s.name s.category s.sig containerId false
      log $ "[SigTest] Parse failed: #" <> show s.idx <> " " <> s.name
