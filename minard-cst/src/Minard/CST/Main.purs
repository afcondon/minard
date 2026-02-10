module Minard.CST.Main where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Either (Either(..))
import Data.Traversable (traverse)
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Encode (encodeJson)
import Data.String.CodeUnits as SCU
import Data.String (Pattern(..))
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (try)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile, readdir, stat)
import Node.FS.Stats (isDirectory)
import Node.Process (argv)
import Minard.CST.Extract (extractFileSpans, DeclSpan)

type FileResult =
  { path :: String
  , moduleName :: String
  , declarations :: Array DeclSpan
  }

main :: Effect Unit
main = do
  args <- argv
  let projectPath = fromMaybe "." (Array.index args 2)
  files <- findPursFiles projectPath projectPath
  results <- traverse (processFile projectPath) files
  let output = Array.catMaybes results
  log (stringify (encodeJson output))

processFile
  :: String
  -> { fullPath :: String, relPath :: String }
  -> Effect (Maybe FileResult)
processFile _ { fullPath, relPath } = do
  result <- try (readTextFile UTF8 fullPath)
  pure $ case result of
    Left _ -> Nothing
    Right content -> extractFileSpans content <#> \spans ->
      { path: relPath
      , moduleName: spans.moduleName
      , declarations: spans.declarations
      }

findPursFiles :: String -> String -> Effect (Array { fullPath :: String, relPath :: String })
findPursFiles rootPath currentPath = do
  result <- try (readdir currentPath)
  case result of
    Left _ -> pure []
    Right entries -> do
      nested <- traverse (\entry -> do
        let fullPath = currentPath <> "/" <> entry
        statResult <- try (stat fullPath)
        case statResult of
          Left _ -> pure []
          Right s
            | isDirectory s ->
                if shouldSkipDir entry then pure []
                else findPursFiles rootPath fullPath
            | isPursFile entry ->
                let relPath = makeRelative rootPath fullPath
                in pure [ { fullPath, relPath } ]
            | otherwise -> pure []
      ) entries
      pure (Array.concat nested)

shouldSkipDir :: String -> Boolean
shouldSkipDir name =
  name == "node_modules"
    || name == "output"
    || name == ".spago"
    || name == ".git"
    || name == "bower_components"
    || name == ".psci_modules"
    || name == ".package-cache"

isPursFile :: String -> Boolean
isPursFile name = isJust (SCU.stripSuffix (Pattern ".purs") name)

makeRelative :: String -> String -> String
makeRelative root full =
  let prefix = root <> "/"
      prefixLen = SCU.length prefix
  in
    if SCU.take prefixLen full == prefix then SCU.drop prefixLen full
    else full
