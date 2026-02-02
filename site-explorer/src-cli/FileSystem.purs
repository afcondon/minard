module HalogenSpider.FileSystem where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff, attempt, throwError, error)
import Effect.Class (liftEffect)
import Node.Buffer as Buffer
import Node.Encoding (Encoding(..))
import Node.FS.Sync as FS
import Node.FS.Stats (Stats, isDirectory, isFile)
import Node.Path as Path

-- | Read a file as UTF-8 text
readTextFile :: String -> Aff String
readTextFile path = do
  result <- attempt $ liftEffect do
    buffer <- FS.readFile path
    Buffer.toString UTF8 buffer
  case result of
    Left err -> throwError (error $ "Failed to read " <> path <> ": " <> show err)
    Right content -> pure content

-- | Check if a path exists
pathExists :: String -> Effect Boolean
pathExists path = FS.exists path

-- | Check if path is a directory
isDir :: String -> Effect Boolean
isDir path = do
  exists <- FS.exists path
  if exists
    then do
      stats <- FS.stat path
      pure (isDirectory stats)
    else pure false

-- | List directory contents
readDir :: String -> Effect (Array String)
readDir = FS.readdir

-- | Find all .purs files recursively in a directory
findPursFiles :: String -> Aff (Array String)
findPursFiles dir = do
  entries <- liftEffect $ readDir dir
  let fullPaths = map (\e -> Path.concat [dir, e]) entries
  results <- traverse processEntry fullPaths
  pure (Array.concat results)
  where
    processEntry :: String -> Aff (Array String)
    processEntry path = do
      isDirectory <- liftEffect $ isDir path
      if isDirectory
        then do
          -- Skip node_modules, output, .spago
          let basename = Path.basename path
          if shouldSkipDir basename
            then pure []
            else findPursFiles path
        else
          if isPursFile path
            then pure [path]
            else pure []

    isPursFile :: String -> Boolean
    isPursFile path = String.drop (String.length path - 5) path == ".purs"

    shouldSkipDir :: String -> Boolean
    shouldSkipDir name =
      name == "node_modules"
      || name == "output"
      || name == ".spago"
      || name == ".git"
      || name == "bower_components"
