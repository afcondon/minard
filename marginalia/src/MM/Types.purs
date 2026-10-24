-- | Decoders for the Marginalia tree.json contract:
-- |   { generatedAt, root, nodes: <recursive TreeNode>, edges: [{source,target,type}] }
-- | This is the exact shape the eventual /api/tree endpoint will serve; for now it
-- | is baked statically by cloudflare-sites/andrewcondon/working/tree.mjs.
module MM.Types
  ( TreeNode(..)
  , Edge
  , TreeData(..)
  , decodeTreeNode
  , decodeTreeData
  , parseTree
  ) where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson, printJsonDecodeError, (.:), (.:?))
import Data.Argonaut.Decode.Error (JsonDecodeError)
import Data.Argonaut.Parser (jsonParser)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Maybe (Maybe, maybe)
import Data.Traversable (traverse)

newtype TreeNode = TreeNode
  { id :: Int
  , name :: String
  , domain :: Maybe String
  , status :: Maybe String
  , tags :: Array String -- semantic; the viewer's theme maps these (and domain/status) to style
  , tagline :: Maybe String -- intrinsic one-line definition (Marginalia content, not CV copy)
  , description :: Maybe String -- the project summary, shown in hover tooltips
  , cover :: Maybe String -- absolute URL of the project's hero screenshot
  , repoUrl :: Maybe String -- absolute GitHub URL (github-mode Cmd-click target); null if no repo
  , value :: Number
  , children :: Array TreeNode
  }

type Edge =
  { source :: Int
  , target :: Int
  , type_ :: String
  }

newtype TreeData = TreeData
  { generatedAt :: String
  , root :: Int
  , rectDepth :: Int
  , nodes :: TreeNode
  , edges :: Array Edge
  }

decodeTreeNode :: Json -> Either JsonDecodeError TreeNode
decodeTreeNode json = do
  o <- decodeJson json
  id <- o .: "id"
  name <- o .: "name"
  domain <- o .:? "domain"
  status <- o .:? "status"
  tags <- maybe [] identity <$> o .:? "tags"
  tagline <- o .:? "tagline"
  description <- o .:? "description"
  cover <- o .:? "cover"
  repoUrl <- o .:? "repoUrl"
  value <- o .: "value"
  (kids :: Array Json) <- o .: "children"
  children <- traverse decodeTreeNode kids
  pure $ TreeNode { id, name, domain, status, tags, tagline, description, cover, repoUrl, value, children }

decodeEdge :: Json -> Either JsonDecodeError Edge
decodeEdge json = do
  o <- decodeJson json
  source <- o .: "source"
  target <- o .: "target"
  type_ <- o .: "type"
  pure { source, target, type_ }

decodeTreeData :: Json -> Either JsonDecodeError TreeData
decodeTreeData json = do
  o <- decodeJson json
  generatedAt <- o .: "generatedAt"
  root <- o .: "root"
  rectDepth <- maybe 1 identity <$> o .:? "rectDepth"
  nodesJson <- o .: "nodes"
  nodes <- decodeTreeNode nodesJson
  (edgesJson :: Array Json) <- o .: "edges"
  edges <- traverse decodeEdge edgesJson
  pure $ TreeData { generatedAt, root, rectDepth, nodes, edges }

parseTree :: String -> Either String TreeData
parseTree s = do
  json <- jsonParser s
  lmap printJsonDecodeError (decodeTreeData json)
