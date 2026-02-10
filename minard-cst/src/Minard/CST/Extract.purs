module Minard.CST.Extract
  ( extractFileSpans
  , FileSpans
  , DeclSpan
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import PureScript.CST (parseModule, toRecovered, RecoveredParserResult(..))
import PureScript.CST.Errors (RecoveredError)
import PureScript.CST.Types
  ( Module(..), ModuleBody(..), ModuleHeader(..), Declaration(..)
  , ModuleName(..), Ident(..), Proper(..), Name(..), Labeled(..), Foreign(..)
  )
import PureScript.CST.Range (rangeOf)

type DeclSpan =
  { name :: String
  , kind :: String
  , span ::
      { start :: { line :: Int, column :: Int }
      , end :: { line :: Int, column :: Int }
      }
  }

type FileSpans =
  { moduleName :: String
  , declarations :: Array DeclSpan
  }

-- | Parse a PureScript source file and extract declaration spans.
extractFileSpans :: String -> Maybe FileSpans
extractFileSpans src = case parseModule src of
  ParseSucceeded m -> Just $ processModule (toRecovered m)
  ParseSucceededWithErrors m _ -> Just $ processModule m
  ParseFailed _ -> Nothing

processModule :: Module RecoveredError -> FileSpans
processModule (Module { header: ModuleHeader { name: Name { name: ModuleName modName } }, body: ModuleBody { decls } }) =
  { moduleName: modName
  , declarations: groupDecls (Array.mapMaybe extractRawDecl decls)
  }

-- Internal: raw declaration before grouping
type RawDecl =
  { name :: String
  , kind :: String
  , startLine :: Int
  , startCol :: Int
  , endLine :: Int
  , endCol :: Int
  }

extractRawDecl :: Declaration RecoveredError -> Maybe RawDecl
extractRawDecl decl =
  let
    range = rangeOf decl
    -- CST parser uses 0-indexed positions; we emit 1-indexed
    mkR n k = Just
      { name: n
      , kind: k
      , startLine: range.start.line + 1
      , startCol: range.start.column + 1
      , endLine: range.end.line + 1
      , endCol: range.end.column + 1
      }
  in
    case decl of
      DeclSignature (Labeled { label: Name { name: Ident n } }) ->
        mkR n "signature"
      DeclValue { name: Name { name: Ident n } } ->
        mkR n "value"
      DeclData { name: Name { name: Proper n } } _ ->
        mkR n "data"
      DeclNewtype { name: Name { name: Proper n } } _ _ _ ->
        mkR n "newtype"
      DeclType { name: Name { name: Proper n } } _ _ ->
        mkR n "typeSynonym"
      DeclClass { name: Name { name: Proper n } } _ ->
        mkR n "typeClass"
      DeclForeign _ _ f -> case f of
        ForeignValue (Labeled { label: Name { name: Ident n } }) ->
          mkR n "value"
        ForeignData _ (Labeled { label: Name { name: Proper n } }) ->
          mkR n "data"
        ForeignKind _ (Name { name: Proper n }) ->
          mkR n "kind"
      DeclKindSignature _ (Labeled { label: Name { name: Proper n } }) ->
        mkR n "kindSignature"
      DeclRole _ _ (Name { name: Proper n }) _ ->
        mkR n "role"
      _ -> Nothing

-- | Group consecutive declarations with the same name, then merge their spans.
-- This combines DeclSignature + DeclValue and multi-clause DeclValue entries.
groupDecls :: Array RawDecl -> Array DeclSpan
groupDecls = Array.concatMap mergeGroup <<< groupConsecutive

groupConsecutive :: Array RawDecl -> Array (Array RawDecl)
groupConsecutive arr = case Array.uncons arr of
  Nothing -> []
  Just { head: first, tail: rest } ->
    finalize $ Array.foldl step { groups: [], current: [ first ] } rest
  where
  step { groups, current } item =
    case Array.last current of
      Just prev | prev.name == item.name ->
        { groups, current: Array.snoc current item }
      _ ->
        { groups: Array.snoc groups current, current: [ item ] }
  finalize { groups, current } = Array.snoc groups current

mergeGroup :: Array RawDecl -> Array DeclSpan
mergeGroup group = case Array.head group, Array.last group of
  Just first, Just lst ->
    let
      primaryDecl = Array.find (\d -> d.kind /= "signature" && d.kind /= "kindSignature") group
      kind = case primaryDecl of
        Just d -> d.kind
        Nothing -> first.kind
    in
      [ { name: first.name
        , kind
        , span:
            { start: { line: first.startLine, column: first.startCol }
            , end: { line: lst.endLine, column: lst.endCol }
            }
        }
      ]
  _, _ -> []
