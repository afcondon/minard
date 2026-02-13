-- | Simplified type AST for rendering
-- |
-- | Converts from PureScript.CST.Types.Type to a rendering-friendly structure.
-- | RenderType and pure helpers are re-exported from hylograph-sigil.
module CE2.Viz.TypeSignature.TypeAST
  ( parseToRenderType
  , extractCtorArgs
  , extractCtorRenderTypes
  , module Hylograph.Sigil.Types
  , module Hylograph.Sigil.Text
  ) where

import Prelude
import Prim hiding (Constraint, Row)

import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Maybe (Maybe(..))
import Data.String.Common (replaceAll)
import Data.String.Pattern (Pattern(..), Replacement(..))
import Data.Tuple (Tuple(..), snd)
import PureScript.CST (RecoveredParserResult(..), parseType)
import PureScript.CST.Types as CST
import Unsafe.Coerce (unsafeCoerce)

-- Re-export types and pure helpers from hylograph-sigil
import Hylograph.Sigil.Types (RenderType(..), RowField, Constraint)
import Hylograph.Sigil.Text (renderTypeToText, collectTypeVars, collectArrowParams)

-- ============================================================
-- CST -> RenderType conversion
-- ============================================================

simplify :: forall e. CST.Type e -> RenderType
simplify = case _ of
  CST.TypeVar (CST.Name n) ->
    let (CST.Ident s) = n.name in TVar s

  CST.TypeConstructor (CST.QualifiedName q) ->
    let (CST.Proper s) = q.name
    in case q.module of
      Nothing -> TCon s
      Just (CST.ModuleName m) -> TCon (m <> "." <> s)

  CST.TypeWildcard _ -> TWildcard

  CST.TypeHole (CST.Name n) ->
    let (CST.Ident s) = n.name in TVar ("?" <> s)

  CST.TypeString _ s -> TString s

  CST.TypeInt _ _ _ -> TCon "<int>"

  CST.TypeRow (CST.Wrapped w) ->
    let { fields, tail } = simplifyRow w.value
    in TRow fields tail

  CST.TypeRecord (CST.Wrapped w) ->
    let { fields, tail } = simplifyRow w.value
    in TRecord fields tail

  CST.TypeForall _ binders _ body ->
    let
      vars = Array.concatMap extractBinderName (NEA.toArray binders)
      inner = simplify body
    in case inner of
      TForall moreVars body' -> TForall (vars <> moreVars) body'
      _ -> TForall vars inner

  CST.TypeKinded ty _ kind ->
    TKinded (simplify ty) (simplify kind)

  CST.TypeApp head args ->
    TApp (simplify head) (map simplify (NEA.toArray args))

  CST.TypeOp head ops ->
    Array.foldl (\acc (Tuple (CST.QualifiedName q) rhs) ->
      let (CST.Operator op) = q.name
      in TOperator acc op (simplify rhs)
    ) (simplify head) (NEA.toArray ops)

  CST.TypeOpName (CST.QualifiedName q) ->
    let (CST.Operator s) = q.name in TCon ("(" <> s <> ")")

  CST.TypeArrow from _ to ->
    TArrow (simplify from) (simplify to)

  CST.TypeArrowName _ -> TCon "(->)"

  CST.TypeConstrained constraint _ body ->
    let
      c = simplifyConstraint constraint
      inner = simplify body
    in case inner of
      TConstrained moreCs body' -> TConstrained (Array.cons c moreCs) body'
      _ -> TConstrained [c] inner

  CST.TypeParens (CST.Wrapped w) ->
    TParens (simplify w.value)

  CST.TypeError _ -> TCon "<error>"

extractBinderName :: forall e. CST.TypeVarBinding (CST.Prefixed (CST.Name CST.Ident)) e -> Array String
extractBinderName = case _ of
  CST.TypeVarName (CST.Prefixed { value: CST.Name n }) ->
    let (CST.Ident s) = n.name in [s]
  CST.TypeVarKinded (CST.Wrapped { value: CST.Labeled { label: CST.Prefixed { value: CST.Name n } } }) ->
    let (CST.Ident s) = n.name in [s]

simplifyRow :: forall e. CST.Row e -> { fields :: Array RowField, tail :: Maybe String }
simplifyRow (CST.Row { labels, tail }) =
  { fields: case labels of
      Nothing -> []
      Just (CST.Separated { head, tail: rest }) ->
        Array.cons (simplifyLabel head) (map (simplifyLabel <<< snd) rest)
  , tail: case tail of
      Nothing -> Nothing
      Just (Tuple _ ty) -> case simplify ty of
        TVar v -> Just v
        _ -> Just "..."
  }

simplifyLabel :: forall e. CST.Labeled (CST.Name CST.Label) (CST.Type e) -> RowField
simplifyLabel (CST.Labeled { label: CST.Name n, value }) =
  let (CST.Label l) = n.name
  in { label: l, value: simplify value }

simplifyConstraint :: forall e. CST.Type e -> Constraint
simplifyConstraint = case _ of
  CST.TypeApp head args ->
    case simplify head of
      TCon name -> { className: name, args: map simplify (NEA.toArray args) }
      _ -> { className: "?", args: [] }
  CST.TypeConstructor (CST.QualifiedName q) ->
    let (CST.Proper s) = q.name
    in { className: s, args: [] }
  _ -> { className: "?", args: [] }

-- ============================================================
-- Pure helpers for PureScript consumers
-- ============================================================

-- | Parse a type signature string to RenderType.
-- | Sanitizes loader artifacts (e.g. trailing comma before row variable)
-- | before parsing.
parseToRenderType :: String -> Maybe RenderType
parseToRenderType input = case parseType sanitized of
  ParseSucceeded ty -> Just (simplify (coerceType ty))
  ParseSucceededWithErrors ty _ -> Just (simplify (coerceType ty))
  ParseFailed _ -> Nothing
  where
    -- The Rust loader expands { row } to Record ( row ) and sometimes
    -- produces ", |" (trailing comma before row variable) which is invalid.
    sanitized = replaceAll (Pattern ", |") (Replacement " |") input
    coerceType :: forall e1 e2. CST.Type e1 -> CST.Type e2
    coerceType = unsafeCoerce

-- | Extract constructor argument types from a constructor signature string.
-- | Constructor sigs look like "a -> b -> MyType a b" — returns ["a", "b"] as text.
extractCtorArgs :: String -> Array String
extractCtorArgs sig = case parseToRenderType sig of
  Nothing -> if sig == "" then [] else [sig]
  Just rt -> case collectArrowParams rt of
    [] -> []  -- not a function type = zero-arg constructor
    params -> Array.init params # case _ of
      Just args -> map renderTypeToText args
      Nothing -> []

-- | Extract constructor argument types as RenderType values.
-- | Constructor sigs look like "a -> b -> MyType a b" — returns [TVar "a", TVar "b"].
extractCtorRenderTypes :: String -> Array RenderType
extractCtorRenderTypes sig = case parseToRenderType sig of
  Nothing -> if sig == "" then [] else [TCon sig]
  Just rt -> case collectArrowParams rt of
    [] -> []  -- not a function type = zero-arg constructor
    params -> case Array.init params of
      Just args -> args
      Nothing -> []
