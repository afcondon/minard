-- | Simplified type AST for rendering
-- |
-- | Converts from PureScript.CST.Types.Type to a rendering-friendly structure,
-- | then exports as plain JavaScript objects for the JS renderer.
module TypeSigViz.TypeAST
  ( parseAndExport
  , JsResult
  ) where

import Prelude
import Prim hiding (Constraint, Row)

import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), snd)
import PureScript.CST (RecoveredParserResult(..), parseType)
import PureScript.CST.Types as CST
import Unsafe.Coerce (unsafeCoerce)

-- ============================================================
-- Simplified AST (internal)
-- ============================================================

data RenderType
  = TVar String
  | TCon String
  | TApp RenderType (Array RenderType)
  | TArrow RenderType RenderType
  | TConstrained (Array Constraint) RenderType
  | TForall (Array String) RenderType
  | TRecord (Array RowField) (Maybe String)
  | TRow (Array RowField) (Maybe String)
  | TParens RenderType
  | TKinded RenderType RenderType
  | TString String
  | TWildcard
  | TOperator RenderType String RenderType

type RowField = { label :: String, value :: RenderType }
type Constraint = { className :: String, args :: Array RenderType }

-- ============================================================
-- CST → RenderType conversion
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
-- Export to JavaScript objects
-- ============================================================

-- | Parse a type signature string and return a JS-friendly object.
-- | Returns { ok: true, ast: ... } or { ok: false, error: "..." }
parseAndExport :: String -> JsResult
parseAndExport input = case parseType input of
  ParseSucceeded ty ->
    mkOk (toJS (simplify (coerceType ty)))
  ParseSucceededWithErrors ty _ ->
    mkOk (toJS (simplify (coerceType ty)))
  ParseFailed _err ->
    mkErr "Parse failed"
  where
    -- Both Void and RecoveredError are fine — simplify doesn't inspect the error parameter
    coerceType :: forall e1 e2. CST.Type e1 -> CST.Type e2
    coerceType = unsafeCoerce

-- JS interop types (opaque foreign)
foreign import data JsObject :: Type
foreign import data JsResult :: Type

foreign import mkOk :: JsObject -> JsResult
foreign import mkErr :: String -> JsResult
foreign import mkObj :: String -> Array { key :: String, value :: JsObject } -> JsObject
foreign import mkStr :: String -> JsObject
foreign import mkArr :: Array JsObject -> JsObject
foreign import mkNull :: JsObject

-- | Convert RenderType to a JavaScript object
toJS :: RenderType -> JsObject
toJS = case _ of
  TVar s -> mkObj "typevar" [kv "name" (mkStr s)]

  TCon s -> mkObj "constructor" [kv "name" (mkStr s)]

  TWildcard -> mkObj "typevar" [kv "name" (mkStr "_")]

  TString s -> mkObj "constructor" [kv "name" (mkStr ("\"" <> s <> "\""))]

  TApp head args ->
    mkObj "applied" [ kv "constructor" (toJS head), kv "args" (mkArr (map toJS args)) ]

  TArrow from to ->
    -- Flatten arrow chain for the renderer
    let parts = collectArrows from to
    in if Array.length parts == 2 then
        mkObj "function"
          [ kv "params" (mkArr (map toJS (Array.take 1 parts)))
          , kv "returnType" (toJS (unsafeLast parts))
          ]
      else
        mkObj "function"
          [ kv "params" (mkArr (map toJS (initSafe parts)))
          , kv "returnType" (toJS (unsafeLast parts))
          ]

  TConstrained constraints body ->
    mkObj "constrained"
      [ kv "constraints" (mkArr (map constraintToJS constraints))
      , kv "body" (toJS body)
      ]

  TForall vars body ->
    mkObj "forall"
      [ kv "vars" (mkArr (map mkStr vars))
      , kv "body" (toJS body)
      ]

  TRecord fields tail ->
    mkObj "record" (fieldProps fields tail)

  TRow fields tail ->
    mkObj "row" (fieldProps fields tail)

  TParens inner ->
    mkObj "parens" [kv "inner" (toJS inner)]

  TKinded ty kind ->
    mkObj "kinded" [kv "type" (toJS ty), kv "kind" (toJS kind)]

  TOperator l op r ->
    mkObj "operator" [kv "left" (toJS l), kv "op" (mkStr op), kv "right" (toJS r)]

fieldProps :: Array RowField -> Maybe String -> Array { key :: String, value :: JsObject }
fieldProps fields tail =
  [ kv "fields" (mkArr (map fieldToJS fields))
  , kv "rowVar" (case tail of
      Just v -> mkStr v
      Nothing -> mkNull)
  ]

fieldToJS :: RowField -> JsObject
fieldToJS f = mkObj "field" [kv "name" (mkStr f.label), kv "type" (toJS f.value)]

constraintToJS :: Constraint -> JsObject
constraintToJS c = mkObj "constraint"
  [ kv "name" (mkStr c.className)
  , kv "args" (mkArr (map toJS c.args))
  ]

kv :: String -> JsObject -> { key :: String, value :: JsObject }
kv k v = { key: k, value: v }

-- | Collect right-nested arrows into a flat list
collectArrows :: RenderType -> RenderType -> Array RenderType
collectArrows from to = case to of
  TArrow from2 to2 -> Array.cons from (collectArrows from2 to2)
  _ -> [from, to]

-- | Unsafe last (we know the array is non-empty)
unsafeLast :: Array RenderType -> RenderType
unsafeLast arr = case Array.last arr of
  Just x -> x
  Nothing -> TCon "?"

-- | Array.init that returns [] for empty
initSafe :: Array RenderType -> Array RenderType
initSafe = Array.init >>> case _ of
  Just a -> a
  Nothing -> []
