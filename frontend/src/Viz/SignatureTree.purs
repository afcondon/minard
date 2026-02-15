-- | HATS Tree rendering of type signatures with semantic HTML.
-- |
-- | Maps PureScript type concepts to semantic HTML elements:
-- |   TVar       → <var>    (mathematical variable)
-- |   TCon       → <code>   (type constructor / identifier)
-- |   TArrow     → <ol>     (ordered parameter list)
-- |   TRecord    → <dl>     (definition list: field → type)
-- |   TForall    → <var>    (quantified variables)
-- |   Constraint → <em>     (qualifying emphasis)
-- |   separators → <small>  (::, →, ., parens — decorative punctuation)
-- |   name       → <dfn>    (term being defined)
-- |
-- | Visual appearance is entirely CSS-driven, enabling alternate renderings
-- | of the same semantic document.
-- |
-- | ADT rail diagrams and ClassDef superclass diagrams still use SVG.
module CE2.Viz.SignatureTree
  ( signatureTree
  , sigletTree
  , renderSignatureInto
  , renderSigletInto
  ) where

import Prelude
import Prim hiding (Constraint, Row)

import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as Set
import Data.String as String
import Effect (Effect)

import Hylograph.HATS (Tree, elem, staticStr)
import Hylograph.HATS.InterpreterTick (rerender)
import Hylograph.Internal.Element.Types (ElementType(..))

import Hylograph.Sigil.Color (assignVarColors, isEffectName)
import Hylograph.Sigil.Text (collectTypeVars, collectArrowParams, collectForallVars)
import Hylograph.Sigil.Types (RenderType(..), Constraint)

-- =============================================================================
-- Public API
-- =============================================================================

-- | Build a HATS tree for a full-size type signature.
signatureTree
  :: { name :: String, sig :: String, ast :: RenderType, typeParams :: Array String, className :: Maybe String }
  -> Tree
signatureTree { name, ast, typeParams, className } =
  let
    allVars = Set.toUnfoldable (collectTypeVars ast) <> collectForallVars ast <> typeParams
    varColors = assignVarColors (Array.nub allVars)
    ctx = { varColors }
  in
  elem Code [staticStr "class" "sig-full"]
    [ -- Header: name [class] [params] ::
      elem Div [staticStr "class" "sig-header"]
        ([ elem Dfn [staticStr "class" "sig-name", staticStr "textContent" name] [] ]
         <> case className of
              Just cn -> [ elem Small [staticStr "class" "sig-sep", staticStr "textContent" (" (" <> cn <> ")")] [] ]
              Nothing -> []
         <> (if Array.null typeParams then []
             else typeParams <#> \v -> varPill ctx v)
         <> [ elem Small [staticStr "class" "sig-dcolon", staticStr "textContent" " ::"] [] ]
        )
    , -- Type body
      elem Div [staticStr "class" "sig-body"]
        [ renderTypeTree ctx ast ]
    ]

-- | Build a HATS tree for a siglet (miniature signature).
sigletTree :: { ast :: RenderType, maxWidth :: Number } -> Tree
sigletTree { ast } =
  let
    allVars = Set.toUnfoldable (collectTypeVars ast)
    varColors = assignVarColors allVars
    ctx = { varColors }
  in
  elem Code [staticStr "class" "sig-siglet"]
    [ renderSigletTypeTree ctx ast ]

-- | Render a full-size signature into a container element.
renderSignatureInto :: String -> { name :: String, sig :: String, ast :: RenderType, typeParams :: Array String, className :: Maybe String } -> Effect Unit
renderSignatureInto selector config = do
  _ <- rerender selector (signatureTree config)
  pure unit

-- | Render a siglet into a container element.
renderSigletInto :: String -> { ast :: RenderType, maxWidth :: Number } -> Effect Unit
renderSigletInto selector config = do
  _ <- rerender selector (sigletTree config)
  pure unit

-- =============================================================================
-- Internal types
-- =============================================================================

type TreeCtx =
  { varColors :: Map String String
  }

-- =============================================================================
-- Full-size type tree builder
-- =============================================================================

renderTypeTree :: TreeCtx -> RenderType -> Tree
renderTypeTree ctx = case _ of
  TVar name ->
    varPill ctx name

  TCon name ->
    if isEffectName name
      then elem Code [staticStr "class" "sig-con sig-con-effect", staticStr "textContent" name] []
      else elem Code [staticStr "class" "sig-con", staticStr "textContent" name] []

  TApp head args ->
    renderAppTree ctx head args

  TArrow from to ->
    renderArrowChainTree ctx (collectArrowParams (TArrow from to))

  TForall vars body ->
    renderForallTree ctx vars body

  TConstrained cs body ->
    elem Div [staticStr "class" "sig-constrained"]
      [ constraintPile ctx cs
      , renderTypeTree ctx body
      ]

  TParens inner ->
    elem Code [staticStr "class" "sig-app"]
      [ elem Small [staticStr "class" "sig-paren", staticStr "textContent" "("] []
      , renderTypeTree ctx inner
      , elem Small [staticStr "class" "sig-paren", staticStr "textContent" ")"] []
      ]

  TRecord fields tail ->
    renderRecordTree ctx fields tail false

  TRow fields tail ->
    renderRecordTree ctx fields tail true

  TOperator l op r ->
    elem Code [staticStr "class" "sig-app"]
      [ renderTypeTree ctx l
      , elem Small [staticStr "class" "sig-op", staticStr "textContent" op] []
      , renderTypeTree ctx r
      ]

  TKinded ty kind ->
    elem Code [staticStr "class" "sig-app"]
      [ renderTypeTree ctx ty
      , elem Small [staticStr "class" "sig-sep", staticStr "textContent" " :: "] []
      , renderTypeTree ctx kind
      ]

  TString s ->
    elem Code [staticStr "class" "sig-string", staticStr "textContent" ("\"" <> s <> "\"")] []

  TWildcard ->
    elem Var [staticStr "class" "sig-var sig-wildcard", staticStr "textContent" "_"] []

-- | Type variable as a <var> element with colored background.
varPill :: TreeCtx -> String -> Tree
varPill ctx name =
  let color = fromMaybe "#0369a1" (Map.lookup name ctx.varColors)
  in elem Var
       [ staticStr "class" "sig-var"
       , staticStr "style" ("background:" <> color)
       , staticStr "textContent" name
       ]
       []

-- | Type application: head arg1 arg2
-- | HKT applications (where head is a TVar) get a distinguishing class.
renderAppTree :: TreeCtx -> RenderType -> Array RenderType -> Tree
renderAppTree ctx head args =
  let
    isHkt = case head of
      TVar _ -> true
      _ -> false
    inner = [ renderTypeTree ctx head ] <> (args <#> renderAppArgTree ctx)
    cls = if isHkt then "sig-app sig-hkt" else "sig-app"
  in elem Code [staticStr "class" cls] inner

-- | Wrap complex args in parens in application context.
renderAppArgTree :: TreeCtx -> RenderType -> Tree
renderAppArgTree ctx t = case t of
  TApp _ _ -> wrapParensTree ctx t
  TArrow _ _ -> wrapParensTree ctx t
  TConstrained _ _ -> wrapParensTree ctx t
  TForall _ _ -> wrapParensTree ctx t
  TOperator _ _ _ -> wrapParensTree ctx t
  _ -> renderTypeTree ctx t

wrapParensTree :: TreeCtx -> RenderType -> Tree
wrapParensTree ctx t =
  elem Code [staticStr "class" "sig-app"]
    [ elem Small [staticStr "class" "sig-paren", staticStr "textContent" "("] []
    , renderTypeTree ctx t
    , elem Small [staticStr "class" "sig-paren", staticStr "textContent" ")"] []
    ]

-- | Arrow chain as an ordered list of parameter types.
-- | The → separators are added via CSS (li + li::before).
renderArrowChainTree :: TreeCtx -> Array RenderType -> Tree
renderArrowChainTree ctx params =
  elem Ol [staticStr "class" "sig-arrow-chain"]
    (params <#> \p ->
      elem Li [staticStr "class" "sig-param"] [ renderTypeTree ctx p ]
    )

-- | Forall: ∀ vars. body
renderForallTree :: TreeCtx -> Array String -> RenderType -> Tree
renderForallTree ctx vars body =
  elem Div [staticStr "class" "sig-forall"]
    [ elem Div [staticStr "class" "sig-forall-row"]
        ([ elem Small [staticStr "class" "sig-forall-symbol", staticStr "textContent" "\x2200"] [] ]
         <> (vars <#> varPill ctx)
         <> [ elem Small [staticStr "class" "sig-forall-dot", staticStr "textContent" "."] [] ]
        )
    , renderTypeTree ctx body
    ]

-- | Constraint pile: stack of emphasized constraint pills.
constraintPile :: TreeCtx -> Array Constraint -> Tree
constraintPile ctx cs =
  elem Div [staticStr "class" "sig-constraints"]
    (cs <#> constraintPillTree ctx)

constraintPillTree :: TreeCtx -> Constraint -> Tree
constraintPillTree ctx c =
  elem Em [staticStr "class" "sig-constraint"]
    ([ elem Code [staticStr "class" "sig-con", staticStr "textContent" c.className] [] ]
     <> if Array.null c.args then []
        else c.args <#> renderTypeTree ctx
    )

-- | Record or row as a definition list: field name → field type.
renderRecordTree :: TreeCtx -> Array { label :: String, value :: RenderType } -> Maybe String -> Boolean -> Tree
renderRecordTree ctx fields tail isRow =
  let
    openClass = case tail of
      Just _ -> "sig-record sig-record-open"
      Nothing -> "sig-record"
  in
  if Array.null fields && tail == Nothing then
    elem Code [staticStr "class" "sig-con", staticStr "textContent" (if isRow then "()" else "{}")] []
  else
    elem Dl [staticStr "class" openClass]
      (Array.concatMap (\f ->
        [ elem Dt [staticStr "class" "sig-field-name", staticStr "textContent" f.label] []
        , elem Dd [staticStr "class" "sig-field-type"] [ renderTypeTree ctx f.value ]
        ]
      ) fields
      <> case tail of
           Just v ->
             [ elem Dd [staticStr "class" "sig-row-tail"]
                 [ elem Small [staticStr "class" "sig-paren", staticStr "textContent" "| "] []
                 , varPill ctx v
                 ]
             ]
           Nothing -> []
      )

-- =============================================================================
-- Siglet (miniature) type tree builder
-- =============================================================================

-- | Siglet rendering: TCon → dot, multi-letter TVar → dot, otherwise recurse.
renderSigletTypeTree :: TreeCtx -> RenderType -> Tree
renderSigletTypeTree ctx = case _ of
  TVar name ->
    if String.length name > 1
      then -- Multi-letter var → colored dot
        let color = fromMaybe "#0369a1" (Map.lookup name ctx.varColors)
        in elem Var [staticStr "class" "sig-dot sig-dot-var", staticStr "style" ("background:" <> color)] []
      else -- Single-letter var → small pill
        varPill ctx name

  TCon name ->
    if isEffectName name
      then elem Small [staticStr "class" "sig-dot sig-dot-effect"] []
      else elem Small [staticStr "class" "sig-dot sig-dot-con"] []

  TApp head args ->
    let
      isHkt = case head of
        TVar _ -> true
        _ -> false
      inner = [ renderSigletTypeTree ctx head ] <> (args <#> renderSigletTypeTree ctx)
      cls = if isHkt then "sig-app sig-hkt" else "sig-app"
    in elem Code [staticStr "class" cls] inner

  TArrow from to ->
    let params = collectArrowParams (TArrow from to)
    in elem Ol [staticStr "class" "sig-arrow-chain"]
         (params <#> \p ->
           elem Li [staticStr "class" "sig-param"] [ renderSigletTypeTree ctx p ]
         )

  TForall _ body ->
    renderSigletTypeTree ctx body

  TConstrained _ body ->
    renderSigletTypeTree ctx body

  TParens inner ->
    elem Code [staticStr "class" "sig-app"]
      [ elem Small [staticStr "class" "sig-paren", staticStr "textContent" "("] []
      , renderSigletTypeTree ctx inner
      , elem Small [staticStr "class" "sig-paren", staticStr "textContent" ")"] []
      ]

  TRecord _ _ ->
    elem Small [staticStr "class" "sig-dot sig-dot-con"] []

  TRow _ _ ->
    elem Small [staticStr "class" "sig-dot sig-dot-con"] []

  TOperator l op r ->
    elem Code [staticStr "class" "sig-app"]
      [ renderSigletTypeTree ctx l
      , elem Small [staticStr "class" "sig-op", staticStr "textContent" op] []
      , renderSigletTypeTree ctx r
      ]

  TKinded ty _ ->
    renderSigletTypeTree ctx ty

  TString _ ->
    elem Code [staticStr "class" "sig-string", staticStr "textContent" "\"\""] []

  TWildcard ->
    elem Var [staticStr "class" "sig-dot sig-dot-var sig-wildcard"] []
