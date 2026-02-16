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
  , sigilBodyTree
  , sigletTree
  , renderSignatureInto
  , renderSigilBodyInto
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
import Hylograph.Sigil.Text (collectTypeVars, collectArrowParams)
import Hylograph.Sigil.Types (RenderType(..), Constraint)

-- =============================================================================
-- Public API
-- =============================================================================

-- | Build a HATS tree for a full-size type signature.
-- | Uses Swiss typographic layout: preamble (∀ left, constraints right),
-- | declaration (name ::), body (indented with left rule).
signatureTree
  :: { name :: String, sig :: String, ast :: RenderType, typeParams :: Array String, className :: Maybe String }
  -> Tree
signatureTree { name, ast, typeParams, className } =
  let
    peeled = peelSignature ast
    allVars = Set.toUnfoldable (collectTypeVars ast) <> peeled.forallVars <> typeParams
    varColors = assignVarColors (Array.nub allVars)
    ctx = { varColors }
    hasPreamble = not (Array.null peeled.forallVars) || not (Array.null peeled.constraints)
  in
  elem Code [staticStr "class" "sig-full"]
    ( (if hasPreamble then [ renderPreamble ctx peeled ] else [])
      <> [ -- Declaration: name ::
           elem Div [staticStr "class" "sig-decl"]
             ([ elem Dfn [staticStr "class" "sig-name", staticStr "textContent" name] [] ]
              <> case className of
                   Just cn -> [ elem Small [staticStr "class" "sig-sep", staticStr "textContent" (" (" <> cn <> ")")] [] ]
                   Nothing -> []
              <> [ elem Small [staticStr "class" "sig-dcolon", staticStr "textContent" " ::"] [] ]
             )
         , -- Body: bare type with left rule
           elem Div [staticStr "class" "sig-body"]
             [ renderTypeTree ctx peeled.body ]
         ]
    )

-- | Build a HATS tree for the sigil body only (no name header, no ::).
-- | Used for inline rendering in value cells when there's enough space.
-- | Still shows preamble (quantifier + constraints) if present.
sigilBodyTree :: { ast :: RenderType } -> Tree
sigilBodyTree { ast } =
  let
    peeled = peelSignature ast
    allVars = Set.toUnfoldable (collectTypeVars ast) <> peeled.forallVars
    varColors = assignVarColors (Array.nub allVars)
    ctx = { varColors }
    hasPreamble = not (Array.null peeled.forallVars) || not (Array.null peeled.constraints)
  in
  elem Code [staticStr "class" "sig-full sig-body-only"]
    ( (if hasPreamble then [ renderPreamble ctx peeled ] else [])
      <> [ elem Div [staticStr "class" "sig-body"]
             [ renderTypeTree ctx peeled.body ]
         ]
    )

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

-- | Render a sigil body (no header) into a container element.
renderSigilBodyInto :: String -> { ast :: RenderType } -> Effect Unit
renderSigilBodyInto selector config = do
  _ <- rerender selector (sigilBodyTree config)
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

-- | Peeled signature: outermost forall/constraints separated from bare body.
type PeeledSig =
  { forallVars :: Array String
  , constraints :: Array Constraint
  , body :: RenderType
  }

-- | Peel outermost forall quantifiers and constraints from the AST.
-- | Nested foralls/constraints (rank-N, inside parens) are left intact.
peelSignature :: RenderType -> PeeledSig
peelSignature (TForall vars inner) =
  let rest = peelSignature inner
  in rest { forallVars = vars <> rest.forallVars }
peelSignature (TConstrained cs inner) =
  let rest = peelSignature inner
  in rest { constraints = cs <> rest.constraints }
peelSignature body =
  { forallVars: [], constraints: [], body }

-- | Render the preamble: quantifier (left) · · · constraints (right).
-- | A dot leader connects quantifier to constraints when both present.
renderPreamble :: TreeCtx -> PeeledSig -> Tree
renderPreamble ctx peeled =
  let
    hasForall = not (Array.null peeled.forallVars)
    hasConstraints = not (Array.null peeled.constraints)
  in
  elem Div [staticStr "class" "sig-preamble"]
    ( (if hasForall
        then [ elem Div [staticStr "class" "sig-quant"]
                 ( [ elem Small [staticStr "class" "sig-quant-sym", staticStr "textContent" "\x2200"] [] ]
                   <> (peeled.forallVars <#> varPill ctx)
                   <> [ elem Small [staticStr "class" "sig-quant-dot", staticStr "textContent" "."] [] ]
                 )
             ]
        else [])
      -- Dot leader between quantifier and constraints
      <> (if hasForall && hasConstraints
           then [ elem Small [staticStr "class" "sig-leader"] [] ]
           else [])
      <> (if hasConstraints
           then [ elem Div [staticStr "class" "sig-ctx"]
                    (peeled.constraints <#> renderCtxItem ctx)
                ]
           else [])
    )

-- | Render a constraint in the preamble (annotation style, not pill).
renderCtxItem :: TreeCtx -> Constraint -> Tree
renderCtxItem ctx c =
  elem Em [staticStr "class" "sig-ctx-item"]
    ( [ elem Code [staticStr "class" "sig-ctx-class", staticStr "textContent" c.className] [] ]
      <> if Array.null c.args then []
         else c.args <#> renderTypeTree ctx
    )

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

-- | Type variable as a <var> element with color via --vc custom property.
-- | CSS context determines rendering: colored text (default) or pill (siglet).
varPill :: TreeCtx -> String -> Tree
varPill ctx name =
  let color = fromMaybe "#0369a1" (Map.lookup name ctx.varColors)
  in elem Var
       [ staticStr "class" "sig-var"
       , staticStr "style" ("--vc:" <> color)
       , staticStr "textContent" name
       ]
       []

-- | Type application: head arg1 arg2
-- | Special-cases Record types to drop the keyword and render as table.
renderAppTree :: TreeCtx -> RenderType -> Array RenderType -> Tree
renderAppTree ctx head args = case head, args of
  -- Record (row) → closed record table, drop "Record" keyword
  TCon "Record", [TRow fields tail] ->
    renderRecordTree ctx fields tail false
  -- Record (+ combination) → row combo table
  TCon "Record", [TParens inner] ->
    let operands = collectPlusOperands inner
    in if Array.length operands > 1
       then renderRowComboTree ctx operands
       else defaultAppTree ctx head args
  -- Default application
  _, _ -> defaultAppTree ctx head args

-- | Default application rendering: HKT gets distinguishing class.
defaultAppTree :: TreeCtx -> RenderType -> Array RenderType -> Tree
defaultAppTree ctx head args =
  let
    isHkt = case head of
      TVar _ -> true
      _ -> false
    inner = [ renderTypeTree ctx head ] <> (args <#> renderAppArgTree ctx)
    cls = if isHkt then "sig-app sig-hkt" else "sig-app"
  in elem Code [staticStr "class" cls] inner

-- | Collect operands from a + operator chain (row combination).
collectPlusOperands :: RenderType -> Array RenderType
collectPlusOperands (TOperator l "+" r) = collectPlusOperands l <> [r]
collectPlusOperands other = [other]

-- | Render row combination as a single-column table of operands.
renderRowComboTree :: TreeCtx -> Array RenderType -> Tree
renderRowComboTree ctx operands =
  let len = Array.length operands
  in elem Ol [staticStr "class" "sig-row-combo"]
       (Array.mapWithIndex (\i op ->
         let isLast = i == len - 1
         in elem Li [staticStr "class" "sig-combo-item"]
              ( [ renderTypeTree ctx op ]
                <> if isLast then []
                   else [ elem Small [staticStr "class" "sig-op", staticStr "textContent" " +"] [] ]
              )
       ) operands)

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

-- | Record or row as a 3-column definition list: name | :: | type.
-- | Row tail uses the middle column for | alignment.
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
        , elem Dd [staticStr "class" "sig-field-sep", staticStr "textContent" "::"] []
        , elem Dd [staticStr "class" "sig-field-type"] [ renderTypeTree ctx f.value ]
        ]
      ) fields
      <> case tail of
           Just v ->
             [ elem Dt [staticStr "class" "sig-field-name"] []
             , elem Dd [staticStr "class" "sig-field-sep", staticStr "textContent" "|"] []
             , elem Dd [staticStr "class" "sig-row-tail"] [ varPill ctx v ]
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
