-- | PureScript Source Code Tokenizer + Halogen HTML Renderer
-- |
-- | Uses `language-cst-parser` to tokenize PureScript source text into a
-- | typed token stream, then renders it as Halogen HTML with CSS classes
-- | for syntax highlighting and clickable identifiers.
module CE2.Viz.SourceCode
  ( KnownDeclaration
  , renderSource
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Map (Map)
import Data.Map as Map
import Data.String as String
import Data.String.CodeUnits as SCU
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import PureScript.CST.Lexer (lexModule)
import PureScript.CST.TokenStream (TokenStream, TokenStep(..), step)
import PureScript.CST.Types (SourceToken, Token(..))

-- =============================================================================
-- Types
-- =============================================================================

-- | A declaration known to the codebase, for cross-reference resolution
type KnownDeclaration =
  { name :: String
  , moduleName :: String
  , packageName :: String
  , kind :: String
  }

-- | A token annotation positioned on a specific line
type LineAnnotation =
  { startCol :: Int    -- 0-indexed column in the line
  , endCol :: Int      -- 0-indexed end column (exclusive)
  , cssClass :: String
  , clickTarget :: Maybe { packageName :: String, moduleName :: String, declName :: String }
  }

-- =============================================================================
-- Public API
-- =============================================================================

-- | Render full module source as syntax-highlighted Halogen HTML.
-- |
-- | Arguments:
-- | - `source`: full module source text
-- | - `knownDecls`: cross-reference index of all known declarations
-- | - `focusRange`: optional line range (1-indexed start, 1-indexed end) for the focused declaration
-- | - `focusKind`: declaration kind string (for accent color)
-- | - `onIdentClick`: callback when a known identifier is clicked (pkgName, modName, declName)
renderSource
  :: forall action slots m
   . String
  -> Array KnownDeclaration
  -> Maybe { startLine :: Int, endLine :: Int }
  -> String
  -> (String -> String -> String -> action)
  -> Array (HH.ComponentHTML action slots m)
renderSource source knownDecls mFocusRange focusKind onIdentClick =
  let
    sourceLines = String.split (String.Pattern "\n") source
    lineCount = Array.length sourceLines
    tokens = collectTokens (lexModule source)
    knownDeclLookup = buildKnownDeclLookup knownDecls
    annotationsByLine = buildAnnotationsByLine tokens knownDeclLookup
    gutterWidth = if lineCount >= 1000 then "4.5em" else if lineCount >= 100 then "3.5em" else "2.5em"
  in
    [ HH.div
        [ HP.class_ (HH.ClassName "ps-source")
        ]
        (Array.mapWithIndex (renderLine annotationsByLine mFocusRange focusKind onIdentClick gutterWidth) sourceLines)
    ]

-- =============================================================================
-- Tokenization
-- =============================================================================

-- | Walk the token stream, collecting all non-layout tokens
collectTokens :: TokenStream -> Array SourceToken
collectTokens stream = go stream []
  where
  go :: TokenStream -> Array SourceToken -> Array SourceToken
  go s acc = case step s of
    TokenEOF _ _ -> acc
    TokenError _ _ _ _ -> acc
    TokenCons tok _ next _ ->
      if isLayoutToken tok.value
        then go next acc
        else go next (Array.snoc acc tok)

-- | Layout tokens are synthetic and don't correspond to source text
isLayoutToken :: Token -> Boolean
isLayoutToken = case _ of
  TokLayoutStart _ -> true
  TokLayoutSep _ -> true
  TokLayoutEnd _ -> true
  _ -> false

-- =============================================================================
-- Token Annotation Building
-- =============================================================================

-- | Set of known declaration names for quick lookup
type KnownDeclLookup =
  { allNames :: Set String
  , typeNames :: Set String
  , declsByName :: Map String KnownDeclaration
  }

buildKnownDeclLookup :: Array KnownDeclaration -> KnownDeclLookup
buildKnownDeclLookup decls =
  { allNames: Set.fromFoldable (map _.name decls)
  , typeNames: Set.fromFoldable $
      Array.mapMaybe (\d -> if isTypeLike d.kind then Just d.name else Nothing) decls
  , declsByName: Map.fromFoldable $ map (\d -> Tuple d.name d) decls
  }
  where
  isTypeLike k = k == "data" || k == "newtype" || k == "type_synonym" || k == "type_class"

-- | Build a Map from line number (0-indexed) to sorted annotations for that line
buildAnnotationsByLine :: Array SourceToken -> KnownDeclLookup -> Map Int (Array LineAnnotation)
buildAnnotationsByLine tokens knownDecls =
  let
    annotations = Array.concatMap (tokenToAnnotations knownDecls) tokens
    -- Group by line
    grouped = Array.foldl (\acc ann ->
        Map.alter (Just <<< Array.cons ann <<< fromMaybeEmpty) ann.line acc
      ) Map.empty annotations
  in
    -- Sort each line's annotations by startCol, strip `line` field
    map (map stripLine <<< Array.sortWith _.startCol) grouped
  where
  stripLine :: AnnotationWithLine -> LineAnnotation
  stripLine a = { startCol: a.startCol, endCol: a.endCol, cssClass: a.cssClass, clickTarget: a.clickTarget }

  fromMaybeEmpty :: Maybe (Array AnnotationWithLine) -> Array AnnotationWithLine
  fromMaybeEmpty (Just arr) = arr
  fromMaybeEmpty Nothing = []

type AnnotationWithLine =
  { line :: Int
  , startCol :: Int
  , endCol :: Int
  , cssClass :: String
  , clickTarget :: Maybe { packageName :: String, moduleName :: String, declName :: String }
  }

-- | Convert a source token to line annotations
-- | For single-line tokens: one annotation on the token's line
-- | For multi-line tokens: annotations on first line only (pragmatic v1)
tokenToAnnotations :: KnownDeclLookup -> SourceToken -> Array AnnotationWithLine
tokenToAnnotations knownDecls tok =
  let
    cssClass = tokenCssClass tok.value
    clickTarget = tokenClickTarget knownDecls tok.value
    line = tok.range.start.line
    startCol = tok.range.start.column
    endCol = tok.range.end.column
  in
    -- Only annotate single-line tokens for v1
    if tok.range.start.line == tok.range.end.line
      then [ { line, startCol, endCol, cssClass, clickTarget } ]
      else []

-- =============================================================================
-- Token Classification
-- =============================================================================

-- | PureScript keywords (lowercase names that are actually keywords)
psKeywords :: Set String
psKeywords = Set.fromFoldable
  [ "module", "where", "import", "do", "let", "in", "case", "of"
  , "if", "then", "else", "data", "type", "class", "instance"
  , "derive", "newtype", "foreign", "infixl", "infixr", "infix"
  , "as", "hiding", "ado"
  ]

-- | Get CSS class for a token
tokenCssClass :: Token -> String
tokenCssClass = case _ of
  TokLowerName Nothing name
    | Set.member name psKeywords -> "ps-keyword"
  TokLowerName (Just _) _ -> "ps-ident"
  TokLowerName Nothing _ -> "ps-ident"
  TokUpperName (Just _) _ -> "ps-type"
  TokUpperName Nothing _ -> "ps-type"
  TokOperator _ _ -> "ps-operator"
  TokSymbolName _ _ -> "ps-operator"
  TokSymbolArrow _ -> "ps-operator"
  TokForall _ -> "ps-keyword"
  TokDoubleColon _ -> "ps-keyword"
  TokRightArrow _ -> "ps-keyword"
  TokLeftArrow _ -> "ps-keyword"
  TokRightFatArrow _ -> "ps-keyword"
  TokEquals -> "ps-keyword"
  TokBackslash -> "ps-keyword"
  TokString _ _ -> "ps-string"
  TokRawString _ -> "ps-string"
  TokChar _ _ -> "ps-string"
  TokInt _ _ -> "ps-number"
  TokNumber _ _ -> "ps-number"
  TokHole _ -> "ps-ident"
  TokPipe -> "ps-keyword"
  _ -> "ps-punct"

-- | Determine if a token should be clickable (links to a known declaration)
tokenClickTarget :: KnownDeclLookup -> Token -> Maybe { packageName :: String, moduleName :: String, declName :: String }
tokenClickTarget knownDecls = case _ of
  TokUpperName Nothing name
    | Set.member name knownDecls.typeNames ->
        case Map.lookup name knownDecls.declsByName of
          Just d -> Just { packageName: d.packageName, moduleName: d.moduleName, declName: name }
          Nothing -> Nothing
  TokLowerName Nothing name
    | not (Set.member name psKeywords) && Set.member name knownDecls.allNames ->
        case Map.lookup name knownDecls.declsByName of
          Just d -> Just { packageName: d.packageName, moduleName: d.moduleName, declName: name }
          Nothing -> Nothing
  _ -> Nothing

-- =============================================================================
-- Line Rendering
-- =============================================================================

-- | Render a single source line with token-based syntax highlighting
renderLine
  :: forall action slots m
   . Map Int (Array LineAnnotation)
  -> Maybe { startLine :: Int, endLine :: Int }
  -> String
  -> (String -> String -> String -> action)
  -> String
  -> Int    -- 0-indexed line index
  -> String -- line text
  -> HH.ComponentHTML action slots m
renderLine annotationsByLine mFocusRange focusKind onIdentClick gutterWidth lineIdx lineText =
  let
    lineNum = lineIdx + 1  -- 1-indexed for display
    isFocused = case mFocusRange of
      Just { startLine, endLine } -> lineNum >= startLine && lineNum <= endLine
      Nothing -> false
    annotations = case Map.lookup lineIdx annotationsByLine of
      Just anns -> anns
      Nothing -> []
    -- Check for line comments in the gap text
    codeSegments = buildLineSegments lineText annotations
  in
    HH.div
      [ HP.classes $
          [ HH.ClassName "ps-line" ]
          <> (if isFocused then [ HH.ClassName "ps-focused" ] else [])
      ]
      ( (if isFocused
          then [ HH.span
                   [ HP.class_ (HH.ClassName "ps-focused-accent")
                   , HP.style $ "border-left-color: " <> kindColor focusKind <> ";"
                   ]
                   []
               ]
          else [])
        <>
        [ HH.span
            [ HP.class_ (HH.ClassName "ps-linenum")
            , HP.style $ "width: " <> gutterWidth <> ";"
            ]
            [ HH.text (show lineNum) ]
        , HH.span
            [ HP.class_ (HH.ClassName "ps-code") ]
            (renderSegments onIdentClick codeSegments)
        ]
      )

-- | A segment of a line: either plain text or an annotated span
data LineSegment
  = PlainText String
  | AnnotatedText String String (Maybe { packageName :: String, moduleName :: String, declName :: String })

-- | Build segments for a line from the line text and sorted annotations
buildLineSegments :: String -> Array LineAnnotation -> Array LineSegment
buildLineSegments lineText annotations =
  go 0 (Array.toUnfoldable annotations)
  where
  lineLen = SCU.length lineText

  go :: Int -> Array LineAnnotation -> Array LineSegment
  go pos anns
    | pos >= lineLen =
        -- Check if remaining text could be a comment in the gap
        []
    | otherwise =
        case Array.uncons anns of
          Nothing ->
            -- No more annotations; emit remaining text
            let remaining = SCU.drop pos lineText
            in if remaining == "" then []
               else [ classifyGapText remaining ]
          Just { head: ann, tail: rest } ->
            if ann.startCol > pos then
              -- Gap before next annotation
              let gapText = SCU.slice pos ann.startCol lineText
                  gapSegment = classifyGapText gapText
                  -- Safely extract annotated text
                  annotatedText = SCU.slice ann.startCol ann.endCol lineText
                  annotatedSegment = AnnotatedText annotatedText ann.cssClass ann.clickTarget
              in [ gapSegment, annotatedSegment ] <> go ann.endCol rest
            else if ann.startCol == pos then
              -- Annotation starts at current position
              let annotatedText = SCU.slice ann.startCol ann.endCol lineText
                  annotatedSegment = AnnotatedText annotatedText ann.cssClass ann.clickTarget
              in [ annotatedSegment ] <> go ann.endCol rest
            else
              -- Skip annotation that starts before current position (shouldn't happen with sorted anns)
              go pos rest

-- | Classify gap text: detect line comments
classifyGapText :: String -> LineSegment
classifyGapText text =
  let trimmed = String.trim text
  in if SCU.take 2 trimmed == "--"
     then AnnotatedText text "ps-comment" Nothing
     else PlainText text

-- | Render line segments to Halogen HTML
renderSegments
  :: forall action slots m
   . (String -> String -> String -> action)
  -> Array LineSegment
  -> Array (HH.ComponentHTML action slots m)
renderSegments onIdentClick segments =
  map (renderSegment onIdentClick) segments

renderSegment
  :: forall action slots m
   . (String -> String -> String -> action)
  -> LineSegment
  -> HH.ComponentHTML action slots m
renderSegment _onIdentClick = case _ of
  PlainText text -> HH.text text
  AnnotatedText text cssClass Nothing ->
    HH.span
      [ HP.class_ (HH.ClassName cssClass) ]
      [ HH.text text ]
  AnnotatedText text cssClass (Just target) ->
    HH.a
      [ HP.classes [ HH.ClassName cssClass, HH.ClassName "ps-clickable" ]
      , HP.title $ target.moduleName <> "." <> target.declName
      , HE.onClick \_ -> _onIdentClick target.packageName target.moduleName target.declName
      ]
      [ HH.text text ]

-- =============================================================================
-- Utilities
-- =============================================================================

-- | Kind color mapping (matches ModuleTreemapEnriched.kindColor)
kindColor :: String -> String
kindColor = case _ of
  "value"        -> "#4e79a7"
  "data"         -> "#59a14f"
  "newtype"      -> "#76b7b2"
  "type_class"   -> "#f28e2b"
  "type_synonym" -> "#edc948"
  "foreign"      -> "#e15759"
  "alias"        -> "#b07aa1"
  _              -> "#999"
