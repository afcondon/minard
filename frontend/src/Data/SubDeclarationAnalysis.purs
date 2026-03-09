-- | Sub-Declaration Analysis
-- |
-- | Extracts case branches from large pattern-match functions (like Halogen's handleAction)
-- | and analyzes their internal structure: state field reads/writes, identifier references,
-- | action dispatches. Groups branches into concern groups by shared state field usage.
-- |
-- | This operates on source text rather than CST for pragmatism — PureScript's case-of
-- | expressions have very regular formatting.
module CE2.Data.SubDeclarationAnalysis
  ( BranchInfo
  , ConcernGroup
  , SubDeclAnalysis
  , CaseExprInfo
  , analyzeModuleSource
  , branchesToDeclGraph
  ) where

import Prelude

import Data.Array as Array
import Data.Array (mapWithIndex, sortBy)
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Set as Set
import Data.String as String
import Data.String.CodeUnits as SCU
import Data.String.Pattern (Pattern(..))
import Data.String.Regex (regex, match)
import Data.String.Regex.Flags (global, noFlags)
import Data.Tuple (Tuple(..))

-- =============================================================================
-- Types
-- =============================================================================

-- | Information about a single case branch
type BranchInfo =
  { name :: String              -- Constructor name from pattern (e.g., "Initialize", "NavigateTo")
  , fullPattern :: String       -- Full pattern text (e.g., "NavigateTo scene")
  , lineStart :: Int            -- 1-indexed line number
  , lineEnd :: Int              -- 1-indexed line number (exclusive)
  , stateReads :: Set.Set String    -- State fields read (state.foo)
  , stateWrites :: Set.Set String   -- State fields written (_ { foo = ... })
  , actionDispatches :: Set.Set String  -- Other action constructors dispatched
  , identifierRefs :: Set.Set String    -- All identifier references
  , lineCount :: Int            -- Number of source lines
  }

-- | A concern group: a set of branches that share state fields
type ConcernGroup =
  { name :: String              -- Auto-generated group name
  , branches :: Set.Set String  -- Branch names in this group
  , sharedFields :: Set.Set String  -- State fields that bind this group
  , allFields :: Set.Set String     -- All state fields touched by any branch in group
  }

-- | Info about a case expression found in the source
type CaseExprInfo =
  { functionName :: String      -- Top-level function containing the case
  , lineStart :: Int
  , branches :: Array BranchInfo
  }

-- | Full analysis result
type SubDeclAnalysis =
  { caseExpressions :: Array CaseExprInfo
  , concernGroups :: Array ConcernGroup
  , allBranches :: Array BranchInfo
  , allStateFields :: Set.Set String
  }

-- =============================================================================
-- Main entry point
-- =============================================================================

-- | Analyze module source text to extract case branches and concern groups.
-- | Focuses on the significant case expressions (5+ branches) to avoid noise
-- | from small pattern matches like `case maybeX of Just x -> ...; Nothing -> ...`
analyzeModuleSource :: String -> SubDeclAnalysis
analyzeModuleSource source =
  let
    srcLines = String.split (Pattern "\n") source
    allCaseExprs = findCaseExpressions srcLines
    -- Focus on significant case expressions only (5+ branches)
    -- Sort by branch count descending so the main handleAction is first
    significantExprs = sortBy (\a b -> compare (Array.length b.branches) (Array.length a.branches)) $
      Array.filter (\ce -> Array.length ce.branches >= 5) allCaseExprs
    allBranches = Array.concatMap _.branches significantExprs
    allFields = foldl (\acc b -> Set.union acc (Set.union b.stateReads b.stateWrites)) Set.empty allBranches
    groups = computeConcernGroups allBranches
  in
    { caseExpressions: significantExprs
    , concernGroups: groups
    , allBranches
    , allStateFields: allFields
    }

-- =============================================================================
-- Case expression finding
-- =============================================================================

-- | Find all case expressions in the source
findCaseExpressions :: Array String -> Array CaseExprInfo
findCaseExpressions srcLines =
  let
    indexed = mapWithIndex Tuple srcLines
    -- Find lines containing "case _ of" or "case \w+ of" that start a block
    caseLines = Array.filter (\(Tuple _i line) -> isCaseOfLine line) indexed
  in
    Array.mapMaybe (\(Tuple lineIdx _line) ->
      -- Look for the function name: scan backwards for a declaration
      let
        funcName = findFunctionName srcLines lineIdx
        branches = extractBranches srcLines (lineIdx + 1)
        -- Prefix branch names with function name to avoid collisions
        prefixedBranches = branches <#> \b -> b { name = funcName <> "/" <> b.name }
      in
        if Array.length prefixedBranches >= 3  -- Only interesting if 3+ branches
        then Just { functionName: funcName, lineStart: lineIdx + 1, branches: prefixedBranches }
        else Nothing
    ) caseLines

-- | Check if a line contains a case-of expression
isCaseOfLine :: String -> Boolean
isCaseOfLine line =
  let trimmed = String.trim line
  in (String.contains (Pattern "case _ of") trimmed
      || String.contains (Pattern "= case _ of") trimmed
      || matchesCasePattern trimmed)

-- | Check for "case <expr> of" pattern
matchesCasePattern :: String -> Boolean
matchesCasePattern line =
  case regex "\\bcase\\b.*\\bof\\s*$" noFlags of
    Left _ -> false
    Right r -> isJust (match r line)

-- | Scan backwards from a case-of line to find the function name
findFunctionName :: Array String -> Int -> String
findFunctionName srcLines lineIdx =
  let
    go i
      | i < 0 = "unknown"
      | otherwise =
        case srcLines `Array.index` i of
          Nothing -> "unknown"
          Just line ->
            case extractDeclName (String.trim line) of
              Just name -> name
              Nothing -> go (i - 1)
  in go lineIdx
  where
  -- Simple pattern: "functionName" at start of line, followed by stuff
  extractDeclName :: String -> Maybe String
  extractDeclName line =
    case regex "^([a-z][a-zA-Z0-9_']*)\\s" noFlags of
      Left _ -> Nothing
      Right r -> case match r line of
        Just groups -> (NEA.toArray groups) `Array.index` 1 >>= identity
        Nothing -> Nothing


-- =============================================================================
-- Branch extraction
-- =============================================================================

-- | Extract case branches starting from the line after "case _ of"
extractBranches :: Array String -> Int -> Array BranchInfo
extractBranches srcLines startLine =
  let
    -- Determine the indentation level of the first branch
    branchIndent = findBranchIndent srcLines startLine
  in case branchIndent of
    Nothing -> []
    Just indent ->
      let
        -- Scan forward collecting branches
        branches = collectBranches srcLines startLine indent
      in map (analyzeBranch srcLines) branches

-- | Find the indentation of the first case branch
findBranchIndent :: Array String -> Int -> Maybe Int
findBranchIndent srcLines startLine =
  let
    go i
      | i >= Array.length srcLines = Nothing
      | otherwise = case srcLines `Array.index` i of
          Nothing -> Nothing
          Just line ->
            let trimmed = String.trim line
            in if String.length trimmed > 0 && not (String.contains (Pattern "--") (String.take 2 trimmed))
               then Just (leadingSpaces line)
               else go (i + 1)
  in go startLine

-- | Count leading spaces
leadingSpaces :: String -> Int
leadingSpaces line =
  SCU.length line - SCU.length (stripLeadingSpaces line)
  where
  stripLeadingSpaces s =
    case SCU.uncons s of
      Just { head: ' ', tail } -> stripLeadingSpaces tail
      _ -> s

-- | Collect branch boundaries
collectBranches :: Array String -> Int -> Int -> Array { name :: String, fullPattern :: String, lineStart :: Int, lineEnd :: Int }
collectBranches srcLines startLine indent =
  let
    nLines = Array.length srcLines
    go i acc currentBranch =
      if i >= nLines then
        -- Close last branch
        case currentBranch of
          Nothing -> acc
          Just b -> Array.snoc acc (b { lineEnd = i })
      else
        case srcLines `Array.index` i of
          Nothing -> acc
          Just line ->
            let spaces = leadingSpaces line
                trimmed = String.trim line
                isEmpty = String.length trimmed == 0
                isComment = String.contains (Pattern "--") (String.take 2 trimmed)
            in
              if isEmpty || isComment then
                go (i + 1) acc currentBranch
              else if spaces == indent && looksLikeBranchStart trimmed then
                -- New branch starts
                let
                  acc' = case currentBranch of
                    Nothing -> acc
                    Just b -> Array.snoc acc (b { lineEnd = i })
                  branchName = extractBranchName trimmed
                  branchPattern = extractBranchPattern trimmed
                in go (i + 1) acc' (Just { name: branchName, fullPattern: branchPattern, lineStart: i + 1, lineEnd: 0 })
              else if spaces < indent && not isEmpty then
                -- Dedent = end of case expression
                case currentBranch of
                  Nothing -> acc
                  Just b -> Array.snoc acc (b { lineEnd = i })
              else
                go (i + 1) acc currentBranch
  in go startLine [] Nothing

-- | Check if a line looks like the start of a case branch
looksLikeBranchStart :: String -> Boolean
looksLikeBranchStart line =
  -- Starts with a capital letter (constructor) or underscore (wildcard)
  case SCU.charAt 0 line of
    Just c -> (c >= 'A' && c <= 'Z') || c == '_'
    Nothing -> false

-- | Extract the constructor name from a branch pattern
extractBranchName :: String -> String
extractBranchName line =
  -- Take the first word (constructor name)
  fromMaybe "unknown" $ Array.head $ String.split (Pattern " ") $
    -- Remove leading constructor from pattern like "Just x ->"
    String.trim line

-- | Extract the full pattern (everything before ->)
extractBranchPattern :: String -> String
extractBranchPattern line =
  case String.indexOf (Pattern "->") line of
    Just idx -> String.trim $ String.take idx line
    Nothing -> String.trim line

-- =============================================================================
-- Branch analysis
-- =============================================================================

-- | Analyze a single branch for state reads, writes, and identifier references
analyzeBranch :: Array String -> { name :: String, fullPattern :: String, lineStart :: Int, lineEnd :: Int } -> BranchInfo
analyzeBranch srcLines branch =
  let
    -- Extract source text for this branch
    branchLines = Array.slice (branch.lineStart - 1) branch.lineEnd srcLines
    branchText = String.joinWith "\n" branchLines

    stateReads = extractStateReads branchText
    stateWrites = extractStateWrites branchText
    actionDispatches = extractActionDispatches branchText
    identifierRefs = extractIdentifiers branchText
  in
    { name: branch.name
    , fullPattern: branch.fullPattern
    , lineStart: branch.lineStart
    , lineEnd: branch.lineEnd
    , stateReads
    , stateWrites
    , actionDispatches
    , identifierRefs
    , lineCount: branch.lineEnd - branch.lineStart + 1
    }

-- | Extract state field reads: patterns like "state.fieldName" or "st.fieldName"
extractStateReads :: String -> Set.Set String
extractStateReads text =
  case regex "(?:state|st|s)\\.(\\w+)" global of
    Left _ -> Set.empty
    Right r -> case match r text of
      Nothing -> Set.empty
      -- Global match returns all matches but not capture groups easily
      -- Use a simpler approach: split and scan
      Just _ -> extractFieldsFromDots text

-- | Extract field names from dot access patterns
extractFieldsFromDots :: String -> Set.Set String
extractFieldsFromDots text =
  let
    -- Find all occurrences of "state." / "st." / "s." followed by a word
    parts = String.split (Pattern "state.") text
    stParts = String.split (Pattern "st.") text
    -- Extract the word after the dot
    extractWord s = case regex "^(\\w+)" noFlags of
      Left _ -> Nothing
      Right r -> case match r s of
        Just groups -> (NEA.toArray groups) `Array.index` 1 >>= identity
        Nothing -> Nothing
    fromParts ps = Set.fromFoldable $ Array.mapMaybe extractWord (Array.drop 1 ps)
  in Set.union (fromParts parts) (fromParts stParts)

-- | Extract state field writes: patterns like "_ { fieldName =" or "_ { fieldName:"
extractStateWrites :: String -> Set.Set String
extractStateWrites text =
  case regex "\\{[^}]*" global of
    Left _ -> Set.empty
    Right _ ->
      let
        -- Find record update patterns: word followed by = or :
        -- Split by { and look for "field =" patterns
        braceChunks = Array.drop 1 $ String.split (Pattern "{") text
        extractFields chunk =
          let
            -- Find "fieldName =" or "fieldName:" patterns
            pairs = String.split (Pattern ",") chunk
          in Array.mapMaybe extractFieldName pairs
        extractFieldName pair =
          let trimmed = String.trim pair
          in case regex "^(\\w+)\\s*[=:]" noFlags of
              Left _ -> Nothing
              Right r -> case match r trimmed of
                Just groups -> (NEA.toArray groups) `Array.index` 1 >>= identity
                Nothing -> Nothing
      in Set.fromFoldable $ Array.concatMap extractFields braceChunks

-- | Extract action dispatches: constructor names that look like actions
extractActionDispatches :: String -> Set.Set String
extractActionDispatches text =
  -- Look for patterns like "handleAction SomeAction" or "NavigateTo" as standalone
  case regex "\\bhandleAction\\s+\\(?\\s*([A-Z]\\w+)" global of
    Left _ -> Set.empty
    Right r -> case match r text of
      Nothing -> Set.empty
      Just _ ->
        -- Simpler: find "handleAction" followed by constructor
        let parts = Array.drop 1 $ String.split (Pattern "handleAction") text
            extractAction s =
              let trimmed = String.trim s
              in case regex "^\\(?\\s*([A-Z]\\w+)" noFlags of
                  Left _ -> Nothing
                  Right r2 -> case match r2 trimmed of
                    Just groups -> (NEA.toArray groups) `Array.index` 1 >>= identity
                    Nothing -> Nothing
        in Set.fromFoldable $ Array.mapMaybe extractAction parts

-- | Extract all identifier references from branch text
extractIdentifiers :: String -> Set.Set String
extractIdentifiers text =
  -- Tokenize by splitting on non-alphanumeric characters
  let
    -- Simple word extraction
    words = Array.filter (\w -> String.length w > 0) $
      String.split (Pattern " ") $
        replaceNonAlpha text
    identifiers = Array.filter isIdentifier words
  in Set.fromFoldable identifiers
  where
  isIdentifier w =
    case SCU.charAt 0 w of
      Just c -> (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
      Nothing -> false
  replaceNonAlpha s = SCU.fromCharArray $ (\c -> if isAlphaNum c then c else ' ') `map` SCU.toCharArray s
  isAlphaNum c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c == '_' || c == '\''

-- =============================================================================
-- Concern group computation
-- =============================================================================

-- | Compute concern groups from branch state field usage
-- | Uses union-find on the bipartite graph of branches ↔ state fields
computeConcernGroups :: Array BranchInfo -> Array ConcernGroup
computeConcernGroups branches =
  let
    -- Build bipartite edges: branch ↔ field
    branchFields = branches <#> \b ->
      Tuple b.name (Set.union b.stateReads b.stateWrites)

    -- For each field, collect all branches that touch it
    fieldToBranches = foldl (\acc (Tuple bName fields) ->
      foldl (\a field ->
        Map.alter (Just <<< Set.insert bName <<< fromMaybe Set.empty) field a
      ) acc (Set.toUnfoldable fields :: Array String)
    ) Map.empty branchFields

    -- Simple union-find via transitive closure
    groups = computeTransitiveClosure fieldToBranches branches

  in groups

-- | Compute transitive closure of branch groups via shared fields
computeTransitiveClosure :: Map.Map String (Set.Set String) -> Array BranchInfo -> Array ConcernGroup
computeTransitiveClosure fieldToBranches branches =
  let
    -- Build adjacency: two branches are connected if they share a field
    branchAdj = foldl (\acc (Tuple _field branchSet) ->
      let bs = Set.toUnfoldable branchSet :: Array String
      in foldl (\a b1 ->
          foldl (\a2 b2 ->
            if b1 /= b2 then
              Map.alter (Just <<< Set.insert b2 <<< fromMaybe Set.empty) b1 a2
            else a2
          ) a bs
        ) acc bs
    ) Map.empty (Map.toUnfoldable fieldToBranches :: Array (Tuple String (Set.Set String)))

    -- BFS to find connected components
    allBranchNames = Set.fromFoldable $ branches <#> _.name
    components = findComponents branchAdj allBranchNames

    -- Build concern groups with metadata
    branchMap = Map.fromFoldable $ branches <#> \b -> Tuple b.name b
    groups = mapWithIndex (\i component ->
      let
        branchInfos = Array.mapMaybe (\name -> Map.lookup name branchMap) (Set.toUnfoldable component :: Array String)
        allFields = foldl (\acc b -> Set.union acc (Set.union b.stateReads b.stateWrites)) Set.empty branchInfos
        -- Find fields shared by 2+ branches in this group
        fieldCounts = foldl (\acc b ->
          foldl (\a field -> Map.alter (Just <<< (_ + 1) <<< fromMaybe 0) field a) acc
            (Set.toUnfoldable (Set.union b.stateReads b.stateWrites) :: Array String)
        ) Map.empty branchInfos
        sharedFields = Set.fromFoldable $ Array.mapMaybe (\(Tuple field count) ->
          if count > 1 then Just field else Nothing
        ) (Map.toUnfoldable fieldCounts :: Array (Tuple String Int))
        groupName = if Set.size component == 1
          then fromMaybe ("Group " <> show (i + 1)) $ Array.head (Set.toUnfoldable component :: Array String)
          else inferGroupName branchInfos sharedFields i
      in
        { name: groupName
        , branches: component
        , sharedFields
        , allFields
        }
    ) components
  in groups

-- | Find connected components via BFS
findComponents :: Map.Map String (Set.Set String) -> Set.Set String -> Array (Set.Set String)
findComponents adj allNodes =
  let
    go remaining acc =
      case Set.findMin remaining of
        Nothing -> acc
        Just start ->
          let component = bfsComponent adj start
              remaining' = Set.difference remaining component
          in go remaining' (Array.snoc acc component)
  in go allNodes []

-- | BFS from a start node to find its connected component
bfsComponent :: Map.Map String (Set.Set String) -> String -> Set.Set String
bfsComponent adj start =
  let
    go queue visited =
      case Array.uncons queue of
        Nothing -> visited
        Just { head: node, tail: rest } ->
          let nbrs = fromMaybe Set.empty (Map.lookup node adj)
              newNbrs = Set.difference nbrs visited
              newQueue = rest <> (Set.toUnfoldable newNbrs :: Array String)
          in go newQueue (Set.union visited newNbrs)
  in go [start] (Set.singleton start)

-- | Infer a group name from the branch names and shared fields
inferGroupName :: Array BranchInfo -> Set.Set String -> Int -> String
inferGroupName branches sharedFields idx =
  let
    branchNames = branches <#> _.name
    -- Try to find common prefix
    prefix = commonPrefix branchNames
  in
    if String.length prefix >= 3 then prefix
    else if Set.size sharedFields > 0 then
      -- Name by dominant shared field
      fromMaybe ("Group " <> show (idx + 1)) $
        Array.head (Set.toUnfoldable sharedFields :: Array String)
    else "Group " <> show (idx + 1)

-- | Find common prefix of an array of strings
commonPrefix :: Array String -> String
commonPrefix strs =
  case Array.head strs of
    Nothing -> ""
    Just first ->
      let
        go i =
          if i >= String.length first then String.take i first
          else
            let ch = SCU.charAt i first
            in if Array.all (\s -> SCU.charAt i s == ch) strs
               then go (i + 1)
               else String.take i first
      in go 0

-- =============================================================================
-- Convert to declaration graph format (for decomposition viz)
-- =============================================================================

-- | Convert branch analysis into the same DeclInfo/FunctionCall format
-- | used by ModuleStructureViz, enabling reuse of decomposition visualization
branchesToDeclGraph
  :: Array BranchInfo
  -> { declarations :: Array { name :: String, kind :: String }
     , internalCalls :: Array { callerName :: String, calleeModule :: String, calleeName :: String, isCrossModule :: Boolean, callCount :: Int }
     }
branchesToDeclGraph branches =
  let
    branchNames = Set.fromFoldable $ branches <#> _.name

    -- Declarations: one per branch
    declarations = branches <#> \b ->
      { name: b.name, kind: "case-branch" }

    -- Internal calls: connect branches that share state fields
    fieldToBranches = foldl (\acc b ->
      let fields = Set.union b.stateReads b.stateWrites
      in foldl (\a field ->
        Map.alter (Just <<< Set.insert b.name <<< fromMaybe Set.empty) field a
      ) acc (Set.toUnfoldable fields :: Array String)
    ) Map.empty branches

    -- For each pair of branches sharing a field, create an edge
    sharedFieldEdges = Array.concatMap (\(Tuple _field branchSet) ->
      let bs = Array.sort (Set.toUnfoldable branchSet :: Array String)
      in Array.concatMap (\(Tuple i b1) ->
        Array.mapMaybe (\b2 ->
          if b1 < b2 then Just { callerName: b1, calleeModule: "", calleeName: b2, isCrossModule: false, callCount: 1 }
          else Nothing
        ) (Array.drop (i + 1) bs)
      ) (mapWithIndex Tuple bs)
    ) (Map.toUnfoldable fieldToBranches :: Array (Tuple String (Set.Set String)))

    -- Also connect branches that dispatch to each other
    dispatchEdges = Array.concatMap (\b ->
      Array.mapMaybe (\target ->
        if Set.member target branchNames && target /= b.name
        then Just { callerName: b.name, calleeModule: "", calleeName: target, isCrossModule: false, callCount: 1 }
        else Nothing
      ) (Set.toUnfoldable b.actionDispatches :: Array String)
    ) branches

    -- Deduplicate edges
    allEdges = Array.nub (sharedFieldEdges <> dispatchEdges)
  in
    { declarations, internalCalls: allEdges }
