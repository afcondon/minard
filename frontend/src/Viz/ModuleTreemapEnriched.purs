-- | Module Treemap Enriched - Treemap with individual declaration circles
-- |
-- | Combines treemap layout with circle-packed individual declarations.
-- |
-- | Key features:
-- | - Treemap rectangles sized by LOC (squarify tiling)
-- | - Individual declarations as circles (not aggregates), colored by kind
-- | - Hover on declaration shows dependencies (what it calls, what calls it)
-- | - Hover on module shows import relationships
-- | - Bubbles allowed to overflow cell boundaries (no clipping)
-- | - Click anywhere in cell = click module
-- |
-- | Two levels of CoordinatedHighlight:
-- | - Module level: hover module highlights imported/importing modules
-- | - Declaration level: hover declaration highlights its dependencies
module CE2.Viz.ModuleTreemapEnriched
  ( Config
  , DeclarationCircle
  , ChildCircle
  , render
  , cleanup
  , kindColor
  , childKindColor
  , childCircleElem
  , packDeclarations
  , packChildren
  , syntheticArityChildren
  , shortModuleName
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl)
import Data.Int (toNumber, floor)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (sqrt)
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)

-- PSD3 HATS Imports
import Hylograph.HATS (Tree, elem, staticStr, thunkedStr, thunkedNum, forEach, withBehaviors, onCoordinatedHighlight, onCoordinatedHighlightWithTooltip, onClick)
import Hylograph.HATS.InterpreterTick (rerender, clearContainer)
import Hylograph.Internal.Element.Types (ElementType(..))
-- Note: ElementType includes SVG, Group, Rect, Circle, Text, Path, Line, etc.
import Hylograph.Internal.Behavior.Types (HighlightClass(..), TooltipTrigger(..))

-- Layout
import DataViz.Layout.Hierarchy.Types (ValuedNode(..))
import DataViz.Layout.Hierarchy.Treemap (TreemapNode(..), treemap, defaultTreemapConfig, squarify, phi)
import DataViz.Layout.Hierarchy.Pack (packSiblingsMap)

import CE2.Data.Loader (V2ModuleListItem, V2ModuleImports, V2Declaration, V2ChildDeclaration, V2FunctionCall, GitStatusData)
import CE2.Types (ColorMode(..), GitFileStatus(..), gitStatusColor, PackageReachability, PackageClusters, ReachabilityStatus(..), getModuleReachability)

-- =============================================================================
-- Types
-- =============================================================================

-- | Configuration for enriched module treemap
type Config =
  { containerSelector :: String
  , width :: Number
  , height :: Number
  , packageName :: String
  , onModuleClick :: Maybe (String -> String -> Effect Unit)  -- packageName -> moduleName -> Effect
  , onDeclarationClick :: Maybe (String -> String -> String -> Effect Unit)  -- pkg -> mod -> decl -> Effect
  , colorMode :: ColorMode       -- Current color mode (for git status / reachability)
  , gitStatus :: Maybe GitStatusData  -- Git status for module coloring
  , reachabilityData :: Maybe PackageReachability  -- Reachability for dead code coloring
  , reachabilityPeek :: Boolean  -- True while R key held (show text overlay)
  , clusterData :: Maybe PackageClusters  -- Cluster data for ClusterView coloring
  }

-- | Module with computed treemap position
type PositionedModule =
  { mod :: V2ModuleListItem
  , x :: Number
  , y :: Number
  , width :: Number
  , height :: Number
  }

-- | Child circle (constructor, class member, instance) within a declaration
type ChildCircle =
  { name :: String
  , fullName :: String  -- Module.Parent.name for unique identification
  , kind :: String      -- "constructor", "class_member", "instance"
  , typeSignature :: Maybe String
  , x :: Number         -- Position relative to parent center
  , y :: Number
  , r :: Number
  }

-- | Individual declaration circle packed within a module cell
type DeclarationCircle =
  { name :: String
  , fullName :: String  -- Module.name for unique identification
  , kind :: String  -- "value", "data", "newtype", "type_class", "type_synonym", "foreign"
  , typeSignature :: Maybe String
  , childCount :: Int  -- Number of constructors/members
  , x :: Number     -- Position relative to cell center
  , y :: Number
  , r :: Number
  , calls :: Array String      -- Declarations this one calls (fullNames)
  , calledBy :: Array String   -- Declarations that call this one (fullNames)
  , children :: Array ChildCircle  -- Nested child circles (constructors, methods)
  }

-- | Enriched render data with declaration circles
type EnrichedModuleData =
  { name :: String
  , shortName :: String
  , loc :: Int
  , declarationCount :: Int
  , x :: Number
  , y :: Number
  , width :: Number
  , height :: Number
  , imports :: Array String
  , importedBy :: Array String
  , declarations :: Array DeclarationCircle  -- Individual declaration bubbles
  , packRadius :: Number                      -- Enclosing radius of the bubble pack
  }

-- | Get git status for a module by name
getModuleGitStatus :: Maybe GitStatusData -> String -> GitFileStatus
getModuleGitStatus mGitStatus moduleName = case mGitStatus of
  Nothing -> GitClean
  Just gs
    | Array.elem moduleName gs.modified -> GitModified
    | Array.elem moduleName gs.staged -> GitStaged
    | Array.elem moduleName gs.untracked -> GitUntracked
    | otherwise -> GitClean

-- | Get module styling based on color mode and git status
type ModuleStyling =
  { fillColor :: String
  , strokeColor :: String
  , strokeWidth :: String
  }

getModuleStyling :: ColorMode -> GitFileStatus -> Maybe ReachabilityStatus -> Maybe Int -> ModuleStyling
getModuleStyling colorMode gitStatus mReachStatus mClusterIdx = case colorMode of
  Reachability ->
    case mReachStatus of
      Just EntryPoint ->
        -- Entry points: bright blue background with thick stroke (most important)
        { fillColor: "#d6eaf8"
        , strokeColor: "#2980b9"
        , strokeWidth: "3"
        }
      Just Reachable ->
        -- Reachable: subtle tinted background (alive, needed)
        { fillColor: "transparent"
        , strokeColor: "rgba(255, 255, 255, 0.3)"
        , strokeWidth: "1"
        }
      Just Unreachable ->
        -- Unreachable: desaturated gray (dead code)
        { fillColor: "rgba(180, 180, 180, 0.25)"
        , strokeColor: "rgba(150, 150, 150, 0.4)"
        , strokeWidth: "1"
        }
      Nothing ->
        -- No reachability data: default
        { fillColor: "transparent"
        , strokeColor: "rgba(255, 255, 255, 0.3)"
        , strokeWidth: "1"
        }
  GitStatus ->
    case gitStatus of
      GitClean ->
        -- Clean modules are dimmed to nearly invisible
        { fillColor: "rgba(240, 240, 240, 0.3)"
        , strokeColor: "rgba(150, 150, 150, 0.2)"
        , strokeWidth: "0.5"
        }
      GitModified ->
        -- Modified: bright orange background with thick stroke
        { fillColor: "#ffecd2"  -- Light orange background
        , strokeColor: "#e67e22"  -- Orange stroke
        , strokeWidth: "3"
        }
      GitStaged ->
        -- Staged: bright green background with thick stroke
        { fillColor: "#d5f5e3"  -- Light green background
        , strokeColor: "#27ae60"  -- Green stroke
        , strokeWidth: "3"
        }
      GitUntracked ->
        -- Untracked: bright purple background with thick stroke
        { fillColor: "#ebdef0"  -- Light purple background
        , strokeColor: "#9b59b6"  -- Purple stroke
        , strokeWidth: "3"
        }
  ClusterView ->
    case mClusterIdx of
      Just idx ->
        let clusterColor = clusterPaletteColor idx
        in { fillColor: clusterColor <> "33"  -- ~20% opacity hex suffix
           , strokeColor: clusterColor
           , strokeWidth: "2"
           }
      Nothing ->
        { fillColor: "transparent"
        , strokeColor: "rgba(255, 255, 255, 0.3)"
        , strokeWidth: "1"
        }
  _ ->
    -- Default styling: transparent so scene background shows through, white grid strokes
    { fillColor: "transparent"
    , strokeColor: "rgba(255, 255, 255, 0.3)"
    , strokeWidth: "1"
    }

-- =============================================================================
-- Public API
-- =============================================================================

-- | Render the enriched module treemap
-- | Takes modules, their imports, declarations, and function calls
render
  :: Config
  -> Array V2ModuleListItem
  -> Array V2ModuleImports
  -> Map Int (Array V2Declaration)
  -> Map Int (Array V2FunctionCall)
  -> Effect Unit
render config modules imports declarationsMap callsMap = do
  -- Clear existing content
  clearContainer config.containerSelector

  -- Filter modules to the package
  let pkgModules = Array.filter (\m -> m.package.name == config.packageName) modules
      pkgModuleNames = Set.fromFoldable $ map _.name pkgModules

  -- Build module-level import maps for highlighting
  let importMap = buildImportMap imports pkgModuleNames
      importedByMap = buildImportedByMap imports pkgModuleNames

  -- Build declaration-level dependency maps
  let { callsTo, calledByMap } = buildDeclarationDependencyMaps pkgModules callsMap pkgModuleNames

  -- Debug: log dependency map sizes
  log $ "[ModuleTreemapEnriched] Package: " <> config.packageName
      <> ", callsMap size: " <> show (Map.size callsMap)
      <> ", callsTo size: " <> show (Map.size callsTo)
      <> ", calledByMap size: " <> show (Map.size calledByMap)

  -- Compute treemap layout
  let positioned = computeModulePositions config pkgModules

  -- Enrich with declaration circles (including dependency info)
  let enriched = positioned <#> \pm ->
        enrichModuleData pm importMap importedByMap declarationsMap callsTo calledByMap

  -- Debug: count declarations with dependencies
  let declsWithCalls = Array.concatMap _.declarations enriched
                       # Array.filter (\d -> Array.length d.calls > 0 || Array.length d.calledBy > 0)
                       # Array.length
      totalDecls = Array.concatMap _.declarations enriched # Array.length
  log $ "[ModuleTreemapEnriched] Total declarations: " <> show totalDecls
      <> ", with dependencies: " <> show declsWithCalls

  -- Render
  renderEnrichedTreemapSVG config enriched

-- | Clean up
cleanup :: String -> Effect Unit
cleanup selector = clearContainer selector

-- =============================================================================
-- Treemap Layout
-- =============================================================================

computeModulePositions :: Config -> Array V2ModuleListItem -> Array PositionedModule
computeModulePositions config modules =
  let
    moduleLeaves :: Array (ValuedNode V2ModuleListItem)
    moduleLeaves = modules <#> \m ->
      VNode
        { data_: m
        , depth: 1
        , height: 0
        , value: toNumber $ max 1 $ fromMaybe m.declarationCount m.loc
        , children: []
        , parent: Nothing
        }

    totalValue = foldl (\acc (VNode n) -> acc + n.value) 0.0 moduleLeaves

    rootData :: V2ModuleListItem
    rootData =
      { id: 0
      , name: config.packageName
      , path: Nothing
      , loc: Nothing
      , package: { id: 0, name: config.packageName, version: "", source: "workspace" }
      , namespacePath: Nothing
      , declarationCount: 0
      }

    root :: ValuedNode V2ModuleListItem
    root = VNode
      { data_: rootData
      , depth: 0
      , height: 1
      , value: totalValue
      , children: moduleLeaves
      , parent: Nothing
      }

    treemapConfig = defaultTreemapConfig
      { size = { width: config.width, height: config.height }
      , paddingInner = 4.0
      , paddingOuter = 2.0
      , tile = squarify phi
      }

    TNode layoutRoot = treemap treemapConfig root
  in
    layoutRoot.children <#> \(TNode child) ->
      { mod: child.data_
      , x: child.x0
      , y: child.y0
      , width: child.x1 - child.x0
      , height: child.y1 - child.y0
      }

-- =============================================================================
-- Module Import Maps (for module-level highlighting)
-- =============================================================================

buildImportMap :: Array V2ModuleImports -> Set String -> Map String (Array String)
buildImportMap imports pkgModuleNames =
  Map.fromFoldable $ imports
    # Array.filter (\imp -> Set.member imp.moduleName pkgModuleNames)
    <#> \imp -> Tuple imp.moduleName (Array.filter (\i -> Set.member i pkgModuleNames) imp.imports)

buildImportedByMap :: Array V2ModuleImports -> Set String -> Map String (Array String)
buildImportedByMap imports pkgModuleNames =
  let
    pairs :: Array (Tuple String String)
    pairs = imports
      # Array.filter (\imp -> Set.member imp.moduleName pkgModuleNames)
      # Array.concatMap (\imp ->
          imp.imports
            # Array.filter (\i -> Set.member i pkgModuleNames)
            <#> \imported -> Tuple imported imp.moduleName
        )
  in foldl (\acc (Tuple imported importer) ->
      Map.alter (Just <<< Array.cons importer <<< fromMaybe []) imported acc
    ) Map.empty pairs

-- =============================================================================
-- Declaration Dependency Maps (for declaration-level highlighting)
-- =============================================================================

-- | Build maps of what each declaration calls and what calls it
-- | Key is "Module.declarationName", value is array of "Module.declarationName"
buildDeclarationDependencyMaps
  :: Array V2ModuleListItem
  -> Map Int (Array V2FunctionCall)
  -> Set String
  -> { callsTo :: Map String (Array String), calledByMap :: Map String (Array String) }
buildDeclarationDependencyMaps modules callsMap pkgModuleNames =
  let
    -- Collect all call relationships within the package
    allCalls :: Array { caller :: String, callee :: String }
    allCalls = Array.concatMap processModuleCalls modules

    processModuleCalls :: V2ModuleListItem -> Array { caller :: String, callee :: String }
    processModuleCalls m =
      let
        moduleCalls = fromMaybe [] $ Map.lookup m.id callsMap
        callerModule = m.name
      in
        moduleCalls
          # Array.filter (\c -> Set.member c.calleeModule pkgModuleNames)  -- Only within package
          <#> \c ->
            { caller: callerModule <> "." <> c.callerName
            , callee: c.calleeModule <> "." <> c.calleeName
            }

    -- Build callsTo map: caller -> [callees]
    callsTo = foldl (\acc { caller, callee } ->
        Map.alter (Just <<< Array.cons callee <<< fromMaybe []) caller acc
      ) Map.empty allCalls

    -- Build calledBy map: callee -> [callers]
    calledByMap = foldl (\acc { caller, callee } ->
        Map.alter (Just <<< Array.cons caller <<< fromMaybe []) callee acc
      ) Map.empty allCalls
  in
    { callsTo, calledByMap }

-- =============================================================================
-- Declaration Circle Packing
-- =============================================================================

enrichModuleData
  :: PositionedModule
  -> Map String (Array String)
  -> Map String (Array String)
  -> Map Int (Array V2Declaration)
  -> Map String (Array String)  -- callsTo
  -> Map String (Array String)  -- calledBy
  -> EnrichedModuleData
enrichModuleData pm importMap importedByMap declarationsMap callsTo calledByMap =
  let
    moduleDecls = fromMaybe [] $ Map.lookup pm.mod.id declarationsMap
    moduleName = pm.mod.name

    -- Create and pack declaration circles with dependency info
    { declarations, packRadius } = packDeclarations moduleDecls moduleName pm.width pm.height callsTo calledByMap
  in
    { name: pm.mod.name
    , shortName: shortModuleName pm.mod.name
    , loc: fromMaybe pm.mod.declarationCount pm.mod.loc
    , declarationCount: Array.length moduleDecls
    , x: pm.x
    , y: pm.y
    , width: pm.width
    , height: pm.height
    , imports: fromMaybe [] $ Map.lookup pm.mod.name importMap
    , importedBy: fromMaybe [] $ Map.lookup pm.mod.name importedByMap
    , declarations
    , packRadius
    }

packDeclarations
  :: Array V2Declaration
  -> String  -- module name
  -> Number
  -> Number
  -> Map String (Array String)  -- callsTo
  -> Map String (Array String)  -- calledBy
  -> { declarations :: Array DeclarationCircle, packRadius :: Number }
packDeclarations decls moduleName cellWidth cellHeight callsTo calledByMap =
  if Array.null decls then
    { declarations: [], packRadius: 0.0 }
  else
    let
      targetRadius = min cellWidth cellHeight / 2.0 * 0.8

      -- First pass: create declarations with packed children
      rawCircles = decls <#> \d ->
        let
          fullName = moduleName <> "." <> d.name

          -- Get children: real children for data/class, synthetic for values
          effectiveChildren = if Array.length d.children > 0
            then d.children  -- Use real children (constructors, methods)
            else case d.kind of
              "value" -> syntheticArityChildren d.typeSignature  -- Parse arity from type sig
              _ -> []  -- No children for type synonyms, foreign, etc.

          childCount = Array.length effectiveChildren

          -- Pack children first if any exist
          packedChildren = packChildren effectiveChildren moduleName d.name d.kind

          -- Radius based on children pack or minimum for childless
          -- Declarations with children are sized to contain their children
          -- Childless declarations get a small fixed size
          baseR = if Array.null effectiveChildren
                  then 6.0  -- Small fixed size for childless
                  else packedChildren.packRadius + 2.0  -- Padding around children
        in
          { name: d.name
          , fullName
          , kind: d.kind
          , typeSignature: d.typeSignature
          , childCount
          , x: 0.0
          , y: 0.0
          , r: max 6.0 (min baseR 50.0)  -- Clamp between 6 and 50
          , calls: fromMaybe [] $ Map.lookup fullName callsTo
          , calledBy: fromMaybe [] $ Map.lookup fullName calledByMap
          , children: packedChildren.children
          }

      -- Second pass: pack all declarations together
      packed = packSiblingsMap (rawCircles <#> \c -> { x: 0.0, y: 0.0, r: c.r })

      scaleFactor = if packed.radius > 0.0
                    then targetRadius / packed.radius
                    else 1.0

      -- Apply scale to declarations and their children
      declarations = Array.zipWith
        (\decl circle -> decl
          { x = circle.x * scaleFactor
          , y = circle.y * scaleFactor
          , r = decl.r * scaleFactor
          , children = decl.children <#> \child -> child
              { x = child.x * scaleFactor
              , y = child.y * scaleFactor
              , r = child.r * scaleFactor
              }
          })
        rawCircles
        packed.circles
    in
      { declarations
      , packRadius: packed.radius * scaleFactor
      }

-- | Generate synthetic child declarations based on function arity from type signature
-- | Parses "a -> b -> c -> d" to extract argument count (3 in this case)
syntheticArityChildren :: Maybe String -> Array V2ChildDeclaration
syntheticArityChildren mTypeSig = case mTypeSig of
  Nothing -> []
  Just typeSig ->
    let
      -- Count arrows that represent function arguments
      -- Simplified: count " -> " occurrences, but be careful about nested types
      arity = countFunctionArity typeSig
    in
      if arity < 1 then []  -- Don't show for simple values (no arrows)
      else Array.range 1 arity <#> \i ->
        { id: 0  -- Synthetic, no real ID
        , name: "arg" <> show i
        , kind: "argument"  -- Synthetic kind for function arguments
        , typeSignature: Nothing  -- Could extract individual arg types later
        , comments: Nothing
        }

-- | Count function arity from type signature
-- | Handles basic cases like "Int -> String -> Effect Unit"
-- | Tries to avoid counting arrows inside type constructors
countFunctionArity :: String -> Int
countFunctionArity typeSig =
  let
    -- Simple heuristic: count " -> " at depth 0 (not inside parens/brackets)
    go :: Int -> Int -> Int -> Int
    go idx depth count =
      if idx >= String.length typeSig then count
      else
        let
          c = String.take 1 (String.drop idx typeSig)
          rest = String.drop (idx + 1) typeSig
        in case c of
          "(" -> go (idx + 1) (depth + 1) count
          ")" -> go (idx + 1) (max 0 (depth - 1)) count
          "[" -> go (idx + 1) (depth + 1) count
          "]" -> go (idx + 1) (max 0 (depth - 1)) count
          "{" -> go (idx + 1) (depth + 1) count
          "}" -> go (idx + 1) (max 0 (depth - 1)) count
          "-" ->
            -- Check for " -> " at depth 0
            if depth == 0 && String.take 2 rest == "> "
            then go (idx + 3) depth (count + 1)  -- Skip "-> "
            else go (idx + 1) depth count
          _ -> go (idx + 1) depth count
  in
    go 0 0 0

-- | Pack children (constructors, methods, instances, or synthetic arguments) within a declaration
packChildren
  :: Array V2ChildDeclaration
  -> String  -- module name
  -> String  -- parent declaration name
  -> String  -- parent kind (for color inheritance)
  -> { children :: Array ChildCircle, packRadius :: Number }
packChildren children moduleName parentName _parentKind =
  if Array.null children then
    { children: [], packRadius: 0.0 }
  else
    let
      -- Create raw child circles with small fixed radius
      -- All children are roughly equal size (slightly varied by name length for visual interest)
      rawChildren = children <#> \c ->
        let
          baseR = 3.0 + min 2.0 (toNumber (String.length c.name) / 10.0)
          fullName = moduleName <> "." <> parentName <> "." <> c.name
        in
          { name: c.name
          , fullName
          , kind: c.kind
          , typeSignature: c.typeSignature
          , x: 0.0
          , y: 0.0
          , r: baseR
          }

      -- Pack children together
      packed = packSiblingsMap (rawChildren <#> \c -> { x: 0.0, y: 0.0, r: c.r })

      -- Position children according to pack layout
      positionedChildren = Array.zipWith
        (\child circle -> child
          { x = circle.x
          , y = circle.y
          })
        rawChildren
        packed.circles
    in
      { children: positionedChildren
      , packRadius: packed.radius
      }

-- =============================================================================
-- SVG Rendering (HATS)
-- =============================================================================

renderEnrichedTreemapSVG :: Config -> Array EnrichedModuleData -> Effect Unit
renderEnrichedTreemapSVG config enriched = do
  -- Debug: count links that would be drawn
  let allCalls = Array.concatMap (\m -> Array.concatMap _.calls m.declarations) enriched
      posMap = buildPositionMap enriched
      matchedCalls = allCalls # Array.filter (\callee -> Map.member callee posMap)
  log $ "[ModuleTreemapEnriched] Link debug: "
      <> show (Array.length allCalls) <> " total outgoing calls, "
      <> show (Array.length matchedCalls) <> " match positions in posMap, "
      <> show (Map.size posMap) <> " declarations in posMap"
  -- Show a few unmatched for debugging
  let unmatched = Array.take 5 $ Array.filter (\callee -> not (Map.member callee posMap)) allCalls
  when (Array.length unmatched > 0) do
    log $ "[ModuleTreemapEnriched] Sample unmatched callees: " <> show unmatched

  let svgTree = buildEnrichedTreemapTree config enriched
  _ <- rerender config.containerSelector svgTree
  pure unit

buildEnrichedTreemapTree :: Config -> Array EnrichedModuleData -> Tree
buildEnrichedTreemapTree config enriched =
  let
    -- Build position map for all declarations
    posMap = buildPositionMap enriched
  in
    elem SVG
      [ staticStr "id" "module-treemap-enriched"
      , staticStr "viewBox" $ "0 0 " <> show config.width <> " " <> show config.height
      , staticStr "width" "100%"
      , staticStr "height" "100%"
      , staticStr "preserveAspectRatio" "xMidYMid meet"
      , staticStr "style" "background: transparent; display: block; border-radius: 8px;"
      ]
      [ -- Module cells with declarations (rendered first = behind)
        forEach "modules" Group enriched _.name (enrichedModuleCell config)
        -- Dependency links layer (rendered last = on top, but pointer-events: none)
      , renderDependencyLinks posMap enriched
      ]

-- =============================================================================
-- Dependency Links Layer
-- =============================================================================

-- | Absolute position of a declaration in the SVG coordinate space
type AbsolutePosition = { x :: Number, y :: Number, r :: Number }

-- | Build map of declaration fullName to absolute position
buildPositionMap :: Array EnrichedModuleData -> Map String AbsolutePosition
buildPositionMap enriched =
  Map.fromFoldable $ Array.concatMap moduleDeclarationPositions enriched
  where
    moduleDeclarationPositions m =
      m.declarations <#> \decl ->
        Tuple decl.fullName
          { x: m.x + m.width / 2.0 + decl.x
          , y: m.y + m.height / 2.0 + decl.y
          , r: decl.r
          }

-- | Render all dependency links as curved paths
renderDependencyLinks :: Map String AbsolutePosition -> Array EnrichedModuleData -> Tree
renderDependencyLinks posMap enriched =
  let links = Array.concatMap (moduleDependencyLinks posMap) enriched
  in elem Group
    [ staticStr "class" "dependency-links"
    , staticStr "pointer-events" "none"
    , thunkedStr "data-link-count" (show (Array.length links))
    ]
    links

-- | Generate links for all declarations in a module
moduleDependencyLinks :: Map String AbsolutePosition -> EnrichedModuleData -> Array Tree
moduleDependencyLinks posMap m =
  Array.concatMap (declarationOutgoingLinks posMap m) m.declarations

-- | Generate outgoing links from a single declaration to its callees
declarationOutgoingLinks :: Map String AbsolutePosition -> EnrichedModuleData -> DeclarationCircle -> Array Tree
declarationOutgoingLinks posMap m decl =
  let
    fromPos = { x: m.x + m.width / 2.0 + decl.x
              , y: m.y + m.height / 2.0 + decl.y
              , r: decl.r
              }
    fromName = decl.fullName
  in
    decl.calls # Array.mapMaybe \calleeName ->
      Map.lookup calleeName posMap <#> \toPos ->
        linkPath fromPos toPos fromName calleeName

-- | Render a curved link between two declaration positions
-- | Uses a quadratic Bezier curve with control point offset perpendicular to the line
-- | Participates in CoordinatedHighlight - glows when either endpoint is hovered
linkPath :: AbsolutePosition -> AbsolutePosition -> String -> String -> Tree
linkPath from to fromName toName =
  let
    -- Distance between points
    dx = to.x - from.x
    dy = to.y - from.y
    dist = sqrt (dx * dx + dy * dy)

    -- Control point: perpendicular offset, scaled by distance
    -- Curve bows outward proportional to distance
    curvature = min 0.3 (dist / 500.0) * 40.0
    -- Perpendicular direction (90 degrees rotated)
    perpX = if dist > 0.0 then -dy / dist * curvature else 0.0
    perpY = if dist > 0.0 then dx / dist * curvature else 0.0
    -- Midpoint
    midX = (from.x + to.x) / 2.0
    midY = (from.y + to.y) / 2.0
    -- Control point
    cx = midX + perpX
    cy = midY + perpY

    -- SVG quadratic Bezier path
    pathD = "M " <> show from.x <> " " <> show from.y
         <> " Q " <> show cx <> " " <> show cy
         <> " " <> show to.x <> " " <> show to.y

    -- Unique link ID for highlight system
    linkId = fromName <> "→" <> toName
  in
    withBehaviors
      [ onCoordinatedHighlight
          { identify: linkId
          , classify: \hoveredId ->
              -- Highlight if either endpoint is hovered
              if hoveredId == fromName || hoveredId == toName then Related
              else Dimmed
          , group: Just "declarations"  -- Same group as declaration circles
          }
      ]
    $ elem Path
        [ thunkedStr "d" pathD
        , staticStr "fill" "none"
        , staticStr "stroke" "#f59e0b"  -- Amber-500, warm orange
        , staticStr "stroke-width" "0.75"
        , staticStr "stroke-opacity" "0.25"
        , staticStr "class" "dependency-link"
        ]
        []

-- | Render a single enriched module cell
enrichedModuleCell :: Config -> EnrichedModuleData -> Tree
enrichedModuleCell config m =
  let
    -- Get git status and reachability status for this module
    gitStatus = getModuleGitStatus config.gitStatus m.name
    reachStatus = config.reachabilityData <#> \rd -> getModuleReachability rd m.name
    -- Get cluster index for this module
    clusterIdx = config.clusterData >>= \cd -> Map.lookup m.name cd.communities
    styling = getModuleStyling config.colorMode gitStatus reachStatus clusterIdx
    -- Dim declaration bubbles for unreachable modules in Reachability mode
    declOpacity = case config.colorMode of
      Reachability -> case reachStatus of
        Just Unreachable -> "0.25"
        _ -> "1"
      _ -> "1"
  in
    withBehaviors
      ( [ onCoordinatedHighlight
            { identify: m.name
            , classify: \hoveredId ->
                if m.name == hoveredId then Primary
                else if Array.elem hoveredId m.imports then Related
                else if Array.elem hoveredId m.importedBy then Related
                else Dimmed
            , group: Just "modules"  -- Module-level highlighting group
            }
        ]
        <> case config.onModuleClick of
             Nothing -> []
             Just onClickHandler -> [ onClick (onClickHandler config.packageName m.name) ]
      )
    $ elem Group
        [ thunkedStr "transform" ("translate(" <> show m.x <> "," <> show m.y <> ")")
        , staticStr "class" "treemap-module-enriched"
        , staticStr "cursor" "pointer"
        ]
        [ -- Module rectangle (background color based on git status)
          elem Rect
            [ staticStr "x" "0"
            , staticStr "y" "0"
            , thunkedNum "width" m.width
            , thunkedNum "height" m.height
            , thunkedStr "fill" styling.fillColor
            , thunkedStr "stroke" styling.strokeColor
            , thunkedStr "stroke-width" styling.strokeWidth
            , staticStr "rx" "2"
            , staticStr "class" "module-rect"
            ]
            []

      -- Centered bubble pack of individual declarations
      , elem Group
          [ thunkedStr "transform"
              ("translate(" <> show (m.width / 2.0) <> "," <> show (m.height / 2.0) <> ")")
          , staticStr "class" "declaration-bubbles"
          , thunkedStr "opacity" declOpacity
          ]
          (map (declarationCircleElem config m.name) m.declarations)

      -- Module name at bottom
      , elem Text
          [ thunkedNum "x" (m.width / 2.0)
          , thunkedNum "y" (m.height - 6.0)
          , staticStr "text-anchor" "middle"
          , staticStr "dominant-baseline" "auto"
          , thunkedStr "font-size" (if m.width > 60.0 then "9" else "7")
          , staticStr "fill" "rgba(255, 255, 255, 0.9)"
          , staticStr "font-family" "system-ui, sans-serif"
          , staticStr "font-weight" "500"
          , thunkedStr "textContent" (truncateName (m.width / 7.0) m.shortName)
          , thunkedStr "opacity" (if m.width > 30.0 && m.height > 25.0 then "1" else "0")
          ]
          []

      -- Declaration count in top-right corner
      , elem Text
          [ thunkedNum "x" (m.width - 3.0)
          , staticStr "y" "10"
          , staticStr "text-anchor" "end"
          , staticStr "font-size" "7"
          , staticStr "fill" "rgba(255, 255, 255, 0.5)"
          , staticStr "font-family" "system-ui, sans-serif"
          , thunkedStr "textContent"
              (if m.width > 35.0 && m.height > 25.0 && m.declarationCount > 0
               then show m.declarationCount
               else "")
          ]
          []

      -- Reachability peek overlay (visible when R key held)
      , if config.reachabilityPeek then
          let
            peekLabel = case reachStatus of
              Just EntryPoint  -> "ENTRY POINT"
              Just Reachable   -> "reachable"
              Just Unreachable -> "UNREACHABLE"
              Nothing          -> ""
            peekBgColor = case reachStatus of
              Just EntryPoint  -> "rgba(41, 128, 185, 0.7)"   -- Blue tint
              Just Reachable   -> "rgba(39, 174, 96, 0.5)"    -- Green tint
              Just Unreachable -> "rgba(192, 57, 43, 0.6)"    -- Red tint
              Nothing          -> "rgba(0, 0, 0, 0.3)"
            peekTextColor = "white"
            fontSize = if m.width > 80.0 then "11" else if m.width > 50.0 then "9" else "7"
            visible = m.width > 25.0 && m.height > 20.0
          in
            elem Group
              [ staticStr "class" "reachability-peek-overlay"
              , thunkedStr "opacity" (if visible then "1" else "0")
              ]
              [ -- Semi-transparent background rect
                elem Rect
                  [ staticStr "x" "0"
                  , staticStr "y" "0"
                  , thunkedNum "width" m.width
                  , thunkedNum "height" m.height
                  , thunkedStr "fill" peekBgColor
                  , staticStr "rx" "2"
                  , staticStr "pointer-events" "none"
                  ]
                  []
              -- Status label centered
              , elem Text
                  [ thunkedNum "x" (m.width / 2.0)
                  , thunkedNum "y" (m.height / 2.0)
                  , staticStr "text-anchor" "middle"
                  , staticStr "dominant-baseline" "middle"
                  , thunkedStr "font-size" fontSize
                  , thunkedStr "fill" peekTextColor
                  , staticStr "font-weight" "bold"
                  , staticStr "font-family" "system-ui, sans-serif"
                  , staticStr "pointer-events" "none"
                  , thunkedStr "textContent" peekLabel
                  ]
                  []
              ]
        else
          elem Group [] []  -- Empty placeholder
      ]

-- | Render an individual declaration circle with dependency highlighting
-- | If the declaration has children, renders them as nested circles inside
-- | Includes data-tooltip for hover information
declarationCircleElem :: Config -> String -> DeclarationCircle -> Tree
declarationCircleElem config moduleName decl =
  let
    -- Build tooltip text for display on hover
    tooltipText = buildDeclarationTooltip decl
    declClickBehavior = case config.onDeclarationClick of
      Nothing -> []
      Just handler -> [ onClick (handler config.packageName moduleName decl.name) ]
  in
  withBehaviors
    ( [ onCoordinatedHighlightWithTooltip
          { identify: decl.fullName
          , classify: \hoveredId ->
              if decl.fullName == hoveredId then Primary
              else if Array.elem hoveredId decl.calls then Related      -- I call this
              else if Array.elem hoveredId decl.calledBy then Related   -- This calls me
              else Dimmed
          , group: Just "declarations"  -- Declaration-level highlighting group
          , tooltip: Just { content: tooltipText, showWhen: OnHover }
          }
      ] <> declClickBehavior
    )
  $ if Array.null decl.children
    then
      -- Simple circle for childless declarations
      elem Circle
        [ thunkedNum "cx" decl.x
        , thunkedNum "cy" decl.y
        , thunkedNum "r" decl.r
        , thunkedStr "fill" (kindColor decl.kind)
        , staticStr "fill-opacity" "0.85"
        , staticStr "stroke" "white"
        , staticStr "stroke-width" "0.5"
        , staticStr "pointer-events" "all"
        , staticStr "cursor" "pointer"
        , thunkedStr "data-name" decl.name
        , thunkedStr "data-fullname" decl.fullName
        , thunkedStr "data-kind" decl.kind
        , thunkedStr "data-calls" (show (Array.length decl.calls))
        , thunkedStr "data-calledby" (show (Array.length decl.calledBy))
        , thunkedStr "data-tooltip" tooltipText  -- Tooltip content
        , staticStr "class" "declaration-circle"
        ]
        []
    else
      -- Group with outer ring and nested child circles
      elem Group
        [ thunkedStr "transform" ("translate(" <> show decl.x <> "," <> show decl.y <> ")")
        , staticStr "class" "declaration-with-children"
        , staticStr "cursor" "pointer"
        , thunkedStr "data-name" decl.name
        , thunkedStr "data-fullname" decl.fullName
        , thunkedStr "data-kind" decl.kind
        , thunkedStr "data-child-count" (show decl.childCount)
        , thunkedStr "data-tooltip" tooltipText  -- Tooltip content
        ]
        [ -- Outer circle (background/container)
          elem Circle
            [ staticStr "cx" "0"
            , staticStr "cy" "0"
            , thunkedNum "r" decl.r
            , thunkedStr "fill" (kindColor decl.kind)
            , staticStr "fill-opacity" "0.3"  -- Lighter fill for container
            , thunkedStr "stroke" (kindColor decl.kind)
            , staticStr "stroke-width" "1.5"
            , staticStr "stroke-opacity" "0.8"
            , staticStr "pointer-events" "all"
            , staticStr "class" "declaration-outer"
            ]
            []
        -- Nested child circles
        , elem Group
            [ staticStr "class" "declaration-children" ]
            (map (childCircleElem decl.kind) decl.children)
        ]

-- | Build tooltip text for a declaration
buildDeclarationTooltip :: DeclarationCircle -> String
buildDeclarationTooltip decl =
  let
    kindStr = case decl.kind of
      "value" -> "value"
      "data" -> "data"
      "newtype" -> "newtype"
      "type_class" -> "class"
      "type_synonym" -> "type"
      "foreign" -> "foreign"
      _ -> decl.kind

    header = decl.name <> " :: " <> kindStr

    typeSig = case decl.typeSignature of
      Just sig -> "\n" <> sig
      Nothing -> ""

    childInfo = if decl.childCount > 0
      then "\n(" <> show decl.childCount <> " " <> childKindName decl.kind <> ")"
      else ""

    depInfo =
      let calls = Array.length decl.calls
          calledBy = Array.length decl.calledBy
      in if calls > 0 || calledBy > 0
         then "\n→ " <> show calls <> " calls, ← " <> show calledBy <> " callers"
         else ""
  in
    header <> typeSig <> childInfo <> depInfo

-- | Get the name for children based on parent kind
childKindName :: String -> String
childKindName = case _ of
  "data" -> "constructors"
  "newtype" -> "constructors"
  "type_class" -> "members"
  "value" -> "arguments"
  _ -> "children"

-- | Render a child circle (constructor, method, instance) inside a declaration
childCircleElem :: String -> ChildCircle -> Tree
childCircleElem parentKind child =
  elem Circle
    [ thunkedNum "cx" child.x
    , thunkedNum "cy" child.y
    , thunkedNum "r" child.r
    , thunkedStr "fill" (childKindColor parentKind child.kind)
    , staticStr "fill-opacity" "0.9"
    , staticStr "stroke" "white"
    , staticStr "stroke-width" "0.3"
    , staticStr "pointer-events" "all"
    , staticStr "cursor" "pointer"
    , thunkedStr "data-name" child.name
    , thunkedStr "data-fullname" child.fullName
    , thunkedStr "data-kind" child.kind
    , staticStr "class" "child-circle"
    ]
    []

-- | Color for child circles - slightly varied from parent
childKindColor :: String -> String -> String
childKindColor parentKind childKind = case childKind of
  "constructor"  -> darkenColor (kindColor parentKind)  -- Darker variant of parent
  "class_member" -> "#ff9f43"  -- Lighter orange for methods
  "instance"     -> "#ffeaa7"  -- Very light yellow for instances
  "argument"     -> "#6a9fcf"  -- Lighter blue for function arguments
  _              -> kindColor parentKind  -- Fallback to parent color

-- | Darken a hex color slightly for visual distinction
darkenColor :: String -> String
darkenColor color = case color of
  "#59a14f" -> "#3d7a36"  -- Darker green for data constructors
  "#76b7b2" -> "#5a9994"  -- Darker teal for newtype constructors
  "#f28e2b" -> "#d97706"  -- Darker orange for class members
  _         -> color      -- No change for others

-- =============================================================================
-- Utility Functions
-- =============================================================================

shortModuleName :: String -> String
shortModuleName name =
  case Array.last (String.split (String.Pattern ".") name) of
    Just short -> short
    Nothing -> name

truncateName :: Number -> String -> String
truncateName maxChars name =
  let maxLen = max 3 (floor maxChars)
  in if String.length name > maxLen
     then String.take maxLen name <> "…"
     else name

-- | Color palette for dependency clusters (10 well-separated hues)
clusterPaletteColor :: Int -> String
clusterPaletteColor idx = case idx `mod` 10 of
  0 -> "#e6194b"  -- Red
  1 -> "#3cb44b"  -- Green
  2 -> "#4363d8"  -- Blue
  3 -> "#f58231"  -- Orange
  4 -> "#911eb4"  -- Purple
  5 -> "#42d4f4"  -- Cyan
  6 -> "#f032e6"  -- Magenta
  7 -> "#bfef45"  -- Lime
  8 -> "#fabebe"  -- Pink
  _ -> "#469990"  -- Teal

kindColor :: String -> String
kindColor = case _ of
  "value"        -> "#4e79a7"  -- Blue - functions/values
  "data"         -> "#59a14f"  -- Green - data types
  "newtype"      -> "#76b7b2"  -- Teal - newtypes
  "type_class"   -> "#f28e2b"  -- Orange - type classes
  "type_synonym" -> "#edc948"  -- Yellow - type synonyms
  "foreign"      -> "#e15759"  -- Red - FFI
  "alias"        -> "#b07aa1"  -- Purple - aliases
  _              -> "#bab0ac"  -- Gray - unknown
