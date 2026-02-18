-- | Interactive Annotation Report View
-- |
-- | Renders the full annotation corpus in a scrollable, grouped hierarchy
-- | (package → module → kind → thread) with filtering by status, kind, and source,
-- | and navigation to module signature maps.
module CE2.Component.AnnotationReportViz
  ( component
  , Input
  , Output(..)
  , Query(..)
  , Slot
  ) where

import Prelude

import Data.Array as Array
import Data.Int (floor) as Int
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core (AttrName(..), ElemName(..), Namespace(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import CE2.Data.Loader as Loader
import CE2.Viz.ModuleTreemapEnriched (DeclarationCircle, ChildCircle, kindColor, childKindColor, packDeclarations)

-- =============================================================================
-- Types
-- =============================================================================

type Input =
  { annotations :: Array Loader.V2Annotation
  , packages :: Array Loader.V2Package
  , modules :: Array Loader.V2ModuleListItem
  , moduleDeclarations :: Map Int (Array Loader.V2Declaration)
  }

data Output
  = NavigateToModule String String    -- packageName, moduleName

data Query a = NoQuery a

type Slot = H.Slot Query Output

type State =
  { annotations :: Array Loader.V2Annotation
  , packages :: Array Loader.V2Package
  , modules :: Array Loader.V2ModuleListItem
  , moduleDeclarations :: Map Int (Array Loader.V2Declaration)
  , moduleNameToId :: Map String Int
  , statusFilter :: String
  , kindFilter :: String
  , sourceFilter :: String
  , collapsedPackages :: Set String
  , collapsedModules :: Set String
  , collapsedThreads :: Set Int
  }

data Action
  = Initialize
  | Receive Input
  | SetStatusFilter String
  | SetKindFilter String
  | SetSourceFilter String
  | TogglePackageCollapse String
  | ToggleModuleCollapse String
  | ToggleThreadCollapse Int
  | ClickModule String String

-- =============================================================================
-- Component
-- =============================================================================

component :: forall m. MonadAff m => H.Component Query Input Output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , receive = Just <<< Receive
        , initialize = Just Initialize
        }
    }

initialState :: Input -> State
initialState input =
  { annotations: input.annotations
  , packages: input.packages
  , modules: input.modules
  , moduleDeclarations: input.moduleDeclarations
  , moduleNameToId: buildModuleNameToId input.modules
  , statusFilter: "all"
  , kindFilter: "all"
  , sourceFilter: "all"
  , collapsedPackages: Set.empty
  , collapsedModules: Set.empty
  , collapsedThreads: Set.empty
  }

buildModuleNameToId :: Array Loader.V2ModuleListItem -> Map String Int
buildModuleNameToId mods =
  Map.fromFoldable $ mods <#> \m -> Tuple m.name m.id

-- =============================================================================
-- Annotation Thread Types and Helpers
-- =============================================================================

type AnnotationThread =
  { root :: Loader.V2Annotation
  , replies :: Array Loader.V2Annotation
  }

buildThreads :: Array Loader.V2Annotation -> Array AnnotationThread
buildThreads anns =
  let
    roots = Array.filter (\a -> a.supersedes == Nothing) anns
    replyMap :: Map Int (Array Loader.V2Annotation)
    replyMap = Array.foldl (\acc a -> case a.supersedes of
      Just sid -> Map.insertWith (<>) sid [a] acc
      Nothing -> acc
    ) Map.empty anns
    collectChain :: Int -> Array Loader.V2Annotation
    collectChain rootId =
      let direct = fromMaybe [] (Map.lookup rootId replyMap)
      in direct <> Array.concatMap (\r -> collectChain r.id) direct
  in roots <#> \root -> { root, replies: collectChain root.id }

-- | Build module name → package name mapping
modulePackageMap :: Array Loader.V2Package -> Array Loader.V2ModuleListItem -> Map String String
modulePackageMap _pkgs mods =
  Map.fromFoldable $ mods <#> \m -> Tuple m.name m.package.name

-- | Extract module name from annotation targetId (e.g. "module:CE2.Main" → "CE2.Main")
annotationModuleName :: Loader.V2Annotation -> String
annotationModuleName ann =
  if ann.targetType == "module" then ann.targetId
  else ann.targetId

-- =============================================================================
-- Filter Logic
-- =============================================================================

applyFilters :: State -> Array Loader.V2Annotation
applyFilters st = st.annotations
  # (if st.statusFilter == "all" then identity else Array.filter (\a -> a.status == st.statusFilter))
  # (if st.kindFilter == "all" then identity else Array.filter (\a -> a.kind == st.kindFilter))
  # (if st.sourceFilter == "all" then identity else Array.filter (\a -> a.source == st.sourceFilter))

-- | Get unique values for a field across all annotations
uniqueValues :: forall a. Ord a => (Loader.V2Annotation -> a) -> Array Loader.V2Annotation -> Array a
uniqueValues f anns = Array.nub $ map f anns

-- =============================================================================
-- Render
-- =============================================================================

render :: forall m. MonadAff m => State -> H.ComponentHTML Action () m
render state =
  let
    filtered = applyFilters state
    modPkgMap = modulePackageMap state.packages state.modules
    grouped = groupByPackageModule modPkgMap filtered
  in
  HH.div
    [ HP.class_ (HH.ClassName "annotation-report")
    ]
    [ renderFilterBar state filtered
    , HH.div
        [ HP.class_ (HH.ClassName "annotation-report-body") ]
        (if Array.null filtered
          then [ HH.div
                   [ HP.style "text-align: center; padding: 60px 20px; color: #999; font-size: 13px;" ]
                   [ HH.text "No annotations match the current filters." ]
               ]
          else Array.concatMap (renderPackageSection state) grouped
        )
    ]

-- =============================================================================
-- Filter Bar
-- =============================================================================

renderFilterBar :: forall m. State -> Array Loader.V2Annotation -> H.ComponentHTML Action () m
renderFilterBar state filtered =
  let
    totalCount = Array.length state.annotations
    filteredCount = Array.length filtered
    statuses = ["all"] <> uniqueValues _.status state.annotations
    kinds = ["all"] <> uniqueValues _.kind state.annotations
    sources = ["all"] <> uniqueValues _.source state.annotations
  in
  HH.div
    [ HP.class_ (HH.ClassName "annotation-report-filter-bar") ]
    [ HH.div
        [ HP.style "display: flex; align-items: center; gap: 16px; flex-wrap: wrap;" ]
        [ -- Status filter
          renderFilterSelect "Status" state.statusFilter statuses SetStatusFilter
        , renderFilterSelect "Kind" state.kindFilter kinds SetKindFilter
        , renderFilterSelect "Source" state.sourceFilter sources SetSourceFilter
        , HH.span
            [ HP.style "font-size: 11px; color: #666; margin-left: auto;" ]
            [ HH.text $ show filteredCount <> " of " <> show totalCount <> " annotations" ]
        ]
    ]

renderFilterSelect :: forall m. String -> String -> Array String -> (String -> Action) -> H.ComponentHTML Action () m
renderFilterSelect label current options mkAction =
  HH.div
    [ HP.style "display: flex; align-items: center; gap: 6px;" ]
    [ HH.span
        [ HP.style "font-size: 9px; font-weight: 600; text-transform: uppercase; letter-spacing: 1px; color: #999;" ]
        [ HH.text label ]
    , HH.select
        [ HP.style "font-family: 'Fira Code', monospace; font-size: 11px; padding: 3px 8px; border: 1px solid #ccc; background: #fff; color: #333; border-radius: 3px;"
        , HE.onValueChange mkAction
        ]
        (options <#> \opt ->
          HH.option
            [ HP.value opt
            , HP.selected (opt == current)
            ]
            [ HH.text opt ]
        )
    ]

-- =============================================================================
-- Grouping
-- =============================================================================

type PackageGroup =
  { packageName :: String
  , moduleGroups :: Array ModuleGroup
  , annotationCount :: Int
  }

type ModuleGroup =
  { moduleName :: String
  , kindGroups :: Array KindGroup
  , annotationCount :: Int
  }

type KindGroup =
  { kind :: String
  , threads :: Array AnnotationThread
  }

groupByPackageModule :: Map String String -> Array Loader.V2Annotation -> Array PackageGroup
groupByPackageModule modPkgMap anns =
  let
    -- Group annotations by module name
    byModule :: Map String (Array Loader.V2Annotation)
    byModule = Array.foldl (\acc a ->
      let modName = annotationModuleName a
          existing = fromMaybe [] (Map.lookup modName acc)
      in Map.insert modName (Array.snoc existing a) acc
    ) Map.empty anns

    -- Group modules by package
    byPackage :: Map String (Array { moduleName :: String, anns :: Array Loader.V2Annotation })
    byPackage = Array.foldl (\acc (Tuple modName modAnns) ->
      let pkgName = fromMaybe "unknown" (Map.lookup modName modPkgMap)
          existing = fromMaybe [] (Map.lookup pkgName acc)
      in Map.insert pkgName (Array.snoc existing { moduleName: modName, anns: modAnns }) acc
    ) Map.empty (Map.toUnfoldable byModule :: Array (Tuple String (Array Loader.V2Annotation)))

    -- Build PackageGroup array
    packageGroups :: Array PackageGroup
    packageGroups = (Map.toUnfoldable byPackage :: Array (Tuple String (Array { moduleName :: String, anns :: Array Loader.V2Annotation })))
      <#> \(Tuple pkgName modEntries) ->
        let
          moduleGroups = modEntries
            # Array.sortBy (comparing _.moduleName)
            <#> \entry ->
              let threads = buildThreads entry.anns
                  kindGroups = groupByKind threads
              in { moduleName: entry.moduleName
                 , kindGroups
                 , annotationCount: Array.length entry.anns
                 }
        in { packageName: pkgName
           , moduleGroups
           , annotationCount: Array.foldl (\acc mg -> acc + mg.annotationCount) 0 moduleGroups
           }
  in packageGroups # Array.sortBy (comparing _.packageName)

groupByKind :: Array AnnotationThread -> Array KindGroup
groupByKind threads =
  let
    grouped = Array.foldl (\acc t ->
      let k = t.root.kind
          existing = fromMaybe [] (Map.lookup k acc)
      in Map.insert k (Array.snoc existing t) acc
    ) Map.empty threads
    unsorted = (Map.toUnfoldable grouped :: Array (Tuple String (Array AnnotationThread)))
      <#> \(Tuple k ts) -> { kind: k, threads: ts }
  in Array.sortBy (comparing _.kind) unsorted

-- =============================================================================
-- Package Section
-- =============================================================================

renderPackageSection :: forall m. State -> PackageGroup -> Array (H.ComponentHTML Action () m)
renderPackageSection state pkg =
  let
    isCollapsed = Set.member pkg.packageName state.collapsedPackages
  in
  [ HH.div
      [ HP.style "border-bottom: 1px solid #e8e4d8;" ]
      [ -- Package header
        HH.div
          [ HP.style "display: flex; align-items: center; gap: 8px; padding: 12px 24px; cursor: pointer; background: #f5f2e8;"
          , HE.onClick \_ -> TogglePackageCollapse pkg.packageName
          ]
          [ HH.span
              [ HP.style "font-size: 10px; color: #999; width: 12px;" ]
              [ HH.text (if isCollapsed then "\x25b6" else "\x25bc") ]
          , HH.span
              [ HP.style "font-weight: 700; font-size: 13px; color: #333; text-transform: uppercase; letter-spacing: 0.5px;" ]
              [ HH.text pkg.packageName ]
          , HH.span
              [ HP.style "font-size: 10px; color: #999; background: #e8e4d8; padding: 1px 6px; border-radius: 8px;" ]
              [ HH.text $ show pkg.annotationCount ]
          ]
      , if isCollapsed
          then HH.text ""
          else HH.div
            [ HP.style "padding: 0;" ]
            (Array.concatMap (renderModuleSection state pkg.packageName) pkg.moduleGroups)
      ]
  ]

-- =============================================================================
-- Module Section
-- =============================================================================

renderModuleSection :: forall m. State -> String -> ModuleGroup -> Array (H.ComponentHTML Action () m)
renderModuleSection state pkgName mg =
  let
    isCollapsed = Set.member mg.moduleName state.collapsedModules
    shortName = shortModuleName mg.moduleName
  in
  [ HH.div
      [ HP.style "border-top: 1px solid #ede9dd; display: flex; align-items: flex-start;" ]
      [ -- Left margin: bubblepack glyph
        HH.div
          [ HP.style "width: 80px; flex-shrink: 0; display: flex; justify-content: center; padding-top: 6px;" ]
          [ renderModuleBubblepack state mg.moduleName ]
      , -- Main content column
        HH.div
          [ HP.style "flex: 1; min-width: 0;" ]
          [ -- Module header
            HH.div
              [ HP.style "display: flex; align-items: center; gap: 8px; padding: 10px 24px 10px 0;" ]
              [ HH.span
                  [ HP.style "font-size: 10px; color: #999; width: 12px; cursor: pointer;"
                  , HE.onClick \_ -> ToggleModuleCollapse mg.moduleName
                  ]
                  [ HH.text (if isCollapsed then "\x25b6" else "\x25bc") ]
              , HH.span
                  [ HP.class_ (HH.ClassName "annotation-report-module-link")
                  , HE.onClick \_ -> ClickModule pkgName mg.moduleName
                  ]
                  [ HH.text shortName ]
              , HH.span
                  [ HP.style "font-size: 9px; color: #aaa;" ]
                  [ HH.text mg.moduleName ]
              , HH.span
                  [ HP.style "font-size: 10px; color: #999; background: #f0ede4; padding: 1px 6px; border-radius: 8px;" ]
                  [ HH.text $ show mg.annotationCount ]
              ]
          , if isCollapsed
              then HH.text ""
              else HH.div
                [ HP.style "padding: 0 24px 12px 0;" ]
                (Array.concatMap (renderKindGroup state) mg.kindGroups)
          ]
      ]
  ]

-- =============================================================================
-- Kind Group
-- =============================================================================

renderKindGroup :: forall m. State -> KindGroup -> Array (H.ComponentHTML Action () m)
renderKindGroup state kg =
  [ HH.div
      [ HP.style "margin-top: 8px;" ]
      [ HH.div
          [ HP.style "font-size: 9px; font-weight: 600; text-transform: uppercase; letter-spacing: 1px; color: #8b7355; margin-bottom: 6px;" ]
          [ HH.text $ kg.kind <> " (" <> show (Array.length kg.threads) <> ")" ]
      , HH.div
          [ HP.style "display: flex; flex-direction: column; gap: 6px;" ]
          (kg.threads <#> renderThread state)
      ]
  ]

-- =============================================================================
-- Thread Rendering
-- =============================================================================

renderThread :: forall m. State -> AnnotationThread -> H.ComponentHTML Action () m
renderThread state thread =
  let
    ann = thread.root
    borderColor = statusBorderColor ann.status
    isCollapsed = Set.member ann.id state.collapsedThreads
    hasReplies = not (Array.null thread.replies)
  in
  HH.div
    [ HP.style $ "border-left: 3px solid " <> borderColor <> "; padding: 8px 12px; background: #faf9f6; border-radius: 0 3px 3px 0;" ]
    ( [ -- Thread header
        HH.div
          [ HP.style "display: flex; justify-content: space-between; align-items: center; margin-bottom: 4px;" ]
          [ HH.span
              [ HP.style "display: flex; align-items: center; gap: 6px;" ]
              [ HH.span
                  [ HP.style $ "font-size: 9px; font-weight: 600; color: " <> statusTextColor ann.status <> "; text-transform: uppercase; letter-spacing: 0.5px;" ]
                  [ HH.text ann.status ]
              , HH.span
                  [ HP.style "font-size: 9px; color: #aaa;" ]
                  [ HH.text $ sourceTag ann.source ]
              , HH.span
                  [ HP.style "font-size: 9px; color: #ccc;" ]
                  [ HH.text $ "c=" <> showConfidence ann.confidence ]
              ]
          , if hasReplies
              then HH.span
                [ HP.style "font-size: 9px; color: #999; cursor: pointer;"
                , HE.onClick \_ -> ToggleThreadCollapse ann.id
                ]
                [ HH.text (if isCollapsed then "\x25b6 " <> show (Array.length thread.replies + 1) else "\x25bc thread") ]
              else HH.text ""
          ]
      , -- Root annotation value
        HH.div
          [ HP.style "font-size: 12px; line-height: 1.6; color: #333; overflow-wrap: break-word;" ]
          [ HH.text ann.value ]
      ] <>
      -- Replies (when expanded)
      if isCollapsed || not hasReplies
        then []
        else Array.concatMap (\reply ->
          [ HH.div
              [ HP.style "margin-left: 12px; padding: 6px 0 0 8px; border-left: 2px solid #e0e0e0; margin-top: 6px;" ]
              [ HH.div
                  [ HP.style "display: flex; align-items: center; gap: 6px; margin-bottom: 2px;" ]
                  [ HH.span
                      [ HP.style $ "font-size: 9px; font-weight: 600; color: " <> statusTextColor reply.status <> ";" ]
                      [ HH.text reply.status ]
                  , HH.span
                      [ HP.style "font-size: 9px; color: #aaa;" ]
                      [ HH.text $ sourceTag reply.source <> " reply" ]
                  ]
              , HH.div
                  [ HP.style "font-size: 11px; line-height: 1.5; color: #555; overflow-wrap: break-word;" ]
                  [ HH.text reply.value ]
              ]
          ]
        ) thread.replies
    )

-- =============================================================================
-- Module Bubblepack Glyph
-- =============================================================================

svgNS :: Namespace
svgNS = Namespace "http://www.w3.org/2000/svg"

svgElem :: forall r w i. String -> Array (HH.IProp r i) -> Array (HH.HTML w i) -> HH.HTML w i
svgElem name = HH.elementNS svgNS (ElemName name)

sa :: forall r i. String -> String -> HH.IProp r i
sa name val = HP.attr (AttrName name) val

renderModuleBubblepack :: forall m. State -> String -> H.ComponentHTML Action () m
renderModuleBubblepack state moduleName =
  case Map.lookup moduleName state.moduleNameToId of
    Nothing -> HH.text ""
    Just moduleId ->
      case Map.lookup moduleId state.moduleDeclarations of
        Nothing -> HH.text ""
        Just decls ->
          if Array.null decls then HH.text ""
          else
            let
              -- Pack declarations using the same algorithm as the enriched treemap
              -- Use a generous target size so packDeclarations doesn't over-compress
              { declarations, packRadius } = packDeclarations decls moduleName 200.0 200.0 Map.empty Map.empty

              -- Compute viewBox from packed radius
              pad = 2.0
              r = packRadius + pad
              viewBox = show (-r) <> " " <> show (-r) <> " " <> show (r * 2.0) <> " " <> show (r * 2.0)
            in
              svgElem "svg"
                [ sa "viewBox" viewBox
                , HP.style "width: 72px; height: 72px; flex-shrink: 0; overflow: visible; display: block;"
                ]
                (Array.concatMap renderDeclCircle declarations)
  where
  renderDeclCircle :: DeclarationCircle -> forall w i. Array (HH.HTML w i)
  renderDeclCircle decl =
    -- Parent circle (semi-transparent fill with kind-colored stroke for containers)
    let hasChildren = not (Array.null decl.children)
    in
    [ svgElem "circle"
        [ sa "cx" (show decl.x)
        , sa "cy" (show decl.y)
        , sa "r" (show decl.r)
        , sa "fill" (kindColor decl.kind)
        , sa "fill-opacity" (if hasChildren then "0.3" else "0.85")
        , sa "stroke" (if hasChildren then kindColor decl.kind else "white")
        , sa "stroke-width" (if hasChildren then "1" else "0.5")
        ]
        [ svgElem "title" []
            [ HH.text $ decl.kind <> ": " <> decl.name ]
        ]
    ] <> (decl.children <#> \child -> renderChildCircle decl child)

  renderChildCircle :: DeclarationCircle -> ChildCircle -> forall w i. HH.HTML w i
  renderChildCircle parent child =
    svgElem "circle"
      [ sa "cx" (show (parent.x + child.x))
      , sa "cy" (show (parent.y + child.y))
      , sa "r" (show child.r)
      , sa "fill" (childKindColor parent.kind child.kind)
      , sa "fill-opacity" "0.85"
      , sa "stroke" "white"
      , sa "stroke-width" "0.3"
      ]
      [ svgElem "title" []
          [ HH.text $ child.kind <> ": " <> child.name ]
      ]

-- =============================================================================
-- Helpers
-- =============================================================================

statusBorderColor :: String -> String
statusBorderColor = case _ of
  "confirmed" -> "#4caf50"
  "rejected"  -> "#e53935"
  "stale"     -> "#f57c00"
  _           -> "#bdbdbd"

statusTextColor :: String -> String
statusTextColor = case _ of
  "confirmed" -> "#2e7d32"
  "rejected"  -> "#c62828"
  "stale"     -> "#e65100"
  _           -> "#999"

sourceTag :: String -> String
sourceTag "ai" = "(ai)"
sourceTag "human" = "(human)"
sourceTag s = if s == "" then "" else "(" <> s <> ")"

showConfidence :: Number -> String
showConfidence n =
  let int = Int.floor (n * 100.0)
  in show int <> "%"

-- | Extract the last segment of a dotted module name
shortModuleName :: String -> String
shortModuleName name =
  case Array.last (String.split (String.Pattern ".") name) of
    Just short -> short
    Nothing -> name

-- =============================================================================
-- Action Handlers
-- =============================================================================

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  Initialize -> pure unit

  Receive input ->
    H.modify_ _
      { annotations = input.annotations
      , packages = input.packages
      , modules = input.modules
      , moduleDeclarations = input.moduleDeclarations
      , moduleNameToId = buildModuleNameToId input.modules
      }

  SetStatusFilter f -> H.modify_ _ { statusFilter = f }
  SetKindFilter f -> H.modify_ _ { kindFilter = f }
  SetSourceFilter f -> H.modify_ _ { sourceFilter = f }

  TogglePackageCollapse pkg ->
    H.modify_ \st -> st { collapsedPackages = toggleSet pkg st.collapsedPackages }

  ToggleModuleCollapse mod ->
    H.modify_ \st -> st { collapsedModules = toggleSet mod st.collapsedModules }

  ToggleThreadCollapse tid ->
    H.modify_ \st -> st { collapsedThreads = toggleSet tid st.collapsedThreads }

  ClickModule pkgName modName ->
    H.raise (NavigateToModule pkgName modName)

-- | Toggle membership in a Set
toggleSet :: forall a. Ord a => a -> Set a -> Set a
toggleSet x s = if Set.member x s then Set.delete x s else Set.insert x s
