-- | Package Report Scene
-- |
-- | Scrollable list of package cards with metrics and annotation summaries.
-- | Entry point to the report flow: PackageReport → AnnotationReport (per package).
module CE2.Component.PackageReportViz
  ( component
  , Input
  , Output(..)
  , Query(..)
  , Slot
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as String
import Data.Traversable (for_)
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import Data.Int as Data.Int

import CE2.Data.Loader as Loader
import CE2.Util.SVG (svgElem, sa)
import CE2.Viz.CommitSparkline as Spark

-- =============================================================================
-- Types
-- =============================================================================

type Input =
  { packages :: Array Loader.V2Package
  , modules :: Array Loader.V2ModuleListItem
  , annotations :: Array Loader.V2Annotation
  , moduleDeclarations :: Map Int (Array Loader.V2Declaration)
  }

data Output
  = NavigateToPackage String         -- → PkgTreemap
  | NavigateToModuleReport String    -- → AnnotationReport (filtered to package)

data Query a = NoQuery a

type Slot = H.Slot Query Output

type State =
  { packages :: Array Loader.V2Package
  , modules :: Array Loader.V2ModuleListItem
  , annotations :: Array Loader.V2Annotation
  , moduleDeclarations :: Map Int (Array Loader.V2Declaration)
  , sortMode :: SortMode
  , filterCategory :: String  -- "all", "workspace", "registry", "extra"
  , numstatCommits :: Map String (Array Loader.NumstatCommit)
  , collapsedPackages :: Array String  -- Not currently used but reserved for expansion
  }

data SortMode
  = SortByName
  | SortByAnnotations
  | SortByModuleCount

derive instance eqSortMode :: Eq SortMode

data Action
  = Initialize
  | Receive Input
  | SetSortMode SortMode
  | SetFilterCategory String
  | ClickPackageName String
  | ClickModuleReport String

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
  { packages: input.packages
  , modules: input.modules
  , annotations: input.annotations
  , moduleDeclarations: input.moduleDeclarations
  , sortMode: SortByAnnotations
  , filterCategory: "all"
  , numstatCommits: Map.empty
  , collapsedPackages: []
  }

-- =============================================================================
-- Computed Data
-- =============================================================================

type PackageCard =
  { pkg :: Loader.V2Package
  , moduleCount :: Int
  , declarationCount :: Int
  , depCount :: Int
  , category :: String
  , packageAnnotations :: Array Loader.V2Annotation
  , moduleAnnotationCount :: Int
  , moduleAnnotationsByStatus :: Map String Int
  }

buildCards :: State -> Array PackageCard
buildCards state =
  let
    -- Module count per package
    modulesByPkg :: Map String (Array Loader.V2ModuleListItem)
    modulesByPkg = Array.foldl (\acc m ->
      Map.alter (Just <<< Array.cons m <<< fromMaybe []) m.package.name acc
    ) Map.empty state.modules

    -- Annotation partitions
    packageAnns :: Map String (Array Loader.V2Annotation)
    packageAnns = Array.foldl (\acc a ->
      if a.targetType == "package"
        then Map.alter (Just <<< Array.cons a <<< fromMaybe []) a.targetId acc
        else acc
    ) Map.empty state.annotations

    -- Module-level annotation counts per package
    modToPkg :: Map String String
    modToPkg = Map.fromFoldable $ state.modules <#> \m -> Tuple m.name m.package.name

    moduleAnnsByPkg :: Map String (Array Loader.V2Annotation)
    moduleAnnsByPkg = Array.foldl (\acc a ->
      if a.targetType == "module"
        then case Map.lookup a.targetId modToPkg of
          Just pkgName -> Map.alter (Just <<< Array.cons a <<< fromMaybe []) pkgName acc
          Nothing -> acc
        else acc
    ) Map.empty state.annotations

    countByStatus :: Array Loader.V2Annotation -> Map String Int
    countByStatus anns = Array.foldl (\acc a ->
      Map.alter (Just <<< (_ + 1) <<< fromMaybe 0) a.status acc
    ) Map.empty anns

    cards = state.packages <#> \pkg ->
      let
        pkgMods = fromMaybe [] (Map.lookup pkg.name modulesByPkg)
        pkgAnns = fromMaybe [] (Map.lookup pkg.name packageAnns)
        modAnns = fromMaybe [] (Map.lookup pkg.name moduleAnnsByPkg)
      in
        { pkg
        , moduleCount: Array.length pkgMods
        , declarationCount: pkg.declarationCount
        , depCount: Array.length pkg.depends
        , category: pkg.source
        , packageAnnotations: pkgAnns
        , moduleAnnotationCount: Array.length modAnns
        , moduleAnnotationsByStatus: countByStatus modAnns
        }

    -- Filter
    filtered = case state.filterCategory of
      "all" -> cards
      cat -> Array.filter (\c -> c.category == cat) cards

    -- Sort
    sorted = case state.sortMode of
      SortByName -> Array.sortBy (comparing (_.pkg >>> _.name)) filtered
      SortByAnnotations -> Array.sortBy (\a b ->
        let aTotal = Array.length a.packageAnnotations + a.moduleAnnotationCount
            bTotal = Array.length b.packageAnnotations + b.moduleAnnotationCount
        in compare bTotal aTotal  -- descending
      ) filtered
      SortByModuleCount -> Array.sortBy (\a b -> compare b.moduleCount a.moduleCount) filtered
  in sorted

-- =============================================================================
-- Render
-- =============================================================================

render :: forall m. MonadAff m => State -> H.ComponentHTML Action () m
render state =
  let
    cards = buildCards state
    totalAnnotations = Array.length state.annotations
    totalPackages = Array.length state.packages
  in
  HH.div
    [ HP.class_ (HH.ClassName "package-report")
    , HP.style "width: 100%; height: 100%; overflow-y: auto; background: #FAFAF8; font-family: -apple-system, 'Helvetica Neue', Helvetica, Arial, sans-serif; color: #333;"
    ]
    [ HH.div
        [ HP.style "max-width: 960px; margin: 0 auto; padding: 40px 24px 80px;" ]
        [ renderHeader totalPackages totalAnnotations
        , renderToolbar state
        , HH.div
            [ HP.style "display: flex; flex-direction: column; gap: 12px;" ]
            (cards <#> renderCard state)
        , if Array.null cards
            then HH.div
              [ HP.style "text-align: center; padding: 60px 20px; color: #999; font-size: 13px;" ]
              [ HH.text "No packages match the current filter." ]
            else HH.text ""
        ]
    ]

renderHeader :: forall w i. Int -> Int -> HH.HTML w i
renderHeader totalPkgs totalAnns =
  HH.div
    [ HP.style "margin-bottom: 32px;" ]
    [ HH.h1
        [ HP.style "font-size: 24px; font-weight: 700; letter-spacing: -0.5px; color: #2C2C2C; margin: 0 0 8px 0;" ]
        [ HH.text "Package Report" ]
    , HH.p
        [ HP.style "font-size: 13px; color: #888; margin: 0;" ]
        [ HH.text $ show totalPkgs <> " packages \x00B7 " <> show totalAnns <> " annotations" ]
    ]

renderToolbar :: forall m. State -> H.ComponentHTML Action () m
renderToolbar state =
  HH.div
    [ HP.style "display: flex; align-items: center; gap: 16px; margin-bottom: 20px; flex-wrap: wrap;" ]
    [ -- Sort buttons
      HH.div
        [ HP.style "display: flex; align-items: center; gap: 4px;" ]
        [ toolbarLabel "Sort"
        , sortBtn "Annotations" SortByAnnotations
        , sortBtn "Name" SortByName
        , sortBtn "Modules" SortByModuleCount
        ]
    , -- Category filter
      HH.div
        [ HP.style "display: flex; align-items: center; gap: 4px;" ]
        [ toolbarLabel "Show"
        , catBtn "All" "all"
        , catBtn "Workspace" "workspace"
        , catBtn "Registry" "registry"
        ]
    ]
  where
  toolbarLabel :: forall w i. String -> HH.HTML w i
  toolbarLabel text =
    HH.span
      [ HP.style "font-size: 9px; text-transform: uppercase; letter-spacing: 0.5px; color: #999; margin-right: 2px;" ]
      [ HH.text text ]

  sortBtn :: String -> SortMode -> H.ComponentHTML Action () m
  sortBtn label mode =
    HH.button
      [ HE.onClick \_ -> SetSortMode mode
      , HP.style $ smallBtnStyle (state.sortMode == mode)
      ]
      [ HH.text label ]

  catBtn :: String -> String -> H.ComponentHTML Action () m
  catBtn label cat =
    HH.button
      [ HE.onClick \_ -> SetFilterCategory cat
      , HP.style $ smallBtnStyle (state.filterCategory == cat)
      ]
      [ HH.text label ]

smallBtnStyle :: Boolean -> String
smallBtnStyle isActive =
  "padding: 3px 10px; font-size: 11px; border: 1px solid "
    <> (if isActive then "#999" else "#ddd")
    <> "; border-radius: 3px; cursor: pointer; background: "
    <> (if isActive then "#e8e4d8" else "#fff")
    <> "; color: " <> (if isActive then "#333" else "#666")
    <> "; font-family: 'Courier New', Courier, monospace;"

-- =============================================================================
-- Package Card
-- =============================================================================

renderCard :: forall m. State -> PackageCard -> H.ComponentHTML Action () m
renderCard state card =
  let
    pkgAnnCount = Array.length card.packageAnnotations
    totalAnnCount = pkgAnnCount + card.moduleAnnotationCount
    hasPkgAnns = pkgAnnCount > 0
    hasModAnns = card.moduleAnnotationCount > 0
  in
  HH.div
    [ HP.style $ "border: 1px solid #e0dcd2; border-radius: 4px; background: #fff; padding: 16px 20px;"
        <> if totalAnnCount > 0 then " border-left: 3px solid #8b7355;" else ""
    ]
    [ -- Top row: package name + category + module count
      HH.div
        [ HP.style "display: flex; align-items: baseline; justify-content: space-between; margin-bottom: 8px;" ]
        [ HH.div
            [ HP.style "display: flex; align-items: baseline; gap: 8px;" ]
            [ HH.span
                [ HP.style "font-weight: 700; font-size: 14px; color: #333; text-transform: uppercase; letter-spacing: 0.5px; cursor: pointer; text-decoration: underline; text-decoration-color: #ccc; text-underline-offset: 3px;"
                , HE.onClick \_ -> ClickPackageName card.pkg.name
                ]
                [ HH.text card.pkg.name ]
            , categoryBadge card.category
            ]
        , HH.span
            [ HP.style "font-size: 11px; color: #999;" ]
            [ HH.text $ show card.moduleCount <> " modules" ]
        ]

    -- Metrics row
    , HH.div
        [ HP.style "display: flex; align-items: center; gap: 16px; margin-bottom: 10px; font-size: 11px; color: #777;" ]
        [ metricSpan (show card.depCount <> " deps")
        , metricSpan (show card.declarationCount <> " decls")
        , metricSpan ("topo " <> show card.pkg.topoLayer)
        , -- Sparkline
          renderPackageSparkline state card.pkg.name
        ]

    -- Package-level annotations
    , if hasPkgAnns
        then HH.div
          [ HP.style "margin-bottom: 8px;" ]
          (card.packageAnnotations <#> renderAnnotationLine)
        else HH.text ""

    -- Module annotation rollup + click-through
    , if hasModAnns
        then HH.div
          [ HP.style "display: flex; align-items: center; justify-content: space-between; padding-top: 8px; border-top: 1px solid #f0ede4;" ]
          [ HH.span
              [ HP.style "font-size: 11px; color: #777;" ]
              [ HH.text $ renderStatusRollup card.moduleAnnotationsByStatus ]
          , HH.button
              [ HE.onClick \_ -> ClickModuleReport card.pkg.name
              , HP.style "font-size: 11px; color: #8b7355; cursor: pointer; background: none; border: 1px solid #d4c9a8; border-radius: 3px; padding: 2px 10px; font-family: inherit;"
              ]
              [ HH.text "detail \x25b8" ]
          ]
        else HH.text ""
    ]

metricSpan :: forall w i. String -> HH.HTML w i
metricSpan text =
  HH.span
    [ HP.style "font-family: 'Courier New', Courier, monospace;" ]
    [ HH.text text ]

categoryBadge :: forall w i. String -> HH.HTML w i
categoryBadge cat =
  let
    color = case cat of
      "workspace" -> "#4a7c59"
      "extra" -> "#7c6b4a"
      _ -> "#999"
  in
  HH.span
    [ HP.style $ "font-size: 9px; color: " <> color <> "; border: 1px solid " <> color <> "; border-radius: 8px; padding: 1px 6px; text-transform: lowercase;" ]
    [ HH.text cat ]

renderAnnotationLine :: forall w i. Loader.V2Annotation -> HH.HTML w i
renderAnnotationLine ann =
  HH.div
    [ HP.style $ "border-left: 3px solid " <> statusColor ann.status <> "; padding: 6px 10px; background: #faf9f6; border-radius: 0 3px 3px 0; margin-bottom: 4px;" ]
    [ HH.div
        [ HP.style "display: flex; align-items: center; gap: 6px; margin-bottom: 2px;" ]
        [ HH.span
            [ HP.style "font-size: 9px; font-weight: 600; text-transform: uppercase; letter-spacing: 0.5px; color: #8b7355;" ]
            [ HH.text ann.kind ]
        , HH.span
            [ HP.style $ "font-size: 9px; font-weight: 600; color: " <> statusTextColor ann.status <> ";" ]
            [ HH.text ann.status ]
        , HH.span
            [ HP.style "font-size: 9px; color: #aaa;" ]
            [ HH.text $ "(" <> ann.source <> ")" ]
        ]
    , HH.div
        [ HP.style "font-size: 12px; line-height: 1.5; color: #333; overflow-wrap: break-word;" ]
        [ HH.text ann.value ]
    ]

renderStatusRollup :: Map String Int -> String
renderStatusRollup statusMap =
  let
    entries = Map.toUnfoldable statusMap :: Array (Tuple String Int)
    parts = entries <#> \(Tuple status count) -> show count <> " " <> status
  in
  "Module annotations: " <> String.joinWith ", " parts

statusColor :: String -> String
statusColor = case _ of
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

-- =============================================================================
-- Package Sparkline
-- =============================================================================

renderPackageSparkline :: forall m. State -> String -> H.ComponentHTML Action () m
renderPackageSparkline state pkgName =
  case Map.lookup pkgName state.numstatCommits of
    Nothing -> HH.text ""
    Just commits ->
      let
        -- Use empty module name → shows only gray total bars (no module highlight)
        bars = Spark.prepareData "" commits
        nBars = Array.length bars
        vbW = max 120.0 (Data.Int.toNumber nBars)
        h = 24.0
        rects = Spark.toSvgRects { width: vbW, height: h } bars
      in if nBars == 0 then HH.text ""
         else svgElem "svg"
            [ sa "viewBox" ("0 0 " <> show vbW <> " " <> show h)
            , sa "preserveAspectRatio" "none"
            , HP.style "width: 120px; height: 24px; border-radius: 2px; border: 1px solid #e8e8e8; background: #fff; flex-shrink: 0;"
            ]
            (rects <#> \r ->
              svgElem "rect"
                [ sa "x" (show r.x)
                , sa "y" (show r.y)
                , sa "width" (show r.width)
                , sa "height" (show r.height)
                , sa "fill" r.fill
                ]
                []
            )

-- =============================================================================
-- Action Handlers
-- =============================================================================

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  Initialize -> do
    state <- H.get
    -- Fetch numstat for workspace packages (sparklines)
    let workspacePkgs = Array.filter (\p -> p.source == "workspace") state.packages
    for_ workspacePkgs \pkg -> do
      result <- liftAff $ Loader.fetchModuleNumstat 200 pkg.name
      case result of
        Left err -> log $ "[PackageReport] Numstat fetch error for " <> pkg.name <> ": " <> err
        Right commits ->
          H.modify_ \st -> st { numstatCommits = Map.insert pkg.name commits st.numstatCommits }

  Receive input ->
    H.modify_ _
      { packages = input.packages
      , modules = input.modules
      , annotations = input.annotations
      , moduleDeclarations = input.moduleDeclarations
      }

  SetSortMode mode ->
    H.modify_ _ { sortMode = mode }

  SetFilterCategory cat ->
    H.modify_ _ { filterCategory = cat }

  ClickPackageName pkgName -> do
    log $ "[PackageReport] Navigate to package: " <> pkgName
    H.raise (NavigateToPackage pkgName)

  ClickModuleReport pkgName -> do
    log $ "[PackageReport] Navigate to module report for: " <> pkgName
    H.raise (NavigateToModuleReport pkgName)
