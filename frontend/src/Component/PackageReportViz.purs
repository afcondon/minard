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
import Data.Int as Data.Int
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number as Number
import Data.String as String
import Data.Traversable (for_)
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import CE2.Data.Loader as Loader
import CE2.Util.SVG (svgElem, sa)
import CE2.Viz.CommitSparkline as Spark
import DataViz.Layout.Hierarchy.Pack (packSiblingsMap)

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
  | NavigateToCommits String         -- → CommitModuleGrid (workspace packages only)

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
  | ClickCommits String

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
    [ -- Sub-nav bar
      renderSubNav state totalPackages totalAnnotations
    , HH.div
        [ HP.style "max-width: 960px; margin: 0 auto; padding: 24px 24px 80px;" ]
        [ HH.div
            [ HP.style "display: flex; flex-direction: column; gap: 12px;" ]
            (cards <#> renderCard state)
        , if Array.null cards
            then HH.div
              [ HP.style "text-align: center; padding: 60px 20px; color: #999; font-size: 13px;" ]
              [ HH.text "No packages match the current filter." ]
            else HH.text ""
        ]
    ]

-- | Sub-navigation bar with title, counts, and filter/sort controls
renderSubNav :: forall m. State -> Int -> Int -> H.ComponentHTML Action () m
renderSubNav state totalPkgs totalAnns =
  HH.div
    [ HP.style "background: #e8e4d8; border-bottom: 1px solid #ccc; padding: 10px 24px; display: flex; align-items: center; justify-content: space-between; flex-wrap: wrap; gap: 8px;" ]
    [ -- Left: title + counts
      HH.div [ HP.style "display: flex; align-items: baseline; gap: 12px;" ]
        [ HH.span
            [ HP.style "font-size: 15px; font-weight: 700; color: #333; letter-spacing: -0.3px;" ]
            [ HH.text "Package Report" ]
        , HH.span
            [ HP.style "font-size: 11px; color: #777;" ]
            [ HH.text $ show totalPkgs <> " packages \x00B7 " <> show totalAnns <> " annotations" ]
        ]
    -- Right: sort + filter controls
    , HH.div [ HP.style "display: flex; align-items: center; gap: 12px;" ]
        [ HH.div [ HP.style "display: flex; align-items: center; gap: 4px;" ]
            [ navLabel "Sort"
            , navBtn "Annotations" (state.sortMode == SortByAnnotations) (SetSortMode SortByAnnotations)
            , navBtn "Name" (state.sortMode == SortByName) (SetSortMode SortByName)
            , navBtn "Modules" (state.sortMode == SortByModuleCount) (SetSortMode SortByModuleCount)
            ]
        , HH.div [ HP.style "display: flex; align-items: center; gap: 4px;" ]
            [ navLabel "Show"
            , navBtn "All" (state.filterCategory == "all") (SetFilterCategory "all")
            , navBtn "Workspace" (state.filterCategory == "workspace") (SetFilterCategory "workspace")
            , navBtn "Registry" (state.filterCategory == "registry") (SetFilterCategory "registry")
            ]
        ]
    ]
  where
  navLabel :: forall w i. String -> HH.HTML w i
  navLabel text =
    HH.span
      [ HP.style "font-size: 9px; text-transform: uppercase; letter-spacing: 0.5px; color: #888; margin-right: 2px;" ]
      [ HH.text text ]

  navBtn :: String -> Boolean -> Action -> H.ComponentHTML Action () m
  navBtn label isActive action =
    HH.button
      [ HE.onClick \_ -> action
      , HP.style $ "padding: 2px 8px; font-size: 10px; border: 1px solid "
          <> (if isActive then "#999" else "#c5c0b4")
          <> "; border-radius: 3px; cursor: pointer; background: "
          <> (if isActive then "#fff" else "transparent")
          <> "; color: " <> (if isActive then "#333" else "#666")
          <> "; font-family: 'Courier New', Courier, monospace;"
      ]
      [ HH.text label ]

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
    pkgModules = Array.filter (\m -> m.package.name == card.pkg.name) state.modules
  in
  HH.div
    [ HP.style $ "border: 1px solid #e0dcd2; border-radius: 4px; background: #fff; padding: 16px 20px;"
        <> if totalAnnCount > 0 then " border-left: 3px solid #8b7355;" else ""
    ]
    [ -- Top row: bubblepack glyph + package name + category + links
      HH.div
        [ HP.style "display: flex; align-items: center; gap: 12px; margin-bottom: 10px;" ]
        [ -- Package visual signature (circle-packed modules)
          renderPackageBubblepack pkgModules
        , -- Name + category
          HH.div [ HP.style "flex: 1; min-width: 0;" ]
            [ HH.div [ HP.style "display: flex; align-items: baseline; gap: 8px; margin-bottom: 2px;" ]
                [ HH.span
                    [ HP.style "font-weight: 700; font-size: 14px; color: #333; text-transform: uppercase; letter-spacing: 0.5px; cursor: pointer; text-decoration: underline; text-decoration-color: #ccc; text-underline-offset: 3px;"
                    , HE.onClick \_ -> ClickPackageName card.pkg.name
                    ]
                    [ HH.text card.pkg.name ]
                , categoryBadge card.category
                ]
            , -- Metrics + links inline
              HH.div [ HP.style "display: flex; align-items: center; gap: 8px; font-size: 11px; color: #888;" ]
                [ metricSpan (show card.depCount <> " deps")
                , metricSpan (show card.declarationCount <> " decls")
                , metricSpan ("topo " <> show card.pkg.topoLayer)
                , HH.span [ HP.style "color: #ccc;" ] [ HH.text "\x00B7" ]
                , -- "N modules: Details" link
                  HH.span []
                    [ HH.text $ show card.moduleCount <> " modules: "
                    , HH.span
                        [ HP.style "color: #2563eb; cursor: pointer; font-weight: 500;"
                        , HE.onClick \_ -> ClickModuleReport card.pkg.name
                        ]
                        [ HH.text "Details" ]
                    ]
                ]
            ]
        , -- Git history sparkline (right side)
          renderPackageSparkline state card
        ]

    -- Package-level annotations
    , if hasPkgAnns
        then HH.div
          [ HP.style "margin-bottom: 8px;" ]
          (card.packageAnnotations <#> renderAnnotationLine)
        else HH.text ""

    -- Module annotation rollup
    , if hasModAnns
        then HH.div
          [ HP.style "padding-top: 8px; border-top: 1px solid #f0ede4; font-size: 11px; color: #777;" ]
          [ HH.text $ renderStatusRollup card.moduleAnnotationsByStatus ]
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
-- Package Bubblepack Glyph
-- =============================================================================

-- | Render a small circle-packed SVG showing module sizes within a package
renderPackageBubblepack :: forall w i. Array Loader.V2ModuleListItem -> HH.HTML w i
renderPackageBubblepack modules =
  if Array.null modules then HH.text ""
  else
    let
      -- Create circles from module LOC (sqrt scaling)
      moduleCircles = modules <#> \m ->
        { x: 0.0, y: 0.0, r: max 2.0 (Number.sqrt (Data.Int.toNumber (fromMaybe 100 m.loc) / 1000.0) * 8.0) }
      packed = packSiblingsMap moduleCircles
      pad = 2.0
      r = packed.radius + pad
      viewBox = show (-r) <> " " <> show (-r) <> " " <> show (r * 2.0) <> " " <> show (r * 2.0)
    in
      svgElem "svg"
        [ sa "viewBox" viewBox
        , HP.style "width: 44px; height: 44px; flex-shrink: 0; display: block;"
        ]
        ( Array.zipWith (\m c ->
            svgElem "circle"
              [ sa "cx" (show c.x)
              , sa "cy" (show c.y)
              , sa "r" (show c.r)
              , sa "fill" "#8b9dc3"
              , sa "fill-opacity" "0.7"
              , sa "stroke" "#fff"
              , sa "stroke-width" "0.5"
              ]
              [ svgElem "title" [] [ HH.text m.name ] ]
          ) modules packed.circles
        )

-- =============================================================================
-- Package Sparkline (colored: green additions, red deletions)
-- =============================================================================

-- | Render sparkline with "Git history:" label as a clickable link to commits
renderPackageSparkline :: forall m. State -> PackageCard -> H.ComponentHTML Action () m
renderPackageSparkline state card =
  case Map.lookup card.pkg.name state.numstatCommits of
    Nothing
      | card.category == "workspace" -> HH.text ""  -- will load async
      | otherwise -> HH.text ""  -- registry packages have no git data
    Just commits ->
      let
        bars = Spark.prepareData "" commits
        nBars = Array.length bars
        vbW = max 160.0 (Data.Int.toNumber nBars)
        h = 32.0
        rects = toColoredRects { width: vbW, height: h } bars
      in if nBars == 0 then HH.text ""
         else
           HH.div
             [ HP.style "display: flex; align-items: center; gap: 6px; flex-shrink: 0; cursor: pointer; padding: 4px 8px; border-radius: 4px; transition: background 150ms ease;"
             , HE.onMouseEnter \_ -> SetSortMode state.sortMode  -- no-op, just for hover CSS
             , HE.onClick \_ -> ClickCommits card.pkg.name
             , HP.class_ (HH.ClassName "sparkline-link")
             ]
             [ HH.span
                 [ HP.style "font-size: 9px; color: #888; white-space: nowrap; font-family: 'Courier New', monospace;" ]
                 [ HH.text $ "Git: " <> show nBars ]
             , svgElem "svg"
                 [ sa "viewBox" ("0 0 " <> show vbW <> " " <> show h)
                 , sa "preserveAspectRatio" "none"
                 , HP.style "width: 160px; height: 32px; border-radius: 3px; border: 1px solid #d5d0c4; background: #f5f2eb; flex-shrink: 0;"
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
             ]

-- | Produce colored sparkline rects: green for additions (above center), red for deletions (below)
toColoredRects :: { width :: Number, height :: Number } -> Array Spark.SparklineBar -> Array Spark.SparklineRect
toColoredRects dims bars =
  let nBars = Array.length bars
      n = Data.Int.toNumber nBars
      pitch = if nBars > 0 then dims.width / n else 1.0
      barW = max 0.5 (pitch * 0.6)
      maxVal = Array.foldl (\acc b -> max acc (max b.totalAdded b.totalDeleted)) 1 bars
      logMax = Number.log (1.0 + Data.Int.toNumber maxVal)
      halfH = dims.height / 2.0
      centerY = halfH
      logScale v = Number.log (1.0 + Data.Int.toNumber v)
  in
    -- Center axis
    [{ x: 0.0, y: centerY - 0.25, width: dims.width, height: 0.5, fill: "#d5d0c4" }]
    <> Array.concatMap (\{ idx, bar } ->
      let x = Data.Int.toNumber idx * pitch + (pitch - barW) / 2.0
      in
        -- Green additions (above center)
        (if bar.totalAdded > 0
          then let barH = halfH * logScale bar.totalAdded / logMax
               in [{ x, y: centerY - barH, width: barW, height: barH, fill: "#22c55e" }]
          else [])
        -- Red deletions (below center)
        <> (if bar.totalDeleted > 0
          then let barH = halfH * logScale bar.totalDeleted / logMax
               in [{ x, y: centerY, width: barW, height: barH, fill: "#ef4444" }]
          else [])
    ) (Array.mapWithIndex (\i b -> { idx: i, bar: b }) bars)

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

  ClickCommits pkgName -> do
    log $ "[PackageReport] Navigate to commits for: " <> pkgName
    H.raise (NavigateToCommits pkgName)
