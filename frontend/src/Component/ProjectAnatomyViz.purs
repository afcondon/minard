-- | Project Anatomy Visualization Component
-- |
-- | Shows the structure of a project's dependency universe:
-- | workspace packages, direct dependencies, and transitive dependencies.
-- | Wraps an AnatomyBeeswarm with stats cards and educational narrative.
module CE2.Component.ProjectAnatomyViz
  ( component
  , Input
  , Output(..)
  , Query(..)
  , Slot
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (elem, foldl, sum)
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS

import CE2.Containers as C
import CE2.Data.Loader as Loader
import CE2.Viz.AnatomyBeeswarm as Beeswarm
import Halogen.HTML.Events as HE

-- =============================================================================
-- Types
-- =============================================================================

type Input =
  { packages :: Array Loader.PackageSetPackage
  }

data Output
  = PackageClicked String
  | NavigateToGalaxy
  | NavigateToProjects

type Slot = H.Slot Query Output

data Query a = NoQuery a

type AnatomyStats =
  { workspaceCount :: Int
  , workspaceNames :: Array String
  , directCount :: Int
  , transitiveCount :: Int
  , unusedCount :: Int
  , totalCount :: Int
  , totalModules :: Int
  , totalDeclarations :: Int
  , maxTopoLayer :: Int
  , entryModule :: Maybe String
  }

type State =
  { packages :: Array Loader.PackageSetPackage
  , unusedPackages :: Array Loader.PackageSetPackage
  , stats :: AnatomyStats
  , handle :: Maybe Beeswarm.BeeswarmHandle
  , actionListener :: Maybe (HS.Listener Action)
  }

data Action
  = Initialize
  | Receive Input
  | Finalize
  | HandlePackageClick String
  | GoToGalaxy
  | GoToProjects

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
        , finalize = Just Finalize
        }
    }

initialState :: Input -> State
initialState input =
  { packages: input.packages
  , unusedPackages: []
  , stats: computeStats input.packages 0
  , handle: Nothing
  , actionListener: Nothing
  }

-- =============================================================================
-- Stats Computation
-- =============================================================================

computeStats :: Array Loader.PackageSetPackage -> Int -> AnatomyStats
computeStats packages unusedCount =
  let
    wsPackages = Array.filter (\p -> p.source == "workspace") packages
    wsNames = Set.fromFoldable $ map _.name wsPackages
    directDepNames = Beeswarm.computeDirectDepNames packages
    transitiveCount = Array.length packages - Set.size wsNames - Set.size directDepNames
    maxLayer = foldl (\acc p -> max acc p.topoLayer) 0 packages
    -- Find entry module from workspace app packages
    entryMod = Array.findMap _.bundleModule wsPackages
  in
    { workspaceCount: Set.size wsNames
    , workspaceNames: Array.sort $ Array.fromFoldable wsNames
    , directCount: Set.size directDepNames
    , transitiveCount: max 0 transitiveCount
    , unusedCount
    , totalCount: Array.length packages
    , totalModules: sum $ map _.moduleCount packages
    , totalDeclarations: 0  -- Not available from PackageSetPackage
    , maxTopoLayer: maxLayer
    , entryModule: entryMod
    }

-- =============================================================================
-- Render
-- =============================================================================

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  let statsColumns = if state.stats.unusedCount > 0 then 4 else 3
  in HH.div
    [ HP.class_ (HH.ClassName "project-anatomy-viz")
    , HP.style "width: 100%; height: 100%; overflow-y: auto; background: #fafaf8; color: #333; font-family: 'Courier New', Courier, monospace;"
    ]
    [ -- Heading
      HH.div
        [ HP.style "padding: 24px 32px 0; max-width: 1200px; margin: 0 auto;" ]
        [ HH.h1
            [ HP.style "font-size: 20px; font-weight: bold; margin: 0 0 4px; letter-spacing: 0.5px;" ]
            [ HH.text "Anatomy of Your Project" ]
        , HH.p
            [ HP.style "font-size: 12px; color: #777; margin: 0 0 16px;" ]
            [ HH.text $ show state.stats.totalCount <> " packages"
                <> " \x00B7 " <> show state.stats.totalModules <> " modules"
            ]
        ]

    -- Beeswarm container
    , HH.div
        [ HP.id C.anatomyBeeswarmContainerId
        , HP.class_ (HH.ClassName "anatomy-beeswarm")
        , HP.style "width: 100%; max-width: 1200px; height: 480px; margin: 0 auto; background: #fafaf8;"
        ]
        []

    -- Explanatory text directly under the viz
    , HH.div
        [ HP.style "max-width: 1200px; margin: 8px auto 0; padding: 0 32px; font-size: 12px; line-height: 1.8; color: #555;" ]
        [ HH.p
            [ HP.style "margin: 0 0 4px;" ]
            [ HH.text $ "The beeswarm shows every package sized by lines of code "
                <> "and positioned by dependency depth. Gold circles are your code, "
                <> "blue are your declared dependencies, and the gray packages are "
                <> "the transitive infrastructure that supports them \x2014 the deeper "
                <> "in the graph, the lighter the shade."
            ]
        , HH.p
            [ HP.style "margin: 0; color: #888; font-style: italic;" ]
            [ HH.text "Click any package to explore its modules." ]
        ]

    -- Stats cards
    , HH.div
        [ HP.style $ "max-width: 1200px; margin: 16px auto 0; padding: 0 32px; display: grid; "
            <> "grid-template-columns: repeat(" <> show statsColumns <> ", 1fr); gap: 16px;"
        ]
        ( [ statsCard (show state.stats.workspaceCount) "Workspace" "Your code"
              "hsl(40, 85%, 60%)"
          , statsCard (show state.stats.directCount) "Direct Dependencies" "Declared in spago.yaml"
              "hsl(210, 65%, 50%)"
          , statsCard (show state.stats.transitiveCount) "Transitive" "Pulled in by deps"
              "hsl(210, 15%, 65%)"
          ]
          <> if state.stats.unusedCount > 0
             then [ statsCard (show state.stats.unusedCount) "Not Used" "Available in registry"
                      "hsl(210, 8%, 82%)" ]
             else []
        )

    -- Dependency matrix (union of all workspace deps with per-project dots)
    , renderDependencyMatrix state.packages

    -- CTA buttons
    , HH.div
        [ HP.style "max-width: 1200px; margin: 0 auto 24px; padding: 0 32px; display: flex; gap: 12px; align-items: center;" ]
        [ HH.button
            [ HE.onClick \_ -> GoToGalaxy
            , HP.style "padding: 10px 24px; border: none; border-radius: 6px; cursor: pointer; font-size: 13px; font-weight: 600; background: #4E79A7; color: white;"
            ]
            [ HH.text "Explore the Galaxy" ]
        , HH.button
            [ HE.onClick \_ -> GoToProjects
            , HP.style "padding: 10px 24px; border: 1px solid #C0BDB4; border-radius: 6px; cursor: pointer; font-size: 13px; font-weight: 500; background: #fff; color: #555;"
            ]
            [ HH.text "Load Another Project" ]
        , HH.span
            [ HP.style "margin-left: 16px; font-size: 11px; color: #AAA; font-style: italic;" ]
            [ HH.text "For AI-generated module summaries, run /annotate from Claude Code." ]
        ]
    ]

-- | Stats card
statsCard :: forall w i. String -> String -> String -> String -> HH.HTML w i
statsCard count title subtitle accentColor =
  HH.div
    [ HP.style $ "background: #fff; border: 1px solid #e0ddd4; border-radius: 4px; padding: 16px; "
        <> "border-top: 3px solid " <> accentColor <> ";"
    ]
    [ HH.div
        [ HP.style "font-size: 28px; font-weight: bold; margin: 0 0 4px;" ]
        [ HH.text count ]
    , HH.div
        [ HP.style "font-size: 12px; font-weight: bold; margin: 0 0 2px;" ]
        [ HH.text title ]
    , HH.div
        [ HP.style "font-size: 11px; color: #999;" ]
        [ HH.text subtitle ]
    ]

-- =============================================================================
-- Dependency Matrix
-- =============================================================================

-- | Workspace package with assigned color
type WorkspaceEntry = { pkg :: Loader.PackageSetPackage, color :: String }

-- | A dependency with usage info across workspace packages
type DepEntry =
  { name :: String
  , users :: Array Int         -- indices into workspace array
  , resolvedSource :: String   -- actual source: "registry" | "workspace" | "extra"
  }

-- | Palette for workspace packages
wsPalette :: Array String
wsPalette = [ "#E8A838", "#4E79A7", "#59A14F", "#E15759", "#76B7B2" ]

-- | Render the unified dependency matrix
renderDependencyMatrix :: forall w i. Array Loader.PackageSetPackage -> HH.HTML w i
renderDependencyMatrix packages =
  let
    wsPackages = Array.sortWith _.name $ Array.filter (\p -> p.source == "workspace") packages
    wsEntries :: Array WorkspaceEntry
    wsEntries = Array.zipWith (\pkg color -> { pkg, color }) wsPackages
      (Array.take (Array.length wsPackages) wsPalette)

    -- Look up resolved source for a dep name from the full packages array
    resolveSource depName =
      case Array.find (\p -> p.name == depName) packages of
        Just p -> p.source
        Nothing -> "registry"

    -- Union of all deps across workspace packages
    allDepNames = Set.fromFoldable $ Array.concatMap _.depends wsPackages

    -- Build entries with usage info
    depEntries :: Array DepEntry
    depEntries = Array.fromFoldable allDepNames
      # map (\depName ->
          { name: depName
          , users: Array.catMaybes $ Array.mapWithIndex (\i pkg ->
              if elem depName pkg.depends then Just i else Nothing
            ) wsPackages
          , resolvedSource: resolveSource depName
          })
      # Array.sortBy (\a b ->
          case compare (Array.length b.users) (Array.length a.users) of
            EQ -> compare a.name b.name
            ord -> ord
        )

    cols = if Array.length depEntries > 30 then 3 else 2
  in
    HH.div
      [ HP.style "max-width: 1200px; margin: 20px auto 24px; padding: 0 32px;" ]
      [ -- Section label
        HH.div
          [ HP.style "font-size: 11px; font-weight: bold; color: #999; margin: 0 0 8px; text-transform: uppercase; letter-spacing: 0.5px;" ]
          [ HH.text "dependency matrix" ]
      -- Legend
      , HH.div
          [ HP.style "display: flex; gap: 16px; flex-wrap: wrap; margin: 0 0 12px; font-size: 11px; color: #555;" ]
          (map renderLegendEntry wsEntries)
      -- Matrix panel
      , HH.div
          [ HP.style $ "background: #fff; border: 1px solid #e0ddd4; border-radius: 4px; padding: 12px 16px; "
              <> "display: grid; grid-template-columns: repeat(" <> show cols <> ", 1fr); gap: 0 24px;"
          ]
          (map (renderDepRow wsEntries) depEntries)
      ]

-- | Legend entry for a workspace package
renderLegendEntry :: forall w i. WorkspaceEntry -> HH.HTML w i
renderLegendEntry entry =
  let
    isApp = entry.pkg.bundleModule /= Nothing
    label = entry.pkg.name <> if isApp then " (app)" else ""
  in
    HH.span
      [ HP.style "display: inline-flex; align-items: center; gap: 4px;" ]
      [ HH.span
          [ HP.style $ "display: inline-block; width: 8px; height: 8px; border-radius: 50%; background: " <> entry.color <> ";" ]
          []
      , HH.text label
      ]

-- | A single row in the dependency matrix
renderDepRow :: forall w i. Array WorkspaceEntry -> DepEntry -> HH.HTML w i
renderDepRow wsEntries entry =
  HH.div
    [ HP.style "display: flex; align-items: center; gap: 8px; padding: 1px 0; font-size: 11px; line-height: 1.5;" ]
    [ -- Dots column
      HH.span
        [ HP.style $ "display: flex; gap: 3px; min-width: " <> show (Array.length wsEntries * 11) <> "px;" ]
        (Array.mapWithIndex (\i we ->
          HH.span
            [ HP.style $ "display: inline-block; width: 7px; height: 7px; border-radius: 50%; box-sizing: border-box; "
                <> if elem i entry.users
                   then "background: " <> we.color <> ";"
                   else "border: 1px solid #ddd; background: transparent;"
            ] []
        ) wsEntries)
    -- Dep name (colored by resolved source)
    , HH.span
        [ HP.style $ case entry.resolvedSource of
            "workspace" -> "color: hsl(40, 85%, 40%); font-weight: bold;"
            "extra"     -> "color: hsl(15, 80%, 50%);"
            _           -> "color: #555;"
        ]
        [ HH.text entry.name ]
    -- Source qualifier for non-registry packages
    , case entry.resolvedSource of
        "workspace" -> HH.span [ HP.style "font-size: 9px; color: #bbb; font-style: italic;" ] [ HH.text "workspace" ]
        "extra"     -> HH.span [ HP.style "font-size: 9px; color: hsl(15, 80%, 50%); font-style: italic;" ] [ HH.text "override" ]
        _           -> HH.text ""
    ]

-- =============================================================================
-- Action Handlers
-- =============================================================================

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  Initialize -> do
    state <- H.get
    log $ "[ProjectAnatomyViz] Initializing with " <> show (Array.length state.packages) <> " packages"

    -- Set up listener for D3 click callbacks
    { emitter, listener } <- liftEffect HS.create
    void $ H.subscribe emitter
    H.modify_ _ { actionListener = Just listener }

    -- Fetch unused packages from API
    unusedResult <- liftAff Loader.fetchUnusedPackages
    let unusedPkgs = case unusedResult of
          Right pkgs -> map Loader.v2PackageToPackageSetPackage pkgs
          Left _ -> []
    let stats = computeStats state.packages (Array.length unusedPkgs)
    H.modify_ _ { unusedPackages = unusedPkgs, stats = stats }

    when (Array.length state.packages > 0) do
      startVisualization state.packages

  Receive input -> do
    state <- H.get
    let packagesChanged = Array.length input.packages /= Array.length state.packages
    when packagesChanged do
      let stats = computeStats input.packages (Array.length state.unusedPackages)
      H.modify_ _ { packages = input.packages, stats = stats }
      startVisualization input.packages

  HandlePackageClick pkgName -> do
    log $ "[ProjectAnatomyViz] Package clicked: " <> pkgName
    H.raise (PackageClicked pkgName)

  GoToGalaxy -> do
    log "[ProjectAnatomyViz] Navigate to Galaxy"
    H.raise NavigateToGalaxy

  GoToProjects -> do
    log "[ProjectAnatomyViz] Navigate to Projects"
    H.raise NavigateToProjects

  Finalize -> do
    state <- H.get
    case state.handle of
      Just handle -> liftEffect handle.stop
      Nothing -> pure unit
    liftEffect $ Beeswarm.cleanup C.anatomyBeeswarmContainer

-- | Start (or restart) the beeswarm visualization
startVisualization :: forall m. MonadAff m => Array Loader.PackageSetPackage -> H.HalogenM State Action () Output m Unit
startVisualization packages = do
  state <- H.get

  -- Stop existing
  case state.handle of
    Just h -> liftEffect h.stop
    Nothing -> pure unit

  let maxLayer = foldl (\acc p -> max acc p.topoLayer) 0 packages

  -- Build click handler via listener
  let mClickHandler = case state.actionListener of
        Just listener -> Just $ \pkgName ->
          HS.notify listener (HandlePackageClick pkgName)
        Nothing -> Nothing

  let config :: Beeswarm.Config
      config =
        { containerSelector: C.anatomyBeeswarmContainer
        , width: 1200.0
        , height: 480.0
        , maxTopoLayer: maxLayer
        , onClick: mClickHandler
        , unusedPackages: state.unusedPackages
        }

  handle <- liftEffect $ Beeswarm.render config packages
  H.modify_ _ { handle = Just handle }
  log "[ProjectAnatomyViz] Beeswarm handle stored"
