-- | Namespace Tree Visualization Component
-- |
-- | Renders the module namespace hierarchy as a radial tidy tree with a
-- | package-based filter panel. Packages are classified as Workspace (gold),
-- | Direct dependency (blue), or Transitive (gray). Default: only workspace
-- | packages checked → small, legible tree.
module CE2.Component.NamespaceTreeViz
  ( component
  , Input
  , Output(..)
  , Query
  , Slot
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Data.Set (Set)
import Data.Set as Set
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS

import Hylograph.HATS.InterpreterTick (clearContainer)

import CE2.Data.Loader as Loader
import CE2.Data.PackageCategory (PackageCategory(..), classify, computeDirectDepNames, categoryColor)
import CE2.Types (ViewTheme)
import CE2.Viz.NamespaceTree as NamespaceTree

-- =============================================================================
-- Types
-- =============================================================================

-- | Input from parent
type Input =
  { namespaceTree :: Array Loader.V2NamespaceTreeNode
  , namespacePackages :: Maybe (Array Loader.NamespacePackageEntry)
  , packages :: Array Loader.V2Package
  , theme :: ViewTheme
  }

-- | No queries
data Query (a :: Type)

-- | Output to parent
data Output = NavigateToPackage String  -- dominant packageName

-- | Slot type for parent component
type Slot = H.Slot Query Output

-- | Component state
type State =
  { lastInput :: Input
  , selectedPackages :: Set String  -- checked package names
  , initialized :: Boolean          -- true after first render with packages
  , actionListener :: Maybe (HS.Listener Action)
  }

-- | Actions
data Action
  = Initialize
  | Receive Input
  | Finalize
  | TogglePackage String
  | SelectCategory PackageCategory
  | ClearCategory PackageCategory
  | HandleNodeClick String  -- packageName

-- | A classified package for the filter panel
type ClassifiedPackage =
  { name :: String
  , category :: PackageCategory
  }

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
  { lastInput: input
  , selectedPackages: defaultSelection input.packages
  , initialized: false
  , actionListener: Nothing
  }

-- | Default selection: workspace packages only
defaultSelection :: Array Loader.V2Package -> Set String
defaultSelection packages =
  Set.fromFoldable $ Array.mapMaybe
    (\p -> if p.source == "workspace" then Just p.name else Nothing)
    packages

-- =============================================================================
-- Render
-- =============================================================================

containerId :: String
containerId = "namespace-tree-container"

containerSelector :: String
containerSelector = "#" <> containerId

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div
    [ HP.class_ (HH.ClassName "namespace-tree-layout")
    , HP.style "position: absolute; top: 0; left: 0; width: 100%; height: 100%; display: flex; flex-direction: row;"
    ]
    [ -- Filter panel (left sidebar)
      renderFilterPanel state
    , -- SVG container (fills remaining space)
      HH.div
        [ HP.id containerId
        , HP.class_ (HH.ClassName "namespace-tree")
        , HP.style "flex: 1; position: relative; overflow: auto;"
        ]
        []
    ]

-- | Render the filter sidebar
renderFilterPanel :: forall m. State -> H.ComponentHTML Action () m
renderFilterPanel state =
  let
    input = state.lastInput
    pkgs = input.packages
    wsNames = Set.fromFoldable $ Array.mapMaybe
      (\p -> if p.source == "workspace" then Just p.name else Nothing) pkgs
    directDepNames = computeDirectDepNames pkgs

    classifiedPkgs :: Array ClassifiedPackage
    classifiedPkgs = pkgs <#> \p ->
      { name: p.name
      , category: classify wsNames directDepNames p
      }

    byCategory :: PackageCategory -> Array ClassifiedPackage
    byCategory cat = Array.filter (\p -> p.category == cat) classifiedPkgs
                   # Array.sortWith _.name

    workspacePkgs = byCategory Workspace
    directPkgs = byCategory DirectDep
    transitivePkgs = byCategory Transitive
  in
    HH.div
      [ HP.class_ (HH.ClassName "namespace-filter-panel") ]
      [ HH.div [ HP.class_ (HH.ClassName "namespace-filter-title") ]
          [ HH.text "Packages" ]
      , renderFilterGroup state "Workspace" Workspace (categoryColor Workspace) workspacePkgs
      , renderFilterGroup state "Direct" DirectDep (categoryColor DirectDep) directPkgs
      , renderFilterGroup state "Transitive" Transitive (categoryColor Transitive) transitivePkgs
      ]

-- | Render a collapsible group of package checkboxes
renderFilterGroup :: forall m. State -> String -> PackageCategory -> String -> Array ClassifiedPackage -> H.ComponentHTML Action () m
renderFilterGroup state label cat color pkgs =
  HH.div
    [ HP.class_ (HH.ClassName "namespace-filter-group") ]
    [ HH.div
        [ HP.class_ (HH.ClassName "namespace-filter-group-header") ]
        [ HH.span
            [ HP.class_ (HH.ClassName "namespace-filter-swatch")
            , HP.style $ "background-color: " <> color <> ";"
            ]
            []
        , HH.span [ HP.class_ (HH.ClassName "namespace-filter-group-label") ]
            [ HH.text $ label <> " (" <> show (Array.length pkgs) <> ")" ]
        , HH.span [ HP.class_ (HH.ClassName "namespace-filter-actions") ]
            [ HH.a
                [ HP.class_ (HH.ClassName "namespace-filter-action")
                , HE.onClick \_ -> SelectCategory cat
                ]
                [ HH.text "All" ]
            , HH.text " "
            , HH.a
                [ HP.class_ (HH.ClassName "namespace-filter-action")
                , HE.onClick \_ -> ClearCategory cat
                ]
                [ HH.text "None" ]
            ]
        ]
    , HH.div
        [ HP.class_ (HH.ClassName "namespace-filter-items") ]
        (pkgs <#> renderCheckbox state)
    ]

-- | Render a single package checkbox
renderCheckbox :: forall m. State -> ClassifiedPackage -> H.ComponentHTML Action () m
renderCheckbox state pkg =
  let isChecked = Set.member pkg.name state.selectedPackages
  in
    HH.label
      [ HP.class_ (HH.ClassName "namespace-filter-checkbox")
      ]
      [ HH.input
          [ HP.type_ HP.InputCheckbox
          , HP.checked isChecked
          , HE.onClick \_ -> TogglePackage pkg.name
          ]
      , HH.text (" " <> pkg.name)
      ]

-- =============================================================================
-- Action Handlers
-- =============================================================================

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  Initialize -> do
    state <- H.get
    let input = state.lastInput
    log $ "[NamespaceTreeViz] Initializing with " <> show (Array.length input.namespaceTree) <> " nodes"

    -- Set up subscription for viz click callbacks -> Halogen actions
    { emitter, listener } <- liftEffect HS.create
    void $ H.subscribe emitter
    H.modify_ _ { actionListener = Just listener }

    renderFilteredTree state

  Receive input -> do
    state <- H.get
    let lastInput = state.lastInput
        dataChanged = Array.length input.namespaceTree /= Array.length lastInput.namespaceTree
        themeChanged = input.theme /= lastInput.theme
        packagesChanged = Array.length input.packages /= Array.length lastInput.packages
        nsPkgChanged = case input.namespacePackages, lastInput.namespacePackages of
          Just a, Just b -> Array.length a /= Array.length b
          Nothing, Nothing -> false
          _, _ -> true

    -- Update selection when packages first arrive
    let newSelection =
          if packagesChanged && not state.initialized
          then defaultSelection input.packages
          else state.selectedPackages

    H.modify_ _
      { lastInput = input
      , selectedPackages = newSelection
      , initialized = state.initialized || packagesChanged
      }

    when (dataChanged || themeChanged || packagesChanged || nsPkgChanged) do
      log "[NamespaceTreeViz] Input changed, re-rendering"
      newState <- H.get
      renderFilteredTree newState

  Finalize -> do
    log "[NamespaceTreeViz] Finalizing"
    liftEffect $ clearContainer containerSelector

  TogglePackage pkgName -> do
    state <- H.get
    let sel = state.selectedPackages
        newSel = if Set.member pkgName sel
                 then Set.delete pkgName sel
                 else Set.insert pkgName sel
    H.modify_ _ { selectedPackages = newSel }
    newState <- H.get
    renderFilteredTree newState

  SelectCategory cat -> do
    state <- H.get
    let pkgNames = packageNamesForCategory cat state.lastInput.packages
        newSel = Set.union state.selectedPackages pkgNames
    H.modify_ _ { selectedPackages = newSel }
    newState <- H.get
    renderFilteredTree newState

  ClearCategory cat -> do
    state <- H.get
    let pkgNames = packageNamesForCategory cat state.lastInput.packages
        newSel = Set.difference state.selectedPackages pkgNames
    H.modify_ _ { selectedPackages = newSel }
    newState <- H.get
    renderFilteredTree newState

  HandleNodeClick pkgName -> do
    log $ "[NamespaceTreeViz] Node clicked, navigating to package: " <> pkgName
    H.raise (NavigateToPackage pkgName)

-- | Get all package names in a category
packageNamesForCategory :: PackageCategory -> Array Loader.V2Package -> Set String
packageNamesForCategory cat packages =
  let
    wsNames = Set.fromFoldable $ Array.mapMaybe
      (\p -> if p.source == "workspace" then Just p.name else Nothing) packages
    directDepNames = computeDirectDepNames packages
  in
    Set.fromFoldable $ Array.mapMaybe
      (\p -> if classify wsNames directDepNames p == cat then Just p.name else Nothing)
      packages

-- | Build the nsPkgMap and packageCategories, then delegate to Viz.NamespaceTree
renderFilteredTree :: forall m. MonadAff m => State -> H.HalogenM State Action () Output m Unit
renderFilteredTree state = do
  let
    input = state.lastInput

    -- Build namespace→packages map
    nsPkgMap :: Map.Map Int (Array { packageName :: String, moduleCount :: Int })
    nsPkgMap = case input.namespacePackages of
      Nothing -> Map.empty
      Just entries ->
        foldl (\acc e ->
          let existing = fromMaybe [] (Map.lookup e.namespaceId acc)
              entry = { packageName: e.packageName, moduleCount: e.moduleCount }
          in Map.insert e.namespaceId (Array.snoc existing entry) acc
        ) Map.empty entries

    -- Build package→category map
    wsNames = Set.fromFoldable $ Array.mapMaybe
      (\p -> if p.source == "workspace" then Just p.name else Nothing) input.packages
    directDepNames = computeDirectDepNames input.packages

    packageCategories :: Map.Map String PackageCategory
    packageCategories = Map.fromFoldable $ input.packages <#> \p ->
      Tuple p.name (classify wsNames directDepNames p)

    clickHandler = case state.actionListener of
      Just listener -> Just $ \pkgName ->
        HS.notify listener (HandleNodeClick pkgName)
      Nothing -> Nothing

    config =
      { containerSelector
      , theme: input.theme
      , selectedPackages: state.selectedPackages
      , nsPkgMap
      , packageCategories
      , onNodeClick: clickHandler
      }

  -- Don't render until we have the namespace→packages mapping;
  -- otherwise the fallback shows ALL 2115 nodes unfiltered.
  when (Map.size nsPkgMap > 0) do
    log $ "[NamespaceTreeViz] renderFilteredTree: selectedPkgs=" <> show (Set.size state.selectedPackages)
      <> " nsPkgMap=" <> show (Map.size nsPkgMap)
      <> " treeNodes=" <> show (Array.length input.namespaceTree)
      <> " packages=" <> show (Array.length input.packages)
    liftEffect $ NamespaceTree.render config input.namespaceTree
