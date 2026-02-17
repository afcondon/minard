-- | Module Treemap Enriched Visualization Component
-- |
-- | A Halogen component wrapper for the enriched module treemap visualization.
-- | Displays modules within a package as a treemap with individual declaration
-- | circles, hover tooltips, and click-to-panel support.
module CE2.Component.ModuleTreemapEnrichedViz
  ( component
  , Input
  , Output(..)
  , Query(..)
  , Slot
  ) where

import Prelude

import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS

import CE2.Containers as C
import CE2.Data.Loader as Loader
import CE2.Types (ColorMode, PackageReachability, PackageClusters, PackagePurity)
import CE2.Viz.ModuleTreemapEnriched as ModuleTreemapEnriched

-- =============================================================================
-- Types
-- =============================================================================

-- | Input from parent - includes declarations and function calls for dependencies
type Input =
  { packageName :: String
  , modules :: Array Loader.V2ModuleListItem
  , imports :: Array Loader.V2ModuleImports
  , declarations :: Map Int (Array Loader.V2Declaration)
  , functionCalls :: Map Int (Array Loader.V2FunctionCall)
  , gitStatus :: Maybe Loader.GitStatusData  -- For git status coloring
  , colorMode :: ColorMode                   -- Current color mode
  , reachabilityData :: Maybe PackageReachability  -- For reachability coloring
  , reachabilityPeek :: Boolean              -- True while R key held (show overlay)
  , clusterData :: Maybe PackageClusters     -- For cluster coloring
  , isAppPackage :: Boolean                  -- True for app packages (Main entry point)
  , purityData :: Maybe PackagePurity        -- For purity peek overlay
  , purityPeek :: Boolean                    -- True while P key held (show overlay)
  }

-- | Output to parent
data Output
  = ModuleClicked String String  -- packageName, moduleName
  | ModuleHovered (Maybe String) -- moduleName (for future coordinated hover)
  | DeclarationClicked String String String  -- packageName, moduleName, declarationName

-- | Slot type for parent component
type Slot = H.Slot Query Output

-- | No queries needed - all control via Input
data Query a = NoQuery a

-- | Component state - only internal state, not copied from Input
type State =
  { initialized :: Boolean
  , actionListener :: Maybe (HS.Listener Action)
  , lastInput :: Input
  }

-- | Actions
data Action
  = Initialize
  | Receive Input
  | HandleModuleClick String String  -- packageName, moduleName
  | HandleDeclarationClick String String String  -- packageName, moduleName, declarationName

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
  { initialized: false
  , actionListener: Nothing
  , lastInput: input
  }

-- =============================================================================
-- Render
-- =============================================================================

render :: forall m. State -> H.ComponentHTML Action () m
render _state =
  HH.div
    [ HP.class_ (HH.ClassName "module-treemap-enriched-viz")
    , HP.style "position: relative; width: 100%; height: 100%;"
    ]
    [ HH.div
        [ HP.id C.pkgTreemapEnrichedContainerId
        , HP.class_ (HH.ClassName "module-treemap-enriched")
        , HP.style "position: absolute; top: 0; left: 0; width: 100%; height: 100%;"
        ]
        []
    ]

-- =============================================================================
-- Action Handlers
-- =============================================================================

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  Initialize -> do
    state <- H.get
    let input = state.lastInput
    log $ "[ModuleTreemapEnrichedViz] Initializing for package: " <> input.packageName
        <> ", modules=" <> show (Array.length input.modules)
        <> ", declarations=" <> show (Map.size input.declarations)
        <> ", functionCalls=" <> show (Map.size input.functionCalls)

    -- Create subscription for D3 callbacks
    { emitter, listener } <- liftEffect HS.create
    void $ H.subscribe emitter
    H.modify_ _ { actionListener = Just listener }

    when (Array.length input.modules > 0) do
      renderTreemap input

  Receive input -> do
    state <- H.get
    let lastInput = state.lastInput

    -- Check what changed
    let packageChanged = input.packageName /= lastInput.packageName
        modulesChanged = Array.length input.modules /= Array.length lastInput.modules
        declarationsChanged = Map.size input.declarations /= Map.size lastInput.declarations
        callsChanged = Map.size input.functionCalls /= Map.size lastInput.functionCalls
        colorModeChanged = input.colorMode /= lastInput.colorMode
        gitStatusChanged = input.gitStatus /= lastInput.gitStatus
        reachabilityChanged = (input.reachabilityData <#> _.packageName) /= (lastInput.reachabilityData <#> _.packageName)
        peekChanged = input.reachabilityPeek /= lastInput.reachabilityPeek
        clusterChanged = (input.clusterData <#> _.packageName) /= (lastInput.clusterData <#> _.packageName)
        purityChanged = (input.purityData <#> _.packageName) /= (lastInput.purityData <#> _.packageName)
        purityPeekChanged = input.purityPeek /= lastInput.purityPeek

    -- Update lastInput for next comparison
    H.modify_ _ { lastInput = input }

    when (packageChanged || modulesChanged || declarationsChanged || callsChanged || colorModeChanged || gitStatusChanged || reachabilityChanged || peekChanged || clusterChanged || purityChanged || purityPeekChanged) do
      log $ "[ModuleTreemapEnrichedViz] Input changed, re-rendering"
      renderTreemap input

  HandleModuleClick pkgName modName -> do
    log $ "[ModuleTreemapEnrichedViz] Module clicked: " <> pkgName <> "/" <> modName
    H.raise (ModuleClicked pkgName modName)

  HandleDeclarationClick pkgName modName declName -> do
    log $ "[ModuleTreemapEnrichedViz] Declaration clicked: " <> pkgName <> "/" <> modName <> "/" <> declName
    H.raise (DeclarationClicked pkgName modName declName)

-- | Render the enriched treemap visualization
renderTreemap :: forall m. MonadAff m => Input -> H.HalogenM State Action () Output m Unit
renderTreemap input = do
  state <- H.get

  -- Filter to this package's modules
  -- Note: filter imports by module name (not ID) because allImports may resolve
  -- to different module IDs than listModules when registry packages have multiple versions
  let pkgModules = Array.filter (\m -> m.package.name == input.packageName) input.modules
      pkgModuleNames = Set.fromFoldable $ map _.name pkgModules
      pkgImports = Array.filter (\imp -> Set.member imp.moduleName pkgModuleNames) input.imports

  log $ "[ModuleTreemapEnrichedViz] Rendering " <> show (Array.length pkgModules)
      <> " modules, " <> show (Array.length pkgImports) <> " import records"
      <> ", " <> show (Map.size input.declarations) <> " declaration sets"
      <> ", " <> show (Map.size input.functionCalls) <> " call sets"

  -- Create click callbacks
  let onModuleClick = makeClickCallback input.packageName state.actionListener
      onDeclarationClick = makeDeclarationClickCallback state.actionListener

  -- Render enriched treemap
  liftEffect $ ModuleTreemapEnriched.render
    { containerSelector: C.pkgTreemapEnrichedContainer
    , width: 1650.0
    , height: 900.0
    , packageName: input.packageName
    , onModuleClick: Just onModuleClick
    , onDeclarationClick: Just onDeclarationClick
    , colorMode: input.colorMode
    , gitStatus: input.gitStatus
    , reachabilityData: input.reachabilityData
    , reachabilityPeek: input.reachabilityPeek
    , clusterData: input.clusterData
    , isAppPackage: input.isAppPackage
    , purityData: input.purityData
    , purityPeek: input.purityPeek
    }
    pkgModules
    pkgImports
    input.declarations
    input.functionCalls

  H.modify_ _ { initialized = true }

-- | Create a click callback that notifies the Halogen listener
makeClickCallback :: String -> Maybe (HS.Listener Action) -> String -> String -> Effect Unit
makeClickCallback _defaultPkg mListener pkgName modName = case mListener of
  Just listener -> HS.notify listener (HandleModuleClick pkgName modName)
  Nothing -> log $ "[ModuleTreemapEnrichedViz] No listener for click: " <> pkgName <> "/" <> modName

-- | Create a declaration click callback that notifies the Halogen listener
makeDeclarationClickCallback :: Maybe (HS.Listener Action) -> String -> String -> String -> Effect Unit
makeDeclarationClickCallback mListener pkgName modName declName = case mListener of
  Just listener -> HS.notify listener (HandleDeclarationClick pkgName modName declName)
  Nothing -> log $ "[ModuleTreemapEnrichedViz] No listener for decl click: " <> pkgName <> "/" <> modName <> "/" <> declName
