-- | Commit Sparkline Visualization Component
-- |
-- | Reusable sparkline showing per-commit line changes for a single module.
-- | Self-contained: fetches numstat data, renders via Canvas 2D.
-- | Designed to appear inline in ModuleSignatureMap and AnnotationReport.
module CE2.Component.CommitSparklineViz
  ( component
  , Input
  , Query(..)
  , Slot
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Int (round, toNumber)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Web.HTML.HTMLCanvasElement as HTMLCanvas
import CE2.Data.Loader as Loader
import CE2.Viz.CommitSparkline as Spark

-- =============================================================================
-- Types
-- =============================================================================

type Input =
  { packageName :: String
  , moduleName :: String
  }

data Query a = NoQuery a

type Slot = H.Slot Query Void

type State =
  { packageName :: String
  , moduleName :: String
  , commits :: Array Loader.NumstatCommit
  , bars :: Array Spark.SparklineBar
  , loading :: Boolean
  , error :: Maybe String
  , ctx :: Maybe Spark.Context2D
  , height :: Number
  }

data Action
  = Initialize
  | Receive Input

canvasRef :: H.RefLabel
canvasRef = H.RefLabel "sparkline-canvas"

-- =============================================================================
-- Component
-- =============================================================================

component :: forall m. MonadAff m => H.Component Query Input Void m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , receive = Just <<< Receive
      , initialize = Just Initialize
      }
  }

initialState :: Input -> State
initialState input =
  { packageName: input.packageName
  , moduleName: input.moduleName
  , commits: []
  , bars: []
  , loading: true
  , error: Nothing
  , ctx: Nothing
  , height: 32.0
  }

-- =============================================================================
-- Render
-- =============================================================================

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div
    [ HP.class_ (HH.ClassName "commit-sparkline")
    , HP.style $ "display: inline-block; height: " <> show (round state.height) <> "px;"
    ]
    [ if state.loading
        then HH.span
          [ HP.style "font-size: 10px; color: #aaa;" ]
          [ HH.text "..." ]
        else case state.error of
          Just _ -> HH.text ""
          Nothing ->
            HH.canvas
              [ HP.ref canvasRef
              , HP.width (Array.length state.bars)
              , HP.height (round state.height)
              , HP.style $ "width: " <> show (Array.length state.bars) <> "px; height: " <> show (round state.height) <> "px;"
              , HP.title $ show (Array.length state.bars) <> " commits for " <> state.moduleName
              ]
    ]

-- =============================================================================
-- Action Handlers
-- =============================================================================

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action () Void m Unit
handleAction = case _ of
  Initialize -> loadAndRender

  Receive input -> do
    state <- H.get
    when (input.packageName /= state.packageName || input.moduleName /= state.moduleName) do
      H.modify_ _ { packageName = input.packageName, moduleName = input.moduleName, loading = true }
      loadAndRender

loadAndRender :: forall m. MonadAff m => H.HalogenM State Action () Void m Unit
loadAndRender = do
  state <- H.get
  H.modify_ _ { loading = true, error = Nothing }
  result <- liftAff $ Loader.fetchModuleNumstat 500 state.packageName
  case result of
    Left err -> do
      log $ "[CommitSparkline] Error: " <> err
      H.modify_ _ { loading = false, error = Just err }
    Right commits -> do
      let bars = Spark.prepareData state.moduleName commits
      log $ "[CommitSparkline] " <> state.moduleName <> ": " <> show (Array.length bars) <> " commits"
      H.modify_ _
        { loading = false
        , commits = commits
        , bars = bars
        }
      -- Canvas is rendered by Halogen after this state change;
      -- we need to get the context after the next render cycle
      renderSparkline

renderSparkline :: forall m. MonadAff m => H.HalogenM State Action () Void m Unit
renderSparkline = do
  state <- H.get
  mElem <- H.getRef canvasRef
  case mElem >>= HTMLCanvas.fromElement of
    Just canvas -> do
      ctx <- liftEffect $ Spark.getContext2D canvas
      H.modify_ _ { ctx = Just ctx }
      let width = toNumber (Array.length state.bars)
      liftEffect $ Spark.render ctx { width, height: state.height } state.bars
    Nothing ->
      log "[CommitSparkline] Canvas not found (will retry on next render)"
