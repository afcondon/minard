-- | Landing Page
-- |
-- | Shows branding, view matrix, system Sankey diagram, and CTA.
module CE2.Component.LandingPageViz
  ( component
  , Input
  , Output(..)
  , Query(..)
  , Slot
  ) where

import Prelude

import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import CE2.Scene (Scene(..)) as Scene
import CE2.Viz.SystemSankey as SystemSankey
import Data.Maybe (Maybe(..))

-- =============================================================================
-- Types
-- =============================================================================

type Input = { dataReady :: Boolean }

data Output = NavigateToScene Scene.Scene

data Query (a :: Type)

type Slot = H.Slot Query Output

type State = { dataReady :: Boolean }

data Action
  = Initialize
  | Receive Input
  | GoToScene Scene.Scene

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
initialState input = { dataReady: input.dataReady }

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  Initialize ->
    liftEffect $ SystemSankey.renderSystemSankey "#system-sankey-container" 900.0 500.0
  Receive input ->
    H.modify_ _ { dataReady = input.dataReady }
  GoToScene scene ->
    H.raise (NavigateToScene scene)

-- =============================================================================
-- Render
-- =============================================================================

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div
    [ HP.style containerStyle ]
    [ HH.div
        [ HP.style "max-width: 1200px; width: 100%; margin: 0 auto; padding: 60px 24px 80px;" ]
        [ renderHero state
        , renderViewMatrix
        , renderSystemDiagram
        , renderGetStarted
        ]
    ]

containerStyle :: String
containerStyle = "width: 100%; height: 100%; overflow-y: auto; background: #FAFAF8; "
  <> "font-family: -apple-system, 'Helvetica Neue', Helvetica, Arial, sans-serif; "
  <> "color: #333;"

-- =============================================================================
-- Hero
-- =============================================================================

renderHero :: forall m. State -> H.ComponentHTML Action () m
renderHero state =
  HH.div
    [ HP.style "text-align: center; margin-bottom: 56px;" ]
    [ -- Logotype
      HH.div
        [ HP.style "margin-bottom: 12px;" ]
        [ HH.span
            [ HP.style $ "font-size: 48px; font-weight: 700; letter-spacing: -1.5px; "
                <> "color: #2C2C2C; font-family: 'Georgia', 'Times New Roman', serif;"
            ]
            [ HH.text "Minard" ]
        ]
    -- Tagline
    , HH.p
        [ HP.style "font-size: 15px; color: #888; margin: 0 0 6px 0; letter-spacing: 0.5px;" ]
        [ HH.text "Code cartography for PureScript" ]
    , HH.p
        [ HP.style "font-size: 12px; color: #AAA; margin: 0 0 24px 0; font-style: italic;" ]
        [ HH.text "build: 2026-03-16 12:45" ]

    -- Navigation links
    , HH.div
        [ HP.style "display: flex; gap: 12px; justify-content: center; flex-wrap: wrap;" ]
        [ navLink "Maps" Scene.GalaxyTreemap state.dataReady
        , navLink "Reports" Scene.PackageReport state.dataReady
        , navLink "Anatomy" Scene.ProjectAnatomy state.dataReady
        , navLink "Git" Scene.GitOverview state.dataReady
        ]

    -- Project context bar
    , HH.div
        [ HP.style "margin-top: 24px; padding: 12px 20px; background: #fff; border: 1px solid #E0DDD4; border-radius: 8px; display: flex; align-items: center; justify-content: space-between;" ]
        [ HH.div [ HP.style "display: flex; align-items: baseline; gap: 8px;" ]
            [ HH.span [ HP.style "font-size: 11px; color: #888;" ] [ HH.text "Current project:" ]
            , HH.span [ HP.style "font-size: 13px; font-weight: 600; color: #333;" ] [ HH.text "minard" ]
            ]
        , HH.div [ HP.style "display: flex; align-items: center; gap: 8px;" ]
            [ HH.button
                [ HE.onClick \_ -> GoToScene Scene.ProjectSetup
                , HP.style "padding: 4px 12px; border: 1px solid #C0BDB4; border-radius: 4px; cursor: pointer; font-size: 11px; background: #fff; color: #555;"
                ]
                [ HH.text "Switch project" ]
            ]
        ]
    ]
  where
  navLink label scene enabled =
    HH.button
      [ HE.onClick \_ -> GoToScene scene
      , HP.style $ "padding: 8px 20px; border: 1px solid "
          <> (if enabled then "#C0BDB4" else "#E0DDD4")
          <> "; border-radius: 20px; cursor: "
          <> (if enabled then "pointer" else "default")
          <> "; font-size: 13px; font-weight: 500; background: "
          <> (if enabled then "#fff" else "#FAFAF8")
          <> "; color: " <> (if enabled then "#444" else "#BBB")
          <> "; transition: all 150ms ease;"
      , HP.disabled (not enabled)
      ]
      [ HH.text label ]

-- =============================================================================
-- View Matrix — screenshot grid showing all view categories × depth levels
-- =============================================================================

renderViewMatrix :: forall w. HH.HTML w Action
renderViewMatrix =
  HH.div
    [ HP.style "margin-bottom: 48px;" ]
    [ HH.h2
        [ HP.style sectionHeadingStyle ]
        [ HH.text "Four Lenses, Three Levels Deep" ]
    , HH.p
        [ HP.style sectionBodyStyle ]
        [ HH.text "Every lens drills from the full registry down to individual modules. Click any screenshot to jump in." ]
    -- Column headers
    , HH.div
        [ HP.style "display: grid; grid-template-columns: 80px repeat(4, 1fr); gap: 8px; margin-top: 20px;" ]
        ( [ HH.div [] [] -- empty top-left corner
          , colHeader "Maps" "Structure"
          , colHeader "Reports" "Quality"
          , colHeader "Anatomy" "Architecture"
          , colHeader "Git" "History"
          -- Row 1: Project level
          , rowLabel "Project"
          , viewThumb "screenshots/maps-1.jpg" "Galaxy treemap" Scene.GalaxyTreemap
          , viewThumb "screenshots/report.jpg" "Package report" Scene.PackageReport
          , viewThumb "screenshots/anatomy.jpg" "Project anatomy" Scene.ProjectAnatomy
          , viewThumb "screenshots/git-1.jpg" "Git overview" Scene.GitOverview
          -- Row 2: Package level
          , rowLabel "Package"
          , viewThumb "screenshots/maps-2.jpg" "Module treemap" (Scene.PkgTreemap "minard-frontend")
          , viewThumb "screenshots/reports-2.jpg" "Module annotations" Scene.AnnotationReport
          , viewThumb "screenshots/anatomy-2-deomposition.jpg" "Decomposition" (Scene.PackageAnatomy "minard-frontend")
          , viewThumb "screenshots/git-2.jpg" "Commit grid" (Scene.CommitModuleGrid "minard-frontend")
          -- Row 3: Module level
          , rowLabel "Module"
          , viewThumb "screenshots/maps-3.jpg" "Module structure" (Scene.ModuleStructure "minard-frontend" "CE2.Component.SceneCoordinator")
          , emptyCell
          , viewThumb "screenshots/anatomy-2-layers.jpg" "Layer analysis" (Scene.PackageAnatomy "minard-frontend")
          , viewThumb "screenshots/git-3.jpg" "Signatures + blame" (Scene.ModuleSignatures "minard-frontend" "CE2.Component.SceneCoordinator")
          ]
        )
    ]
  where
  colHeader label subtitle =
    HH.div [ HP.style "text-align: center; padding-bottom: 4px;" ]
      [ HH.div [ HP.style "font-size: 12px; font-weight: 600; color: #333;" ] [ HH.text label ]
      , HH.div [ HP.style "font-size: 9px; color: #999;" ] [ HH.text subtitle ]
      ]
  rowLabel label =
    HH.div [ HP.style "display: flex; align-items: center; justify-content: flex-end; padding-right: 8px;" ]
      [ HH.span [ HP.style "font-size: 10px; font-weight: 600; color: #888; text-transform: uppercase; letter-spacing: 0.5px; writing-mode: horizontal-tb;" ]
          [ HH.text label ]
      ]
  viewThumb src caption scene =
    HH.div
      [ HP.style "cursor: pointer; border: 1px solid #E0DDD4; border-radius: 4px; overflow: hidden; transition: border-color 150ms ease, box-shadow 150ms ease;"
      , HE.onClick \_ -> GoToScene scene
      ]
      [ HH.img
          [ HP.src src
          , HP.style "width: 100%; height: auto; display: block;"
          ]
      , HH.div [ HP.style "padding: 4px 6px; font-size: 9px; color: #888; text-align: center; background: #fff;" ]
          [ HH.text caption ]
      ]
  emptyCell =
    HH.div [ HP.style "display: flex; align-items: center; justify-content: center; border: 1px dashed #E0DDD4; border-radius: 4px; min-height: 80px;" ]
      [ HH.span [ HP.style "font-size: 9px; color: #ccc;" ] [ HH.text "\x2014" ] ]

-- =============================================================================
-- Get Started CTA
-- =============================================================================

-- =============================================================================
-- System Diagram (HATS Sankey)
-- =============================================================================

renderSystemDiagram :: forall m. H.ComponentHTML Action () m
renderSystemDiagram =
  HH.div
    [ HP.style "margin-bottom: 48px;" ]
    [ HH.h2
        [ HP.style sectionHeadingStyle ]
        [ HH.text "How It Works" ]
    , HH.p
        [ HP.style sectionBodyStyle ]
        [ HH.text "Source code, git history, and AI analysis flow through a Rust loader into DuckDB, then out through four families of interactive visualizations \x2014 all connecting back to your editor." ]
    , HH.div
        [ HP.id "system-sankey-container"
        , HP.style "width: 100%; min-height: 300px; margin: 16px 0; border: 1px solid #E8E6E0; border-radius: 6px; background: #fff; overflow: hidden;"
        ]
        []
    ]

-- =============================================================================
-- Get Started CTA
-- =============================================================================

renderGetStarted :: forall m. H.ComponentHTML Action () m
renderGetStarted =
  HH.div
    [ HP.style "margin-bottom: 48px;" ]
    [ HH.h2
        [ HP.style sectionHeadingStyle ]
        [ HH.text "Get Started" ]
    , HH.p
        [ HP.style sectionBodyStyle ]
        [ HH.text "Explore Minard's own codebase, or add your PureScript project." ]
    , HH.div
        [ HP.style "display: flex; gap: 12px; margin-top: 16px;" ]
        [ HH.button
            [ HE.onClick \_ -> GoToScene Scene.GalaxyTreemap
            , HP.style "padding: 12px 28px; border: none; border-radius: 6px; cursor: pointer; font-size: 14px; font-weight: 600; background: #2D7D46; color: white; letter-spacing: 0.3px;"
            ]
            [ HH.text "Explore Minard" ]
        , HH.button
            [ HE.onClick \_ -> GoToScene Scene.ProjectSetup
            , HP.style "padding: 12px 28px; border: 1px solid #C0BDB4; border-radius: 6px; cursor: pointer; font-size: 14px; font-weight: 500; background: #fff; color: #555; letter-spacing: 0.3px;"
            ]
            [ HH.text "Add your project" ]
        ]
    ]

-- =============================================================================
-- Shared Styles
-- =============================================================================

sectionHeadingStyle :: String
sectionHeadingStyle = "font-size: 18px; font-weight: 600; margin: 0 0 8px 0; letter-spacing: -0.3px; color: #333;"

sectionBodyStyle :: String
sectionBodyStyle = "font-size: 15px; color: #666; margin: 0 0 4px 0; line-height: 1.6; max-width: 800px;"
