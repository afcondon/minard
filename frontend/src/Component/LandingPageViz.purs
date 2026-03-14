-- | Landing Page
-- |
-- | Stateless presentation component for the Minard landing page.
-- | Shows branding, navigation links, architecture diagram, and CTA.
module CE2.Component.LandingPageViz
  ( component
  , Input
  , Output(..)
  , Query(..)
  , Slot
  ) where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import CE2.Util.SVG (svgElem, sa)
import CE2.Scene (Scene(..)) as Scene
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
  = Receive Input
  | GoToScene Scene.Scene

-- =============================================================================
-- Component
-- =============================================================================

component :: forall m. H.Component Query Input Output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , receive = Just <<< Receive
        }
    }

initialState :: Input -> State
initialState input = { dataReady: input.dataReady }

handleAction :: forall m. Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
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
        , renderArchitectureDiagram
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
        [ HH.text "Lifting the fog of war since 2026" ]

    -- Navigation links
    , HH.div
        [ HP.style "display: flex; gap: 12px; justify-content: center; flex-wrap: wrap;" ]
        [ navLink "Galaxy" Scene.GalaxyTreemap state.dataReady
        , navLink "Reports" Scene.PackageReport state.dataReady
        , navLink "Anatomy" Scene.ProjectAnatomy state.dataReady
        , navLink "Projects" Scene.ProjectSetup true  -- always clickable
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
-- Architecture Diagram
-- =============================================================================

renderArchitectureDiagram :: forall w. HH.HTML w Action
renderArchitectureDiagram =
  HH.div
    [ HP.style "margin-bottom: 48px;" ]
    [ HH.h2
        [ HP.style sectionHeadingStyle ]
        [ HH.text "How It Works" ]
    , HH.p
        [ HP.style sectionBodyStyle ]
        [ HH.text "Minard is a three-stage pipeline. A Rust loader reads the compiled output of your PureScript project, extracts every package, module, and declaration into a DuckDB database, then a PureScript API server makes it all available to the browser." ]
    , svgElem "svg"
        [ sa "viewBox" "0 0 760 140"
        , HP.style "width: 100%; height: auto; display: block; margin: 20px 0;"
        ]
        [ -- Stage 1: Your Project
          archBox 0.0 20.0 180.0 100.0 "#F5F0E8" "#C0BDB4"
        , archLabel 90.0 55.0 "Your Project" "500" "13"
        , archLabel 90.0 75.0 "spago build output" "400" "10.5"
        , archLabel 90.0 91.0 "docs.json \x00D7 N" "400" "10.5"

        -- Arrow 1->2
        , archArrow 180.0 70.0 260.0 70.0
        , archLabel 220.0 62.0 "read" "400" "9"

        -- Stage 2: Loader + DB
        , archBox 260.0 20.0 200.0 100.0 "#EBF0F5" "#A0B4C8"
        , archLabel 360.0 50.0 "Rust Loader" "500" "13"
        , archLabel 360.0 68.0 "\x2193" "400" "12"
        , archLabel 360.0 86.0 "DuckDB" "500" "12"
        , archLabel 360.0 102.0 "packages \x00B7 modules \x00B7 decls" "400" "8.5"

        -- Arrow 2->3
        , archArrow 460.0 70.0 540.0 70.0
        , archLabel 500.0 62.0 "query" "400" "9"

        -- Stage 3: Minard UI
        , archBox 540.0 20.0 220.0 100.0 "#EDF5ED" "#A0C8A0"
        , archLabel 650.0 50.0 "API Server" "500" "13"
        , archLabel 650.0 68.0 "\x2193" "400" "12"
        , archLabel 650.0 86.0 "Browser" "500" "12"
        , archLabel 650.0 102.0 "20+ visualization scenes" "400" "8.5"
        ]

    -- Callout cards below diagram
    , HH.div
        [ HP.style "display: grid; grid-template-columns: 1fr 1fr 1fr; gap: 16px; margin-top: 16px;" ]
        [ calloutCard "Compile Once, Explore Everything"
            "Run spago build in your project to produce docs.json files. The Rust loader reads these along with your spago.lock to resolve the full dependency graph \x2014 every package version, every module, every exported declaration."
        , calloutCard "A Database, Not a Cache"
            "Everything goes into DuckDB \x2014 a real analytical database. Packages, modules, declarations, imports, function calls, type class instances. The API server queries it with SQL, so exploration is fast even for large codebases."
        , calloutCard "From Visualization to Editor"
            "Every view connects back to your source. Click a module or declaration to open it in VS Code. Minard runs locally alongside your editor, not in a cloud dashboard you context-switch to."
        ]
    ]

archBox :: forall w i. Number -> Number -> Number -> Number -> String -> String -> HH.HTML w i
archBox x y w h fill stroke =
  svgElem "rect"
    [ sa "x" (show x), sa "y" (show y)
    , sa "width" (show w), sa "height" (show h)
    , sa "fill" fill, sa "stroke" stroke, sa "stroke-width" "1.5"
    , sa "rx" "6"
    ] []

archLabel :: forall w i. Number -> Number -> String -> String -> String -> HH.HTML w i
archLabel x y label weight size =
  svgElem "text"
    [ sa "x" (show x), sa "y" (show y)
    , sa "text-anchor" "middle"
    , sa "font-family" "-apple-system, 'Helvetica Neue', sans-serif"
    , sa "font-size" size, sa "font-weight" weight
    , sa "fill" "#444"
    ] [ HH.text label ]

archArrow :: forall w i. Number -> Number -> Number -> Number -> HH.HTML w i
archArrow x1 y1 x2 y2 =
  svgElem "g" []
    [ svgElem "line"
        [ sa "x1" (show x1), sa "y1" (show y1)
        , sa "x2" (show (x2 - 6.0)), sa "y2" (show y2)
        , sa "stroke" "#999", sa "stroke-width" "1.5"
        ] []
    , svgElem "polygon"
        [ sa "points" (show x2 <> "," <> show y2 <> " " <> show (x2 - 8.0) <> "," <> show (y2 - 4.0) <> " " <> show (x2 - 8.0) <> "," <> show (y2 + 4.0))
        , sa "fill" "#999"
        ] []
    ]

calloutCard :: forall w i. String -> String -> HH.HTML w i
calloutCard title body =
  HH.div
    [ HP.style "background: #fff; border: 1px solid #E8E6E0; border-radius: 6px; padding: 16px;" ]
    [ HH.div
        [ HP.style "font-size: 12px; font-weight: 600; color: #444; margin-bottom: 6px;" ]
        [ HH.text title ]
    , HH.p
        [ HP.style "font-size: 11.5px; color: #666; margin: 0; line-height: 1.6;" ]
        [ HH.text body ]
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
        [ HH.text "Load a PureScript project to begin exploring." ]
    , HH.div
        [ HP.style "margin-top: 16px;" ]
        [ HH.button
            [ HE.onClick \_ -> GoToScene Scene.ProjectSetup
            , HP.style "padding: 12px 28px; border: none; border-radius: 6px; cursor: pointer; font-size: 14px; font-weight: 600; background: #2D7D46; color: white; letter-spacing: 0.3px;"
            ]
            [ HH.text "Projects \x2192" ]
        ]
    ]

-- =============================================================================
-- Shared Styles
-- =============================================================================

sectionHeadingStyle :: String
sectionHeadingStyle = "font-size: 18px; font-weight: 600; margin: 0 0 8px 0; letter-spacing: -0.3px; color: #333;"

sectionBodyStyle :: String
sectionBodyStyle = "font-size: 15px; color: #666; margin: 0 0 4px 0; line-height: 1.6; max-width: 800px;"
