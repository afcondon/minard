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
import CE2.BuildInfo as BuildInfo
import CE2.Scene (Scene(..)) as Scene
import CE2.Util.SVG (svgElem, sa)
import CE2.Viz.SystemSankey as SystemSankey
import Data.Maybe (Maybe(..))

-- =============================================================================
-- Types
-- =============================================================================

type Input = { dataReady :: Boolean }

data Output = NavigateToScene Scene.Scene

data Query (a :: Type)

type Slot = H.Slot Query Output

type State = { dataReady :: Boolean, hoveredView :: Maybe String, showInstallModal :: Boolean }

data Action
  = Initialize
  | Receive Input
  | GoToScene Scene.Scene
  | HoverView (Maybe String)
  | DismissModal

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
initialState input = { dataReady: input.dataReady, hoveredView: Nothing, showInstallModal: false }

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  Initialize ->
    liftEffect $ SystemSankey.renderSystemSankey "#system-sankey-container" 900.0 500.0
  Receive input ->
    H.modify_ _ { dataReady = input.dataReady }
  GoToScene scene -> do
    state <- H.get
    if state.dataReady
      then H.raise (NavigateToScene scene)
      else H.modify_ _ { showInstallModal = true }
  DismissModal ->
    H.modify_ _ { showInstallModal = false }
  HoverView mView ->
    H.modify_ _ { hoveredView = mView }

-- =============================================================================
-- Render
-- =============================================================================

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div
    [ HP.style containerStyle ]
    [ if state.showInstallModal then renderInstallModal else HH.text ""
    , if BuildInfo.isStaticDeploy then renderCloneBanner else HH.text ""
    , HH.div
        [ HP.style "max-width: 1200px; width: 100%; margin: 0 auto; padding: 60px 24px 80px; background: linear-gradient(180deg, #FAFAF8 0%, #F5F0E6 100%);" ]
        [ renderHero state
        , heroOrnament
        , renderScrollInvite
        , ornamentWithAnchor "section-what"
        , renderViewMatrix state
        , ornamentWithAnchor "section-how"
        , renderSystemDiagram
        , ornamentWithAnchor "section-why"
        , renderDistillation
        , ornamentWithAnchor "section-ai"
        , renderAISection
        , ornamentWithAnchor "section-api"
        , renderAPITranscript
        , sectionOrnament
        , renderGetStarted state
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
        [ HP.style "font-size: 15px; color: #888; margin: 0 0 20px 0; letter-spacing: 0.5px; font-style: italic;" ]
        [ HH.text $ "Fine code cartography since " <> BuildInfo.buildStamp ]

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
        [ HP.style "margin-top: 24px; padding: 10px 20px; background: #fff; border: 1px solid #E0DDD4; border-radius: 8px; display: inline-flex; align-items: center; gap: 24px;" ]
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
          <> "; border-radius: 20px; cursor: pointer"
          <> "; font-size: 13px; font-weight: 500; background: "
          <> (if enabled then "#fff" else "#FAFAF8")
          <> "; color: " <> (if enabled then "#444" else "#BBB")
          <> "; transition: all 150ms ease;"
      ]
      [ HH.text label ]

-- =============================================================================
-- Scroll Invite — separator between navigation and explanatory content
-- =============================================================================

renderScrollInvite :: forall m. H.ComponentHTML Action () m
renderScrollInvite =
  HH.div
    [ HP.style "margin: 24px 0; text-align: center;" ]
    [ HH.p
        [ HP.style "font-size: 28px; font-weight: 600; color: #444; margin: 0 0 16px; letter-spacing: -0.5px; font-family: 'Georgia', 'Times New Roman', serif;" ]
        [ HH.text "New to Minard?" ]
    -- Purpose paragraph
    , HH.p
        [ HP.style "font-size: 15px; color: #666; line-height: 1.7; max-width: 640px; margin: 0 auto 28px; padding: 0 16px;" ]
        [ HH.text "Minard is a new integrated approach to understanding and navigating large codebases in collaboration with agents. It is written in PureScript and Rust and aimed at PureScript codebases, with other languages coming later." ]
    -- Section links in ovals
    , HH.div
        [ HP.style "display: flex; gap: 16px; justify-content: center; flex-wrap: wrap;" ]
        [ sectionLink "#section-what" "What" "Four lenses, three levels"
        , sectionLink "#section-how" "How" "The data pipeline"
        , sectionLink "#section-why" "Why" "Distillation, not dashboard"
        , sectionLink "#section-ai" "AI" "A first-class participant"
        , sectionLink "#section-api" "API" "6 queries, 0 files read"
        ]
    ]
  where
  sectionLink href label subtitle =
    HH.a
      [ HP.href href
      , HP.style $ "text-decoration: none; padding: 10px 24px; border: 1.5px solid #D8D0BC; border-radius: 28px; "
          <> "display: inline-flex; flex-direction: column; align-items: center; min-width: 140px; "
          <> "transition: background 150ms ease, border-color 150ms ease; background: #fff;"
      ]
      [ HH.span [ HP.style "font-size: 15px; font-weight: 600; color: #444; display: block; margin-bottom: 2px;" ] [ HH.text label ]
      , HH.span [ HP.style "font-size: 10px; color: #999;" ] [ HH.text subtitle ]
      ]

-- =============================================================================
-- View Matrix — screenshot grid showing all view categories × depth levels
-- =============================================================================

renderViewMatrix :: forall w. State -> HH.HTML w Action
renderViewMatrix state =
  let hv = state.hoveredView
  in
  HH.div
    [ HP.style "margin-bottom: 16px;" ]
    [ HH.h2
        [ HP.style sectionHeadingStyle ]
        [ HH.text "Four Lenses, Three Levels Deep" ]
    , HH.p
        [ HP.style sectionBodyStyle ]
        [ HH.text "Every lens drills from the full registry down to individual modules. Hover to learn more, click to jump in." ]
    -- Column headers
    , HH.div
        [ HP.style "display: grid; grid-template-columns: 80px repeat(4, 1fr); column-gap: 8px; row-gap: 8px; margin-top: 20px;" ]
        ( [ HH.div [] [] -- empty top-left corner
          , colHeader "Maps" "Structure"
          , colHeader "Reports" "Quality"
          , colHeader "Anatomy" "Architecture"
          , colHeader "Git" "History"
          -- Row 1: Project level
          , rowLabel "Project"
          , viewThumb "galaxy" "screenshots/maps-1.jpg" "Galaxy treemap" "Packages as rectangles, modules as bubbles inside them \x2014 two levels of structure, each clickable" Scene.GalaxyTreemap hv
          , viewThumb "report" "screenshots/report.jpg" "Package report" "AI-generated quality observations for each module, with human review workflow" Scene.PackageReport hv
          , viewThumb "anatomy" "screenshots/anatomy.jpg" "Project anatomy" "Your dependency universe as a force-directed beeswarm, showing what your project actually stands on" Scene.ProjectAnatomy hv
          , viewThumb "git-overview" "screenshots/git-1.jpg" "Git overview" "Commit activity across all modules, showing where development energy is concentrated" Scene.GitOverview hv
          -- Row spacer
          , rowSpacer
          -- Row 2: Package level
          , rowLabel "Package"
          , viewThumb "treemap" "screenshots/maps-2.jpg" "Module treemap" "Modules as rectangles, declarations as bubbles \x2014 click a rectangle to enter, a bubble to jump straight to it" (Scene.PkgTreemap "minard-frontend") hv
          , viewThumb "annotations" "screenshots/reports-2.jpg" "Module annotations" "AI observations aggregated across all modules, filterable by status and type" Scene.AnnotationReport hv
          , viewThumb "decomposition" "screenshots/anatomy-2-deomposition.jpg" "Decomposition" "Biconnected component analysis revealing the structural blocks and bridges in the dependency graph" (Scene.PackageAnatomy "minard-frontend") hv
          , viewThumb "commit-grid" "screenshots/git-2.jpg" "Commit grid" "Per-module commit history as a heatmap, showing co-change patterns over time" (Scene.CommitModuleGrid "minard-frontend") hv
          -- Row spacer
          , rowSpacer
          -- Row 3: Module level — single spanning box for ModuleStructure
          , rowLabel "Module"
          , moduleStructureBox hv
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
  rowSpacer =
    HH.div [ HP.style "grid-column: 1 / -1; height: 12px;" ] []
  -- | Thumbnail with blur-others hover effect and large description below
  viewThumb viewId src caption description scene hoveredView =
    let isHovered = hoveredView == Just viewId
        anyActive = hoveredView /= Nothing
        dimmed = anyActive && not isHovered
        filterStyle = if dimmed then "filter: blur(2px) saturate(0.3); opacity: 0.3; " else ""
        borderStyle = if isHovered then "border: 2px solid #C9B8A0; box-shadow: 0 2px 8px rgba(0,0,0,0.12); " else "border: 1px solid #E0DDD4; "
    in
    HH.div
      [ HP.style "transition: all 200ms ease;"
      , HE.onMouseEnter \_ -> HoverView (Just viewId)
      , HE.onMouseLeave \_ -> HoverView Nothing
      ]
      [ HH.div
          [ HP.style $ "cursor: pointer; border-radius: 4px; overflow: hidden; " <> filterStyle <> borderStyle
          , HE.onClick \_ -> GoToScene scene
          ]
          [ HH.img
              [ HP.src src
              , HP.style "width: 100%; height: auto; display: block;"
              ]
          , HH.div [ HP.style "padding: 4px 6px; font-size: 9px; color: #888; text-align: center; background: #fff;" ]
              [ HH.text caption ]
          ]
      , if isHovered
          then HH.div [ HP.style "padding: 8px 4px 0; font-size: 13px; color: #444; text-align: center; line-height: 1.4; font-weight: 500;" ]
            [ HH.text description ]
          else HH.text ""
      ]
  moduleStructureBox hoveredView =
    let anyMsvHovered = case hoveredView of
          Just v -> v == "msv-sigs" || v == "msv-layers" || v == "msv-concerns" || v == "msv-deps" || v == "msv-layann"
          Nothing -> false
        anyActive = hoveredView /= Nothing
        -- Dim the whole box if a non-masonry thumb is hovered
        boxDimmed = anyActive && not anyMsvHovered && hoveredView /= Nothing
        boxFilter = if boxDimmed then "filter: blur(2px) saturate(0.3); opacity: 0.3; " else ""
    in
    HH.div
      [ HP.style $ "grid-column: 2 / -1; cursor: pointer; border: 2px solid #C9B8A0; border-radius: 6px; overflow: hidden; background: #fff; transition: all 200ms ease; " <> boxFilter
      , HE.onClick \_ -> GoToScene (Scene.ModuleStructure "minard-frontend" "CE2.Component.SceneCoordinator")
      ]
      [ -- Masonry grid: 3 columns, mixed heights
        HH.div [ HP.style "columns: 3; column-gap: 6px; padding: 8px;" ]
          [ masonryThumb "msv-sigs" "screenshots/msv-signatures.jpg" "Signatures" "Type signatures with blame age, showing what each declaration does and when it last changed" hoveredView
          , masonryThumb "msv-layers" "screenshots/msv-layers.jpg" "Layers" "Call hierarchy organized into dependency layers, revealing the module\x2019s internal architecture" hoveredView
          , masonryThumb "msv-concerns" "screenshots/msv-concerns.jpg" "Concerns" "Declarations clustered by shared calling patterns, surfacing implicit groupings in the code" hoveredView
          , masonryThumb "msv-deps" "screenshots/msv-dependencies.jpg" "Dependencies" "Cross-module call graph for a single declaration, tracing what it uses and what uses it" hoveredView
          , masonryThumb "msv-layann" "screenshots/msv-layers-annotations.jpg" "Layers + Annotations" "AI-generated structural observations layered over the call hierarchy" hoveredView
          ]
      -- Caption
      , HH.div [ HP.style "padding: 6px 12px; text-align: center; background: #fff; border-top: 1px solid #E8E4D8;" ]
          [ HH.span [ HP.style "font-size: 10px; font-weight: 600; color: #555;" ] [ HH.text "Module Planet" ]
          , HH.span [ HP.style "font-size: 9px; color: #999; margin-left: 8px;" ] [ HH.text "Signatures, dependencies, layers, concerns, blame \x2014 all in one view" ]
          ]
      ]
  masonryThumb viewId src caption description hoveredView =
    let isHovered = hoveredView == Just viewId
        anyActive = hoveredView /= Nothing
        dimmed = anyActive && not isHovered
        filterStyle = if dimmed then "filter: blur(2px) saturate(0.3); opacity: 0.3; " else ""
        borderStyle = if isHovered then "border: 2px solid #C9B8A0; box-shadow: 0 1px 6px rgba(0,0,0,0.1); " else "border: 1px solid #E0DDD4; "
    in
    HH.div
      [ HP.style $ "break-inside: avoid; margin-bottom: 6px; transition: all 200ms ease; " <> filterStyle
      , HE.onMouseEnter \_ -> HoverView (Just viewId)
      , HE.onMouseLeave \_ -> HoverView Nothing
      ]
      [ HH.img [ HP.src src, HP.style $ "width: 100%; height: auto; display: block; border-radius: 3px; " <> borderStyle ]
      , HH.div [ HP.style "font-size: 8px; color: #999; text-align: center; padding: 2px 0;" ] [ HH.text caption ]
      , if isHovered
          then HH.div [ HP.style "padding: 4px 2px 2px; font-size: 11px; color: #444; text-align: center; line-height: 1.4; font-weight: 500;" ]
            [ HH.text description ]
          else HH.text ""
      ]

-- =============================================================================
-- System Diagram (HATS Sankey)
-- =============================================================================

renderSystemDiagram :: forall m. H.ComponentHTML Action () m
renderSystemDiagram =
  HH.div
    [ HP.style "margin-bottom: 16px;" ]
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
-- Distillation Pyramid
-- =============================================================================

renderDistillation :: forall m. H.ComponentHTML Action () m
renderDistillation =
  HH.div
    [ HP.style "margin-bottom: 16px; display: flex; gap: 32px; align-items: flex-start;" ]
    [ -- Pyramid SVG
      HH.div [ HP.style "flex-shrink: 0;" ]
        [ svgElem "svg"
            [ sa "viewBox" "0 0 320 280"
            , HP.style "width: 320px; height: 280px; display: block;"
            ]
            [ -- Level 0: Source code (widest)
              trapezoid 10.0 10.0 310.0 10.0 280.0 60.0 40.0 60.0 "#E8E0CC" "#C9B8A0"
            , levelLabel 160.0 40.0 "Your Source" "#555"
            , sizeLabel 300.0 40.0 "~200 KB"

            -- Level 1: Compiled output
            , trapezoid 40.0 65.0 280.0 65.0 255.0 115.0 65.0 115.0 "#EBF0F5" "#A0B4C8"
            , levelLabel 160.0 95.0 "Compiled Output" "#555"
            , sizeLabel 275.0 95.0 "~5 MB"

            -- Level 2: DuckDB
            , trapezoid 65.0 120.0 255.0 120.0 225.0 170.0 95.0 170.0 "#D5E8E8" "#6B9FA3"
            , levelLabel 160.0 150.0 "DuckDB" "#555"
            , sizeLabel 245.0 150.0 "~2 MB"

            -- Level 3: API response
            , trapezoid 95.0 175.0 225.0 175.0 200.0 225.0 120.0 225.0 "#E0E8F0" "#7A9AB4"
            , levelLabel 160.0 205.0 "API Response" "#555"
            , sizeLabel 218.0 205.0 "~50 KB"

            -- Level 4: What you see (narrowest)
            , trapezoid 120.0 230.0 200.0 230.0 185.0 270.0 135.0 270.0 "#F0E8D8" "#C0A870"
            , levelLabel 160.0 255.0 "Insight" "#555"
            , sizeLabel 200.0 255.0 "~5 KB"

            -- Arrow on the right side
            , svgElem "text"
                [ sa "x" "310", sa "y" "145"
                , sa "text-anchor" "middle"
                , sa "font-size" "11", sa "fill" "#999"
                , sa "font-family" "-apple-system, sans-serif"
                , sa "transform" "rotate(90, 310, 145)"
                ]
                [ HH.text "1000\x00D7 distillation \x2192" ]
            ]
        ]
    -- Explanation text
    , HH.div [ HP.style "flex: 1; padding-top: 8px;" ]
        [ HH.h2
            [ HP.style sectionHeadingStyle ]
            [ HH.text "Distillation, Not Dashboard" ]
        , HH.p
            [ HP.style $ sectionBodyStyle <> " margin-bottom: 12px;" ]
            [ HH.text "Minard is a 1,000\x00D7 compression pipeline. Your source code passes through the PureScript compiler, gets loaded into DuckDB, and is served as targeted API responses \x2014 each view shows exactly the structural insight you need, nothing more." ]
        , HH.p
            [ HP.style $ sectionBodyStyle <> " margin-bottom: 12px;" ]
            [ HH.text "The annotations layer works in reverse: AI and human observations flow back into the database, enriching future views. Every confirmed or disputed annotation makes the next report more accurate." ]
        , HH.p
            [ HP.style sectionBodyStyle ]
            [ HH.text "This is why Minard uses a real database, not a cache. The value is in the connections \x2014 cross-module call graphs, dependency layers, commit co-change patterns \x2014 that only emerge when you query across the whole codebase at once." ]
        ]
    ]
  where
  -- A trapezoid shape (4 corners, filled)
  trapezoid x1 y1 x2 y2 x3 y3 x4 y4 fill stroke =
    svgElem "polygon"
      [ sa "points" (pt x1 y1 <> " " <> pt x2 y2 <> " " <> pt x3 y3 <> " " <> pt x4 y4)
      , sa "fill" fill
      , sa "stroke" stroke
      , sa "stroke-width" "1"
      ]
      []
  pt x y = show x <> "," <> show y

  levelLabel x y text color =
    svgElem "text"
      [ sa "x" (show x), sa "y" (show y)
      , sa "text-anchor" "middle"
      , sa "dominant-baseline" "middle"
      , sa "font-size" "11"
      , sa "font-weight" "600"
      , sa "fill" color
      , sa "font-family" "-apple-system, 'Helvetica Neue', sans-serif"
      ]
      [ HH.text text ]

  sizeLabel x y text =
    svgElem "text"
      [ sa "x" (show x), sa "y" (show y)
      , sa "text-anchor" "start"
      , sa "dominant-baseline" "middle"
      , sa "font-size" "9"
      , sa "font-weight" "400"
      , sa "fill" "#999"
      , sa "font-family" "'Fira Code', monospace"
      ]
      [ HH.text text ]

-- =============================================================================
-- AI Section
-- =============================================================================

renderAISection :: forall m. H.ComponentHTML Action () m
renderAISection =
  HH.div
    [ HP.style "margin-bottom: 16px;" ]
    [ HH.h2
        [ HP.style sectionHeadingStyle ]
        [ HH.text "AI as a First-Class Participant" ]
    , HH.p
        [ HP.style $ sectionBodyStyle <> " margin-bottom: 24px;" ]
        [ HH.text "Minard doesn\x2019t just visualize code \x2014 it gives AI a structured way to reason about your codebase and record what it finds." ]
    -- Three cards
    , HH.div
        [ HP.style "display: grid; grid-template-columns: repeat(3, 1fr); gap: 20px;" ]
        [ aiCard "Cached Thinking"
            "AI annotations are observations, not suggestions. They record structural analysis \x2014 \x201Cthis module bridges two concerns\x201D, \x201Cthis function is the only path between layers\x201D \x2014 and persist in the database for both bots and humans to build on."
            "When you start or resume work on a large project, the thinking is already there."
        , aiCard "Conversational Review"
            "Every annotation can be confirmed, disputed, or superseded. Human review isn\x2019t approval \x2014 it\x2019s a conversation that builds shared understanding over time."
            "The history of interpretation is as valuable as the code itself."
        , aiCard "API-First for Agents"
            "The full structural database is available via REST API. An AI agent can query module dependencies, find articulation points, trace call graphs, and read prior annotations \x2014 the same data the visualizations use."
            "Your coding assistant doesn\x2019t need to grep \x2014 it can ask the database."
        ]
    ]
  where
  aiCard title body emphasis =
    HH.div
      [ HP.style "padding: 20px; background: #fff; border: 1px solid #E0DDD4; border-radius: 6px;" ]
      [ HH.h3
          [ HP.style "font-size: 14px; font-weight: 600; color: #333; margin: 0 0 8px;" ]
          [ HH.text title ]
      , HH.p
          [ HP.style "font-size: 13px; color: #666; line-height: 1.6; margin: 0 0 12px;" ]
          [ HH.text body ]
      , HH.p
          [ HP.style "font-size: 12px; color: #888; font-style: italic; margin: 0; line-height: 1.5;" ]
          [ HH.text emphasis ]
      ]

-- =============================================================================
-- API Transcript — what an AI learns in 6 queries
-- =============================================================================

renderAPITranscript :: forall m. H.ComponentHTML Action () m
renderAPITranscript =
  HH.div
    [ HP.style "margin-bottom: 16px;" ]
    [ HH.h2
        [ HP.style sectionHeadingStyle ]
        [ HH.text "What an AI Learns in 6 API Calls" ]
    , HH.p
        [ HP.style $ sectionBodyStyle <> " margin-bottom: 24px;" ]
        [ HH.text "No source files opened. No grep. No token-heavy file reads. Just structured queries to the database." ]
    -- Transcript
    , HH.div
        [ HP.style "max-width: 800px; margin: 0 auto; font-family: 'Fira Code', 'SF Mono', 'Courier New', monospace; font-size: 12px; line-height: 1.6;" ]
        [ queryBlock "1" "What is this project?"
            "GET /api/v2/stats"
            [ "437 packages, 4,556 modules, 50,270 declarations"
            , "84,177 function calls tracked"
            , "194 registry packages, 157 local"
            ]
            "In one call, the agent knows the scale of the entire dependency universe."

        , queryBlock "2" "Which packages are the actual project code?"
            "GET /api/v2/packages  \x2192  filter source = \"workspace\""
            [ "minard-frontend: 81 modules, 34,503 LOC, topo layer 24 [app entry: CE2.Main]"
            , "minard-server: topo layer 22"
            , "cartography-database: topo layer 17"
            , "minard-cst, type-sig-viz: utility packages"
            ]
            "Five workspace packages. The frontend is where the code lives. The agent now knows where to focus."

        , queryBlock "3" "What are the biggest modules?"
            "GET /api/v2/modules  \x2192  sort by LOC descending"
            [ "CE2.Data.Loader: 2,030 LOC, 100 declarations \x2014 data layer"
            , "CE2.Component.SceneCoordinator: 1,618 LOC \x2014 navigation orchestrator"
            , "CE2.Viz.ModuleTreemapEnriched: 1,406 LOC \x2014 treemap rendering"
            , "CE2.Component.ModuleAnatomyViz: 1,288 LOC \x2014 structural analysis"
            ]
            "The agent can now prioritize: these four modules represent the architectural spine."

        , queryBlock "4" "What do the annotations say about the most complex module?"
            "GET /api/v2/annotations?target_id=CE2.Component.SceneCoordinator"
            [ "[architecture] Orchestrator. Owns the Scene state machine and dispatches to visualization slots."
            , "[quality] handleAction has 25+ branches spanning ~900 lines. State has 25+ fields mixing concerns."
            , "[summary] Central coordinator (1,618 LOC). Manages drill-down: treemap \x2192 beeswarm \x2192 module \x2192 declaration."
            ]
            "Previous AI analysis is cached in the database. No need to re-read 1,600 lines to understand the module."

        , queryBlock "5" "What does every module do?"
            "GET /api/v2/annotations?kind=summary"
            [ "85 module summaries returned, each 1\x20133 sentences"
            , "SceneCoordinator: \"Central navigation coordinator... manages Powers-of-Ten drill-down\""
            , "Data.Loader: \"API client and data transformation engine (2,030 LOC)\""
            , "ModulePlanetViz: \"Unified module view combining signatures, dependencies, layers, concerns\""
            ]
            "A complete map of the codebase in one query. The agent can answer 'where is X handled?' without grep."

        , queryBlock "6" "What are the known architectural concerns?"
            "GET /api/v2/annotations?kind=architecture"
            [ "minard-frontend: \"Four-layer architecture: Core types, Data, Viz, Components\""
            , "SceneCoordinator: \"Orchestrator. All user interactions flow through its action handler.\""
            , "Data.Loader: \"Single bridge at fetchPackageSetFromV2 separating two cohesive groups\""
            ]
            "Structural knowledge that would take hours of code reading, available instantly."
        ]
    -- Summary
    , HH.div
        [ HP.style "max-width: 800px; margin: 24px auto 0; padding: 16px 20px; background: #fff; border: 1.5px solid #D8D0BC; border-radius: 6px;" ]
        [ HH.p
            [ HP.style "font-size: 14px; color: #444; margin: 0; line-height: 1.6; text-align: center;" ]
            [ HH.text "6 queries. 0 source files. An AI agent now knows the project\x2019s shape, its biggest risks, what every module does, and where to start working \x2014 all from cached structural analysis in the database." ]
        ]
    ]
  where
  queryBlock num question endpoint responses insight =
    HH.div
      [ HP.style "margin-bottom: 28px;" ]
      [ -- Question
        HH.div [ HP.style "margin-bottom: 6px;" ]
          [ HH.span [ HP.style "font-size: 11px; font-weight: 700; color: #C0A870; margin-right: 8px;" ] [ HH.text ("Q" <> num) ]
          , HH.span [ HP.style "font-size: 13px; font-weight: 600; color: #333; font-family: -apple-system, 'Helvetica Neue', sans-serif;" ] [ HH.text question ]
          ]
      -- Endpoint
      , HH.div [ HP.style "padding: 6px 12px; background: #2C2C2C; color: #8BE9FD; border-radius: 4px 4px 0 0; font-size: 11px;" ]
          [ HH.text $ "\x276F " <> endpoint ]
      -- Response
      , HH.div [ HP.style "padding: 10px 12px; background: #3C3C3C; color: #E0E0E0; border-radius: 0 0 4px 4px; font-size: 11px;" ]
          (responses <#> \line ->
            HH.div [ HP.style "margin-bottom: 3px;" ]
              [ HH.text $ "\x2022 " <> line ]
          )
      -- Insight
      , HH.div [ HP.style "margin-top: 6px; font-size: 12px; color: #888; font-style: italic; font-family: -apple-system, 'Helvetica Neue', sans-serif;" ]
          [ HH.text insight ]
      ]

-- =============================================================================
-- Clone Banner (shown at top when no backend is available)
-- =============================================================================

renderCloneBanner :: forall m. H.ComponentHTML Action () m
renderCloneBanner =
  HH.div
    [ HP.style $ "background: #2D7D46; color: #fff; padding: 14px 24px; text-align: center; "
        <> "font-size: 14px; line-height: 1.6; position: sticky; top: 0; z-index: 900;"
    ]
    [ HH.span
        [ HP.style "font-weight: 600; margin-right: 8px;" ]
        [ HH.text "Clone and run Minard locally:" ]
    , HH.code
        [ HP.style "background: rgba(255,255,255,0.2); padding: 4px 10px; border-radius: 4px; font-size: 13px; font-family: 'SF Mono', 'Fira Code', 'Consolas', monospace; letter-spacing: 0.3px;" ]
        [ HH.text "git clone https://github.com/afcondon/minard && cd minard && make bootstrap && make start" ]
    ]

-- =============================================================================
-- Get Started CTA
-- =============================================================================

renderGetStarted :: forall m. State -> H.ComponentHTML Action () m
renderGetStarted state =
  HH.div
    [ HP.style "margin-bottom: 16px; text-align: center;" ]
    [ HH.h2
        [ HP.style $ sectionHeadingStyle <> " margin-bottom: 16px;" ]
        [ HH.text "Start Exploring" ]
    , if BuildInfo.isStaticDeploy
        then renderGetStartedClone
        else renderGetStartedLive state
    ]

renderGetStartedLive :: forall m. State -> H.ComponentHTML Action () m
renderGetStartedLive state =
  HH.div_
    [ HH.p
        [ HP.style "font-size: 15px; color: #555; line-height: 1.7; max-width: 640px; margin: 0 auto 24px;" ]
        [ HH.text "Explore Minard\x2019s own codebase right now:" ]
    -- Mirror the nav buttons from the hero
    , HH.div
        [ HP.style "display: flex; gap: 12px; justify-content: center; flex-wrap: wrap; margin-bottom: 28px;" ]
        [ ctaNavLink "Maps" Scene.GalaxyTreemap state.dataReady
        , ctaNavLink "Reports" Scene.PackageReport state.dataReady
        , ctaNavLink "Anatomy" Scene.ProjectAnatomy state.dataReady
        , ctaNavLink "Git" Scene.GitOverview state.dataReady
        ]
    , HH.p
        [ HP.style "font-size: 15px; color: #555; line-height: 1.7; max-width: 640px; margin: 0 auto 20px;" ]
        [ HH.text "Or load your own PureScript project and analyse it with Minard:" ]
    , HH.button
        [ HE.onClick \_ -> GoToScene Scene.ProjectSetup
        , HP.style "padding: 12px 28px; border: 1.5px solid #C0BDB4; border-radius: 28px; cursor: pointer; font-size: 14px; font-weight: 500; background: #fff; color: #555; letter-spacing: 0.3px;"
        ]
        [ HH.text "Add your project" ]
    ]
  where
  ctaNavLink label scene enabled =
    HH.button
      [ HE.onClick \_ -> GoToScene scene
      , HP.style $ "padding: 10px 24px; border: 1.5px solid "
          <> (if enabled then "#2D7D46" else "#E0DDD4")
          <> "; border-radius: 28px; cursor: pointer"
          <> "; font-size: 14px; font-weight: 600; background: "
          <> (if enabled then "#2D7D46" else "#FAFAF8")
          <> "; color: " <> (if enabled then "#fff" else "#BBB")
          <> "; transition: all 150ms ease;"
      ]
      [ HH.text label ]

renderGetStartedClone :: forall m. H.ComponentHTML Action () m
renderGetStartedClone =
  HH.div_
    [ HH.p
        [ HP.style "font-size: 15px; color: #555; line-height: 1.7; max-width: 640px; margin: 0 auto 24px;" ]
        [ HH.text "Minard runs locally on your machine. Clone, bootstrap, and start exploring:" ]
    , HH.div
        [ HP.style "background: #1a1a2e; border-radius: 8px; padding: 20px 28px; max-width: 540px; margin: 0 auto 24px; text-align: left;" ]
        [ HH.code
            [ HP.style "color: #c8dce8; font-size: 14px; font-family: 'SF Mono', 'Fira Code', 'Consolas', monospace; line-height: 2; white-space: pre;" ]
            [ HH.text "git clone https://github.com/afcondon/minard\ncd minard && make bootstrap && make start" ]
        ]
    , HH.a
        [ HP.href "https://github.com/afcondon/minard"
        , HP.target "_blank"
        , HP.style "display: inline-block; padding: 12px 28px; background: #2D7D46; color: white; border-radius: 28px; font-size: 14px; font-weight: 600; text-decoration: none; letter-spacing: 0.3px;"
        ]
        [ HH.text "View on GitHub" ]
    ]

-- =============================================================================
-- Shared Styles
-- =============================================================================

-- | Typographic ornament between sections (fleur-de-lis for the French cartographer)
sectionOrnament :: forall m. H.ComponentHTML Action () m
sectionOrnament = ornamentWithId Nothing

ornamentWithAnchor :: forall m. String -> H.ComponentHTML Action () m
ornamentWithAnchor anchorId = ornamentWithId (Just anchorId)

ornamentWithId :: forall m. Maybe String -> H.ComponentHTML Action () m
ornamentWithId mId =
  HH.div
    ( [ HP.style "text-align: center; margin: 40px 0; color: #C0B090; font-size: 16px; letter-spacing: 8px;" ]
      <> case mId of
          Just anchorId -> [ HP.id anchorId ]
          Nothing -> []
    )
    [ HH.text "\x2767 \x269C \x2619" ]

-- | Larger ornament for the hero separator
heroOrnament :: forall m. H.ComponentHTML Action () m
heroOrnament =
  HH.div
    [ HP.style "text-align: center; margin: 48px 0; color: #B8A880; font-size: 28px; letter-spacing: 16px;" ]
    [ HH.text "\x2767 \x269C \x2619" ]

-- | Modal shown when user clicks a nav button without a backend
renderInstallModal :: forall m. H.ComponentHTML Action () m
renderInstallModal =
  HH.div
    [ HP.style "position: fixed; top: 0; left: 0; width: 100%; height: 100%; z-index: 1000; display: flex; align-items: center; justify-content: center; background: rgba(0,0,0,0.4);"
    , HE.onClick \_ -> DismissModal
    ]
    [ HH.div
        [ HP.style "background: #FAFAF8; border: 2px solid #C9B8A0; border-radius: 12px; padding: 40px 48px; max-width: 520px; text-align: center; box-shadow: 0 8px 32px rgba(0,0,0,0.2);" ]
        [ HH.div
            [ HP.style "font-size: 36px; margin-bottom: 16px; font-family: 'Georgia', 'Times New Roman', serif; font-weight: 700; color: #2C2C2C;" ]
            [ HH.text "Minard" ]
        , HH.p
            [ HP.style "font-size: 15px; color: #555; line-height: 1.7; margin: 0 0 20px;" ]
            [ HH.text "Minard needs to be installed locally to run. Clone, bootstrap, and start:" ]
        , HH.div
            [ HP.style "background: #1a1a2e; border-radius: 6px; padding: 14px 20px; margin: 0 0 20px; text-align: left;" ]
            [ HH.code
                [ HP.style "color: #c8dce8; font-size: 13px; font-family: 'SF Mono', 'Fira Code', 'Consolas', monospace; line-height: 2; white-space: pre;" ]
                [ HH.text "git clone https://github.com/afcondon/minard\ncd minard && make bootstrap && make start" ]
            ]
        , HH.a
            [ HP.href "https://github.com/afcondon/minard"
            , HP.target "_blank"
            , HP.style "display: inline-block; padding: 12px 28px; background: #2D7D46; color: white; border-radius: 28px; font-size: 14px; font-weight: 600; text-decoration: none; letter-spacing: 0.3px; margin-bottom: 12px;"
            ]
            [ HH.text "View on GitHub" ]
        , HH.p
            [ HP.style "font-size: 12px; color: #999; margin: 0;" ]
            [ HH.text "Click anywhere to dismiss" ]
        ]
    ]

sectionHeadingStyle :: String
sectionHeadingStyle = "font-size: 15px; font-weight: 600; margin: 0 0 8px 0; letter-spacing: 0.3px; color: #555; text-transform: uppercase;"

sectionBodyStyle :: String
sectionBodyStyle = "font-size: 15px; color: #666; margin: 0 0 4px 0; line-height: 1.6; max-width: 800px;"
