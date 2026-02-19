-- | Project Management Scene
-- |
-- | Pure Halogen HTML component (no D3/canvas). Light theme, clean typographic layout.
-- | Two modes:
-- |   Welcome mode (no projects): rich onboarding page with branding, architecture
-- |     diagram, Sankey data flow, feature narrative, and guided project loading
-- |   Management mode (projects exist): table of projects + add project form
module CE2.Component.ProjectManagementViz
  ( component
  , Input
  , Output(..)
  , Query(..)
  , Slot
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core (AttrName(..), ElemName(..), Namespace(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import CE2.Data.Loader as Loader
import CE2.Scene (Scene(..)) as Scene

-- =============================================================================
-- Types
-- =============================================================================

type Input =
  { projects :: Array Loader.ProjectInfo
  , dataReady :: Boolean
  }

data Output
  = ProjectAdded Loader.LoadResult
  | NavigateToProject Int
  | NavigateToScene Scene.Scene
  | ProjectDeleted Int

data Query a = RefreshProjects (Array Loader.ProjectInfo) a

type Slot = H.Slot Query Output

-- | State machine for the add-project flow
data AddPhase
  = Idle
  | EnteringPath
  | Validating
  | ValidationResult Loader.PathValidation
  | LoadingProject
  | LoadSuccess Loader.LoadResult
  | LoadError String

derive instance eqAddPhase :: Eq AddPhase

type State =
  { projects :: Array Loader.ProjectInfo
  , dataReady :: Boolean
  , addPhase :: AddPhase
  , pathInput :: String
  , nameOverride :: String
  , confirmDeleteId :: Maybe Int
  }

data Action
  = Initialize
  | Receive Input
  | StartAddProject
  | CancelAdd
  | SetPathInput String
  | SetNameOverride String
  | DoValidate
  | DoLoad
  | DoDelete Int
  | ConfirmDelete Int
  | CancelDelete
  | ExploreProject Int
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
        , handleQuery = handleQuery
        , receive = Just <<< Receive
        , initialize = Just Initialize
        }
    }

initialState :: Input -> State
initialState input =
  { projects: input.projects
  , dataReady: input.dataReady
  , addPhase: if Array.null input.projects then EnteringPath else Idle
  , pathInput: ""
  , nameOverride: ""
  , confirmDeleteId: Nothing
  }

handleQuery :: forall m a. Query a -> H.HalogenM State Action () Output m (Maybe a)
handleQuery = case _ of
  RefreshProjects projects a -> do
    H.modify_ _ { projects = projects }
    pure (Just a)

-- =============================================================================
-- SVG Helpers
-- =============================================================================

svgNS :: Namespace
svgNS = Namespace "http://www.w3.org/2000/svg"

svgElem :: forall r w i. String -> Array (HH.IProp r i) -> Array (HH.HTML w i) -> HH.HTML w i
svgElem name = HH.elementNS svgNS (ElemName name)

sa :: forall r i. String -> String -> HH.IProp r i
sa name val = HP.attr (AttrName name) val

-- | Cubic bezier path for a Sankey link band
-- | Goes from (x0, y0)-(x0, y0+bw) on left to (x1, y1)-(x1, y1+bw) on right
sankeyPath :: Number -> Number -> Number -> Number -> Number -> String
sankeyPath x0 y0 x1 y1 bw =
  let cx = (x0 + x1) / 2.0
  in "M " <> n x0 <> " " <> n y0
    <> " C " <> n cx <> " " <> n y0 <> ", " <> n cx <> " " <> n y1 <> ", " <> n x1 <> " " <> n y1
    <> " L " <> n x1 <> " " <> n (y1 + bw)
    <> " C " <> n cx <> " " <> n (y1 + bw) <> ", " <> n cx <> " " <> n (y0 + bw) <> ", " <> n x0 <> " " <> n (y0 + bw)
    <> " Z"
  where n x = show x

-- =============================================================================
-- Render
-- =============================================================================

render :: forall m. MonadAff m => State -> H.ComponentHTML Action () m
render state =
  HH.div
    [ HP.style containerStyle ]
    [ HH.div
        [ HP.style "max-width: 800px; width: 100%; margin: 0 auto; padding: 60px 24px 80px;" ]
        [ renderHero state
        , renderArchitectureDiagram
        , renderSankeySection
        , renderWhatYoullSee
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

renderHero :: forall m. MonadAff m => State -> H.ComponentHTML Action () m
renderHero state =
  let hasProjects = not (Array.null state.projects)
  in HH.div
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

    -- Navigation links (when data is loaded)
    , if hasProjects
        then HH.div
          [ HP.style "display: flex; gap: 12px; justify-content: center; flex-wrap: wrap;" ]
          [ navLink "Anatomy" Scene.ProjectAnatomy state.dataReady
          , navLink "Galaxy" Scene.GalaxyTreemap state.dataReady
          , navLink "Report" Scene.AnnotationReport state.dataReady
          ]
        else HH.text ""
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

renderArchitectureDiagram :: forall w i. HH.HTML w i
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

        -- Arrow 1→2
        , archArrow 180.0 70.0 260.0 70.0
        , archLabel 220.0 62.0 "read" "400" "9"

        -- Stage 2: Loader + DB
        , archBox 260.0 20.0 200.0 100.0 "#EBF0F5" "#A0B4C8"
        , archLabel 360.0 50.0 "Rust Loader" "500" "13"
        , archLabel 360.0 68.0 "\x2193" "400" "12"
        , archLabel 360.0 86.0 "DuckDB" "500" "12"
        , archLabel 360.0 102.0 "packages \x00B7 modules \x00B7 decls" "400" "8.5"

        -- Arrow 2→3
        , archArrow 460.0 70.0 540.0 70.0
        , archLabel 500.0 62.0 "query" "400" "9"

        -- Stage 3: Minard UI
        , archBox 540.0 20.0 220.0 100.0 "#EDF5ED" "#A0C8A0"
        , archLabel 650.0 50.0 "API Server" "500" "13"
        , archLabel 650.0 68.0 "\x2193" "400" "12"
        , archLabel 650.0 86.0 "Browser" "500" "12"
        , archLabel 650.0 102.0 "6 visualization scenes" "400" "8.5"
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

-- =============================================================================
-- Sankey Data Flow Diagram
-- =============================================================================

renderSankeySection :: forall w i. HH.HTML w i
renderSankeySection =
  HH.div
    [ HP.style "margin-bottom: 48px;" ]
    [ HH.h2
        [ HP.style sectionHeadingStyle ]
        [ HH.text "Information Flow" ]
    , HH.p
        [ HP.style sectionBodyStyle ]
        [ HH.text "Minard combines data from multiple sources \x2014 your compiled types, the package registry, git history, and human and AI annotations \x2014 into a unified exploration experience. This diagram shows how information flows through the system." ]
    , svgElem "svg"
        [ sa "viewBox" "0 0 860 420"
        , HP.style "width: 100%; height: auto; display: block; margin: 20px 0; background: #FEFDFB; border: 1px solid #E8E6E0; border-radius: 6px; padding: 8px;"
        ]
        -- Column headers
        [ svgElem "text" [ sa "x" "70", sa "y" "22", sa "text-anchor" "middle", sa "font-size" "9", sa "fill" "#AAA", sa "font-weight" "600", sa "letter-spacing" "1" ] [ HH.text "SOURCES" ]
        , svgElem "text" [ sa "x" "420", sa "y" "22", sa "text-anchor" "middle", sa "font-size" "9", sa "fill" "#AAA", sa "font-weight" "600", sa "letter-spacing" "1" ] [ HH.text "ENGINE" ]
        , svgElem "text" [ sa "x" "750", sa "y" "22", sa "text-anchor" "middle", sa "font-size" "9", sa "fill" "#AAA", sa "font-weight" "600", sa "letter-spacing" "1" ] [ HH.text "VIEWS" ]

        -- === LINKS (rendered first, behind nodes) ===

        -- spago.yaml → Loader
        , sankeyLink 140.0 52.0 350.0 42.0 14.0 "#E8D5B8" "0.5"
        -- docs.json → Loader
        , sankeyLink 140.0 102.0 350.0 62.0 28.0 "#C9D6E8" "0.5"
        -- Package set → Loader
        , sankeyLink 140.0 157.0 350.0 96.0 12.0 "#D5C8B8" "0.45"

        -- Loader → DuckDB
        , sankeyLink 490.0 60.0 490.0 188.0 40.0 "#B8C8D8" "0.4"

        -- DuckDB → API
        , sankeyLink 490.0 228.0 490.0 298.0 36.0 "#B8D8C8" "0.4"

        -- Git → API
        , sankeyLink 140.0 218.0 350.0 310.0 10.0 "#E8C8C0" "0.5"

        -- Annotations → API
        , sankeyLink 140.0 290.0 350.0 326.0 14.0 "#D8D0B8" "0.5"

        -- API → Galaxy Treemap
        , sankeyLink 490.0 300.0 670.0 52.0 14.0 "#C8D8C0" "0.45"
        -- API → Package Treemaps
        , sankeyLink 490.0 316.0 670.0 102.0 12.0 "#C8D8C0" "0.45"
        -- API → Module Signatures
        , sankeyLink 490.0 330.0 670.0 152.0 12.0 "#C8D8C0" "0.45"
        -- API → Type Explorer
        , sankeyLink 490.0 344.0 670.0 202.0 8.0 "#C8D8C0" "0.4"
        -- API → Codebase Report
        , sankeyLink 490.0 354.0 670.0 245.0 10.0 "#D8D0C0" "0.45"

        -- Feedback: Report → AI → Annotations (dashed curve at bottom)
        , svgElem "path"
            [ sa "d" "M 750 280 C 750 380, 200 400, 100 340"
            , sa "fill" "none"
            , sa "stroke" "#B07AA1"
            , sa "stroke-width" "1.5"
            , sa "stroke-dasharray" "6 3"
            , sa "opacity" "0.6"
            ] []
        , svgElem "text" [ sa "x" "430", sa "y" "395", sa "text-anchor" "middle", sa "font-size" "9", sa "fill" "#B07AA1", sa "font-style" "italic" ] [ HH.text "AI reads report, proposes annotations, you review" ]

        -- === SOURCE NODES (left column, x=10) ===

        , sankeyNode 10.0 42.0 130.0 30.0 "#F5EFE4" "#C0B8A0"
        , sankeyNodeLabel 75.0 62.0 "Project Config" "11"
        , sankeyNodeSub 75.0 72.0 "spago.yaml \x00B7 spago.lock"

        , sankeyNode 10.0 88.0 130.0 48.0 "#E4ECF5" "#A0B4C8"
        , sankeyNodeLabel 75.0 110.0 "Compiled Output" "11"
        , sankeyNodeSub 75.0 122.0 "output/*/docs.json"

        , sankeyNode 10.0 148.0 130.0 28.0 "#F0EBE0" "#C0B8A0"
        , sankeyNodeLabel 75.0 167.0 "Package Registry" "10"

        , sankeyNode 10.0 210.0 130.0 28.0 "#F5E8E4" "#C8A8A0"
        , sankeyNodeLabel 75.0 228.0 "Git Repository" "10.5"
        , svgElem "text" [ sa "x" "118", sa "y" "220", sa "font-size" "7.5", sa "fill" "#C0392B", sa "font-style" "italic" ] [ HH.text "*" ]

        , sankeyNode 10.0 270.0 130.0 50.0 "#F0EDE0" "#C8C0A0"
        , sankeyNodeLabel 75.0 290.0 "Annotations" "11"
        , sankeyNodeSub 75.0 302.0 "human + AI"
        , sankeyNodeSub 75.0 313.0 "session notes"

        -- === ENGINE NODES (middle column, x=350) ===

        , sankeyNode 350.0 35.0 140.0 90.0 "#E4ECF5" "#8CA8C8"
        , sankeyNodeLabel 420.0 68.0 "Rust Loader" "12"
        , sankeyNodeSub 420.0 82.0 "reads docs.json"
        , sankeyNodeSub 420.0 93.0 "resolves dependencies"
        , sankeyNodeSub 420.0 104.0 "extracts declarations"

        , sankeyNode 350.0 180.0 140.0 55.0 "#E4F0F0" "#88B8B0"
        , sankeyNodeLabel 420.0 207.0 "DuckDB" "12"
        , sankeyNodeSub 420.0 220.0 "packages \x00B7 modules \x00B7 decls"

        , sankeyNode 350.0 290.0 140.0 80.0 "#E8F0E8" "#88B888"
        , sankeyNodeLabel 420.0 318.0 "API Server" "12"
        , sankeyNodeSub 420.0 332.0 "PureScript + HTTPurple"
        , sankeyNodeSub 420.0 343.0 "30+ endpoints"
        , sankeyNodeSub 420.0 354.0 "CORS \x00B7 JSON"

        -- === VIEW NODES (right column, x=670) ===

        , sankeyNode 670.0 42.0 180.0 36.0 "#E8F0E4" "#88C088"
        , sankeyNodeLabel 760.0 64.0 "Galaxy Treemap" "11"

        , sankeyNode 670.0 92.0 180.0 36.0 "#E8F0E4" "#88C088"
        , sankeyNodeLabel 760.0 114.0 "Package Treemaps" "11"

        , sankeyNode 670.0 142.0 180.0 36.0 "#E8F0E4" "#88C088"
        , sankeyNodeLabel 760.0 164.0 "Module Signature Maps" "11"

        , sankeyNode 670.0 192.0 180.0 30.0 "#EDE8F0" "#A890B8"
        , sankeyNodeLabel 760.0 211.0 "Type Class Explorer" "11"

        , sankeyNode 670.0 238.0 180.0 42.0 "#F0EDE4" "#C0B890"
        , sankeyNodeLabel 760.0 258.0 "Codebase Report" "11"
        , sankeyNodeSub 760.0 271.0 "annotations \x00B7 AI insights"

        -- Git asterisk footnote
        , svgElem "text" [ sa "x" "10", sa "y" "410", sa "font-size" "8.5", sa "fill" "#999", sa "font-style" "italic" ] [ HH.text "* Git integration: modified files shown in module maps. Full history analysis coming soon." ]
        ]

    -- Explanation cards below the Sankey
    , HH.div
        [ HP.style "display: grid; grid-template-columns: 1fr 1fr 1fr; gap: 16px; margin-top: 16px;" ]
        [ sankeyCallout "1. Compile Once, Explore Everything"
            "Run spago build in your project to produce docs.json files. The Rust loader reads these along with your spago.lock to resolve the full dependency graph \x2014 every package version, every module, every exported declaration."
        , sankeyCallout "2. A Database, Not a Cache"
            "Everything goes into DuckDB \x2014 a real analytical database. Packages, modules, declarations, imports, function calls, type class instances. The API server queries it with SQL, so exploration is fast even for large codebases."
        , sankeyCallout "3. Understanding Accumulates"
            "As you explore, annotate what you learn. Claude can read the full codebase report and propose annotations you review. Each session builds on the last \x2014 your understanding of the code is preserved in the database alongside the code itself."
        ]
    ]

sankeyLink :: forall w i. Number -> Number -> Number -> Number -> Number -> String -> String -> HH.HTML w i
sankeyLink x0 y0 x1 y1 bw color opacity =
  svgElem "path"
    [ sa "d" (sankeyPath x0 y0 x1 y1 bw)
    , sa "fill" color
    , sa "opacity" opacity
    ] []

sankeyNode :: forall w i. Number -> Number -> Number -> Number -> String -> String -> HH.HTML w i
sankeyNode x y w h fill stroke =
  svgElem "rect"
    [ sa "x" (show x), sa "y" (show y)
    , sa "width" (show w), sa "height" (show h)
    , sa "fill" fill, sa "stroke" stroke
    , sa "stroke-width" "1"
    , sa "rx" "4"
    ] []

sankeyNodeLabel :: forall w i. Number -> Number -> String -> String -> HH.HTML w i
sankeyNodeLabel x y label size =
  svgElem "text"
    [ sa "x" (show x), sa "y" (show y)
    , sa "text-anchor" "middle"
    , sa "font-family" "-apple-system, 'Helvetica Neue', sans-serif"
    , sa "font-size" size, sa "font-weight" "600"
    , sa "fill" "#444"
    ] [ HH.text label ]

sankeyNodeSub :: forall w i. Number -> Number -> String -> HH.HTML w i
sankeyNodeSub x y label =
  svgElem "text"
    [ sa "x" (show x), sa "y" (show y)
    , sa "text-anchor" "middle"
    , sa "font-family" "-apple-system, 'Helvetica Neue', sans-serif"
    , sa "font-size" "8.5", sa "font-weight" "400"
    , sa "fill" "#888"
    ] [ HH.text label ]

sankeyCallout :: forall w i. String -> String -> HH.HTML w i
sankeyCallout title body =
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
-- What You'll See
-- =============================================================================

renderWhatYoullSee :: forall w i. HH.HTML w i
renderWhatYoullSee =
  HH.div
    [ HP.style "margin-bottom: 48px;" ]
    [ HH.h2
        [ HP.style sectionHeadingStyle ]
        [ HH.text "What You'll See" ]
    , HH.p
        [ HP.style sectionBodyStyle ]
        [ HH.text "Once your project is loaded, Minard gives you five ways to understand your code \x2014 from the 10,000-foot view down to individual declarations." ]
    , HH.div
        [ HP.style "display: flex; flex-direction: column; gap: 12px; margin-top: 16px;" ]
        [ featureCard "Galaxy Treemap"
            "Your project's dependency universe."
            "Every package in your spago.yaml plus their transitive dependencies from the package set. This is your galaxy \x2014 typically 50-150 packages, each subdivided into its modules. Hold R to trace reachability from your application's entrypoint and see what's actually used versus what's just along for the ride."
            (Just "galaxy-treemap.jpg")
        , featureCard "Package Treemaps"
            "Zoom into any package."
            "Each module appears as a cell sized by its declaration count, filled with circle-packed declarations \x2014 values, data types, type classes, instances. Color encodes declaration kind. Click any module to go deeper."
            (Just "package-treemap.jpg")
        , featureCard "Module Signature Maps"
            "Every export, every type signature."
            "A module's full public API laid out as a signature map: declaration names on the left, type signatures rendered with proper PureScript formatting. See at a glance what a module offers and how complex its types are."
            (Just "module-declarations.jpg")
        , featureCard "Type Class Explorer"
            "Navigate the type class hierarchy."
            "Which classes have the most instances? Which modules are most polymorphic? See the relationships between type classes, their methods, and their instances across your entire dependency graph."
            (Just "typeclasses.jpg")
        , featureCard "Codebase Report"
            "AI-assisted understanding."
            "Annotate modules and declarations with purpose, status, and architectural notes. Claude reads the full report and proposes annotations you review \x2014 building a shared understanding that persists across sessions."
            (Just "report.jpg")
        , featureCard "Add More Projects"
            "Compare and cross-reference."
            "Load additional PureScript applications to see how different projects use the same packages, compare dependency overlap, and understand how different entrypoints light up different parts of the registry."
            (Just "bubblepack-beeswarm.jpg")
        ]
    ]

featureCard :: forall w i. String -> String -> String -> Maybe String -> HH.HTML w i
featureCard title subtitle body mImage =
  HH.div
    [ HP.style "background: #fff; border: 1px solid #E8E6E0; border-radius: 6px; padding: 20px 24px; display: flex; gap: 20px; align-items: flex-start;" ]
    [ HH.div
        [ HP.style "flex: 1; min-width: 0;" ]
        [ HH.div
            [ HP.style "font-size: 14px; font-weight: 600; color: #333; margin-bottom: 2px;" ]
            [ HH.text title ]
        , HH.div
            [ HP.style "font-size: 12px; font-weight: 500; color: #4E79A7; margin-bottom: 6px;" ]
            [ HH.text subtitle ]
        , HH.p
            [ HP.style "font-size: 12px; color: #666; margin: 0; line-height: 1.6;" ]
            [ HH.text body ]
        ]
    , case mImage of
        Just src -> HH.img
          [ HP.src src
          , HP.alt title
          , HP.style "width: 240px; height: auto; border-radius: 4px; border: 1px solid #E8E6E0; flex-shrink: 0; box-shadow: 0 1px 3px rgba(0,0,0,0.08);"
          ]
        Nothing -> HH.text ""
    ]

-- =============================================================================
-- Get Started
-- =============================================================================

renderGetStarted :: forall m. MonadAff m => State -> H.ComponentHTML Action () m
renderGetStarted state =
  HH.div
    [ HP.style "margin-bottom: 48px;" ]
    [ HH.h2
        [ HP.style sectionHeadingStyle ]
        [ HH.text "Get Started" ]
    , HH.p
        [ HP.style sectionBodyStyle ]
        [ HH.text "Point Minard at a built PureScript project. The loader (stage 1 in the architecture above) will read your compiled output, resolve all dependencies, and populate the database. This typically takes 3-15 seconds depending on the size of your dependency graph." ]
    , HH.div
        [ HP.style cardStyle ]
        [ renderAddForm state ]
    ]

-- =============================================================================
-- Shared Styles
-- =============================================================================

sectionHeadingStyle :: String
sectionHeadingStyle = "font-size: 18px; font-weight: 600; margin: 0 0 8px 0; letter-spacing: -0.3px; color: #333;"

sectionBodyStyle :: String
sectionBodyStyle = "font-size: 13px; color: #666; margin: 0 0 4px 0; line-height: 1.6; max-width: 640px;"

cardStyle :: String
cardStyle = "background: #fff; border: 1px solid #E0DDD4; border-radius: 6px; "
  <> "padding: 32px; box-shadow: 0 1px 3px rgba(0,0,0,0.06); margin-top: 16px;"

-- =============================================================================
-- Add Project Form
-- =============================================================================

renderAddForm :: forall m. MonadAff m => State -> H.ComponentHTML Action () m
renderAddForm state =
  HH.div_
    [ -- Path input
      HH.div
        [ HP.style "margin-bottom: 12px;" ]
        [ HH.label
            [ HP.style "display: block; font-size: 11px; font-weight: 500; color: #888; margin-bottom: 4px; text-transform: uppercase; letter-spacing: 0.3px;" ]
            [ HH.text "Project Path" ]
        , HH.div
            [ HP.style "display: flex; gap: 8px;" ]
            [ HH.input
                [ HP.type_ HP.InputText
                , HP.value state.pathInput
                , HP.placeholder "/path/to/your/purescript-project"
                , HE.onValueInput SetPathInput
                , HP.style inputStyle
                ]
            , case state.addPhase of
                Validating ->
                  HH.button
                    [ HP.style (buttonStyle <> " opacity: 0.6;")
                    , HP.disabled true
                    ]
                    [ HH.text "Checking..." ]
                LoadingProject ->
                  HH.button
                    [ HP.style (buttonStyle <> " opacity: 0.6;")
                    , HP.disabled true
                    ]
                    [ HH.text "Loading..." ]
                _ ->
                  HH.button
                    [ HE.onClick \_ -> DoValidate
                    , HP.style buttonStyle
                    , HP.disabled (state.pathInput == "")
                    ]
                    [ HH.text "Validate" ]
            ]
        ]

    -- Name override (optional, shown after validation succeeds)
    , case state.addPhase of
        ValidationResult v | v.valid ->
          HH.div
            [ HP.style "margin-bottom: 12px;" ]
            [ HH.label
                [ HP.style "display: block; font-size: 11px; font-weight: 500; color: #888; margin-bottom: 4px; text-transform: uppercase; letter-spacing: 0.3px;" ]
                [ HH.text "Project Name (optional)" ]
            , HH.input
                [ HP.type_ HP.InputText
                , HP.value state.nameOverride
                , HP.placeholder v.projectName
                , HE.onValueInput SetNameOverride
                , HP.style inputStyle
                ]
            ]
        _ -> HH.text ""

    -- Validation checklist
    , case state.addPhase of
        ValidationResult v -> renderChecklist v
        _ -> HH.text ""

    -- Load button / success / error
    , case state.addPhase of
        ValidationResult v | v.valid ->
          HH.div
            [ HP.style "margin-top: 16px;" ]
            [ HH.button
                [ HE.onClick \_ -> DoLoad
                , HP.style (buttonStyle <> " background: #2D7D46; color: white;")
                ]
                [ HH.text "Load Project" ]
            ]
        LoadingProject ->
          HH.div
            [ HP.style "margin-top: 16px; display: flex; align-items: center; gap: 8px;" ]
            [ HH.div
                [ HP.style "width: 200px; height: 4px; background: #E0DDD4; border-radius: 2px; overflow: hidden;" ]
                [ HH.div
                    [ HP.style "width: 60%; height: 100%; background: #4E79A7; border-radius: 2px; animation: pulse 1.5s ease-in-out infinite;" ]
                    []
                ]
            , HH.span
                [ HP.style "font-size: 12px; color: #888;" ]
                [ HH.text "Loading project..." ]
            ]
        LoadSuccess result ->
          HH.div
            [ HP.style "margin-top: 16px; padding: 16px; background: #F0F9F0; border: 1px solid #C3E6C3; border-radius: 4px;" ]
            [ HH.div
                [ HP.style "font-weight: 500; color: #2D7D46; margin-bottom: 8px;" ]
                [ HH.text "Project loaded successfully" ]
            , case result.elapsedMs of
                Just ms -> HH.p
                    [ HP.style "font-size: 11px; color: #666; margin: 0 0 12px 0;" ]
                    [ HH.text $ "Completed in " <> show (ms / 1000.0) <> "s" ]
                Nothing -> HH.text ""
            , HH.button
                [ HE.onClick \_ -> ExploreProject 0  -- Signal to navigate
                , HP.style "padding: 12px 32px; border: none; border-radius: 6px; cursor: pointer; font-size: 14px; font-weight: 600; background: #2D7D46; color: white; letter-spacing: 0.3px;"
                ]
                [ HH.text "Explore Your Code" ]
            ]
        LoadError err ->
          HH.div
            [ HP.style "margin-top: 16px; padding: 16px; background: #FDF0F0; border: 1px solid #E6C3C3; border-radius: 4px;" ]
            [ HH.div
                [ HP.style "font-weight: 500; color: #C0392B; margin-bottom: 4px;" ]
                [ HH.text "Load failed" ]
            , HH.p
                [ HP.style "font-size: 11px; color: #888; margin: 0; white-space: pre-wrap;" ]
                [ HH.text err ]
            , HH.button
                [ HE.onClick \_ -> DoLoad
                , HP.style (buttonStyle <> " margin-top: 8px;")
                ]
                [ HH.text "Retry" ]
            ]
        _ -> HH.text ""

    -- Cancel button (when not idle)
    , case state.addPhase of
        Idle -> HH.text ""
        LoadSuccess _ -> HH.text ""
        _ ->
          if not (Array.null state.projects) then
            HH.button
              [ HE.onClick \_ -> CancelAdd
              , HP.style "background: none; border: none; color: #999; cursor: pointer; font-size: 11px; margin-top: 8px; padding: 0;"
              ]
              [ HH.text "Cancel" ]
          else HH.text ""
    ]

inputStyle :: String
inputStyle = "flex: 1; padding: 8px 10px; border: 1px solid #D0CFC8; border-radius: 4px; "
  <> "font-size: 13px; font-family: 'SF Mono', 'Menlo', monospace; outline: none; "
  <> "background: #FAFAF8;"

buttonStyle :: String
buttonStyle = "padding: 8px 16px; border: 1px solid #C0BDB4; border-radius: 4px; "
  <> "cursor: pointer; font-size: 12px; background: #F5F4F0; color: #333;"

-- =============================================================================
-- Validation Checklist
-- =============================================================================

renderChecklist :: forall m. Loader.PathValidation -> H.ComponentHTML Action () m
renderChecklist v =
  HH.div
    [ HP.style "margin-top: 16px; padding: 12px 16px; background: #FAFAF8; border: 1px solid #E0DDD4; border-radius: 4px;" ]
    [ HH.div [ HP.style "font-size: 11px; font-weight: 500; color: #888; margin-bottom: 8px; text-transform: uppercase; letter-spacing: 0.3px;" ]
        [ HH.text "Prerequisites" ]
    , checkItem v.checks.directoryExists "Directory exists"
    , checkItem v.checks.spagoLockExists "spago.lock found"
    , checkItem v.checks.outputDirExists "output/ directory exists"
    , checkItem (v.checks.docsJsonCount > 0) ("docs.json files: " <> show v.checks.docsJsonCount)
    , checkItem v.checks.loaderBinaryExists "Loader binary found"
    -- Show issues with remediation messages
    , if Array.length v.issues > 0
        then HH.div
          [ HP.style "margin-top: 8px; padding-top: 8px; border-top: 1px solid #E0DDD4;" ]
          (map renderIssue v.issues)
        else HH.text ""
    ]

checkItem :: forall m w. Boolean -> String -> HH.HTML w m
checkItem ok label =
  HH.div
    [ HP.style "display: flex; align-items: center; gap: 6px; margin-bottom: 4px; font-size: 12px;" ]
    [ HH.span
        [ HP.style $ "font-size: 14px; " <> if ok then "color: #2D7D46;" else "color: #C0392B;" ]
        [ HH.text $ if ok then "\x2713" else "\x2717" ]
    , HH.span_ [ HH.text label ]
    ]

renderIssue :: forall m w. Loader.ValidationIssue -> HH.HTML w m
renderIssue issue =
  HH.div
    [ HP.style $ "font-size: 11px; margin-bottom: 4px; padding-left: 20px; "
        <> if issue.severity == "error" then "color: #C0392B;" else "color: #B8860B;"
    ]
    [ HH.text issue.message ]

-- =============================================================================
-- Action Handlers
-- =============================================================================

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  Initialize -> pure unit

  Receive input -> do
    H.modify_ _ { projects = input.projects, dataReady = input.dataReady }

  StartAddProject ->
    H.modify_ _ { addPhase = EnteringPath, pathInput = "", nameOverride = "" }

  CancelAdd ->
    H.modify_ _ { addPhase = Idle, pathInput = "", nameOverride = "" }

  SetPathInput s ->
    H.modify_ _ { pathInput = s }

  SetNameOverride s ->
    H.modify_ _ { nameOverride = s }

  DoValidate -> do
    state <- H.get
    H.modify_ _ { addPhase = Validating }
    result <- liftAff $ Loader.validateProjectPath state.pathInput
    case result of
      Left err -> do
        log $ "[ProjectMgmt] Validation error: " <> err
        H.modify_ _ { addPhase = LoadError ("Validation failed: " <> err) }
      Right validation ->
        H.modify_ _ { addPhase = ValidationResult validation }

  DoLoad -> do
    state <- H.get
    H.modify_ _ { addPhase = LoadingProject }
    let name = if state.nameOverride == "" then Nothing else Just state.nameOverride
    result <- liftAff $ Loader.loadProject
      { path: state.pathInput
      , name: name
      , label: Nothing
      }
    case result of
      Left err -> do
        log $ "[ProjectMgmt] Load error: " <> err
        H.modify_ _ { addPhase = LoadError err }
      Right loadResult ->
        if loadResult.success then do
          H.modify_ _ { addPhase = LoadSuccess loadResult }
          H.raise (ProjectAdded loadResult)
        else
          H.modify_ _ { addPhase = LoadError (fromMaybe "Unknown error" loadResult.error) }

  ConfirmDelete projectId ->
    H.modify_ _ { confirmDeleteId = Just projectId }

  CancelDelete ->
    H.modify_ _ { confirmDeleteId = Nothing }

  DoDelete projectId -> do
    H.modify_ _ { confirmDeleteId = Nothing }
    result <- liftAff $ Loader.deleteProject projectId
    case result of
      Left err ->
        log $ "[ProjectMgmt] Delete error: " <> err
      Right _ -> do
        H.modify_ \s -> s { projects = Array.filter (\p -> p.id /= projectId) s.projects }
        H.raise (ProjectDeleted projectId)

  ExploreProject projectId ->
    H.raise (NavigateToProject projectId)

  GoToScene scene ->
    H.raise (NavigateToScene scene)
