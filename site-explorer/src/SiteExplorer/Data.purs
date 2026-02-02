module SiteExplorer.Data where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import SiteExplorer.Types (RouteInfo, RouteStatus(..))

-- | Load route data - for now using sample data
-- | In production, this would parse from embedded JSON or fetch from server
loadRouteData :: Effect { routes :: Array RouteInfo, baseUrl :: String }
loadRouteData = pure
  { routes: sampleRoutes
  , baseUrl: "http://100.101.177.83"
  }

-- | Sample route data for testing
-- | This includes a mix of reachable and unreachable to demonstrate clustering
sampleRoutes :: Array RouteInfo
sampleRoutes =
  -- Reachable routes (gold nodes, right cluster)
  [ mkRoute "Home" "/" "Hylograph.Home" Reachable (Just 0) Nothing
  , mkRoute "TourIndex" "/tour" "Component.Tour.TourIndex" Reachable (Just 1) (Just "/")
  , mkRoute "TourScrolly" "/tour/scrolly" "Component.Tour.TourScrolly" Reachable (Just 2) (Just "/tour")
  , mkRoute "TourMotionScrollyHATS" "/tour/scrolly2" "Component.Tour.TourMotionScrollyHATS" Reachable (Just 2) (Just "/tour")
  , mkRoute "TourSimpsons" "/tour/simpsons" "Component.Tour.TourSimpsons" Reachable (Just 2) (Just "/tour")
  , mkRoute "ForcePlayground" "/force-playground" "Component.ForcePlayground" Reachable (Just 1) (Just "/")
  , mkRoute "TreeBuilder" "/tree-builder" "TreeBuilder.App" Reachable (Just 1) (Just "/")
  , mkRoute "SimpsonsV2" "/simpsons-v2" "Page.Simpsons" Reachable (Just 1) (Just "/")
  , mkRoute "ShowcaseLuaEdge" "/showcase/lua-edge" "Component.Showcase.ShowcaseLuaEdge" Reachable (Just 1) (Just "/")
  , mkRoute "Acknowledgements" "/acknowledgements" "Hylograph.Acknowledgements" Reachable (Just 1) (Just "/")
  , mkRoute "GettingStarted" "/getting-started" "Hylograph.Tutorial.GettingStarted" Reachable (Just 2) (Just "/tour")
  , mkRoute "HowtoIndex" "/howto" "Hylograph.HowTo.HowtoIndex" Reachable (Just 2) (Just "/tour")
  , mkRoute "Reference" "/reference" "Hylograph.Reference.Reference" Reachable (Just 2) (Just "/tour")
  , mkRoute "Understanding" "/understanding" "Hylograph.Understanding" Reachable (Just 2) (Just "/tour")

  -- Unreachable routes (burgundy nodes, left cluster)
  , mkRoute "TourMotionScrolly" "/tour/scrolly2-legacy" "Component.Tour.TourMotionScrolly" Unreachable Nothing Nothing
  , mkRoute "Showcase" "/showcase" "Component.Showcase.ShowcaseIndex" Unreachable Nothing Nothing
  , mkRoute "ModuleGraph" "/module-graph" "Component.ModuleGraph" Unreachable Nothing Nothing
  , mkRoute "MermaidTreeDemo" "/mermaid-tree-demo" "Component.MermaidTreeDemo" Unreachable Nothing Nothing
  , mkRoute "SPLOM" "/splom" "Component.SPLOM" Unreachable Nothing Nothing
  , mkRoute "TreeAPI" "/tree-api" "Component.TreeAPI" Unreachable Nothing Nothing
  , mkRoute "ForceConfigV2Test" "/force-config-v2-test" "Component.ForceConfigV2Test" Unreachable Nothing Nothing
  , mkRoute "GUPDebug" "/gup-debug" "Component.GUPDebug" Unreachable Nothing Nothing
  , mkRoute "AnimatedAttrTest" "/animated-attr-test" "Component.AnimatedAttrTest" Unreachable Nothing Nothing
  , mkRoute "Wizard" "/wizard" "Component.Wizard" Unreachable Nothing Nothing
  , mkRoute "SimpleChartDemo" "/simple-chart-demo" "Component.SimpleChartDemo" Unreachable Nothing Nothing
  , mkRoute "GeneralUpdatePattern" "/general-update-pattern" "Component.GeneralUpdatePattern" Unreachable Nothing Nothing
  , mkRoute "SankeyDemo" "/sankey-demo" "Component.SankeyDemo" Unreachable Nothing Nothing
  , mkRoute "PieDonutDemo" "/pie-donut-demo" "Component.PieDonutDemo" Unreachable Nothing Nothing
  , mkRoute "TourFPFTW" "/tour-fp-ftw" "Component.TourFPFTW" Unreachable Nothing Nothing
  , mkRoute "UnderstandingTreeAPI" "/understanding-tree-api" "Component.UnderstandingTreeAPI" Unreachable Nothing Nothing
  , mkRoute "NotFound" "/not-found" "Hylograph.NotFound" Unreachable Nothing Nothing
  , mkRoute "TreeBuilderV2" "/tree-builder-v2" "TreeBuilder.V2" Unreachable Nothing Nothing

  -- Archived routes (taupe nodes, left cluster with unreachable)
  , mkRoute "TourMotionScrollyOld" "/tour/motion-scrolly-old" "Component.Tour.OldScrolly" Archived Nothing Nothing
  , mkRoute "ReferenceModules" "/reference/modules" "Hylograph.Reference.Modules" Archived Nothing Nothing

  -- Extra (spidered but not defined - coral nodes)
  , mkRoute "/_attribute_variants" "/_attribute_variants" "?" Extra (Just 4) (Just "/understanding/attributes")
  , mkRoute "/_common_attributes" "/_common_attributes" "?" Extra (Just 4) (Just "/understanding/attributes")
  , mkRoute "/core-primitives" "/core-primitives" "?" Extra (Just 4) (Just "/understanding/scenes")
  ]

mkRoute :: String -> String -> String -> RouteStatus -> Maybe Int -> Maybe String -> RouteInfo
mkRoute name urlPath componentModule status depth foundFrom =
  { name, urlPath, componentModule, status, depth, foundFrom }
