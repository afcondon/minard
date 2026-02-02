-- | Container IDs and Selectors
-- |
-- | Central registry of all DOM element IDs used by CE2 visualizations.
-- | Both Halogen (render) and Viz modules (D3) import from here.
-- |
-- | Organization: One section per scene, with container + child elements.
-- |
-- | Usage:
-- |   Halogen:  HP.id Containers.galaxyBeeswarmContainerId
-- |   Viz:      containerSelector: Containers.galaxyBeeswarmContainer
-- |   PSD3:     container: Containers.galaxyBeeswarmNodes
module CE2.Containers
  ( -- Types
    ElementId
  , Selector
  , toSelector
    -- GalaxyTreemap
  , galaxyTreemapContainerId
  , galaxyTreemapContainer
    -- GalaxyBeeswarm (has both treemap and beeswarm layers)
  , galaxyBeeswarmContainerId
  , galaxyBeeswarmContainer
  , galaxyBeeswarmNodesId
  , galaxyBeeswarmNodes
  , galaxyBeeswarmTreemapContainerId
  , galaxyBeeswarmTreemapContainer
    -- SolarSwarm / BubblePackBeeswarm
  , solarSwarmContainerId
  , solarSwarmContainer
  , solarSwarmNodesId
  , solarSwarmNodes
    -- BubblePackBeeswarm (current names, alias for SolarSwarm)
  , bubblePackBeeswarmContainerId
  , bubblePackBeeswarmContainer
  , bubblePackBeeswarmNodesId
  , bubblePackBeeswarmNodes
    -- PkgNeighborhood
  , pkgNeighborhoodContainerId
  , pkgNeighborhoodContainer
  , pkgNeighborhoodNodesId
  , pkgNeighborhoodNodes
    -- PkgTreemap
  , pkgTreemapContainerId
  , pkgTreemapContainer
    -- PkgModuleBeeswarm (overlay: treemap + beeswarm)
  , pkgModuleBeeswarmContainerId
  , pkgModuleBeeswarmContainer
  , pkgModuleBeeswarmNodesId
  , pkgModuleBeeswarmNodes
  , pkgModuleBeeswarmTreemapContainerId
  , pkgModuleBeeswarmTreemapContainer
    -- ModuleBeeswarm (standalone, not overlay)
  , moduleBeeswarmContainerId
  , moduleBeeswarmContainer
  , moduleBeeswarmNodesId
  , moduleBeeswarmNodes
    -- ModuleBubblePack
  , moduleBubblePackContainerId
  , moduleBubblePackContainer
  , moduleBubblePackNodesId
  , moduleBubblePackNodes
    -- CirclePack
  , circlePackContainerId
  , circlePackContainer
  , circlePackNodesId
  , circlePackNodes
    -- Module Chord (single-package view)
  , moduleChordContainerId
  , moduleChordContainer
    -- Module Adjacency (single-package view)
  , moduleAdjacencyContainerId
  , moduleAdjacencyContainer
    -- Package Chord (project package dependencies)
  , packageChordContainerId
  , packageChordContainer
    -- Package Adjacency (project package dependencies)
  , packageAdjacencyContainerId
  , packageAdjacencyContainer
  ) where

import Prelude

-- =============================================================================
-- Types
-- =============================================================================

-- | Element ID without the # prefix (for HP.id)
type ElementId = String

-- | CSS selector with # prefix (for D3 select)
type Selector = String

-- | Convert an element ID to a selector
toSelector :: ElementId -> Selector
toSelector id = "#" <> id

-- =============================================================================
-- GalaxyTreemap Scene
-- =============================================================================

galaxyTreemapContainerId :: ElementId
galaxyTreemapContainerId = "galaxy-treemap-container"

galaxyTreemapContainer :: Selector
galaxyTreemapContainer = toSelector galaxyTreemapContainerId

-- =============================================================================
-- GalaxyBeeswarm Scene (two layers: treemap background + beeswarm)
-- =============================================================================

-- Beeswarm layer (above)
galaxyBeeswarmContainerId :: ElementId
galaxyBeeswarmContainerId = "galaxy-beeswarm-container"

galaxyBeeswarmContainer :: Selector
galaxyBeeswarmContainer = toSelector galaxyBeeswarmContainerId

galaxyBeeswarmNodesId :: ElementId
galaxyBeeswarmNodesId = "galaxy-beeswarm-nodes"

galaxyBeeswarmNodes :: Selector
galaxyBeeswarmNodes = toSelector galaxyBeeswarmNodesId

-- Treemap layer (below)
galaxyBeeswarmTreemapContainerId :: ElementId
galaxyBeeswarmTreemapContainerId = "galaxy-treemap-container"

galaxyBeeswarmTreemapContainer :: Selector
galaxyBeeswarmTreemapContainer = toSelector galaxyBeeswarmTreemapContainerId

-- =============================================================================
-- SolarSwarm Scene (BubblePackBeeswarm)
-- =============================================================================

-- Future names (when we rename the component)
solarSwarmContainerId :: ElementId
solarSwarmContainerId = "solar-swarm-container"

solarSwarmContainer :: Selector
solarSwarmContainer = toSelector solarSwarmContainerId

solarSwarmNodesId :: ElementId
solarSwarmNodesId = "solar-swarm-nodes"

solarSwarmNodes :: Selector
solarSwarmNodes = toSelector solarSwarmNodesId

-- Current names (BubblePackBeeswarm component)
bubblePackBeeswarmContainerId :: ElementId
bubblePackBeeswarmContainerId = "bubblepack-beeswarm-container"

bubblePackBeeswarmContainer :: Selector
bubblePackBeeswarmContainer = toSelector bubblePackBeeswarmContainerId

bubblePackBeeswarmNodesId :: ElementId
bubblePackBeeswarmNodesId = "bubblepack-nodes-group"

bubblePackBeeswarmNodes :: Selector
bubblePackBeeswarmNodes = toSelector bubblePackBeeswarmNodesId

-- =============================================================================
-- PkgNeighborhood Scene
-- =============================================================================

pkgNeighborhoodContainerId :: ElementId
pkgNeighborhoodContainerId = "pkg-neighborhood-container"

pkgNeighborhoodContainer :: Selector
pkgNeighborhoodContainer = toSelector pkgNeighborhoodContainerId

pkgNeighborhoodNodesId :: ElementId
pkgNeighborhoodNodesId = "pkg-neighborhood-nodes"

pkgNeighborhoodNodes :: Selector
pkgNeighborhoodNodes = toSelector pkgNeighborhoodNodesId

-- =============================================================================
-- PkgTreemap Scene
-- =============================================================================

pkgTreemapContainerId :: ElementId
pkgTreemapContainerId = "pkg-treemap-container"

pkgTreemapContainer :: Selector
pkgTreemapContainer = toSelector pkgTreemapContainerId

-- =============================================================================
-- PkgModuleBeeswarm Scene (overlay: treemap + module beeswarm)
-- =============================================================================

-- Treemap layer (below)
pkgModuleBeeswarmTreemapContainerId :: ElementId
pkgModuleBeeswarmTreemapContainerId = "pkg-module-treemap-container"

pkgModuleBeeswarmTreemapContainer :: Selector
pkgModuleBeeswarmTreemapContainer = toSelector pkgModuleBeeswarmTreemapContainerId

-- Beeswarm layer (above)
pkgModuleBeeswarmContainerId :: ElementId
pkgModuleBeeswarmContainerId = "pkg-module-beeswarm-container"

pkgModuleBeeswarmContainer :: Selector
pkgModuleBeeswarmContainer = toSelector pkgModuleBeeswarmContainerId

pkgModuleBeeswarmNodesId :: ElementId
pkgModuleBeeswarmNodesId = "pkg-module-beeswarm-nodes"

pkgModuleBeeswarmNodes :: Selector
pkgModuleBeeswarmNodes = toSelector pkgModuleBeeswarmNodesId

-- =============================================================================
-- ModuleBeeswarm (standalone component, not overlay)
-- =============================================================================

moduleBeeswarmContainerId :: ElementId
moduleBeeswarmContainerId = "module-beeswarm-container"

moduleBeeswarmContainer :: Selector
moduleBeeswarmContainer = toSelector moduleBeeswarmContainerId

moduleBeeswarmNodesId :: ElementId
moduleBeeswarmNodesId = "module-beeswarm-nodes"

moduleBeeswarmNodes :: Selector
moduleBeeswarmNodes = toSelector moduleBeeswarmNodesId

-- =============================================================================
-- ModuleBubblePack (standalone component)
-- =============================================================================

moduleBubblePackContainerId :: ElementId
moduleBubblePackContainerId = "module-bubblepack-container"

moduleBubblePackContainer :: Selector
moduleBubblePackContainer = toSelector moduleBubblePackContainerId

moduleBubblePackNodesId :: ElementId
moduleBubblePackNodesId = "module-bubblepack-nodes-group"

moduleBubblePackNodes :: Selector
moduleBubblePackNodes = toSelector moduleBubblePackNodesId

-- =============================================================================
-- CirclePack (standalone component)
-- =============================================================================

circlePackContainerId :: ElementId
circlePackContainerId = "circlepack-container"

circlePackContainer :: Selector
circlePackContainer = toSelector circlePackContainerId

circlePackNodesId :: ElementId
circlePackNodesId = "packages-group"

circlePackNodes :: Selector
circlePackNodes = toSelector circlePackNodesId

-- =============================================================================
-- Module Chord Diagram (single-package module dependencies)
-- =============================================================================

moduleChordContainerId :: ElementId
moduleChordContainerId = "module-chord-container"

moduleChordContainer :: Selector
moduleChordContainer = toSelector moduleChordContainerId

-- =============================================================================
-- Module Adjacency Matrix (single-package module dependencies)
-- =============================================================================

moduleAdjacencyContainerId :: ElementId
moduleAdjacencyContainerId = "module-adjacency-container"

moduleAdjacencyContainer :: Selector
moduleAdjacencyContainer = toSelector moduleAdjacencyContainerId

-- =============================================================================
-- Package Chord Diagram (project package dependencies)
-- =============================================================================

packageChordContainerId :: ElementId
packageChordContainerId = "package-chord-container"

packageChordContainer :: Selector
packageChordContainer = toSelector packageChordContainerId

-- =============================================================================
-- Package Adjacency Matrix (project package dependencies)
-- =============================================================================

packageAdjacencyContainerId :: ElementId
packageAdjacencyContainerId = "package-adjacency-container"

packageAdjacencyContainer :: Selector
packageAdjacencyContainer = toSelector packageAdjacencyContainerId
