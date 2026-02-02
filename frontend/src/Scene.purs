-- | Scene-Based Navigation (Streamlined)
-- |
-- | Clean state machine for "YouTube teaser" navigation path.
-- | Linear exploration with optional overlays.
-- |
-- | Navigation path:
-- |   GalaxyTreemap → GalaxyBeeswarm → SolarSwarm → PkgTreemap → PkgModuleBeeswarm
-- |   (with focalPackage as filter on SolarSwarm, not a separate scene)
module CE2.Scene
  ( Scene(..)
  , parentScene
  , sceneLabel
  , isGalaxyScene
  , isSolarScene
  , isPackageScene
  , isModuleScene
  ) where

import Prelude

-- | Scene type representing distinct visualization states
-- |
-- | 6 scenes for streamlined teaser navigation.
-- | Note: PkgNeighborhood was merged into SolarSwarm (focalPackage filter).
data Scene
  = GalaxyTreemap                   -- Entry: blueprint treemap of full registry
  | GalaxyBeeswarm                  -- Topo beeswarm with scope filtering
  | SolarSwarm                      -- Bubblepacks: packages with modules inside (optionally filtered to focal)
  | PkgTreemap String               -- Paperwhite module treemap
  | PkgModuleBeeswarm String        -- Module beeswarm overlay on treemap
  | OverlayChordMatrix              -- Optional chord/matrix overlay (toggled)

derive instance eqScene :: Eq Scene

instance showScene :: Show Scene where
  show GalaxyTreemap = "GalaxyTreemap"
  show GalaxyBeeswarm = "GalaxyBeeswarm"
  show SolarSwarm = "SolarSwarm"
  show (PkgTreemap pkg) = "PkgTreemap(" <> pkg <> ")"
  show (PkgModuleBeeswarm pkg) = "PkgModuleBeeswarm(" <> pkg <> ")"
  show OverlayChordMatrix = "OverlayChordMatrix"

-- | Get the parent scene for back navigation
-- |
-- | Linear navigation path (instant jumps back):
-- |   GalaxyTreemap → GalaxyTreemap (root)
-- |   GalaxyBeeswarm → GalaxyTreemap
-- |   SolarSwarm → GalaxyBeeswarm
-- |   PkgTreemap → SolarSwarm
-- |   PkgModuleBeeswarm → PkgTreemap (same package)
-- |   OverlayChordMatrix → SolarSwarm (overlay returns to context)
parentScene :: Scene -> Scene
parentScene = case _ of
  GalaxyTreemap -> GalaxyTreemap           -- Root - no parent
  GalaxyBeeswarm -> GalaxyTreemap
  SolarSwarm -> GalaxyBeeswarm
  PkgTreemap _pkg -> SolarSwarm            -- Back to SolarSwarm (may have focal set)
  PkgModuleBeeswarm pkg -> PkgTreemap pkg  -- Back to same package's treemap
  OverlayChordMatrix -> SolarSwarm         -- Overlay closes back to SolarSwarm

-- | Human-readable label for display in navigation UI
sceneLabel :: Scene -> String
sceneLabel = case _ of
  GalaxyTreemap -> "Galaxy (Treemap)"
  GalaxyBeeswarm -> "Galaxy (Beeswarm)"
  SolarSwarm -> "Project Packages"
  PkgTreemap pkg -> pkg <> " Modules"
  PkgModuleBeeswarm pkg -> pkg <> " Module Flow"
  OverlayChordMatrix -> "Dependency Matrix"

-- | Check if scene is at the Galaxy level (registry-wide)
isGalaxyScene :: Scene -> Boolean
isGalaxyScene GalaxyTreemap = true
isGalaxyScene GalaxyBeeswarm = true
isGalaxyScene _ = false

-- | Check if scene is at the Solar level (project scope)
isSolarScene :: Scene -> Boolean
isSolarScene SolarSwarm = true
isSolarScene OverlayChordMatrix = true     -- Overlay is at Solar level
isSolarScene _ = false

-- | Check if scene is at the Package level
isPackageScene :: Scene -> Boolean
isPackageScene (PkgTreemap _) = true
isPackageScene (PkgModuleBeeswarm _) = true
isPackageScene _ = false

-- | Check if scene is at the Module level (deepest zoom)
isModuleScene :: Scene -> Boolean
isModuleScene (PkgModuleBeeswarm _) = true
isModuleScene _ = false
