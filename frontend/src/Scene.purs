-- | Scene-Based Navigation
-- |
-- | Clean state machine with breadcrumb navigation.
-- | Drill-in interaction with modifier+click for filtering.
-- |
-- | Navigation path:
-- |   GalaxyTreemap → PkgTreemap → ModuleOverview → DeclarationDetail
-- |   (with focalPackage as filter on SolarSwarm, not a separate scene)
module CE2.Scene
  ( Scene(..)
  , BreadcrumbSegment
  , parentScene
  , sceneBreadcrumbs
  , sceneLabel
  , shortModuleName
  , isGalaxyScene
  , isSolarScene
  , isPackageScene
  , isModuleScene
  , sceneToString
  , sceneFromString
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.String as String

-- | Scene type representing distinct visualization states
data Scene
  = GalaxyTreemap                   -- Entry: blueprint treemap of full registry
  | GalaxyBeeswarm                  -- Topo beeswarm with scope filtering
  | SolarSwarm                      -- Bubblepacks: packages with modules inside (optionally filtered to focal)
  | PkgTreemap String               -- Paperwhite module treemap
  | PkgModuleBeeswarm String        -- Module beeswarm overlay on treemap
  | ModuleOverview String String    -- Module overview: bubble pack + declaration listing (pkg, module)
  | DeclarationDetail String String String  -- Single declaration detail (pkg, module, decl)
  | TypeClassGrid                   -- Grid view of all type classes with method/instance counts

derive instance eqScene :: Eq Scene

instance showScene :: Show Scene where
  show GalaxyTreemap = "GalaxyTreemap"
  show GalaxyBeeswarm = "GalaxyBeeswarm"
  show SolarSwarm = "SolarSwarm"
  show (PkgTreemap pkg) = "PkgTreemap(" <> pkg <> ")"
  show (PkgModuleBeeswarm pkg) = "PkgModuleBeeswarm(" <> pkg <> ")"
  show (ModuleOverview pkg mod) = "ModuleOverview(" <> pkg <> "," <> mod <> ")"
  show (DeclarationDetail pkg mod decl) = "DeclarationDetail(" <> pkg <> "," <> mod <> "," <> decl <> ")"
  show TypeClassGrid = "TypeClassGrid"

-- | Get the parent scene for back navigation
parentScene :: Scene -> Scene
parentScene = case _ of
  GalaxyTreemap -> GalaxyTreemap           -- Root - no parent
  GalaxyBeeswarm -> GalaxyTreemap
  SolarSwarm -> GalaxyBeeswarm
  PkgTreemap _pkg -> SolarSwarm            -- Back to SolarSwarm (may have focal set)
  PkgModuleBeeswarm pkg -> PkgTreemap pkg  -- Back to same package's treemap
  ModuleOverview pkg _ -> PkgTreemap pkg   -- Back to package treemap
  DeclarationDetail pkg mod _ -> ModuleOverview pkg mod  -- Back to module overview
  TypeClassGrid -> GalaxyTreemap           -- Type class view returns to galaxy

-- | A segment in the breadcrumb trail
type BreadcrumbSegment = { kind :: String, label :: String, scene :: Scene }

-- | Build breadcrumb trail for a scene
-- | The last segment is the current scene (displayed bold, not clickable).
-- | Earlier segments are clickable navigation targets.
sceneBreadcrumbs :: Scene -> Array BreadcrumbSegment
sceneBreadcrumbs = case _ of
  GalaxyTreemap       -> [reg]
  GalaxyBeeswarm      -> [reg]
  TypeClassGrid       -> [reg]
  SolarSwarm          -> [reg, { kind: "", label: "Packages", scene: SolarSwarm }]
  PkgTreemap pkg      -> [reg, pkgSeg pkg]
  PkgModuleBeeswarm p -> [reg, pkgSeg p]
  ModuleOverview p m  -> [reg, pkgSeg p, modSeg p m]
  DeclarationDetail p m d -> [reg, pkgSeg p, modSeg p m
                                  , { kind: "Decl", label: d, scene: DeclarationDetail p m d }]
  where
    reg = { kind: "", label: "Registry", scene: GalaxyTreemap }
    pkgSeg pkg = { kind: "Package", label: pkg, scene: PkgTreemap pkg }
    modSeg p m = { kind: "Module", label: shortModuleName m, scene: ModuleOverview p m }

-- | Human-readable label for display in navigation UI
sceneLabel :: Scene -> String
sceneLabel = case _ of
  GalaxyTreemap -> "Galaxy (Treemap)"
  GalaxyBeeswarm -> "Galaxy (Beeswarm)"
  SolarSwarm -> "Project Packages"
  PkgTreemap pkg -> pkg <> " Modules"
  PkgModuleBeeswarm pkg -> pkg <> " Module Flow"
  ModuleOverview _ mod -> shortModuleName mod
  DeclarationDetail _ _ decl -> decl
  TypeClassGrid -> "Type Classes"

-- | Check if scene is at the Galaxy level (registry-wide)
isGalaxyScene :: Scene -> Boolean
isGalaxyScene GalaxyTreemap = true
isGalaxyScene GalaxyBeeswarm = true
isGalaxyScene _ = false

-- | Check if scene is at the Solar level (project scope)
isSolarScene :: Scene -> Boolean
isSolarScene SolarSwarm = true
isSolarScene _ = false

-- | Check if scene is at the Package level
isPackageScene :: Scene -> Boolean
isPackageScene (PkgTreemap _) = true
isPackageScene (PkgModuleBeeswarm _) = true
isPackageScene _ = false

-- | Check if scene is at the Module level (deepest zoom)
isModuleScene :: Scene -> Boolean
isModuleScene (PkgModuleBeeswarm _) = true
isModuleScene (ModuleOverview _ _) = true
isModuleScene (DeclarationDetail _ _ _) = true
isModuleScene _ = false

-- | Serialize scene to string for browser history state
sceneToString :: Scene -> String
sceneToString = show  -- Use the Show instance

-- | Parse scene from string (browser history state)
-- | Returns the parsed scene or Nothing if invalid
sceneFromString :: String -> Maybe Scene
sceneFromString str
  | str == "GalaxyTreemap" = Just GalaxyTreemap
  | str == "GalaxyBeeswarm" = Just GalaxyBeeswarm
  | str == "SolarSwarm" = Just SolarSwarm
  | str == "TypeClassGrid" = Just TypeClassGrid
  | String.take 11 str == "PkgTreemap(" =
      let inner = String.drop 11 str
          pkg = String.take (String.length inner - 1) inner  -- Remove trailing ")"
      in Just (PkgTreemap pkg)
  | String.take 18 str == "PkgModuleBeeswarm(" =
      let inner = String.drop 18 str
          pkg = String.take (String.length inner - 1) inner
      in Just (PkgModuleBeeswarm pkg)
  | String.take 15 str == "ModuleOverview(" =
      let inner = String.drop 15 str
          content = String.take (String.length inner - 1) inner  -- Remove trailing ")"
      in case String.indexOf (String.Pattern ",") content of
          Just idx ->
            let pkg = String.take idx content
                mod = String.drop (idx + 1) content
            in Just (ModuleOverview pkg mod)
          Nothing -> Nothing
  | String.take 18 str == "DeclarationDetail(" =
      let inner = String.drop 18 str
          content = String.take (String.length inner - 1) inner
      in case String.indexOf (String.Pattern ",") content of
          Just idx1 ->
            let pkg = String.take idx1 content
                rest = String.drop (idx1 + 1) content
            in case String.indexOf (String.Pattern ",") rest of
                Just idx2 ->
                  let mod = String.take idx2 rest
                      decl = String.drop (idx2 + 1) rest
                  in Just (DeclarationDetail pkg mod decl)
                Nothing -> Nothing
          Nothing -> Nothing
  | otherwise = Nothing

-- | Extract the last segment of a dotted module name
shortModuleName :: String -> String
shortModuleName name =
  case Array.last (String.split (String.Pattern ".") name) of
    Just short -> short
    Nothing -> name
