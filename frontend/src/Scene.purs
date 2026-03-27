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
  , isMapScene
  , isGitScene
  , isAnatomyScene
  , isReportScene
  , isProjectScene
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
  | ModuleStructure String String -- Module structure view: diagrams + annotations (pkg, module)
  | ModuleSignatures String String -- Type signatures + git blame ribbon (pkg, module)
  | TypeClassGrid                   -- Grid view of all type classes with method/instance counts
  | NamespaceTree                   -- Horizontal tidy tree of module namespace hierarchy
  | PackageReport                   -- Package-level report cards with metrics + annotations
  | AnnotationReport                -- Interactive annotation report view
  | ProjectManagement               -- Project onboarding / management screen
  | ProjectAnatomy                  -- Project anatomy: workspace/direct/transitive beeswarm
  | PackageAnatomy String            -- Biconnected component decomposition of module graph (package)
  | ModuleAnatomy String String     -- Declaration-level decomposition (pkg, module)
  | CompareModules String String String String  -- Before/after comparison (pkg1, mod1, pkg2, mod2)
  | CompareSnapshots String String Int          -- Cross-snapshot comparison (pkg, module, beforeSnapshotId)
  | ProjectSetup                               -- Project CRUD (add/delete/list)
  | SnapshotManagement                         -- Snapshot creation + cleanup
  | GitOverview                                -- Git landing page: package inventory by source
  | CommitModuleGrid String                    -- Commit-module change grid (package)
  | CoChangeCube String                        -- 3D co-change tensor (package)

derive instance eqScene :: Eq Scene

instance showScene :: Show Scene where
  show GalaxyTreemap = "GalaxyTreemap"
  show GalaxyBeeswarm = "GalaxyBeeswarm"
  show SolarSwarm = "SolarSwarm"
  show (PkgTreemap pkg) = "PkgTreemap(" <> pkg <> ")"
  show (PkgModuleBeeswarm pkg) = "PkgModuleBeeswarm(" <> pkg <> ")"
  show (ModuleOverview pkg mod) = "ModuleOverview(" <> pkg <> "," <> mod <> ")"
  show (DeclarationDetail pkg mod decl) = "DeclarationDetail(" <> pkg <> "," <> mod <> "," <> decl <> ")"
  show (ModuleStructure pkg mod) = "ModuleStructure(" <> pkg <> "," <> mod <> ")"
  show (ModuleSignatures pkg mod) = "ModuleSignatures(" <> pkg <> "," <> mod <> ")"
  show TypeClassGrid = "TypeClassGrid"
  show NamespaceTree = "NamespaceTree"
  show PackageReport = "PackageReport"
  show AnnotationReport = "AnnotationReport"
  show ProjectManagement = "ProjectManagement"
  show ProjectSetup = "ProjectSetup"
  show ProjectAnatomy = "ProjectAnatomy"
  show (PackageAnatomy pkg) = "PackageAnatomy(" <> pkg <> ")"
  show (ModuleAnatomy pkg mod) = "ModuleAnatomy(" <> pkg <> "," <> mod <> ")"
  show (CompareModules p1 m1 p2 m2) = "CompareModules(" <> p1 <> "," <> m1 <> "," <> p2 <> "," <> m2 <> ")"
  show (CompareSnapshots p m sid) = "CompareSnapshots(" <> p <> "," <> m <> "," <> show sid <> ")"
  show SnapshotManagement = "SnapshotManagement"
  show GitOverview = "GitOverview"
  show (CommitModuleGrid pkg) = "CommitModuleGrid(" <> pkg <> ")"
  show (CoChangeCube pkg) = "CoChangeCube(" <> pkg <> ")"

-- | Get the parent scene for back navigation
parentScene :: Scene -> Scene
parentScene = case _ of
  GalaxyTreemap -> GalaxyTreemap           -- Root - no parent
  GalaxyBeeswarm -> GalaxyTreemap
  SolarSwarm -> GalaxyBeeswarm
  PkgTreemap _pkg -> SolarSwarm            -- Back to SolarSwarm (may have focal set)
  PkgModuleBeeswarm pkg -> PkgTreemap pkg  -- Back to same package's treemap
  ModuleOverview pkg _ -> PkgTreemap pkg   -- Back to package treemap
  DeclarationDetail pkg mod _ -> ModuleStructure pkg mod  -- Back to signature map (primary module view)
  ModuleStructure pkg _ -> PkgTreemap pkg                -- Back to package treemap
  ModuleSignatures pkg mod -> ModuleStructure pkg mod   -- Back to module structure
  TypeClassGrid -> GalaxyTreemap           -- Type class view returns to galaxy
  NamespaceTree -> GalaxyTreemap           -- Namespace tree returns to galaxy
  PackageReport -> ProjectManagement        -- Package report returns to landing
  AnnotationReport -> PackageReport         -- Module report returns to package report
  ProjectManagement -> ProjectManagement   -- Root-level, no parent
  ProjectSetup -> ProjectManagement        -- Back to landing
  ProjectAnatomy -> ProjectAnatomy         -- Root-level, no parent
  PackageAnatomy _ -> ProjectAnatomy     -- Back to project anatomy
  ModuleAnatomy pkg _ -> PackageAnatomy pkg  -- Back to package anatomy
  CompareModules _ _ _ _ -> GalaxyTreemap               -- Compare view returns to galaxy
  CompareSnapshots p m _ -> ModuleStructure p m      -- Back to the module being compared
  SnapshotManagement -> ProjectSetup                   -- Back to project setup
  GitOverview -> GitOverview                          -- Root of git family
  CommitModuleGrid _ -> GitOverview                   -- Back to git overview
  CoChangeCube pkg -> CommitModuleGrid pkg            -- Back to 2D commit grid

-- | A segment in the breadcrumb trail
type BreadcrumbSegment = { kind :: String, label :: String, scene :: Scene }

-- | Build breadcrumb trail for a scene
-- | The last segment is the current scene (displayed bold, not clickable).
-- | Earlier segments are clickable navigation targets.
sceneBreadcrumbs :: Scene -> Array BreadcrumbSegment
sceneBreadcrumbs = case _ of
  -- Maps family (Powers of Ten: Galaxy → SolarSystem → Planet)
  GalaxyTreemap       -> [mapsSeg, galaxySeg]
  GalaxyBeeswarm      -> [mapsSeg, galaxySeg]
  SolarSwarm          -> [mapsSeg, galaxySeg]
  PkgTreemap pkg      -> [mapsSeg, galaxySeg, solarSeg pkg]
  PkgModuleBeeswarm p -> [mapsSeg, galaxySeg, solarSeg p]
  ModuleStructure p m -> [mapsSeg, galaxySeg, solarSeg p, planetSeg p m]
  ModuleSignatures p m -> [mapsSeg, galaxySeg, solarSeg p, planetSeg p m]
  ModuleOverview p m  -> [mapsSeg, galaxySeg, solarSeg p, planetSeg p m]
  DeclarationDetail p m d -> [mapsSeg, galaxySeg, solarSeg p, planetSeg p m
                                  , { kind: "Decl", label: d, scene: DeclarationDetail p m d }]
  CompareModules p1 m1 _ m2 -> [mapsSeg, galaxySeg, solarSeg p1, { kind: "", label: shortModuleName m1 <> " vs " <> shortModuleName m2, scene: CompareModules p1 m1 p1 m2 }]
  CompareSnapshots p m _ -> [mapsSeg, galaxySeg, solarSeg p, planetSeg p m, { kind: "", label: "Compare", scene: CompareSnapshots p m 0 }]
  GitOverview -> [gitSeg]
  CommitModuleGrid pkg -> [gitSeg, gitPkgSeg pkg, { kind: "", label: "Commits", scene: CommitModuleGrid pkg }]
  CoChangeCube pkg -> [gitSeg, gitPkgSeg pkg, { kind: "", label: "Commits", scene: CommitModuleGrid pkg }, { kind: "", label: "Cube", scene: CoChangeCube pkg }]

  -- Anatomy family
  ProjectAnatomy      -> [anatomySeg]
  PackageAnatomy pkg -> [anatomySeg, { kind: "Package", label: pkg, scene: PackageAnatomy pkg }]
  ModuleAnatomy p m -> [anatomySeg, { kind: "Package", label: p, scene: PackageAnatomy p }, { kind: "Module", label: shortModuleName m, scene: ModuleAnatomy p m }]

  -- Reports family
  PackageReport       -> [reportsSeg]
  AnnotationReport    -> [reportsSeg, { kind: "", label: "Modules", scene: AnnotationReport }]

  -- Projects family
  ProjectManagement   -> [{ kind: "", label: "Home", scene: ProjectManagement }]
  ProjectSetup        -> [projectsSeg]
  SnapshotManagement  -> [projectsSeg, { kind: "", label: "Snapshots", scene: SnapshotManagement }]

  -- Deferred views
  TypeClassGrid       -> [mapsSeg, { kind: "", label: "Types", scene: TypeClassGrid }]
  NamespaceTree       -> [mapsSeg, { kind: "", label: "Namespaces", scene: NamespaceTree }]
  where
    mapsSeg = { kind: "", label: "Maps", scene: GalaxyTreemap }
    galaxySeg = { kind: "", label: "Galaxy", scene: GalaxyTreemap }
    anatomySeg = { kind: "", label: "Anatomy", scene: ProjectAnatomy }
    reportsSeg = { kind: "", label: "Reports", scene: PackageReport }
    projectsSeg = { kind: "", label: "Projects", scene: ProjectSetup }
    gitSeg = { kind: "", label: "Git", scene: GitOverview }
    gitPkgSeg pkg = { kind: "Package", label: pkg, scene: CommitModuleGrid pkg }
    gitModSeg p m = { kind: "Module", label: shortModuleName m, scene: ModuleStructure p m }
    solarSeg pkg = { kind: "SolarSystem", label: pkg, scene: PkgTreemap pkg }
    planetSeg p m = { kind: "Planet", label: shortModuleName m, scene: ModuleStructure p m }

-- | Human-readable label for display in navigation UI
sceneLabel :: Scene -> String
sceneLabel = case _ of
  GalaxyTreemap -> "Galaxy"
  GalaxyBeeswarm -> "Galaxy (Beeswarm)"
  SolarSwarm -> "Project Packages"
  PkgTreemap pkg -> pkg <> " Modules"
  PkgModuleBeeswarm pkg -> pkg <> " Module Flow"
  ModuleOverview _ mod -> shortModuleName mod
  DeclarationDetail _ _ decl -> decl
  ModuleStructure _ mod -> shortModuleName mod
  ModuleSignatures _ mod -> shortModuleName mod <> " Signatures"
  TypeClassGrid -> "Type Classes"
  NamespaceTree -> "Namespace Tree"
  PackageReport -> "Package Report"
  AnnotationReport -> "Annotations"
  ProjectManagement -> "Home"
  ProjectSetup -> "Projects"
  ProjectAnatomy -> "Project Anatomy"
  PackageAnatomy pkg -> pkg <> " Anatomy"
  ModuleAnatomy _ mod -> shortModuleName mod <> " Anatomy"
  CompareModules _ m1 _ m2 -> shortModuleName m1 <> " vs " <> shortModuleName m2
  CompareSnapshots _ m _ -> shortModuleName m <> " (Compare)"
  SnapshotManagement -> "Snapshots"
  GitOverview -> "Git Overview"
  CommitModuleGrid pkg -> pkg <> " Commits"
  CoChangeCube pkg -> pkg <> " Co-Change Cube"

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
isPackageScene (CommitModuleGrid _) = true
isPackageScene (CoChangeCube _) = true
isPackageScene _ = false

-- | Check if scene is at the Module level (deepest zoom)
isModuleScene :: Scene -> Boolean
isModuleScene (PkgModuleBeeswarm _) = true
isModuleScene (ModuleOverview _ _) = true
isModuleScene (DeclarationDetail _ _ _) = true
isModuleScene (ModuleStructure _ _) = true
isModuleScene (ModuleSignatures _ _) = true
isModuleScene _ = false

-- | Check if scene belongs to the Maps family (treemap / powers-of-ten drill chain)
isMapScene :: Scene -> Boolean
isMapScene = case _ of
  GalaxyTreemap -> true
  GalaxyBeeswarm -> true
  SolarSwarm -> true
  PkgTreemap _ -> true
  PkgModuleBeeswarm _ -> true
  ModuleOverview _ _ -> true
  ModuleStructure _ _ -> true
  DeclarationDetail _ _ _ -> true
  CompareModules _ _ _ _ -> true
  CompareSnapshots _ _ _ -> true
  _ -> false

-- | Check if scene belongs to the Git family
isGitScene :: Scene -> Boolean
isGitScene = case _ of
  GitOverview -> true
  CommitModuleGrid _ -> true
  CoChangeCube _ -> true
  ModuleSignatures _ _ -> true
  _ -> false

-- | Check if scene belongs to the Anatomy family
isAnatomyScene :: Scene -> Boolean
isAnatomyScene = case _ of
  ProjectAnatomy -> true
  PackageAnatomy _ -> true
  ModuleAnatomy _ _ -> true
  _ -> false

-- | Check if scene belongs to the Reports family
isReportScene :: Scene -> Boolean
isReportScene = case _ of
  PackageReport -> true
  AnnotationReport -> true
  _ -> false

-- | Check if scene belongs to the Projects family
isProjectScene :: Scene -> Boolean
isProjectScene = case _ of
  ProjectSetup -> true
  SnapshotManagement -> true
  _ -> false

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
  | str == "NamespaceTree" = Just NamespaceTree
  | str == "PackageReport" = Just PackageReport
  | str == "AnnotationReport" = Just AnnotationReport
  | str == "ProjectManagement" = Just ProjectManagement
  | str == "ProjectAnatomy" = Just ProjectAnatomy
  | String.take 16 str == "PackageAnatomy(" =
      let inner = String.drop 16 str
          pkg = String.take (String.length inner - 1) inner
      in Just (PackageAnatomy pkg)
  | str == "ProjectSetup" = Just ProjectSetup
  | str == "SnapshotManagement" = Just SnapshotManagement
  | str == "GitOverview" = Just GitOverview
  | String.take 14 str == "ModuleAnatomy(" =
      let inner = String.drop 14 str
          content = String.take (String.length inner - 1) inner
      in case String.indexOf (String.Pattern ",") content of
          Just idx ->
            let pkg = String.take idx content
                mod = String.drop (idx + 1) content
            in Just (ModuleAnatomy pkg mod)
          Nothing -> Nothing
  | String.take 17 str == "CommitModuleGrid(" =
      let inner = String.drop 17 str
          pkg = String.take (String.length inner - 1) inner
      in Just (CommitModuleGrid pkg)
  | String.take 14 str == "CoChangeCube(" =
      let inner = String.drop 14 str
          pkg = String.take (String.length inner - 1) inner
      in Just (CoChangeCube pkg)
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
  | String.take 16 str == "ModuleStructure(" =
      let inner = String.drop 16 str
          content = String.take (String.length inner - 1) inner
      in case String.indexOf (String.Pattern ",") content of
          Just idx ->
            let pkg = String.take idx content
                mod = String.drop (idx + 1) content
            in Just (ModuleStructure pkg mod)
          Nothing -> Nothing
  | String.take 17 str == "ModuleSignatures(" =
      let inner = String.drop 17 str
          content = String.take (String.length inner - 1) inner
      in case String.indexOf (String.Pattern ",") content of
          Just idx ->
            let pkg = String.take idx content
                mod = String.drop (idx + 1) content
            in Just (ModuleSignatures pkg mod)
          Nothing -> Nothing
  | otherwise = Nothing

-- | Extract the last segment of a dotted module name
shortModuleName :: String -> String
shortModuleName name =
  case Array.last (String.split (String.Pattern ".") name) of
    Just short -> short
    Nothing -> name
