-- | Core types for Code Explorer
module CE2.Types
  ( module CE2.Types
  ) where

import Prelude
import Data.Foldable (foldr)
import Data.Int (toNumber)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Set (Set)
import Data.Set as Set
import Hylograph.Kernel.D3.Simulation (SimulationNode, Link)
import Hylograph.Transition.Tick as Tick

-- | A module in the codebase
type Module =
  { name :: String
  , package :: String
  , depends :: Array String
  , path :: String
  , loc :: Maybe Int
  }

-- | A package in the codebase
type Package =
  { name :: String
  , depends :: Array String
  , modules :: Array String
  }

-- | A simulation node (module or package)
-- | Extends SimulationNode with app-specific fields
type SimNode = SimulationNode
  ( name :: String
  , nodeType :: NodeType
  , package :: String
  , path :: String         -- Source file path (for opening in editor)
  , r :: Number            -- Radius (based on LOC or constant)
  , cluster :: Int         -- For coloring by package
  , targets :: Array Int   -- Outgoing dependency IDs
  , sources :: Array Int   -- Incoming dependency IDs (dependents)
  , gridX :: Number        -- Grid position (for Grid scene)
  , gridY :: Number
  , orbitAngle :: Number   -- Orbital angle (for Orbit scene, packages only)
  , treeX :: Number        -- Tree position (vertical tree layout)
  , treeY :: Number
  , radialX :: Number      -- Radial tree position (polar projection of tree layout)
  , radialY :: Number
  , isInTree :: Boolean    -- True if node is reachable from root in spanning tree
  , topoX :: Number        -- Topological position (for Topo scene, packages only)
  , topoY :: Number
  , topoLayer :: Int       -- Topological layer (0 = no dependencies)
  , source :: String       -- Package source: "workspace" | "registry" | "extra"
  )

-- | Node type discriminator
data NodeType
  = ModuleNode
  | PackageNode

derive instance eqNodeType :: Eq NodeType
derive instance ordNodeType :: Ord NodeType

-- | Scale level for "Powers of Ten" navigation
-- | From highest abstraction (registry timeline) down to lowest (within-module)
data ScaleLevel
  = RegistryTimelineScale  -- Package persistence across all package set versions
  | PackageSetScale        -- Single package set as beeswarm (568 packages)
  | ProjectDepsScale       -- Our packages + direct dependencies (bubble pack swarm)
  | ProjectDepsTreemapScale -- Our packages + direct dependencies (treemap view)
  | ProjectOnlyScale       -- Just our ~10 packages (~150 modules)
  | ModuleSubtreeScale     -- Focused branch from a specific module
  | WithinModuleScale      -- Inside a single module (declarations)

derive instance eqScaleLevel :: Eq ScaleLevel
derive instance ordScaleLevel :: Ord ScaleLevel

instance showScaleLevel :: Show ScaleLevel where
  show RegistryTimelineScale = "Registry Timeline"
  show PackageSetScale = "Package Set"
  show ProjectDepsScale = "Project + Deps (Swarm)"
  show ProjectDepsTreemapScale = "Project + Deps (Treemap)"
  show ProjectOnlyScale = "Project Only"
  show ModuleSubtreeScale = "Module Subtree"
  show WithinModuleScale = "Within Module"

-- | Scale levels in order from highest to lowest
allScaleLevels :: Array ScaleLevel
allScaleLevels =
  [ RegistryTimelineScale
  , PackageSetScale
  , ProjectDepsScale
  , ProjectDepsTreemapScale
  , ProjectOnlyScale
  , ModuleSubtreeScale
  , WithinModuleScale
  ]

-- =============================================================================
-- Treemap Parameterization Types
-- =============================================================================

-- | What to render inside each treemap cell
-- | Enables same treemap layout with different visual representations
data CellContents
  = CellEmpty           -- Just the rectangle, no content
  | CellText            -- Package/module name as text label
  | CellCircle          -- Single circle (package or module)
  | CellModuleCircles   -- Circles for each module in package
  | CellBubblePack      -- Packed layout: outer = container, inner = children

derive instance eqCellContents :: Eq CellContents

instance showCellContents :: Show CellContents where
  show CellEmpty = "Empty"
  show CellText = "Text"
  show CellCircle = "Circle"
  show CellModuleCircles = "Module Circles"
  show CellBubblePack = "Bubble Pack"

-- | Visual theme for treemap - distinguishes scale levels
-- | Each theme defines background, stroke, and text colors
-- | 5-level dark-to-light luminance gradient matching abstract→concrete
data ViewTheme
  = MidnightTheme     -- Near-black background, white text (package set / galaxy)
  | BlueprintTheme    -- Dark navy, white text (neighborhood / solar system)
  | SteelTheme        -- Blueprint blue, white text/strokes (package level)
  | MistTheme         -- Pale blue, dark text (module level)
  | DaylightTheme     -- White, dark text (declaration level)

derive instance eqViewTheme :: Eq ViewTheme

instance showViewTheme :: Show ViewTheme where
  show MidnightTheme = "Midnight"
  show BlueprintTheme = "Blueprint"
  show SteelTheme = "Steel"
  show MistTheme = "Mist"
  show DaylightTheme = "Daylight"

-- | Check if a theme uses light text on dark background
isDarkTheme :: ViewTheme -> Boolean
isDarkTheme MidnightTheme = true
isDarkTheme BlueprintTheme = true
isDarkTheme SteelTheme = true
isDarkTheme _ = false

-- | Get theme colors for rendering
-- | Returns { background, stroke, text, textMuted }
themeColors :: ViewTheme -> { background :: String, stroke :: String, text :: String, textMuted :: String }
themeColors MidnightTheme =
  { background: "#0A0A1A"
  , stroke: "rgba(255, 255, 255, 0.25)"
  , text: "rgba(255, 255, 255, 0.9)"
  , textMuted: "rgba(255, 255, 255, 0.5)"
  }
themeColors BlueprintTheme =
  { background: "#082B5A"
  , stroke: "rgba(255, 255, 255, 0.25)"
  , text: "rgba(255, 255, 255, 0.9)"
  , textMuted: "rgba(255, 255, 255, 0.5)"
  }
themeColors SteelTheme =
  { background: "#0E4C8A"
  , stroke: "rgba(255, 255, 255, 0.3)"
  , text: "rgba(255, 255, 255, 0.9)"
  , textMuted: "rgba(255, 255, 255, 0.5)"
  }
themeColors MistTheme =
  { background: "#D8E4EE"
  , stroke: "rgba(0, 0, 0, 0.12)"
  , text: "rgba(0, 0, 0, 0.87)"
  , textMuted: "rgba(0, 0, 0, 0.54)"
  }
themeColors DaylightTheme =
  { background: "#FFFFFF"
  , stroke: "rgba(0, 0, 0, 0.1)"
  , text: "rgba(0, 0, 0, 0.87)"
  , textMuted: "rgba(0, 0, 0, 0.54)"
  }

-- | Registry packages used by this project (CE2 direct dependencies)
-- | These are the public registry package names that CE2 depends on.
-- | Local packages (psd3-*, ce-*) are not in the public registry.
projectPackages :: Array String
projectPackages =
  [ "aff"
  , "affjax"
  , "affjax-web"
  , "argonaut-codecs"
  , "argonaut-core"
  , "arrays"
  , "console"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "foreign"
  , "foreign-object"
  , "halogen"
  , "integers"
  , "maybe"
  , "newtype"
  , "nullable"
  , "numbers"
  , "ordered-collections"
  , "prelude"
  , "random"
  , "strings"
  , "tuples"
  , "web-dom"
  , "web-events"
  , "web-html"
  , "web-uievents"
  ]

-- | Check if a package is one of ours
isProjectPackage :: String -> Boolean
isProjectPackage pkg = pkg `elem` projectPackages
  where
  elem x xs = any (_ == x) xs
  any f = foldr (\x acc -> f x || acc) false

-- | A link between nodes
-- | Extends the library's Link type with app-specific linkType field
type SimLink = Link Int (linkType :: LinkType)

-- | Link type discriminator
-- | M2M_Tree: Module-to-module link in spanning tree (for tree visualization)
-- | M2M_Graph: Module-to-module link NOT in spanning tree (redundant edge)
-- | P2P: Package-to-package dependency
-- | M2P: Module-to-package containment
data LinkType
  = M2M_Tree       -- Module link in spanning tree
  | M2M_Graph      -- Module link not in spanning tree
  | P2P            -- Package to package
  | M2P            -- Module to package (containment)

derive instance eqLinkType :: Eq LinkType
derive instance ordLinkType :: Ord LinkType

instance showLinkType :: Show LinkType where
  show M2M_Tree = "M2M-Tree"
  show M2M_Graph = "M2M-Graph"
  show P2P = "P2P"
  show M2P = "M2P"

-- | Link predicates
isTreeLink :: SimLink -> Boolean
isTreeLink l = l.linkType == M2M_Tree

isGraphLink :: SimLink -> Boolean
isGraphLink l = l.linkType == M2M_Graph

-- | The complete model
type Model =
  { nodes :: Array SimNode
  , links :: Array SimLink
  , packages :: Array Package
  }

-- =============================================================================
-- Scale Transition Types (Powers of Ten animated transitions)
-- =============================================================================

-- | Phases of a scale-level transition animation
-- | Used for animated zoom-in (coarser → finer scale)
data ScaleTransitionPhase
  = ZoomOut            -- ViewBox expands first (establish new scale)
  | FadeOut            -- Fade irrelevant packages (opacity 1→0)
  | PopIn              -- Remaining packages grow into bubble packs with modules
  | MoveToTreemap      -- Packages move to treemap positions
  | CrossfadeToModules -- Package circles fade, modules appear
  | TransitionDone     -- Animation complete

derive instance eqScaleTransitionPhase :: Eq ScaleTransitionPhase

instance showScaleTransitionPhase :: Show ScaleTransitionPhase where
  show ZoomOut = "ZoomOut"
  show FadeOut = "FadeOut"
  show PopIn = "PopIn"
  show MoveToTreemap = "MoveToTreemap"
  show CrossfadeToModules = "CrossfadeToModules"
  show TransitionDone = "TransitionDone"

-- | A module positioned within its parent package (for packed circle transitions)
type PackedModulePosition =
  { nodeId :: Int           -- Module's node ID (for looking up full data in modelData)
  , x :: Number             -- Position relative to package center
  , y :: Number
  , r :: Number             -- Radius
  }

-- | A package with its modules packed inside (for transitions)
type PackedPackageData =
  { packageId :: Int                          -- Package's node ID (matches DOM data-id)
  , name :: String
  , modules :: Array PackedModulePosition     -- Positioned modules inside
  , enclosingRadius :: Number                 -- Radius needed to contain all modules
  , color :: String                           -- Package color
  }

-- | Package rectangle for treemap backdrop (used in treemap transitions)
type TreemapRect =
  { name :: String
  , x :: Number
  , y :: Number
  , width :: Number
  , height :: Number
  }

-- | ViewBox specification for SVG viewport (includes origin offset)
type ViewBoxSpec =
  { minX :: Number
  , minY :: Number
  , width :: Number
  , height :: Number
  }

-- | State for an in-progress scale transition
-- | All state lives in Halogen; this record is stored as Maybe in app state
type ScaleTransitionState =
  { phase :: ScaleTransitionPhase
  , progress :: Tick.Progress           -- 0.0 to 1.0 within current phase
  , fromScale :: ScaleLevel
  , toScale :: ScaleLevel
  , fadingIds :: Set Int                -- IDs of nodes being faded out
  , remainingIds :: Set Int             -- IDs of nodes that stay visible
  , sourcePositions :: Map Int { x :: Number, y :: Number, r :: Number }
  , targetPositions :: Map Int { x :: Number, y :: Number, r :: Number }
  -- Viewport zoom parameters (for GrowCircles phase)
  , sourceViewBox :: { width :: Number, height :: Number }
  , targetViewBox :: { width :: Number, height :: Number }
  , radiusMultiplier :: Number          -- How much to grow circles (e.g., 3.0 = 3x bigger)
  -- Packed module data (computed at transition start, used during PopIn)
  , packedPackages :: Array PackedPackageData
  -- Treemap data (computed when starting MoveToTreemap phase)
  , treemapRects :: Array TreemapRect
  , treemapTargets :: Map Int { x :: Number, y :: Number }  -- Treemap centers by ID
  -- Name-based position maps (for MoveToTreemap when matching different data sources)
  -- Used because DOM groups have IDs from packageSetData, but treemap computed from modelData
  , sourcePositionsByName :: Map String { x :: Number, y :: Number }
  , treemapTargetsByName :: Map String { x :: Number, y :: Number }
  }

-- =============================================================================
-- Circle Coloring (shared by treemap and beeswarm views)
-- =============================================================================

-- | Color mode for circle coloring
data ColorMode
  = DefaultUniform      -- Use theme's default circle color
  | ProjectScope        -- Highlight project packages
  | FullRegistryTopo    -- Color by topo layer across full registry
  | ProjectScopeTopo    -- Color by topo layer within project scope
  | PublishDate         -- Color by publish date (beeswarm default)
  | GitStatus           -- Color by git working tree status (modified/staged/clean)
  | Reachability        -- Color by external reachability (dead code detection)
  | ClusterView         -- Color by dependency cluster (connected components / communities)

derive instance eqColorMode :: Eq ColorMode

instance showColorMode :: Show ColorMode where
  show DefaultUniform = "DefaultUniform"
  show ProjectScope = "ProjectScope"
  show FullRegistryTopo = "FullRegistryTopo"
  show ProjectScopeTopo = "ProjectScopeTopo"
  show PublishDate = "PublishDate"
  show GitStatus = "GitStatus"
  show Reachability = "Reachability"
  show ClusterView = "ClusterView"

-- | Bright color for project packages (theme-appropriate)
brightColor :: ViewTheme -> String
brightColor theme = if isDarkTheme theme
  then "rgba(255, 255, 255, 1.0)"
  else "rgba(0, 0, 0, 0.8)"

-- | Medium color for transitive packages
mediumColor :: ViewTheme -> String
mediumColor theme = if isDarkTheme theme
  then "rgba(255, 255, 255, 0.6)"
  else "rgba(0, 0, 0, 0.5)"

-- | Dim color for non-project packages
dimColor :: ViewTheme -> String
dimColor theme = if isDarkTheme theme
  then "rgba(255, 255, 255, 0.15)"
  else "rgba(0, 0, 0, 0.15)"

-- | Convert topo layer to a color (cool purple -> warm orange)
topoLayerColor :: Int -> Int -> String
topoLayerColor layer maxLayer =
  let
    t = if maxLayer > 0
          then toNumber layer / toNumber maxLayer
          else 0.0
    -- Color scale: cool purple (leaves) -> warm orange (roots)
    h = 270.0 - t * 240.0  -- 270 (purple) -> 30 (orange)
    s = 70.0 + t * 10.0    -- 70% -> 80%
    l = 60.0 - t * 5.0     -- 60% -> 55%
  in
    "hsl(" <> show h <> ", " <> show s <> "%, " <> show l <> "%)"

-- =============================================================================
-- Git Status Colors
-- =============================================================================

-- | Git file status for coloring
data GitFileStatus
  = GitModified    -- Modified but not staged (working tree changes)
  | GitStaged      -- Staged for commit
  | GitUntracked   -- New untracked file
  | GitClean       -- No changes

derive instance eqGitFileStatus :: Eq GitFileStatus

-- | Color for git status (works on all themes)
gitStatusColor :: GitFileStatus -> String
gitStatusColor GitModified = "#e67e22"   -- Orange/amber for modified
gitStatusColor GitStaged = "#27ae60"     -- Green for staged
gitStatusColor GitUntracked = "#9b59b6"  -- Purple for new/untracked
gitStatusColor GitClean = "rgba(128, 128, 128, 0.3)"  -- Dim gray for clean

-- =============================================================================
-- Beeswarm Scope (package filtering for GUP)
-- =============================================================================

-- | Scope for filtering which packages appear in the beeswarm
-- | The simulation supports GUP - changing scope triggers enter/exit animations
data BeeswarmScope
  = AllPackages          -- Full registry (568 packages)
  | ProjectOnly          -- Just our project packages (~25)
  | ProjectWithDeps      -- Project + direct dependencies
  | ProjectWithTransitive -- Project + all transitive dependencies

derive instance eqBeeswarmScope :: Eq BeeswarmScope

instance showBeeswarmScope :: Show BeeswarmScope where
  show AllPackages = "AllPackages"
  show ProjectOnly = "ProjectOnly"
  show ProjectWithDeps = "ProjectWithDeps"
  show ProjectWithTransitive = "ProjectWithTransitive"

-- =============================================================================
-- Git Status at Package Level (aggregated from module status)
-- =============================================================================

-- | Package-level git status for coloring
-- | Contains sets of package names that have modified/staged/untracked modules
type PackageGitStatus =
  { packagesWithModified :: Set String    -- Packages containing modified modules
  , packagesWithStaged :: Set String      -- Packages containing staged modules
  , packagesWithUntracked :: Set String   -- Packages containing untracked modules
  }

-- | Get the effective git status for a package
-- | Priority: modified > staged > untracked > clean
getPackageGitStatus :: PackageGitStatus -> String -> GitFileStatus
getPackageGitStatus pgs packageName
  | Set.member packageName pgs.packagesWithModified = GitModified
  | Set.member packageName pgs.packagesWithStaged = GitStaged
  | Set.member packageName pgs.packagesWithUntracked = GitUntracked
  | otherwise = GitClean

-- =============================================================================
-- Reachability Status (for dead code detection)
-- =============================================================================

-- | Module reachability status for coloring
data ReachabilityStatus
  = Reachable       -- Transitively imported from outside the package
  | Unreachable     -- Not reachable from any external consumer
  | EntryPoint      -- Directly imported by external module (a root)

derive instance eqReachabilityStatus :: Eq ReachabilityStatus

-- | Package-level reachability: set of reachable module names
type PackageReachability =
  { reachable :: Set String      -- Modules transitively reachable from outside
  , entryPoints :: Set String    -- Modules directly imported from outside
  , packageName :: String        -- Which package this was computed for
  , isApp :: Boolean             -- True if resolved as app mode (Main entry point)
  }

-- | Get reachability status for a module
getModuleReachability :: PackageReachability -> String -> ReachabilityStatus
getModuleReachability pr moduleName
  | Set.member moduleName pr.entryPoints = EntryPoint
  | Set.member moduleName pr.reachable = Reachable
  | otherwise = Unreachable

-- =============================================================================
-- Cluster Data (for dependency cluster coloring)
-- =============================================================================

-- | Package-level clustering: connected components and communities
type PackageClusters =
  { clusters :: Array (Set String)    -- Connected components (undirected)
  , communities :: Map String Int     -- Module -> community index (from label propagation)
  , packageName :: String
  }

-- | Short display name for reachability status
showReachabilityStatus :: ReachabilityStatus -> String
showReachabilityStatus EntryPoint = "entry point"
showReachabilityStatus Reachable = "reachable"
showReachabilityStatus Unreachable = "unreachable"
