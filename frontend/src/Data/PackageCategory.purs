-- | Shared Package Classification
-- |
-- | Classifies packages into Workspace / DirectDep / Transitive categories
-- | and provides representative colors for each. Extracted from AnatomyBeeswarm
-- | so both that visualization and NamespaceTree can share the logic.
module CE2.Data.PackageCategory
  ( PackageCategory(..)
  , classify
  , computeDirectDepNames
  , categoryColor
  , categoryStroke
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set

-- | Package categories for coloring
data PackageCategory = Workspace | DirectDep | Transitive

derive instance eqPackageCategory :: Eq PackageCategory
derive instance ordPackageCategory :: Ord PackageCategory

instance showPackageCategory :: Show PackageCategory where
  show Workspace = "Workspace"
  show DirectDep = "DirectDep"
  show Transitive = "Transitive"

-- | Compute the set of direct dependency names from workspace packages
computeDirectDepNames
  :: forall r
   . Array { name :: String, source :: String, depends :: Array String | r }
  -> Set String
computeDirectDepNames packages =
  let
    wsNames = Set.fromFoldable $ Array.mapMaybe
      (\p -> if p.source == "workspace" then Just p.name else Nothing) packages
    allWsDeps = foldl (\acc p ->
      if p.source == "workspace"
        then Set.union acc (Set.fromFoldable p.depends)
        else acc
      ) Set.empty packages
  in
    Set.difference allWsDeps wsNames

-- | Classify a package into workspace, direct dep, or transitive
classify
  :: forall r
   . Set String
  -> Set String
  -> { name :: String, source :: String | r }
  -> PackageCategory
classify wsNames directDepNames pkg
  | pkg.source == "workspace" = Workspace
  | Set.member pkg.name wsNames = Workspace
  | Set.member pkg.name directDepNames = DirectDep
  | otherwise = Transitive

-- | Fixed representative color per category (no topoLayer variation)
categoryColor :: PackageCategory -> String
categoryColor = case _ of
  Workspace  -> "hsl(40, 85%, 55%)"
  DirectDep  -> "hsl(210, 65%, 50%)"
  Transitive -> "hsl(210, 15%, 65%)"

-- | Stroke color per category
categoryStroke :: PackageCategory -> String
categoryStroke = case _ of
  Workspace  -> "hsl(40, 85%, 40%)"
  DirectDep  -> "hsl(210, 65%, 35%)"
  Transitive -> "hsl(210, 10%, 50%)"
