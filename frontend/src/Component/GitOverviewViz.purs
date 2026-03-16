-- | Git Overview — Landing page for the Git navigation category
-- |
-- | Shows package inventory from a versioning perspective: workspace targets
-- | vs registry dependencies, with version, LOC, and module counts.
-- | Workspace packages link to their commit history grids.
module CE2.Component.GitOverviewViz
  ( component
  , Input
  , Output(..)
  , Query(..)
  , Slot
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits as SCU
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import CE2.Data.Loader as Loader
import CE2.Util.SVG (svgElem, sa)

-- =============================================================================
-- Types
-- =============================================================================

type Input =
  { packages :: Array Loader.V2Package
  }

data Output
  = NavigateToCommitGrid String  -- package name
  | NavigateToPackage String     -- package name (drill into Maps view)

type Slot = H.Slot Query Output

data Query a = NoQuery a

type State =
  { packages :: Array Loader.V2Package
  }

data Action
  = Receive Input
  | ClickWorkspacePackage String
  | ClickRegistryPackage String

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
        }
    }

initialState :: Input -> State
initialState input = { packages: input.packages }

-- =============================================================================
-- Render
-- =============================================================================

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  let
    workspace = Array.filter (\p -> p.source == "workspace") state.packages
    registry = Array.filter (\p -> p.source == "registry") state.packages
    extra = Array.filter (\p -> p.source == "extra") state.packages
    -- Split workspace into app targets (have bundleModule) vs libraries
    apps = Array.filter (\p -> p.bundleModule /= Nothing) workspace
    libs = Array.filter (\p -> p.bundleModule == Nothing) workspace
    totalLoc = Array.foldl (\acc p -> acc + p.totalLoc) 0 state.packages
    totalModules = Array.foldl (\acc p -> acc + p.moduleCount) 0 state.packages
  in
    HH.div
      [ HP.style "overflow-y: auto; padding: 24px 32px; position: absolute; top: 0; left: 0; width: 100%; height: 100%; box-sizing: border-box; background: #faf8f3;" ]
      [ -- Header
        HH.div [ HP.style "margin-bottom: 24px;" ]
          [ HH.div [ HP.style "font-size: 18px; font-weight: 700; color: #333; margin-bottom: 4px;" ]
              [ HH.text "Git Overview" ]
          , HH.div [ HP.style "font-size: 12px; color: #888;" ]
              [ HH.text $ show (Array.length state.packages) <> " packages \x00B7 "
                  <> show totalModules <> " modules \x00B7 "
                  <> formatLoc totalLoc <> " lines"
              ]
          ]

      -- Workspace targets
      , if not (Array.null workspace) then
          HH.div [ HP.style "margin-bottom: 24px;" ]
            [ sectionHeader "Workspace" (Array.length workspace) "Packages you own — click to see commit history"
            , if not (Array.null apps) then
                HH.div [ HP.style "margin-bottom: 12px;" ]
                  [ subHeader "App targets" (Array.length apps)
                  , HH.div [ HP.style "display: grid; grid-template-columns: repeat(auto-fill, minmax(320px, 1fr)); gap: 8px;" ]
                      (apps <#> renderWorkspaceCard)
                  ]
              else HH.text ""
            , if not (Array.null libs) then
                HH.div []
                  [ subHeader "Library targets" (Array.length libs)
                  , HH.div [ HP.style "display: grid; grid-template-columns: repeat(auto-fill, minmax(320px, 1fr)); gap: 8px;" ]
                      (libs <#> renderWorkspaceCard)
                  ]
              else HH.text ""
            ]
        else HH.text ""

      -- Registry dependencies
      , if not (Array.null registry) then
          HH.div [ HP.style "margin-bottom: 24px;" ]
            [ sectionHeader "Registry" (Array.length registry) "Published packages from the PureScript registry"
            , HH.div [ HP.style "display: grid; grid-template-columns: repeat(auto-fill, minmax(280px, 1fr)); gap: 6px;" ]
                (Array.sortBy (\a b -> compare a.name b.name) registry <#> renderRegistryCard)
            ]
        else HH.text ""

      -- Extra packages
      , if not (Array.null extra) then
          HH.div []
            [ sectionHeader "Extra" (Array.length extra) "Additional packages (git dependencies, local overrides)"
            , HH.div [ HP.style "display: grid; grid-template-columns: repeat(auto-fill, minmax(280px, 1fr)); gap: 6px;" ]
                (extra <#> renderRegistryCard)
            ]
        else HH.text ""
      ]

sectionHeader :: forall m w. String -> Int -> String -> HH.HTML w m
sectionHeader label count description =
  HH.div [ HP.style "margin-bottom: 8px;" ]
    [ HH.div [ HP.style "display: flex; align-items: baseline; gap: 8px; margin-bottom: 2px;" ]
        [ HH.span [ HP.style "font-size: 13px; font-weight: 700; color: #333; text-transform: uppercase; letter-spacing: 0.5px;" ]
            [ HH.text label ]
        , HH.span [ HP.style "font-size: 11px; padding: 1px 6px; border-radius: 8px; background: #333; color: #fff; font-weight: 600;" ]
            [ HH.text (show count) ]
        ]
    , HH.div [ HP.style "font-size: 11px; color: #999;" ]
        [ HH.text description ]
    ]

subHeader :: forall m w. String -> Int -> HH.HTML w m
subHeader label count =
  HH.div [ HP.style "display: flex; align-items: baseline; gap: 6px; margin-bottom: 6px;" ]
    [ HH.span [ HP.style "font-size: 10px; font-weight: 600; color: #888; text-transform: uppercase; letter-spacing: 0.5px;" ]
        [ HH.text label ]
    , HH.span [ HP.style "font-size: 9px; color: #aaa;" ]
        [ HH.text (show count) ]
    ]

renderWorkspaceCard :: forall m. Loader.V2Package -> H.ComponentHTML Action () m
renderWorkspaceCard pkg =
  HH.div
    [ HP.style "padding: 10px 14px; background: #fff; border: 1px solid #d5d0c4; border-radius: 4px; cursor: pointer; transition: border-color 150ms ease;"
    , HE.onClick \_ -> ClickWorkspacePackage pkg.name
    ]
    [ HH.div [ HP.style "display: flex; align-items: center; justify-content: space-between; margin-bottom: 4px; gap: 6px;" ]
        [ HH.div [ HP.style "display: flex; align-items: center; gap: 6px; min-width: 0;" ]
            [ packageShape pkg
            , HH.span [ HP.style "font-size: 13px; font-weight: 600; color: #333;" ]
                [ HH.text pkg.name ]
            ]
        , HH.span [ HP.style "font-size: 10px; color: #999; flex-shrink: 0;" ]
            [ HH.text pkg.version ]
        ]
    , HH.div [ HP.style "display: flex; gap: 12px; font-size: 10px; color: #888;" ]
        [ HH.text $ show pkg.moduleCount <> " modules"
        , HH.text $ formatLoc pkg.totalLoc <> " lines"
        , HH.text $ show pkg.declarationCount <> " decls"
        ]
    , case pkg.bundleModule of
        Just bm -> HH.div [ HP.style "margin-top: 4px; font-size: 9px; color: #2563eb;" ]
          [ HH.text $ "entry: " <> bm ]
        Nothing -> HH.text ""
    ]

renderRegistryCard :: forall m. Loader.V2Package -> H.ComponentHTML Action () m
renderRegistryCard pkg =
  HH.div
    [ HP.style "padding: 6px 10px; background: #f5f2eb; border: 1px solid #e0ddd4; border-radius: 3px; font-size: 11px; display: flex; align-items: center; justify-content: space-between; gap: 8px; cursor: pointer;"
    , HE.onClick \_ -> ClickRegistryPackage pkg.name
    ]
    [ packageShape pkg
    , HH.span [ HP.style "color: #555; font-weight: 500; min-width: 0; overflow: hidden; text-overflow: ellipsis; white-space: nowrap;" ]
        [ HH.text pkg.name ]
    , HH.span [ HP.style "flex-shrink: 0; color: #999; font-size: 10px;" ]
        [ HH.text $ pkg.version <> " \x00B7 " <> show pkg.moduleCount <> "m" ]
    ]

-- =============================================================================
-- Helpers
-- =============================================================================

packageShape :: forall w i. Loader.V2Package -> HH.HTML w i
packageShape pkg =
  let
    color = case pkg.source of
      "workspace" -> "#E8A735"
      "extra" -> "#4A9A8A"
      _ -> "#3B82C8"
    isApp = pkg.bundleModule /= Nothing
  in svgElem "svg"
    [ sa "viewBox" "0 0 16 16"
    , HP.style "width: 16px; height: 16px; flex-shrink: 0;"
    ]
    [ if isApp
      then svgElem "rect"
        [ sa "x" "1", sa "y" "1", sa "width" "14", sa "height" "14"
        , sa "rx" "3", sa "fill" color, sa "stroke" "rgba(0,0,0,0.15)", sa "stroke-width" "0.5"
        ] []
      else svgElem "circle"
        [ sa "cx" "8", sa "cy" "8", sa "r" "7"
        , sa "fill" color, sa "stroke" "rgba(0,0,0,0.15)", sa "stroke-width" "0.5"
        ] []
    ]

formatLoc :: Int -> String
formatLoc n
  | n >= 1000 = show (n / 1000) <> "k"
  | otherwise = show n

-- =============================================================================
-- Action Handlers
-- =============================================================================

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  Receive input ->
    H.modify_ _ { packages = input.packages }

  ClickWorkspacePackage pkgName ->
    H.raise (NavigateToCommitGrid pkgName)

  ClickRegistryPackage pkgName ->
    H.raise (NavigateToPackage pkgName)
