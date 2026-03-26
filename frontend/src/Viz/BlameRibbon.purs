-- | Git Blame Ribbon — pure render functions
-- |
-- | Renders a vertical strip of colored lines representing git blame data.
-- | Each line is colored by commit age (light = old, warm = recent).
-- | Standalone: no Halogen component, just HTML-producing functions.
module CE2.Viz.BlameRibbon
  ( renderBlameRibbon
  , renderBlameIndicator
  , blameLineAge
  , blameAgeColor
  , formatRelativeTime
  ) where

import Prelude

import Data.Array as Array
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (abs)
import Data.String as String
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import CE2.Data.Loader as Loader

-- | FFI for relative time formatting ("3 days ago", "2 months ago", etc.)
foreign import formatRelativeTime :: Int -> String

-- | Render a vertical blame ribbon from blame data.
-- | `onLineClick` is called with the line number when a line is clicked.
renderBlameRibbon :: forall w i. { blameData :: Maybe Loader.BlameResult, loading :: Boolean, onLineClick :: Int -> i } -> HH.HTML w i
renderBlameRibbon { loading: true } =
  HH.div [ HP.style "width: 120px; flex-shrink: 0; display: flex; align-items: center; justify-content: center; color: #999; font-size: 11px;" ]
    [ HH.text "Loading..." ]
renderBlameRibbon { blameData: Nothing } =
  HH.div [ HP.style "width: 120px; flex-shrink: 0; display: flex; align-items: center; justify-content: center; color: #bbb; font-size: 10px;" ]
    [ HH.text "No git history" ]
renderBlameRibbon { blameData: Just blame, onLineClick } =
  let lineCount = Array.length blame.lines
  in HH.div
    [ HP.style "width: 120px; flex-shrink: 0; overflow-y: auto; border: 1px solid #d5d0c4; border-radius: 4px; background: #faf8f3;" ]
    [ HH.div
        [ HP.style "display: flex; flex-direction: column;" ]
        (Array.mapWithIndex (\idx blameLine ->
          let
            age = blameLineAge blame.oldestTime blame.newestTime blameLine.authorTime
            bgColor = blameAgeColor age
            prevHash = Array.index blame.lines (idx - 1) <#> _.hash
            isGroupStart = idx > 0 && prevHash /= Just blameLine.hash
            tooltip = blameLine.shortHash <> " \x00B7 " <> blameLine.author
              <> " \x00B7 " <> formatRelativeTime blameLine.authorTime
              <> "\n" <> blameLine.summary
          in HH.div
            [ HP.style $ "height: 2px; background: " <> bgColor <> ";"
                <> (if isGroupStart then " border-top: 1px solid rgba(0,0,0,0.15);" else "")
            , HP.title tooltip
            , HE.onClick \_ -> onLineClick blameLine.lineNum
            ]
            []
        ) blame.lines)
    , HH.div
        [ HP.style "padding: 6px 8px; border-top: 1px solid #d5d0c4; font-size: 9px; color: #888; line-height: 1.5;" ]
        [ HH.text $ show lineCount <> " lines"
        , HH.br_
        , HH.text $ blameAuthorSummary blame
        ]
    ]

-- | Render a compact age indicator for a single declaration.
-- | Shows relative time text with an age-colored dot.
renderBlameIndicator :: forall w i. Loader.BlameResult -> { startLine :: Int, endLine :: Int } -> HH.HTML w i
renderBlameIndicator blame { startLine, endLine } =
  let
    declBlameLines = Array.filter (\bl -> bl.lineNum >= startLine && bl.lineNum <= endLine) blame.lines
    avgTime = case declBlameLines of
      [] -> blame.newestTime
      lines -> Array.foldl (\acc bl -> acc + bl.authorTime) 0 lines / Array.length lines
    age = blameLineAge blame.oldestTime blame.newestTime avgTime
    bgColor = blameAgeColor age
    newestBlameLine = Array.foldl (\acc bl -> if bl.authorTime > acc.authorTime then bl else acc)
      { authorTime: 0, author: "", summary: "", hash: "", shortHash: "", lineNum: 0 }
      declBlameLines
    timeText = if newestBlameLine.authorTime > 0
      then formatRelativeTime newestBlameLine.authorTime
      else ""
  in
  HH.div [ HP.style "display: flex; align-items: center; gap: 4px; font-size: 9px; color: #999;" ]
    [ HH.span [ HP.style $ "display: inline-block; width: 6px; height: 6px; border-radius: 50%; background: " <> bgColor <> ";" ] []
    , HH.text timeText
    ]

-- =============================================================================
-- Helpers
-- =============================================================================

-- | Compute age as 0..1 (0 = oldest, 1 = newest)
blameLineAge :: Int -> Int -> Int -> Number
blameLineAge oldest newest t =
  if newest <= oldest then 0.5
  else Int.toNumber (t - oldest) / Int.toNumber (newest - oldest)

-- | Map age to a warm color scale (light/cool = old, warm/dark = recent)
blameAgeColor :: Number -> String
blameAgeColor age
  | age < 0.25 = "rgb(240,244,248)"
  | age < 0.5  = "rgb(238,236,228)"
  | age < 0.75 = "rgb(240,224,200)"
  | age < 0.9  = "rgb(238,196,160)"
  | otherwise  = "rgb(232,168,124)"

-- | Summarize unique authors in blame data
blameAuthorSummary :: Loader.BlameResult -> String
blameAuthorSummary blame =
  let
    authors = Array.nub $ map _.author blame.lines
    count = Array.length authors
  in if count <= 2
    then String.joinWith ", " authors
    else fromMaybe "" (Array.head authors) <> " + " <> show (count - 1) <> " others"
