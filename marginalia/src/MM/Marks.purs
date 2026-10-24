-- | Shared drawing kit for the timeline views: SVG element helpers, the fate marks
-- | (one function draws a bar of a given fate, used by the charts AND their legends,
-- | so a legend cannot drift from what is drawn), and date formatting.
module MM.Marks
  ( svgNS
  , attr
  , svgEl
  , num
  , rectAt
  , lineAt
  , pathD
  , textAt
  , barHeight
  , hatchId
  , hatchDef
  , fateMark
  , endMarkWidth
  , unfinished
  , fmtDate
  , monthShort
  ) where

import Prelude

import Data.Date (Date, Month(..), day, month, year)
import Data.Enum (fromEnum)
import Halogen.HTML as HH
import Halogen.HTML.Core (AttrName(..), ElemName(..), Namespace(..))
import Halogen.HTML.Properties as HP
import MM.Palette as Palette
import MM.Timeline.Types (Fate(..))

-- SVG helpers -------------------------------------------------------------------

svgNS :: Namespace
svgNS = Namespace "http://www.w3.org/2000/svg"

attr :: forall r i. String -> String -> HP.IProp r i
attr k v = HP.attr (AttrName k) v

svgEl :: forall r w i. String -> Array (HP.IProp r i) -> Array (HH.HTML w i) -> HH.HTML w i
svgEl name = HH.elementNS svgNS (ElemName name)

num :: Number -> String
num = show

rectAt :: forall r w i. Number -> Number -> Number -> Number -> Array (HP.IProp r i) -> HH.HTML w i
rectAt x y w h props =
  svgEl "rect" ([ attr "x" (num x), attr "y" (num y), attr "width" (num (max 0.0 w)), attr "height" (num h) ] <> props) []

lineAt :: forall r w i. Number -> Number -> Number -> Number -> Array (HP.IProp r i) -> HH.HTML w i
lineAt x1 y1 x2 y2 props =
  svgEl "line" ([ attr "x1" (num x1), attr "y1" (num y1), attr "x2" (num x2), attr "y2" (num y2) ] <> props) []

pathD :: forall r w i. String -> Array (HP.IProp r i) -> HH.HTML w i
pathD d props = svgEl "path" ([ attr "d" d ] <> props) []

textAt :: forall r w i. Number -> Number -> String -> Array (HP.IProp r i) -> HH.HTML w i
textAt x y s props = svgEl "text" ([ attr "x" (num x), attr "y" (num y) ] <> props) [ HH.text s ]

-- Fate marks ----------------------------------------------------------------------
--
-- One function draws a bar of a given fate in a given colour, used by both the chart
-- and the legend, so the legend cannot drift from what the chart draws.

barHeight :: Number
barHeight = 8.0

hatchId :: String -> String
hatchId key = "tl-hatch-" <> key

-- A hatch pattern per colour: the lane colour, lightened, under ink diagonals.
hatchDef :: forall w i. String -> String -> HH.HTML w i
hatchDef key colour =
  svgEl "pattern"
    [ attr "id" (hatchId key), attr "patternUnits" "userSpaceOnUse", attr "width" "4", attr "height" "4"
    , attr "patternTransform" "rotate(45)"
    ]
    [ rectAt 0.0 0.0 4.0 4.0 [ attr "fill" (Palette.barFill colour), attr "fill-opacity" "0.45" ]
    , lineAt 0.0 0.0 0.0 4.0 [ attr "stroke" Palette.ink, attr "stroke-width" "1.6" ]
    ]

fateMark :: forall w i. String -> String -> Fate -> Number -> Number -> Number -> Array (HH.HTML w i)
fateMark key colour fate x0 x1 y =
  let
    w = x1 - x0
    h = barHeight
    solid = Palette.barFill colour
    stroke = [ attr "stroke" Palette.ink, attr "stroke-width" "0.9" ]
  in
    case fate of
      Shipped ->
        [ rectAt x0 y w h ([ attr "fill" solid ] <> stroke) ]
      Active ->
        -- solid, but open at the right: no closing edge, an arrowhead instead
        [ rectAt x0 y w h [ attr "fill" solid ]
        , pathD ("M" <> num x1 <> " " <> num y <> "H" <> num x0 <> "V" <> num (y + h) <> "H" <> num x1)
            ([ attr "fill" "none" ] <> stroke)
        , pathD ("M" <> num (x1 + 1.0) <> " " <> num (y - 2.0) <> "L" <> num (x1 + 6.0) <> " " <> num (y + h / 2.0) <> "L" <> num (x1 + 1.0) <> " " <> num (y + h + 2.0))
            [ attr "fill" "none", attr "stroke" Palette.ink, attr "stroke-width" "1.3", attr "stroke-linejoin" "miter" ]
        ]
      Parked ->
        -- outlined, then a pause mark
        [ rectAt x0 y w h [ attr "fill" Palette.cream, attr "stroke" Palette.ink, attr "stroke-width" "1.1" ]
        , lineAt (x1 + 2.5) y (x1 + 2.5) (y + h) [ attr "stroke" Palette.ink, attr "stroke-width" "1.2" ]
        , lineAt (x1 + 5.0) y (x1 + 5.0) (y + h) [ attr "stroke" Palette.ink, attr "stroke-width" "1.2" ]
        ]
      HalfDone ->
        [ rectAt x0 y w h ([ attr "fill" ("url(#" <> hatchId key <> ")") ] <> stroke) ]
      Abandoned ->
        [ rectAt x0 y w h
            [ attr "fill" solid, attr "fill-opacity" "0.22"
            , attr "stroke" Palette.ink, attr "stroke-width" "0.9", attr "stroke-dasharray" "2.5 2"
            , attr "stroke-opacity" "0.75"
            ]
        ]
      Superseded ->
        -- half-height: it continued, but as something else
        [ rectAt x0 (y + h / 4.0) w (h / 2.0) ([ attr "fill" solid ] <> stroke) ]
      Unknown ->
        [ rectAt x0 y w h
            [ attr "fill" "none", attr "stroke" Palette.ink, attr "stroke-width" "0.9"
            , attr "stroke-dasharray" "0.8 2", attr "stroke-linecap" "round"
            ]
        ]

endMarkWidth :: Fate -> Number
endMarkWidth = case _ of
  Active -> 7.0
  Parked -> 6.0
  _ -> 0.0

unfinished :: Fate -> Boolean
unfinished f = f == HalfDone || f == Abandoned

fmtDate :: Date -> String
fmtDate d = show (fromEnum (day d)) <> " " <> monthShort (month d) <> " " <> show (fromEnum (year d))

monthShort :: Month -> String
monthShort = case _ of
  January -> "Jan"
  February -> "Feb"
  March -> "Mar"
  April -> "Apr"
  May -> "May"
  June -> "Jun"
  July -> "Jul"
  August -> "Aug"
  September -> "Sep"
  October -> "Oct"
  November -> "Nov"
  December -> "Dec"
