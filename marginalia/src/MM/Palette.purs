-- | The Mondrian palette, shared by the map and the timeline so an umbrella is the
-- | same colour in both views. Same values as `cloudflare-sites/andrewcondon/working/
-- | manifest.json` (red, yellow, blue, black, white) and the map's editorial cells.
module MM.Palette
  ( cream
  , ink
  , red
  , yellow
  , blue
  , black
  , white
  , umbrellaColor
  , inkOn
  , barFill
  ) where

import Prelude

import MM.Timeline.Types (UmbrellaId(..))

cream :: String
cream = "#f5f2eb"

ink :: String
ink = "#16140f"

red :: String
red = "#e23b2e"

yellow :: String
yellow = "#f6c700"

blue :: String
blue = "#1d54b8"

black :: String
black = "#16140f"

white :: String
white = "#faf8f3"

-- | Keyed on project identity (Marginalia id), as the map's editorial theme is.
umbrellaColor :: UmbrellaId -> String
umbrellaColor (UmbrellaId id) = case id of
  125 -> red -- Hylograph
  133 -> yellow -- Humboldt
  223 -> blue -- Atlantis
  136 -> black -- Personal Infrastructure (Infovore)
  134 -> white -- Polyglot
  _ -> white

-- | Legible text/mark colour on top of a palette colour.
inkOn :: String -> String
inkOn c
  | c == yellow || c == white || c == cream || c == "#cdbfa3" = ink
  | otherwise = cream

-- | The colour of a SOLID timeline bar. Mondrian's white is a real colour on the map,
-- | where cells are framed by black lines; a white bar on the cream page would read as
-- | an outline (which is how "parked" is drawn), so white bars get a warm grey.
barFill :: String -> String
barFill c
  | c == white = "#d6cfbf"
  | otherwise = c
