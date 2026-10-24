-- | The Halogen component: fetch tree.json, lay it out, render the treemap as inline SVG.
-- |
-- | Two viewer-side THEMES, chosen by the projection's tier count (rectDepth):
-- |   * Editorial (one tier, the curated shingle) -- De Stijl: black grid, solid saturated
-- |     rectangles keyed on identity, big type, bubble-pack demoted to a faint texture.
-- |   * Atlas (two tiers, the full Marginalia view) -- light domain-tinted cells.
-- | Hovering a cell (or its corner info icon) tooltips the project's description; hovering
-- | a bubble tooltips that sub-project's. The tree carries no colour; style is applied here.
module MM.Component (component) where

import Prelude

import Affjax.ResponseFormat as ARF
import Affjax.Web as AX
import Data.Array (concatMap, elem, filter)
import Data.Either (Either(..))
import Data.Int (toNumber)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (sqrt)
import Data.String.Common (toUpper)
import Data.String (length) as Str
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core (AttrName(..), ClassName(..), ElemName(..), Namespace(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import MM.Layout (Bubble, Cell, layoutTree)
import MM.Types (Edge, TreeData(..), parseTree)
import Web.UIEvent.MouseEvent (MouseEvent, clientX, clientY, ctrlKey, metaKey)

-- Modifier-click a bubble/cell to open its target in a new tab. The target depends on
-- the link policy the embedding page chose (GitHub for the public showcase, the project
-- tracker for the in-house newspaper) -- see Component.js / window.MM_CONFIG.
foreign import openInNewTabImpl :: String -> Effect Unit

-- Link policy + data source, read once from the host page at init. Same bundle, two
-- deployments: the public showcase (baked tree.json, GitHub links) and the in-house
-- newspaper (live /api/tree, tracker links).
type LinkConfig = { mode :: String, trackerBase :: String, dataUrl :: String }

foreign import readLinkConfig :: Effect LinkConfig

-- Resolve a node's modifier-click target under the active policy. Nothing => no link
-- (the click falls back to toggling the tooltip).
resolveLink :: LinkConfig -> Int -> Maybe String -> Maybe String
resolveLink cfg pid repoUrl = case cfg.mode of
  "github" -> repoUrl
  "none" -> Nothing
  _ -> Just (cfg.trackerBase <> "/#project/" <> show pid)

vbW :: Number
vbW = 1600.0

vbH :: Number
vbH = 1000.0

type Tip = { title :: String, body :: String, cover :: Maybe String, x :: Int, y :: Int, key :: String, pinned :: Boolean }

type Loaded =
  { cells :: Array Cell
  , edges :: Array Edge
  , rectDepth :: Int
  , showEdges :: Boolean
  , tip :: Maybe Tip
  , link :: LinkConfig
  }

data State
  = Loading
  | Failed String
  | Ready Loaded

data Action
  = Initialize
  | ToggleEdges
  | HoverTip Tip -- transient (desktop hover); ignored while a tip is pinned
  | LeaveTip
  | ClickTip Tip -- tap/click: toggle a pinned tip (touch-friendly)
  | OpenLink String -- modifier-click: open the resolved target (tracker page or repo)

data Theme = Atlas | Editorial

themeClass :: Theme -> String
themeClass = case _ of
  Editorial -> "mm-editorial"
  Atlas -> "mm-atlas"

component :: forall q i o m. MonadAff m => H.Component q i o m
component =
  H.mkComponent
    { initialState: const Loading
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> do
    cfg <- liftEffect readLinkConfig
    resp <- H.liftAff $ AX.get ARF.string cfg.dataUrl
    case resp of
      Left err -> H.put (Failed (AX.printError err))
      Right r -> case parseTree r.body of
        Left e -> H.put (Failed e)
        Right td@(TreeData rec) ->
          H.put (Ready { cells: layoutTree vbW vbH td, edges: rec.edges, rectDepth: rec.rectDepth, showEdges: false, tip: Nothing, link: cfg })
  ToggleEdges ->
    H.modify_ case _ of
      Ready l -> Ready (l { showEdges = not l.showEdges })
      s -> s
  HoverTip t ->
    H.modify_ case _ of
      Ready l -> case l.tip of
        Just p | p.pinned -> Ready l -- a pin holds; hover doesn't override
        _ -> Ready (l { tip = Just t })
      s -> s
  LeaveTip ->
    H.modify_ case _ of
      Ready l -> case l.tip of
        Just p | p.pinned -> Ready l
        _ -> Ready (l { tip = Nothing })
      s -> s
  ClickTip t ->
    H.modify_ case _ of
      Ready l -> case l.tip of
        Just p | p.pinned && p.key == t.key -> Ready (l { tip = Nothing }) -- tap again dismisses
        _ -> Ready (l { tip = Just t })
      s -> s
  OpenLink url ->
    liftEffect (openInNewTabImpl url)

-- SVG helpers --------------------------------------------------------------

svgNS :: Namespace
svgNS = Namespace "http://www.w3.org/2000/svg"

attr :: forall r i. String -> String -> HP.IProp r i
attr k v = HP.attr (AttrName k) v

svgEl :: forall r w i. String -> Array (HP.IProp r i) -> Array (HH.HTML w i) -> HH.HTML w i
svgEl name = HH.elementNS svgNS (ElemName name)

rectEl :: forall r w i. Number -> Number -> Number -> Number -> String -> Array (HP.IProp r i) -> HH.HTML w i
rectEl x y w h fill extra =
  svgEl "rect"
    ([ attr "x" (show x), attr "y" (show y), attr "width" (show w), attr "height" (show h), attr "fill" fill ] <> extra)
    []

textEl :: forall r w i. Number -> Number -> String -> Array (HP.IProp r i) -> HH.HTML w i
textEl x y s props =
  svgEl "text" ([ attr "x" (show x), attr "y" (show y), attr "pointer-events" "none" ] <> props) [ HH.text s ]

clampN :: Number -> Number -> Number -> Number
clampN lo hi x = max lo (min hi x)

-- Tooltip plumbing ---------------------------------------------------------

-- Hover (transient) + tap (toggle pin). `key` identifies the target so a second tap on the
-- same one dismisses it. Works on both pointer and touch.
tipProps
  :: forall r
   . Maybe String
  -> String
  -> String
  -> Maybe String
  -> Maybe String
  -> Array (HP.IProp (onMouseMove :: MouseEvent, onMouseLeave :: MouseEvent, onClick :: MouseEvent | r) Action)
tipProps link key title body cover =
  let mk pinned e = { title, body: fromMaybe "" body, cover, x: clientX e, y: clientY e, key, pinned }
  in
    [ HE.onMouseMove (\e -> HoverTip (mk false e))
    , HE.onMouseLeave (\_ -> LeaveTip)
    -- Cmd/Ctrl-click opens the resolved target (repo or tracker); a plain tap toggles the tooltip.
    , HE.onClick (\e -> case (if metaKey e || ctrlKey e then link else Nothing) of
        Just url -> OpenLink url
        Nothing -> ClickTip (mk true e))
    ]

-- Atlas palette (semantic, by domain) --------------------------------------

domainColor :: Maybe String -> String
domainColor = case _ of
  Just "programming" -> "#1d54b8"
  Just "music" -> "#e23b2e"
  Just "infrastructure" -> "#16140f"
  Just "house" -> "#f6c700"
  Just "woodworking" -> "#cdbfa3"
  Just "garden" -> "#2e8b57"
  _ -> "#9a948a"

themeColor :: forall r. { domain :: Maybe String | r } -> String
themeColor n = domainColor n.domain

statusOpacity :: Maybe String -> String
statusOpacity = case _ of
  Just "active" -> "1"
  Just "blocked" -> "0.95"
  Just "someday" -> "0.8"
  Just "idea" -> "0.72"
  Just "done" -> "0.6"
  Just "dormant" -> "0.6"
  Just "evolved" -> "0.5"
  Just "defunct" -> "0.4"
  _ -> "0.85"

-- Editorial palette (De Stijl, keyed on project identity) -------------------

editorialColor :: Int -> String
editorialColor = case _ of
  125 -> "#e23b2e"
  133 -> "#f6c700"
  134 -> "#faf8f3"
  223 -> "#1d54b8"
  136 -> "#16140f"
  _ -> "#faf8f3"

inkOn :: String -> String
inkOn = case _ of
  "#f6c700" -> "#16140f"
  "#faf8f3" -> "#16140f"
  "#cdbfa3" -> "#16140f"
  _ -> "#f5f2eb"

edgeColor :: String -> String
edgeColor = case _ of
  "blocks" -> "#e23b2e"
  "feeds_into" -> "#1d54b8"
  "informs" -> "#2e8b57"
  _ -> "#6b665d"

-- | Projects to leave off the map, by Marginalia project ID.
-- |
-- | Keyed by slug until 2026-09-13, when Marginalia retired the slugs. The id
-- | is the better key anyway: a slug could not be resolved by anything — no
-- | route, no search, `GET /api/projects/<slug>` a 404 — so filling this list
-- | in meant reading a slug off the tree JSON and pasting it back, while an id
-- | is the number in the project's own URL.
excluded :: Array Int
excluded = []

-- Render -------------------------------------------------------------------

render :: forall m. State -> H.ComponentHTML Action () m
render = case _ of
  Loading -> HH.div [ HP.class_ (ClassName "mm-msg") ] [ HH.text "Loading the project graph..." ]
  Failed e -> HH.div [ HP.class_ (ClassName "mm-msg mm-err") ] [ HH.text ("Could not load tree.json: " <> e) ]
  Ready l ->
    let
      theme = if l.rectDepth == 1 then Editorial else Atlas
      visible = filter (\c -> not (elem c.id excluded)) l.cells
      backdrop = case theme of
        Editorial -> [ rectEl 0.0 0.0 vbW vbH "#16140f" [] ]
        Atlas -> []
      cellHtml = case theme of
        Editorial -> concatMap (editorialCell l.link) visible
        Atlas -> concatMap (atlasCell l.link) visible
      arcs = if l.showEdges then renderArcs (positionsOf visible) l.edges else []
      masthead = case theme of
        Editorial -> [ HH.h1 [ HP.class_ (ClassName "mm-title") ] [ HH.text "What I've been working on\x2026" ] ]
        Atlas -> []
    in
      HH.div [ HP.class_ (ClassName ("mm-stage " <> themeClass theme)) ]
        ( masthead <>
            [ svgEl "svg"
                [ attr "viewBox" ("0 0 " <> show vbW <> " " <> show vbH)
                , attr "width" "100%"
                , attr "height" "100%"
                , attr "preserveAspectRatio" "xMidYMid meet"
                ]
                (backdrop <> cellHtml <> arcs)
            , HH.button
                [ HP.class_ (ClassName ("mm-toggle" <> if l.showEdges then " on" else ""))
                , HE.onClick \_ -> ToggleEdges
                ]
                [ HH.text "dependencies" ]
            ] <> tooltip l.tip
        )

tooltip :: forall w. Maybe Tip -> Array (HH.HTML w Action)
tooltip = case _ of
  Nothing -> []
  Just t ->
    let
      coverImg = case t.cover of
        Just u -> [ HH.img [ HP.src u, HP.class_ (ClassName "mm-tip-img") ] ]
        Nothing -> []
    in
      [ HH.div
          [ HP.class_ (ClassName "mm-tip")
          , HP.style ("left:" <> show (t.x + 14) <> "px; top:" <> show (t.y + 16) <> "px")
          ]
          ( coverImg
              <> [ HH.div [ HP.class_ (ClassName "mm-tip-title") ] [ HH.text t.title ] ]
              <> (if t.body == "" then [] else [ HH.div [ HP.class_ (ClassName "mm-tip-body") ] [ HH.text t.body ] ])
          )
      ]

-- Editorial cell (De Stijl) ------------------------------------------------

editorialCell :: forall w. LinkConfig -> Cell -> Array (HH.HTML w Action)
editorialCell cfg cell =
  let
    link = resolveLink cfg cell.id cell.repoUrl
    c = editorialColor cell.id
    ink = inkOn c
    pad = 22.0
    nameSize = clampN 20.0 56.0 (cell.h * 0.085)
    countSize = clampN 26.0 72.0 (cell.h * 0.14)
    countStr = show cell.count
    countW = toNumber (Str.length countStr) * countSize * 0.6
    kickerHtml = case cell.tagline of
      Just t ->
        [ textEl (cell.x + pad) (cell.y + pad + 8.0) (toUpper t)
            [ attr "fill" ink, attr "font-size" "14", attr "font-weight" "500"
            , attr "letter-spacing" "0.14em", attr "fill-opacity" "0.85"
            ]
        ]
      Nothing -> []
    nameY = cell.y + pad + (if cell.tagline == Nothing then 8.0 + nameSize else 34.0 + nameSize)
  in
    [ rectEl cell.x cell.y cell.w cell.h c (tipProps link ("c" <> show cell.id) cell.name cell.description cell.cover) ]
      <> map (editorialBubble cfg ink) cell.bubbles
      <> kickerHtml
      <>
        [ textEl (cell.x + pad) nameY cell.name
            [ attr "fill" ink, attr "font-size" (show nameSize), attr "font-weight" "300" ]
        , textEl (cell.x + pad) (cell.y + cell.h - pad) countStr
            [ attr "fill" ink, attr "font-size" (show countSize), attr "font-weight" "200" ]
        , textEl (cell.x + pad + countW + 10.0) (cell.y + cell.h - pad - countSize * 0.06) "SUB-PROJECTS"
            [ attr "fill" ink, attr "font-size" "13", attr "font-weight" "500"
            , attr "letter-spacing" "0.12em", attr "fill-opacity" "0.8"
            ]
        , infoIcon cfg ink cell
        ]

infoIcon :: forall w. LinkConfig -> String -> Cell -> HH.HTML w Action
infoIcon cfg ink cell =
  let
    link = resolveLink cfg cell.id cell.repoUrl
    ix = cell.x + cell.w - 26.0
    iy = cell.y + cell.h - 26.0
  in
    svgEl "g" (tipProps link ("c" <> show cell.id) cell.name cell.description cell.cover <> [ attr "style" "cursor:pointer" ])
      [ svgEl "circle"
          [ attr "cx" (show ix), attr "cy" (show iy), attr "r" "11"
          , attr "fill" "none", attr "stroke" ink, attr "stroke-opacity" "0.55", attr "stroke-width" "1.5"
          ]
          []
      , textEl ix (iy + 5.0) "i"
          [ attr "fill" ink, attr "fill-opacity" "0.85", attr "font-size" "15"
          , attr "font-style" "italic", attr "font-weight" "500", attr "text-anchor" "middle"
          ]
      ]

editorialBubble :: forall w. LinkConfig -> String -> Bubble -> HH.HTML w Action
editorialBubble cfg ink b =
  let
    link = resolveLink cfg b.id b.repoUrl
    -- Coverage map: fill-opacity says whether this sub-project has a DESCRIPTION (text),
    -- a thin ring says whether it has a COVER (thumbnail). Faint, ring-less dots are the
    -- gaps to go fill in -- Cmd/Ctrl-click jumps to the project to do exactly that.
    hasText = case b.description of
      Just s -> s /= ""
      Nothing -> false
    hasCover = case b.cover of
      Just _ -> true
      Nothing -> false
    geom = [ attr "cx" (show b.x), attr "cy" (show b.y), attr "r" (show b.r) ]
    fillOp
      | not b.leaf = "0.04"
      | hasText = "0.20"
      | otherwise = "0.06"
    ring
      | hasCover = [ attr "stroke" ink, attr "stroke-opacity" "0.6", attr "stroke-width" "1.4" ]
      | not b.leaf = [ attr "stroke" ink, attr "stroke-opacity" "0.30", attr "stroke-width" "1.2" ]
      | otherwise = []
    style = [ attr "fill" ink, attr "fill-opacity" fillOp, attr "style" "cursor:pointer" ] <> ring
  in
    svgEl "circle" (geom <> style <> tipProps link ("b" <> show b.id) b.name b.description b.cover) []

bubbleTitle :: Bubble -> String
bubbleTitle b = b.name <> case b.status of
  Just s -> "  \x00b7  " <> s
  Nothing -> ""

-- Atlas cell (light, domain-tinted, prominent bubbles) ---------------------

atlasCell :: forall w. LinkConfig -> Cell -> Array (HH.HTML w Action)
atlasCell cfg cell =
  if cell.leaf then atlasUmbrella cfg cell
  else atlasDomain cell

atlasDomain :: forall w. Cell -> Array (HH.HTML w Action)
atlasDomain cell =
  let c = themeColor cell
  in
    [ rectEl cell.x cell.y cell.w cell.h "none" [ attr "stroke" c, attr "stroke-width" "2.5" ]
    , textEl (cell.x + 6.0) (cell.y + 18.0) cell.name
        [ attr "fill" c, attr "font-size" "15", attr "font-weight" "600", attr "letter-spacing" "0.12em" ]
    ]

atlasUmbrella :: forall w. LinkConfig -> Cell -> Array (HH.HTML w Action)
atlasUmbrella cfg cell =
  let
    c = themeColor cell
    link = resolveLink cfg cell.id cell.repoUrl
  in
    [ rectEl cell.x cell.y cell.w cell.h c
        ([ attr "fill-opacity" "0.07", attr "stroke" c, attr "stroke-width" "1" ] <> tipProps link ("c" <> show cell.id) cell.name cell.description cell.cover)
    ]
      <> map (atlasBubble cfg) (filter _.leaf cell.bubbles)
      <>
        [ textEl (cell.x + 5.0) (cell.y + 14.0) cell.name
            [ attr "fill" c, attr "font-size" "12", attr "font-weight" "500" ]
        ]

atlasBubble :: forall w. LinkConfig -> Bubble -> HH.HTML w Action
atlasBubble cfg b =
  svgEl "circle"
    ( [ attr "cx" (show b.x), attr "cy" (show b.y), attr "r" (show b.r)
      , attr "fill" (themeColor b), attr "fill-opacity" (statusOpacity b.status)
      , attr "stroke" "#16140f", attr "stroke-width" "0.6"
      ] <> tipProps (resolveLink cfg b.id b.repoUrl) ("b" <> show b.id) (bubbleTitle b) b.description b.cover
    )
    []

-- Dependency arcs ----------------------------------------------------------

type Pos = { x :: Number, y :: Number }

positionsOf :: Array Cell -> Map Int Pos
positionsOf cells = Map.fromFoldable (concatMap entries cells)
  where
  entries c =
    [ Tuple c.id { x: c.x + c.w / 2.0, y: c.y + c.h / 2.0 } ]
      <> map (\b -> Tuple b.id { x: b.x, y: b.y }) c.bubbles

renderArcs :: forall w i. Map Int Pos -> Array Edge -> Array (HH.HTML w i)
renderArcs posMap edges = concatMap (renderArc posMap) edges

renderArc :: forall w i. Map Int Pos -> Edge -> Array (HH.HTML w i)
renderArc posMap e =
  case Map.lookup e.source posMap, Map.lookup e.target posMap of
    Just a, Just b ->
      [ svgEl "path"
          [ attr "d" (arcPath a b)
          , attr "fill" "none"
          , attr "stroke" (edgeColor e.type_)
          , attr "stroke-width" "2"
          , attr "stroke-opacity" "0.85"
          ]
          []
      ]
    _, _ -> []

arcPath :: Pos -> Pos -> String
arcPath a b =
  let
    dx = b.x - a.x
    dy = b.y - a.y
    dist = sqrt (dx * dx + dy * dy)
    curv = min 0.3 (dist / 600.0) * 90.0
    mx = (a.x + b.x) / 2.0 + (if dist > 0.0 then -dy / dist * curv else 0.0)
    my = (a.y + b.y) / 2.0 + (if dist > 0.0 then dx / dist * curv else 0.0)
  in
    "M " <> show a.x <> " " <> show a.y
      <> " Q " <> show mx <> " " <> show my
      <> " " <> show b.x <> " " <> show b.y
