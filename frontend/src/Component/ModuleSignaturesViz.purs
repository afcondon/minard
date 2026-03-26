-- | Module Signatures Visualization Component
-- |
-- | Shows a module's type signatures alongside a git blame ribbon.
-- | Each signature card has a blame-age indicator. The blame ribbon
-- | shows the entire file as colored lines grouped by commit.
module CE2.Component.ModuleSignaturesViz
  ( component
  , Input
  , Output(..)
  , Query(..)
  , Slot
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Int (toNumber) as Int
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as Set
import Data.String as String
import Data.String.CodeUnits as SCU
import Effect (Effect)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core (PropName(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS

import CE2.Data.Loader as Loader
import CE2.Util.SVG (svgElem, sa)
import CE2.Viz.ModuleTreemapEnriched (DeclarationCircle, kindColor, childKindColor, packDeclarations)
import CE2.Viz.ModuleStructure as MSM
import CE2.Viz.SignatureTree as SigTree
import CE2.Viz.SourceCode as SourceCode
import PureScript.CST.Lexer (lexModule)

-- | Open a URI in the browser (used for vscode:// links)
foreign import openUri :: String -> Effect Unit

-- | Format a unix timestamp as a relative time string (e.g. "3 days ago")
foreign import formatRelativeTime :: Int -> String

-- =============================================================================
-- Types
-- =============================================================================

type Input =
  { packageName :: String
  , moduleName :: String
  , declarations :: Array Loader.V2Declaration
  , functionCalls :: Map Int (Array Loader.V2FunctionCall)
  , showBlameRibbon :: Boolean
  , showModuleHeader :: Boolean
  }

data Output
  = DeclarationClicked String String String  -- pkg, mod, decl
  | NavigateToStructure                      -- link back to structure view

type Slot = H.Slot Query Output

data Query a = NoQuery a

type State =
  { initialized :: Boolean
  , actionListener :: Maybe (HS.Listener Action)
  , lastInput :: Input
  , lanes :: Array MSM.Lane
  , measuredCells :: Array MSM.MeasuredCell
  , blameData :: Maybe Loader.BlameResult
  , blameLoading :: Boolean
  , moduleSource :: Maybe String
  , filePath :: Maybe String
  , kindFilter :: Maybe String  -- Nothing = show all, Just "value" = show only values, etc.
  }

data Action
  = Initialize
  | Receive Input
  | Finalize
  | HandleDeclarationClick String String String
  | BlameLineClicked Int
  | SignatureClicked String  -- open VS Code at declaration
  | GoToStructure
  | SetKindFilter (Maybe String)

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
        , initialize = Just Initialize
        , finalize = Just Finalize
        }
    }

initialState :: Input -> State
initialState input =
  { initialized: false
  , actionListener: Nothing
  , lastInput: input
  , lanes: []
  , measuredCells: []
  , blameData: Nothing
  , blameLoading: false
  , moduleSource: Nothing
  , filePath: Nothing
  , kindFilter: Nothing
  }

-- =============================================================================
-- Render
-- =============================================================================

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  let rootStyle = if state.lastInput.showModuleHeader
        then "overflow-y: auto; position: absolute; top: 0; left: 0; width: 100%; height: 100%; box-sizing: border-box;"
        else "overflow-y: auto; width: 100%; box-sizing: border-box;"
  in
  HH.div
    [ HP.class_ (HH.ClassName "module-signatures")
    , HP.style rootStyle
    ]
    [ -- Module header row with bubblepack glyph (hidden when planet provides its own)
      if state.lastInput.showModuleHeader then renderModuleHeader state else HH.text ""
    -- Kind filter bar
    , renderKindFilter state
    -- Blame ribbon + signatures layout
    , HH.div
        [ HP.style "display: flex; gap: 12px; min-height: 0; padding: 0 16px 12px 16px;" ]
        [ -- Left: blame ribbon (hidden when parent provides its own)
          if state.lastInput.showBlameRibbon then renderBlameRibbon state else HH.text ""
        -- Right: signature cards
        , HH.div
            [ HP.style "flex: 1; min-width: 0;" ]
            ( (if Array.null state.lanes && state.initialized
                then [ HH.div [ HP.style "padding: 24px; color: #999; font-size: 12px; text-align: center;" ]
                         [ HH.text "No declarations" ] ]
                else state.lanes <#> renderLane state)
            <> (if state.lastInput.showBlameRibbon then [ renderInternalDeclarations state ] else [])
            )
        ]
    ]

-- | Module header row with bubblepack glyph, module name, and Structure link
renderModuleHeader :: forall m. State -> H.ComponentHTML Action () m
renderModuleHeader state =
  HH.div
    [ HP.classes [ HH.ClassName "page-subnav", HH.ClassName "page-subnav--split" ]
    , HP.style "height: 52px;"
    ]
    [ HH.div [ HP.style "display: flex; align-items: center; gap: 12px; min-width: 0;" ]
        [ renderModuleBubblepack state
        , HH.span
            [ HP.class_ (HH.ClassName "page-subnav__title")
            , HP.style "flex: 1; min-width: 0; overflow: hidden; text-overflow: ellipsis; white-space: nowrap;"
            ]
            [ HH.text state.lastInput.moduleName ]
        ]
    , HH.span
        [ HP.style "font-size: 10px; color: #2563eb; cursor: pointer; flex-shrink: 0;"
        , HE.onClick \_ -> GoToStructure
        ]
        [ HH.text "Structure \x2192" ]
    ]

-- | Small bubblepack glyph showing declaration circles packed together
renderModuleBubblepack :: forall m. State -> H.ComponentHTML Action () m
renderModuleBubblepack state =
  let decls = state.lastInput.declarations
  in if Array.null decls then HH.text ""
     else
       let
         { declarations, packRadius } = packDeclarations decls state.lastInput.moduleName 200.0 200.0 Map.empty Map.empty
         pad = 2.0
         r = packRadius + pad
         viewBox = show (-r) <> " " <> show (-r) <> " " <> show (r * 2.0) <> " " <> show (r * 2.0)
       in
         svgElem "svg"
           [ sa "viewBox" viewBox
           , HP.style "width: 44px; height: 44px; flex-shrink: 0; overflow: visible; display: block;"
           ]
           (Array.concatMap renderDeclCircle declarations)
  where
  renderDeclCircle :: DeclarationCircle -> forall w i. Array (HH.HTML w i)
  renderDeclCircle decl =
    let hasChildren = not (Array.null decl.children)
    in
    [ svgElem "circle"
        [ sa "cx" (show decl.x), sa "cy" (show decl.y), sa "r" (show decl.r)
        , sa "fill" (kindColor decl.kind)
        , sa "fill-opacity" (if hasChildren then "0.3" else "0.85")
        , sa "stroke" (if hasChildren then kindColor decl.kind else "white")
        , sa "stroke-width" (if hasChildren then "1" else "0.5")
        ] []
    ] <> (decl.children <#> \child ->
      svgElem "circle"
        [ sa "cx" (show (decl.x + child.x)), sa "cy" (show (decl.y + child.y)), sa "r" (show child.r)
        , sa "fill" (childKindColor decl.kind child.kind)
        , sa "fill-opacity" "0.85", sa "stroke" "white", sa "stroke-width" "0.3"
        ] [])

-- | Kind filter bar
renderKindFilter :: forall m. State -> H.ComponentHTML Action () m
renderKindFilter state =
  let
    kinds = Array.nub $ map _.kind $ Array.concatMap _.cells state.lanes
    kindCounts = kinds <#> \k -> { kind: k, count: Array.length $ Array.filter (\c -> c.kind == k) (Array.concatMap _.cells state.lanes) }
    isActive k = state.kindFilter == Just k
  in
    HH.div
      [ HP.class_ (HH.ClassName "page-filter-bar") ]
      ( [ HH.span
            [ HP.class_ (HH.ClassName $ "filter-pill" <> if state.kindFilter == Nothing then " active" else "")
            , HE.onClick \_ -> SetKindFilter Nothing
            ]
            [ HH.text "All" ]
        ]
      <> (kindCounts <#> \kc ->
          HH.span
            [ HP.class_ (HH.ClassName $ "filter-pill" <> if isActive kc.kind then " active" else "")
            , HE.onClick \_ -> SetKindFilter (Just kc.kind)
            ]
            [ HH.text $ kc.kind <> " " <> show kc.count ]
        )
      )

-- =============================================================================
-- Blame Ribbon
-- =============================================================================

renderBlameRibbon :: forall m. State -> H.ComponentHTML Action () m
renderBlameRibbon state
  | state.blameLoading =
      HH.div [ HP.style "width: 120px; flex-shrink: 0; display: flex; align-items: center; justify-content: center; color: #999; font-size: 11px;" ]
        [ HH.text "Loading..." ]
  | otherwise = case state.blameData of
      Nothing ->
        HH.div [ HP.style "width: 120px; flex-shrink: 0; display: flex; align-items: center; justify-content: center; color: #bbb; font-size: 10px;" ]
          [ HH.text "No git history" ]
      Just blame ->
        let lineCount = Array.length blame.lines
        in HH.div
          [ HP.style "width: 120px; flex-shrink: 0; overflow-y: auto; border: 1px solid #d5d0c4; border-radius: 4px; background: #faf8f3;"
          ]
          [ -- Compact ribbon: one thin line per source line
            HH.div
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
                  , HE.onClick \_ -> BlameLineClicked blameLine.lineNum
                  ]
                  []
              ) blame.lines)
          , -- Summary
            HH.div
              [ HP.style "padding: 6px 8px; border-top: 1px solid #d5d0c4; font-size: 9px; color: #888; line-height: 1.5;" ]
              [ HH.text $ show lineCount <> " lines"
              , HH.br_
              , HH.text $ blameAuthorSummary blame
              ]
          ]

-- | Summarize unique authors in blame data
blameAuthorSummary :: Loader.BlameResult -> String
blameAuthorSummary blame =
  let
    authors = Array.nub $ map _.author blame.lines
    count = Array.length authors
  in if count <= 2
    then String.joinWith ", " authors
    else fromMaybe "" (Array.head authors) <> " + " <> show (count - 1) <> " others"

-- =============================================================================
-- Signature Cards
-- =============================================================================

renderLane :: forall m. State -> MSM.Lane -> H.ComponentHTML Action () m
renderLane state lane =
  let filteredCells = case state.kindFilter of
        Nothing -> lane.cells
        Just k -> Array.filter (\c -> c.kind == k) lane.cells
  in if Array.null filteredCells then HH.text ""
     else
       HH.div [ HP.style "margin-bottom: 16px;" ]
         [ renderLaneHeader lane
         , HH.div
             []
             (filteredCells <#> renderFullCell state)
         ]

renderLaneHeader :: forall m. MSM.Lane -> H.ComponentHTML Action () m
renderLaneHeader lane =
  HH.div
    [ HP.style $ "display:flex; align-items:center; gap:8px; padding:4px 0; margin-bottom:6px; border-bottom: 2px solid " <> lane.accent <> ";" ]
    [ HH.span
        [ HP.style $ "font-family: 'Courier New', Courier, monospace; font-size:11px; font-weight:700; color:" <> lane.accent <> "; text-transform:uppercase; letter-spacing:0.5px;" ]
        [ HH.text lane.label ]
    , HH.span
        [ HP.style $ "font-size:9px; padding:1px 5px; border-radius:8px; background:" <> lane.accent <> "; color:white; font-weight:600;" ]
        [ HH.text (show (Array.length lane.cells)) ]
    ]

renderFullCell :: forall m. State -> MSM.MeasuredCell -> H.ComponentHTML Action () m
renderFullCell state cell =
  let
    blameIndicator = renderBlameIndicator state cell
    baseProps =
      [ HP.id ("sig-cell-" <> cell.name)
      , HP.class_ (HH.ClassName "sigmap-cell")
      , HP.style $ "margin-bottom:6px;"
          <> " overflow:auto;"
          <> " padding:" <> show MSM.cellPad <> "px;"
          <> " box-sizing:border-box;"
          <> " background:" <> MSM.kindBackground cell.kind <> ";"
          <> " border:1px solid " <> MSM.kindBorder cell.kind <> ";"
          <> " border-radius:3px;"
          <> " cursor:pointer;"
      , HE.onClick \_ -> SignatureClicked cell.name
      ]
  in case cellHtml cell of
    Just html ->
      HH.div baseProps
        [ blameIndicator
        , HH.div [ HP.prop (PropName "innerHTML") html ] []
        ]
    Nothing ->
      HH.div baseProps
        [ blameIndicator
        , HH.div
            [ HP.style "font-size:11px; color:#333; font-family:'Fira Code','SF Mono',monospace;" ]
            [ HH.text (cell.name <> if cell.sig == "" then "" else " :: " <> cell.sig) ]
        ]

-- | Small colored indicator showing blame age for a declaration
renderBlameIndicator :: forall m. State -> MSM.MeasuredCell -> H.ComponentHTML Action () m
renderBlameIndicator state cell = case state.blameData of
  Nothing -> HH.text ""
  Just blame ->
    -- Find the source span for this declaration
    let
      mDecl = Array.find (\d -> d.name == cell.name) state.lastInput.declarations
      mSpan = mDecl >>= _.sourceSpan
    in case mSpan of
      Nothing -> HH.text ""
      Just span ->
        let
          startLine = fromMaybe 1 (Array.head span.start)
          endLine = fromMaybe startLine (Array.head span.end)
          -- Get blame lines for this declaration's range
          declBlameLines = Array.filter (\bl -> bl.lineNum >= startLine && bl.lineNum <= endLine) blame.lines
          -- Average age for this declaration
          avgTime = case declBlameLines of
            [] -> blame.newestTime
            bls -> Array.foldl (\acc bl -> acc + bl.authorTime) 0 bls / Array.length bls
          age = blameLineAge blame.oldestTime blame.newestTime avgTime
          bgColor = blameAgeColor age
          -- Most recent commit touching this declaration
          newestBlameLine = Array.foldl (\acc bl -> if bl.authorTime > acc.authorTime then bl else acc)
            { lineNum: 0, hash: "", shortHash: "", author: "", authorTime: 0, summary: "" }
            declBlameLines
          tooltip = if newestBlameLine.authorTime > 0
            then newestBlameLine.shortHash <> " \x00B7 " <> formatRelativeTime newestBlameLine.authorTime
                 <> "\n" <> newestBlameLine.author <> ": " <> newestBlameLine.summary
            else ""
          lineCount = endLine - startLine + 1
        in HH.div
          [ HP.style $ "display: flex; align-items: center; gap: 6px; margin-bottom: 4px; font-size: 9px; color: #888;"
          , HP.title tooltip
          ]
          [ HH.span
              [ HP.style $ "display: inline-block; width: 8px; height: 8px; border-radius: 50%; background: " <> bgColor <> "; border: 1px solid rgba(0,0,0,0.1);" ]
              []
          , HH.text $ if newestBlameLine.authorTime > 0
              then formatRelativeTime newestBlameLine.authorTime
              else show lineCount <> " lines"
          ]

-- | Generate HTML for a cell's content
cellHtml :: MSM.MeasuredCell -> Maybe String
cellHtml cell = case cell.dataDecl of
  Just dd -> Just $ SigTree.renderDataDecl
    { name: cell.name, typeParams: dd.typeParams, constructors: dd.constructors, keyword: dd.keyword }
  Nothing -> case cell.classDecl of
    Just cd ->
      let
        classHtml = SigTree.renderClassDecl
          { name: cell.name, typeParams: cd.typeParams, superclasses: cd.superclasses, methods: cd.methods }
        instancesHtml =
          if Array.null cd.instances then ""
          else renderInstancesHtml cd.instances
      in Just (classHtml <> instancesHtml)
    Nothing -> case cell.typeSynonym of
      Just ts -> Just $ SigTree.renderTypeSynonym
        { name: cell.name, typeParams: ts.typeParams, body: ts.body }
      Nothing -> case cell.ast of
        Just ast ->
          if cell.foreignImport
          then Just $ SigTree.renderForeignImport { name: cell.name, ast }
          else Just $ SigTree.renderSignature
            { name: cell.name, sig: cell.sig, ast, typeParams: [], className: Nothing }
        Nothing -> Nothing

renderInstancesHtml :: Array { name :: String, sig :: Maybe String } -> String
renderInstancesHtml instances =
  let
    count = Array.length instances
    instanceItems = Array.foldl (\acc inst ->
      acc <> "<li class=\"sig-class-instance\">"
        <> "<code class=\"sig-class-instance-name\">" <> inst.name <> "</code>"
        <> "</li>"
    ) "" instances
  in
    "<div class=\"sig-class-instances\">"
    <> "<div class=\"sig-class-instances-header\">"
    <> show count <> " instance" <> (if count == 1 then "" else "s")
    <> "</div>"
    <> "<ul class=\"sig-class-instance-list\">" <> instanceItems <> "</ul>"
    <> "</div>"

-- =============================================================================
-- Internal (non-exported) declarations
-- =============================================================================

renderInternalDeclarations :: forall m. State -> H.ComponentHTML Action () m
renderInternalDeclarations state =
  let
    exportedNames = Set.fromFoldable $ state.lastInput.declarations <#> _.name
    -- Gather all unique names from function calls that aren't exports
    allCallNames = Array.concatMap (\calls ->
      Array.concatMap (\c -> [c.callerName, c.calleeName]) calls
    ) (Array.fromFoldable (Map.values state.lastInput.functionCalls))
    internalNames = Array.nub $ Array.filter (\n -> not (Set.member n exportedNames) && not (isCompilerGenerated n)) allCallNames
    sorted = Array.sort internalNames
  in
    if Array.null sorted then HH.text ""
    else HH.div [ HP.style "margin-top: 16px;" ]
      [ HH.div [ HP.style "border-top: 2px dashed #ccc; margin-bottom: 8px;" ] []
      , HH.div [ HP.style "font-size: 9px; color: #999; font-weight: 600; letter-spacing: 0.5px; text-transform: uppercase; margin-bottom: 6px;" ]
          [ HH.text $ "Internal (" <> show (Array.length sorted) <> ")" ]
      , HH.div [ HP.style "display: flex; flex-wrap: wrap; gap: 3px; padding: 4px 0;" ]
          (sorted <#> \name ->
            HH.span
              [ HP.style $ "display: inline-block; width: 8px; height: 8px; border-radius: 50%; cursor: pointer; transition: transform 100ms ease; background: " <> internalDotColor name <> ";"
              , HP.title name
              , HE.onClick \_ -> SignatureClicked name
              ]
              []
          )
      ]

-- | Muted color for internal declaration dots, based on name length
internalDotColor :: String -> String
internalDotColor name =
  let colors = ["#8b9dc3", "#9bb59c", "#c4a882", "#b8929a", "#a3a0c4", "#8cb8b0", "#c4b078", "#b5a0a8"]
      idx = SCU.length name `mod` Array.length colors
  in fromMaybe "#999" (Array.index colors idx)

isCompilerGenerated :: String -> Boolean
isCompilerGenerated name =
  SCU.take 7 name == "discard" || SCU.take 4 name == "bind" || SCU.take 2 name == "$$"

findDeclLine :: String -> State -> Maybe Int
findDeclLine name state =
  Array.findMap (\d -> if d.name == name then d.sourceSpan >>= \s -> Array.head s.start else Nothing) state.lastInput.declarations

-- =============================================================================
-- Age helpers
-- =============================================================================

blameLineAge :: Int -> Int -> Int -> Number
blameLineAge oldest newest t =
  if newest <= oldest then 0.5
  else Int.toNumber (t - oldest) / Int.toNumber (newest - oldest)

blameAgeColor :: Number -> String
blameAgeColor age
  | age < 0.25 = "rgb(240,244,248)"
  | age < 0.5  = "rgb(238,236,228)"
  | age < 0.75 = "rgb(240,224,200)"
  | age < 0.9  = "rgb(238,196,160)"
  | otherwise  = "rgb(232,168,124)"

-- =============================================================================
-- Action Handlers
-- =============================================================================

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  Initialize -> do
    state <- H.get
    let input = state.lastInput
    log $ "[ModuleSignaturesViz] Initializing: " <> input.moduleName

    { emitter, listener } <- liftEffect HS.create
    void $ H.subscribe emitter
    H.modify_ _ { actionListener = Just listener, initialized = true }

    -- Prepare signature cards
    let onDeclClick = makeDeclarationClickCallback (Just listener)
    measured <- liftEffect $ MSM.prepareCells
      { containerSelector: ""
      , moduleName: input.moduleName
      , packageName: input.packageName
      , onDeclarationClick: Just onDeclClick
      }
      input.declarations
    let lanes = MSM.groupIntoLanes measured
    H.modify_ _ { lanes = lanes, measuredCells = measured }

    -- Fetch blame data
    H.modify_ _ { blameLoading = true }
    blameResult <- liftAff $ Loader.fetchModuleBlame input.moduleName
    case blameResult of
      Right blame -> H.modify_ _ { blameData = Just blame, blameLoading = false, filePath = Just blame.filePath }
      Left err -> do
        log $ "[ModuleSignaturesViz] Blame not available: " <> err
        H.modify_ _ { blameLoading = false }

    -- Fetch source for line counting in indicators
    sourceResult <- liftAff $ Loader.fetchModuleSource input.moduleName
    case sourceResult of
      Right src -> H.modify_ _ { moduleSource = Just src.source }
      Left _ -> pure unit

  Receive input -> do
    state <- H.get
    let changed = input.moduleName /= state.lastInput.moduleName
    H.modify_ _ { lastInput = input }
    when (changed && state.initialized) do
      H.modify_ _ { lanes = [], measuredCells = [], blameData = Nothing, blameLoading = false, moduleSource = Nothing, filePath = Nothing }
      handleAction Initialize

  Finalize ->
    log "[ModuleSignaturesViz] Finalizing"

  HandleDeclarationClick pkgName modName declName ->
    H.raise (DeclarationClicked pkgName modName declName)

  BlameLineClicked lineNum -> do
    state <- H.get
    case state.filePath of
      Just fp -> liftEffect $ openUri ("vscode://file/" <> fp <> ":" <> show lineNum)
      Nothing -> pure unit

  SignatureClicked declName -> do
    state <- H.get
    -- Find line number for this declaration
    let mLine = Array.findMap (\d ->
          if d.name == declName then d.sourceSpan >>= \s -> Array.head s.start
          else Nothing
        ) state.lastInput.declarations
    case state.filePath of
      Just fp -> do
        let lineArg = case mLine of
              Just l -> ":" <> show l
              Nothing -> ""
        liftEffect $ openUri ("vscode://file/" <> fp <> lineArg)
      Nothing ->
        -- Fallback: raise as declaration click for navigation
        H.raise (DeclarationClicked state.lastInput.packageName state.lastInput.moduleName declName)

  GoToStructure ->
    H.raise NavigateToStructure

  SetKindFilter mKind ->
    H.modify_ \s -> s { kindFilter = if s.kindFilter == mKind then Nothing else mKind }

-- =============================================================================
-- Declaration click callback
-- =============================================================================

makeDeclarationClickCallback :: Maybe (HS.Listener Action) -> String -> String -> String -> Effect Unit
makeDeclarationClickCallback mListener pkgName modName declName = case mListener of
  Just listener -> HS.notify listener (HandleDeclarationClick pkgName modName declName)
  Nothing -> pure unit
