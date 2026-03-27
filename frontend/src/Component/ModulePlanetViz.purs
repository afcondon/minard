-- | Module Planet — Unified Module View
-- |
-- | Lightroom-style panel layout combining all module-level views:
-- | Signatures, Structure (layers/concerns/cutpoints), Dependencies,
-- | Annotations, and Overview. Panels toggle open/closed independently.
-- | On large displays, multiple panels visible simultaneously.
module CE2.Component.ModulePlanetViz
  ( component
  , Input
  , Output(..)
  , Query(..)
  , Slot
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Int as Int
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (log) as Num
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))

import CE2.Component.ConcernsPanel as ConcernsPanel
import CE2.Component.CutpointsPanel as CutpointsPanel
import CE2.Component.LayerDiagramPanel as LayerDiagramPanel
import CE2.Component.ModuleAnnotationsViz as AnnotationsViz
import CE2.Component.ModuleSignaturesViz as SignaturesViz
import CE2.Viz.BlameRibbon as BlameRibbon
import CE2.Viz.CommitSparkline as Spark
import CE2.Component.DeclarationUsageGraph as UsageGraphViz
import CE2.Data.Loader as Loader

-- =============================================================================
-- Types
-- =============================================================================

type Input =
  { packageName :: String
  , moduleName :: String
  , declarations :: Array Loader.V2Declaration
  , annotations :: Array Loader.V2Annotation
  , functionCalls :: Map.Map Int (Array Loader.V2FunctionCall)
  , moduleNameToId :: Map.Map String Int
  }

data Output
  = DeclarationClicked String String String  -- pkg, mod, decl
  | AnnotationStatusChanged Int String
  | AnnotationReplyCreated
      { targetType :: String
      , targetId :: String
      , kind :: String
      , value :: String
      , supersedes :: Int
      }
  | CompareSnapshotsClicked
  | NavigateToGitView String  -- packageName → git commit grid

type Slot = H.Slot Query Output

data Query a = NoQuery a

-- | Which panels are currently open
data Panel = PanelSignatures | PanelDependencies | PanelLayers | PanelCutpoints | PanelConcerns | PanelAnnotations

derive instance eqPanel :: Eq Panel
derive instance ordPanel :: Ord Panel

type ChildSlots =
  ( signatures :: SignaturesViz.Slot Unit
  , dependencies :: UsageGraphViz.Slot Unit
  , layers :: LayerDiagramPanel.Slot Unit
  , cutpoints :: CutpointsPanel.Slot Unit
  , concerns :: ConcernsPanel.Slot Unit
  , annotations :: AnnotationsViz.Slot Unit
  )

_signatures :: Proxy "signatures"
_signatures = Proxy

_dependencies :: Proxy "dependencies"
_dependencies = Proxy

_layers :: Proxy "layers"
_layers = Proxy

_cutpoints :: Proxy "cutpoints"
_cutpoints = Proxy

_concerns :: Proxy "concerns"
_concerns = Proxy

_annotations :: Proxy "annotations"
_annotations = Proxy

type State =
  { lastInput :: Input
  , openPanels :: Set Panel
  , focusedDeclaration :: Maybe String
  , blameData :: Maybe Loader.BlameResult
  , blameLoading :: Boolean
  , sparklineBars :: Array Spark.SparklineBar
  , hoveredCommit :: Maybe String    -- highlighted commit hash (from blame or dot hover)
  , hoveredDeclName :: Maybe String  -- hovered declaration name (for tooltip + blame highlight)
  , allDeclNames :: Array String     -- all declaration names including internals from calls
  }

data Action
  = Initialize
  | Receive Input
  | TogglePanel Panel
  | FocusDeclaration (Maybe String)
  | HoverBlame (Maybe String)     -- commit hash from blame ribbon
  | HoverDecl (Maybe String)      -- declaration name from dot cloud
  | NavigateToGit
  | HandleSignaturesOutput SignaturesViz.Output
  | HandleDependenciesOutput UsageGraphViz.Output
  | HandleLayersOutput LayerDiagramPanel.Output
  | HandleCutpointsOutput CutpointsPanel.Output
  | HandleConcernsOutput ConcernsPanel.Output
  | HandleAnnotationsOutput AnnotationsViz.Output

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
        }
    }

initialState :: Input -> State
initialState input =
  { lastInput: input
  , openPanels: Set.fromFoldable [PanelSignatures]
  , focusedDeclaration: Nothing
  , blameData: Nothing
  , blameLoading: false
  , sparklineBars: []
  , hoveredCommit: Nothing
  , hoveredDeclName: Nothing
  , allDeclNames: buildAllDeclNames input
  }

-- =============================================================================
-- Render
-- =============================================================================

render :: forall m. MonadAff m => State -> H.ComponentHTML Action ChildSlots m
render state =
  HH.div
    [ HP.style "display: flex; flex-direction: column; width: 100%; height: 100%; overflow: hidden;" ]
    [ -- Header: panel toggles + sparkline (merged row)
      renderPanelBar state
    -- Main area: blame ribbon (left, persistent) + panels (right)
    , HH.div
        [ HP.style "flex: 1; min-height: 0; display: flex; overflow: hidden;" ]
        [ -- Left: persistent blame ribbon (scrolls with panels)
          renderBlameColumn state
        -- Right: scrollable panel stack
        , HH.div
            [ HP.style "flex: 1; min-width: 0; overflow-y: auto;" ]
            ( -- Sparkline strip (click → git view)
              (if Array.length state.sparklineBars > 0
                then [ HH.div
                    [ HP.style "padding: 4px 16px; background: #f0ece0; border-bottom: 1px solid #d8d0bc; cursor: pointer;"
                    , HE.onClick \_ -> NavigateToGit
                    ]
                    [ Spark.renderSparkline state.sparklineBars ]
                  ]
                else [])
              -- Declaration map (all declarations as colored dots)
              <> [ renderDeclarationMap state ]
              <> Array.catMaybes
                  [ if isPanelOpen PanelSignatures state
                      then Just (renderSignaturesPanel state)
                      else Nothing
                  , if isPanelOpen PanelDependencies state || state.focusedDeclaration /= Nothing
                      then Just (renderDependenciesPanel state)
                      else Nothing
                  , if isPanelOpen PanelLayers state
                      then Just (renderLayersPanel state)
                      else Nothing
                  , if isPanelOpen PanelCutpoints state
                      then Just (renderCutpointsPanel state)
                      else Nothing
                  , if isPanelOpen PanelConcerns state
                      then Just (renderConcernsPanel state)
                      else Nothing
                  , if isPanelOpen PanelAnnotations state
                      then Just (renderAnnotationsPanel state)
                      else Nothing
                  ]
            )
        ]
    ]

-- | Top bar with panel toggle buttons
renderPanelBar :: forall m. State -> H.ComponentHTML Action ChildSlots m
renderPanelBar state =
  HH.div
    [ HP.style "display: flex; align-items: center; gap: 2px; padding: 6px 12px; background: #e8e4d8; border-bottom: 1px solid #d8d0bc; flex-shrink: 0;" ]
    [ HH.span
        [ HP.style "font-size: 12px; font-weight: 700; color: #555; margin-right: 12px; font-family: 'Courier New', Courier, monospace;" ]
        [ HH.text $ shortModuleName state.lastInput.moduleName ]
    , panelToggle "Signatures" PanelSignatures state
    , panelToggle "Dependencies" PanelDependencies state
    , panelToggle "Layers" PanelLayers state
    , panelToggle "Cutpoints" PanelCutpoints state
    , panelToggle "Concerns" PanelConcerns state
    , panelToggle "Annotations" PanelAnnotations state
    , HH.span [ HP.style "flex: 1;" ] []
    , HH.span
        [ HP.style "font-size: 10px; color: #888; white-space: nowrap;" ]
        [ HH.text $ show (Array.length state.lastInput.declarations) <> " declarations" ]
    ]

panelToggle :: forall m. String -> Panel -> State -> H.ComponentHTML Action ChildSlots m
panelToggle label panel state =
  let isOpen = isPanelOpen panel state
      style = if isOpen
        then "font-size: 11px; font-weight: 600; color: #333; background: #fff; border: 1px solid #c8c0a8; border-radius: 3px; padding: 3px 10px; cursor: pointer;"
        else "font-size: 11px; font-weight: 500; color: #888; background: transparent; border: 1px solid transparent; border-radius: 3px; padding: 3px 10px; cursor: pointer;"
  in
  HH.span
    [ HP.style style
    , HE.onClick \_ -> TogglePanel panel
    ]
    [ HH.text label ]

isPanelOpen :: Panel -> State -> Boolean
isPanelOpen panel state = Set.member panel state.openPanels

-- =============================================================================
-- Panel Rendering
-- =============================================================================

renderSignaturesPanel :: forall m. MonadAff m => State -> H.ComponentHTML Action ChildSlots m
renderSignaturesPanel state =
  let input = state.lastInput
  in
  HH.div
    [ HP.style "border-bottom: 2px solid #e8e4d8;" ]
    [ HH.slot _signatures unit SignaturesViz.component
        { packageName: input.packageName
        , moduleName: input.moduleName
        , declarations: input.declarations
        , functionCalls: input.functionCalls
        , showBlameRibbon: false
        , showModuleHeader: false
        }
        HandleSignaturesOutput
    ]

renderDependenciesPanel :: forall m. MonadAff m => State -> H.ComponentHTML Action ChildSlots m
renderDependenciesPanel state =
  let
    input = state.lastInput
    declName = fromMaybe (firstValueDecl input.declarations) state.focusedDeclaration
    mDecl = Array.find (\d -> d.name == declName) input.declarations
    modId = Map.lookup input.moduleName input.moduleNameToId
    calls = fromMaybe [] (modId >>= \mid -> Map.lookup mid input.functionCalls)
  in
  HH.div
    [ HP.style "border-bottom: 2px solid #e8e4d8;" ]
    [ -- Header with close button
      HH.div
        [ HP.style "display: flex; align-items: center; justify-content: space-between; padding: 8px 16px; background: #f8f6f0; border-bottom: 1px solid #e8e4d8;" ]
        [ HH.span
            [ HP.style "font-size: 12px; font-weight: 600; color: #555; font-family: 'Courier New', Courier, monospace;" ]
            [ HH.text $ "Dependencies: " <> declName ]
        , HH.span
            [ HP.style "font-size: 14px; color: #999; cursor: pointer; padding: 2px 6px;"
            , HE.onClick \_ -> FocusDeclaration Nothing
            ]
            [ HH.text "\x00D7" ]
        ]
    -- Usage graph
    , HH.div
        [ HP.style "height: 500px; overflow: hidden;" ]
        [ HH.slot _dependencies unit UsageGraphViz.component
            { packageName: input.packageName
            , moduleName: input.moduleName
            , declarationName: declName
            , focusTypeSignature: mDecl >>= _.typeSignature
            , declarations: input.declarations
            , moduleCalls: calls
            , allCalls: input.functionCalls
            , moduleNameToId: input.moduleNameToId
            }
            HandleDependenciesOutput
        ]
    ]

renderLayersPanel :: forall m. MonadAff m => State -> H.ComponentHTML Action ChildSlots m
renderLayersPanel state =
  let input = state.lastInput
  in
  HH.div
    [ HP.style "border-bottom: 2px solid #e8e4d8;" ]
    [ HH.slot _layers unit LayerDiagramPanel.component
        { moduleName: input.moduleName
        , declarations: input.declarations
        , functionCalls: input.functionCalls
        }
        HandleLayersOutput
    ]

renderCutpointsPanel :: forall m. MonadAff m => State -> H.ComponentHTML Action ChildSlots m
renderCutpointsPanel state =
  let input = state.lastInput
  in
  HH.div
    [ HP.style "border-bottom: 2px solid #e8e4d8;" ]
    [ HH.slot _cutpoints unit CutpointsPanel.component
        { moduleName: input.moduleName
        , declarations: input.declarations
        , functionCalls: input.functionCalls
        }
        HandleCutpointsOutput
    ]

renderConcernsPanel :: forall m. MonadAff m => State -> H.ComponentHTML Action ChildSlots m
renderConcernsPanel state =
  HH.div
    [ HP.style "border-bottom: 2px solid #e8e4d8;" ]
    [ HH.slot _concerns unit ConcernsPanel.component
        { moduleName: state.lastInput.moduleName }
        HandleConcernsOutput
    ]

renderAnnotationsPanel :: forall m. MonadAff m => State -> H.ComponentHTML Action ChildSlots m
renderAnnotationsPanel state =
  let input = state.lastInput
      declNames = input.declarations <#> _.name
  in
  HH.div
    [ HP.style "padding: 12px 16px;" ]
    [ HH.slot _annotations unit AnnotationsViz.component
        { moduleName: input.moduleName
        , annotations: input.annotations
        , declarationNames: declNames
        }
        HandleAnnotationsOutput
    ]

-- =============================================================================
-- Helpers
-- =============================================================================

shortModuleName :: String -> String
shortModuleName name =
  case Array.last (String.split (String.Pattern ".") name) of
    Just short -> short
    Nothing -> name

firstValueDecl :: Array Loader.V2Declaration -> String
firstValueDecl decls =
  case Array.find (\d -> d.kind == "value") decls of
    Just d -> d.name
    Nothing -> case Array.head decls of
      Just d -> d.name
      Nothing -> "component"

-- =============================================================================
-- Blame Column (persistent left strip)
-- =============================================================================

renderBlameColumn :: forall m. State -> H.ComponentHTML Action ChildSlots m
renderBlameColumn state =
  HH.div
    [ HP.style "flex-shrink: 0; overflow-y: auto; cursor: pointer;" ]
    [ case state.blameData of
        Nothing ->
          if state.blameLoading
            then HH.div [ HP.style "width: 80px; display: flex; align-items: center; justify-content: center; color: #999; font-size: 10px; padding: 8px;" ] [ HH.text "..." ]
            else HH.text ""
        Just blame ->
          HH.div
            [ HP.style "width: 80px; border: 1px solid #d5d0c4; border-radius: 4px; background: #faf8f3;" ]
            [ HH.div [ HP.style "display: flex; flex-direction: column;" ]
                (Array.mapWithIndex (\idx blameLine ->
                  let
                    age = BlameRibbon.blameLineAge blame.oldestTime blame.newestTime blameLine.authorTime
                    bgColor = recencyColor age
                    prevHash = Array.index blame.lines (idx - 1) <#> _.hash
                    isGroupStart = idx > 0 && prevHash /= Just blameLine.hash
                    isHighlighted = state.hoveredCommit == Just blameLine.hash
                    opacity = if isHighlighted then "1.0" else case state.hoveredCommit of
                      Nothing -> "1.0"
                      Just _ -> "0.3"
                  in HH.div
                    [ HP.style $ "height: 2px; background: " <> bgColor <> "; opacity: " <> opacity <> "; transition: opacity 100ms ease;"
                        <> (if isGroupStart then " border-top: 1px solid rgba(0,0,0,0.15);" else "")
                    , HP.title (blameLine.shortHash <> " \x00B7 " <> BlameRibbon.formatRelativeTime blameLine.authorTime <> "\n" <> blameLine.summary)
                    , HE.onMouseEnter \_ -> HoverBlame (Just blameLine.hash)
                    , HE.onMouseLeave \_ -> HoverBlame Nothing
                    , HE.onClick \_ -> NavigateToGit
                    ]
                    []
                ) blame.lines)
            ]
    ]

-- =============================================================================
-- Declaration Map (all declarations as dots)
-- =============================================================================

-- | Info needed to render each dot in the declaration map
type DeclDot =
  { name :: String
  , exported :: Boolean
  , sourceSpan :: Maybe { start :: Array Int, end :: Array Int, name :: String }
  , loc :: Int  -- lines of code (0 if unknown)
  }

renderDeclarationMap :: forall m. State -> H.ComponentHTML Action ChildSlots m
renderDeclarationMap state =
  let
    decls = state.lastInput.declarations
    internalNames = state.allDeclNames
    exportedNames = Set.fromFoldable $ decls <#> _.name
    -- Build dots for all declarations
    exportedDots :: Array DeclDot
    exportedDots = decls <#> \d ->
      { name: d.name
      , exported: true
      , sourceSpan: d.sourceSpan
      , loc: spanLoc d.sourceSpan
      }
    internalDots :: Array DeclDot
    internalDots = Array.filter (\n -> not (Set.member n exportedNames)) internalNames <#> \n ->
      { name: n, exported: false, sourceSpan: Nothing, loc: 0 }
    allDots = exportedDots <> internalDots
  in
  if Array.null allDots then HH.text ""
  else HH.div
    [ HP.style "padding: 6px 16px; border-bottom: 1px solid #e8e4d8; background: #faf8f3;"
    , HE.onMouseLeave \_ -> HoverDecl Nothing
    ]
    [ HH.div
        [ HP.style "display: flex; flex-wrap: wrap; gap: 3px; align-items: center;" ]
        (allDots <#> \d ->
          let
            age = declBlameAge state d.sourceSpan
            color = recencyColor age
            isHovered = state.hoveredDeclName == Just d.name
            commitMatch = case state.hoveredCommit of
              Nothing -> true
              Just hash -> declMatchesCommit state d.sourceSpan hash
            -- Size by LOC: min 6px, max 20px, log-scaled
            baseSize = if d.loc > 0
              then max 6.0 (min 20.0 (4.0 + logScale (Int.toNumber d.loc) * 4.0))
              else if d.exported then 10.0 else 6.0
            sizeStr = show baseSize <> "px"
            border = if d.exported then "border: 1.5px solid rgba(0,0,0,0.25); " else ""
            dimmed = case state.hoveredCommit of
              Nothing -> false
              Just _ -> not commitMatch
            opac = if dimmed then "0.15" else "1.0"
          in
          HH.span
            [ HP.style $ "display: inline-block; width: " <> sizeStr <> "; height: " <> sizeStr <> "; border-radius: 50%; cursor: pointer; background: " <> color <> "; " <> border <> "opacity: " <> opac <> "; transition: opacity 100ms ease, transform 100ms ease;"
                <> (if isHovered then " transform: scale(1.5); z-index: 1;" else "")
            , HP.title d.name
            , HE.onMouseEnter \_ -> HoverDecl (Just d.name)
            , HE.onClick \_ -> FocusDeclaration (Just d.name)
            ]
            []
        )
    ]

-- | Compute LOC from source span
spanLoc :: Maybe { start :: Array Int, end :: Array Int, name :: String } -> Int
spanLoc = case _ of
  Just span -> case Array.index span.start 0, Array.index span.end 0 of
    Just s, Just e -> max 1 (e - s + 1)
    _, _ -> 0
  Nothing -> 0

-- | Log scale for LOC sizing
logScale :: Number -> Number
logScale n = Num.log (1.0 + n)

-- | Get the blame age for a declaration based on its source span
declBlameAge :: State -> Maybe { start :: Array Int, end :: Array Int, name :: String } -> Number
declBlameAge state mSpan = case state.blameData, mSpan of
  Just blame, Just span ->
    let startLine = fromMaybe 1 (Array.index span.start 0)
        endLine = fromMaybe startLine (Array.index span.end 0)
        declLines = Array.filter (\bl -> bl.lineNum >= startLine && bl.lineNum <= endLine) blame.lines
        avgTime = case declLines of
          [] -> blame.newestTime
          ls -> Array.foldl (\acc bl -> acc + bl.authorTime) 0 ls / Array.length ls
    in BlameRibbon.blameLineAge blame.oldestTime blame.newestTime avgTime
  _, _ -> 0.5

-- | Check if a declaration's source span includes lines from a given commit
declMatchesCommit :: State -> Maybe { start :: Array Int, end :: Array Int, name :: String } -> String -> Boolean
declMatchesCommit state mSpan hash = case state.blameData, mSpan of
  Just blame, Just span ->
    let startLine = fromMaybe 1 (Array.index span.start 0)
        endLine = fromMaybe startLine (Array.index span.end 0)
    in Array.any (\bl -> bl.lineNum >= startLine && bl.lineNum <= endLine && bl.hash == hash) blame.lines
  _, _ -> false

-- | Build all declaration names: exported + internals from call data
buildAllDeclNames :: Input -> Array String
buildAllDeclNames input =
  let exportedNames = Set.fromFoldable $ input.declarations <#> _.name
      modId = Map.lookup input.moduleName input.moduleNameToId
      calls = fromMaybe [] (modId >>= \mid -> Map.lookup mid input.functionCalls)
      callNames = Array.concatMap (\c -> [c.callerName, c.calleeName]) calls
      internalNames = Array.nub $ Array.filter (\n -> not (Set.member n exportedNames) && not (isCompilerGenerated n)) callNames
  in Array.sort internalNames

isCompilerGenerated :: String -> Boolean
isCompilerGenerated name =
  String.take 7 name == "discard" || String.take 4 name == "bind" || String.take 2 name == "$$"

-- | Find the most recent commit hash for a declaration by name
findDeclCommit :: State -> String -> Maybe String
findDeclCommit state name = do
  blame <- state.blameData
  decl <- Array.find (\d -> d.name == name) state.lastInput.declarations
  span <- decl.sourceSpan
  startLine <- Array.index span.start 0
  endLine <- Array.index span.end 0
  let declLines = Array.filter (\bl -> bl.lineNum >= startLine && bl.lineNum <= endLine) blame.lines
  newest <- Array.foldl (\acc bl -> case acc of
    Nothing -> Just bl
    Just prev -> if bl.authorTime > prev.authorTime then Just bl else acc
    ) Nothing declLines
  pure newest.hash

-- | Recency color: bright red = very recent, warm orange = medium, pale yellow = old
recencyColor :: Number -> String
recencyColor age
  | age > 0.9  = "#e63946"
  | age > 0.75 = "#e76f51"
  | age > 0.5  = "#f4a261"
  | age > 0.25 = "#e9c46a"
  | otherwise  = "#f0e6c8"

-- =============================================================================
-- Action Handlers
-- =============================================================================

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action ChildSlots Output m Unit
handleAction = case _ of
  Initialize -> do
    state <- H.get
    loadModuleData state.lastInput

  Receive input -> do
    state <- H.get
    let moduleChanged = input.moduleName /= state.lastInput.moduleName
    H.modify_ _ { lastInput = input, allDeclNames = buildAllDeclNames input }
    when moduleChanged do
      H.modify_ _ { blameData = Nothing, blameLoading = false, sparklineBars = [], focusedDeclaration = Nothing, hoveredCommit = Nothing, hoveredDeclName = Nothing }
      loadModuleData input

  TogglePanel panel -> do
    state <- H.get
    let newPanels = if Set.member panel state.openPanels
          then Set.delete panel state.openPanels
          else Set.insert panel state.openPanels
    H.modify_ _ { openPanels = newPanels }

  FocusDeclaration mDecl -> do
    H.modify_ _ { focusedDeclaration = mDecl }
    case mDecl of
      Just _ -> do
        state <- H.get
        when (not $ Set.member PanelDependencies state.openPanels) do
          H.modify_ _ { openPanels = Set.insert PanelDependencies state.openPanels }
      Nothing -> pure unit

  HoverBlame mHash -> do
    H.modify_ _ { hoveredCommit = mHash }

  HoverDecl mName -> do
    state <- H.get
    -- When hovering a declaration, find its most recent commit and highlight that
    let commitHash = case mName of
          Just name -> findDeclCommit state name
          Nothing -> Nothing
    H.modify_ _ { hoveredDeclName = mName, hoveredCommit = commitHash }

  NavigateToGit -> do
    state <- H.get
    H.raise (NavigateToGitView state.lastInput.packageName)

  HandleSignaturesOutput output -> case output of
    SignaturesViz.DeclarationClicked pkgName modName declName -> do
      log $ "[ModulePlanet] Signature clicked: " <> declName
      state <- H.get
      if modName == state.lastInput.moduleName
        then handleAction (FocusDeclaration (Just declName))
        else H.raise (DeclarationClicked pkgName modName declName)
    SignaturesViz.NavigateToStructure ->
      pure unit  -- Already on the planet page, no-op

  HandleDependenciesOutput output -> case output of
    UsageGraphViz.NodeClicked _pkgName modName declName -> do
      state <- H.get
      if modName == state.lastInput.moduleName
        then handleAction (FocusDeclaration (Just declName))
        else H.raise (DeclarationClicked state.lastInput.packageName modName declName)
    UsageGraphViz.ModuleClicked modName -> do
      state <- H.get
      H.raise (DeclarationClicked state.lastInput.packageName modName "")
    UsageGraphViz.OpenFocusInEditor -> do
      -- Let parent handle VS Code opening
      state <- H.get
      case state.focusedDeclaration of
        Just declName -> H.raise (DeclarationClicked state.lastInput.packageName state.lastInput.moduleName declName)
        Nothing -> pure unit
    UsageGraphViz.ViewModuleSignatures _ -> do
      -- Already visible — ensure signatures panel is open
      state <- H.get
      when (not $ Set.member PanelSignatures state.openPanels) do
        H.modify_ _ { openPanels = Set.insert PanelSignatures state.openPanels }
    UsageGraphViz.ViewPackage pkgName -> do
      H.raise (DeclarationClicked pkgName "" "")

  HandleLayersOutput output -> case output of
    LayerDiagramPanel.DeclarationClicked declName -> do
      handleAction (FocusDeclaration (Just declName))
    LayerDiagramPanel.DeclarationHovered _ ->
      pure unit

  HandleCutpointsOutput output -> case output of
    CutpointsPanel.DeclarationClicked declName -> do
      handleAction (FocusDeclaration (Just declName))

  HandleConcernsOutput output -> case output of
    ConcernsPanel.DeclarationClicked declName -> do
      handleAction (FocusDeclaration (Just declName))

  HandleAnnotationsOutput output -> case output of
    AnnotationsViz.AnnotationStatusChanged annId newStatus -> do
      H.raise (AnnotationStatusChanged annId newStatus)
    AnnotationsViz.AnnotationReplyCreated reply -> do
      H.raise (AnnotationReplyCreated reply)
    AnnotationsViz.DeclarationClicked declName -> do
      handleAction (FocusDeclaration (Just declName))

-- | Fetch blame and sparkline data for the current module
loadModuleData :: forall m. MonadAff m => Input -> H.HalogenM State Action ChildSlots Output m Unit
loadModuleData input = do
  -- Fetch blame
  H.modify_ _ { blameLoading = true }
  void $ H.fork do
    blameResult <- liftAff $ Loader.fetchModuleBlame input.moduleName
    case blameResult of
      Right blame -> H.modify_ _ { blameData = Just blame, blameLoading = false }
      Left _ -> H.modify_ _ { blameLoading = false }
  -- Fetch sparkline
  void $ H.fork do
    -- Find the package name for numstat fetch
    let pkgName = input.packageName
    numstatResult <- liftAff $ Loader.fetchModuleNumstat 500 pkgName
    case numstatResult of
      Right commits -> do
        let bars = Spark.prepareData input.moduleName commits
        H.modify_ _ { sparklineBars = bars }
      Left _ -> pure unit
