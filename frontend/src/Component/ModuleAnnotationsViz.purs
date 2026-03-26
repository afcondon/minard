-- | Module Annotations Panel Component
-- |
-- | Self-contained panel for displaying and interacting with module annotations.
-- | Shows threaded conversations between human and LLM reviewers, with
-- | confirm/dispute/reply actions. Renders inline declaration name references
-- | as clickable links.
module CE2.Component.ModuleAnnotationsViz
  ( component
  , Input
  , Output(..)
  , Query(..)
  , Slot
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl)
import Data.Int (toNumber)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Ordering (Ordering)
import Data.Set (Set)
import Data.Set as Set
import Data.String.Common as SC
import Data.String.CodeUnits as SCU
import Data.String.Pattern (Pattern(..))
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import CE2.Component.AnnotationGuide (renderAnnotationGuide)
import CE2.Data.Loader as Loader

-- =============================================================================
-- Types
-- =============================================================================

type Input =
  { moduleName :: String
  , annotations :: Array Loader.V2Annotation
  , declarationNames :: Array String  -- for inline reference matching
  }

data Output
  = AnnotationStatusChanged Int String       -- annId, newStatus
  | AnnotationReplyCreated                   -- reply annotation created
      { targetType :: String
      , targetId :: String
      , kind :: String
      , value :: String
      , supersedes :: Int
      }
  | DeclarationClicked String               -- declaration name clicked in annotation text

type Slot = H.Slot Query Output

data Query a = NoQuery a

type State =
  { lastInput :: Input
  , collapsedThreads :: Set Int
  , replyingTo :: Maybe Int
  , replyText :: String
  }

data Action
  = Receive Input
  | ConfirmAnnotation Int
  | DisputeAnnotation Int
  | StartReply Int
  | CancelReply
  | UpdateReplyText String
  | SubmitReply
  | ToggleThreadCollapse Int
  | ClickDeclaration String

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
initialState input =
  { lastInput: input
  , collapsedThreads: Set.empty
  , replyingTo: Nothing
  , replyText: ""
  }

-- =============================================================================
-- Render
-- =============================================================================

render :: forall m. State -> H.ComponentHTML Action () m
render state
  | Array.null state.lastInput.annotations =
      HH.div_ [ renderAnnotationGuide { compact: true } ]
render state =
  let
    anns = state.lastInput.annotations
    threads = buildThreads anns
    kindGroups = groupThreadsByKind threads
    sorted = kindGroups # Array.sortBy (comparing _.kind)
  in
  HH.div_
    [ HH.div
        [ HP.style "display: grid; grid-template-columns: repeat(auto-fit, minmax(280px, 1fr)); gap: 0;" ]
        (Array.concatMap (\grp ->
          grp.threads <#> \thread ->
            renderThread state thread
        ) sorted)
    ]

-- =============================================================================
-- Thread Rendering
-- =============================================================================

renderThread :: forall m. State -> AnnotationThread -> H.ComponentHTML Action () m
renderThread state thread =
  let
    ann = thread.root
    borderColor = statusBorderColor ann.status
    isCollapsed = Set.member ann.id state.collapsedThreads
    hasReplies = not (Array.null thread.replies)
    declNames = state.lastInput.declarationNames
  in
  HH.div
    [ HP.style $ "padding: 10px 16px; border-right: 1px solid #e0e0e0; border-left: 3px solid " <> borderColor <> "; overflow-wrap: break-word;" ]
    ( [ HH.div
          [ HP.style "display: flex; justify-content: space-between; align-items: center; margin-bottom: 6px;" ]
          [ HH.span
              [ HP.style "font-weight: 600; color: #999; text-transform: uppercase; font-size: 9px; letter-spacing: 1px;" ]
              [ HH.text (ann.kind <> sourceTag ann.source) ]
          , if hasReplies
            then HH.span
              [ HP.style "font-family: 'Fira Code', monospace; font-size: 9px; color: #999; cursor: pointer;"
              , HE.onClick \_ -> ToggleThreadCollapse ann.id
              ]
              [ HH.text (if isCollapsed then "\x25b6 " <> show (Array.length thread.replies + 1) else "\x25bc thread") ]
            else HH.text ""
          ]
      ] <>
      ( if isCollapsed && hasReplies
        then
          let latest = fromMaybe ann (Array.last thread.replies)
          in [ renderAnnotationContent latest declNames
             , renderAnnotationFooter state latest
             ]
        else
          [ renderAnnotationContent ann declNames
          , renderAnnotationFooter state ann
          ] <>
          Array.concatMap (\reply ->
            [ HH.div
                [ HP.style "margin-left: 12px; padding-left: 8px; border-left: 2px solid #e0e0e0; opacity: 0.8; margin-top: 8px;" ]
                [ HH.div
                    [ HP.style "font-size: 9px; color: #999; margin-bottom: 4px;" ]
                    [ HH.text (sourceTag reply.source <> " reply") ]
                , renderAnnotationContent reply declNames
                , renderAnnotationFooter state reply
                ]
            ]
          ) thread.replies
      ) <>
      ( case state.replyingTo of
          Just rid | rid == ann.id || Array.any (\r -> r.id == rid) thread.replies ->
            [ renderReplyInput state ]
          _ -> []
      )
    )

renderAnnotationContent :: forall m. Loader.V2Annotation -> Array String -> H.ComponentHTML Action () m
renderAnnotationContent ann declNames =
  HH.ul
    [ HP.style "margin: 0; padding: 0 0 0 16px; list-style: disc; color: #444; font-size: 12px; line-height: 1.5;" ]
    (splitSentences ann.value <#> \sentence ->
      HH.li
        [ HP.style "margin-bottom: 3px;" ]
        (annotateText sentence declNames)
    )

renderAnnotationFooter :: forall m. State -> Loader.V2Annotation -> H.ComponentHTML Action () m
renderAnnotationFooter state ann =
  let
    replyBtn =
      HH.span
        [ HP.style "font-family: 'Fira Code', monospace; font-size: 9px; color: #666; cursor: pointer; padding: 1px 6px; border: 1px solid #ccc; border-radius: 2px;"
        , HE.onClick \_ -> StartReply ann.id
        ]
        [ HH.text "Reply" ]
    isReplying = state.replyingTo == Just ann.id
  in case ann.status of
    "proposed" ->
      HH.div
        [ HP.style "margin-top: 6px; display: flex; gap: 8px;" ]
        [ HH.span
            [ HP.style "font-family: 'Fira Code', monospace; font-size: 9px; color: #4caf50; cursor: pointer; padding: 1px 6px; border: 1px solid #4caf50; border-radius: 2px;"
            , HE.onClick \_ -> ConfirmAnnotation ann.id
            ]
            [ HH.text "Confirm" ]
        , HH.span
            [ HP.style "font-family: 'Fira Code', monospace; font-size: 9px; color: #e53935; cursor: pointer; padding: 1px 6px; border: 1px solid #e53935; border-radius: 2px;"
            , HE.onClick \_ -> DisputeAnnotation ann.id
            ]
            [ HH.text "Dispute" ]
        , if isReplying then HH.text "" else replyBtn
        ]
    "confirmed" ->
      HH.div
        [ HP.style "margin-top: 6px; display: flex; gap: 8px; align-items: center;" ]
        [ HH.span [ HP.style "font-family: 'Fira Code', monospace; font-size: 9px; color: #4caf50;" ] [ HH.text "\x2713 Confirmed" ]
        , if isReplying then HH.text "" else replyBtn
        ]
    "rejected" ->
      HH.div
        [ HP.style "margin-top: 6px; display: flex; gap: 8px; align-items: center;" ]
        [ HH.span [ HP.style "font-family: 'Fira Code', monospace; font-size: 9px; color: #e53935;" ] [ HH.text "\x2717 Disputed" ]
        , if isReplying then HH.text "" else replyBtn
        ]
    "stale" ->
      HH.div
        [ HP.style "margin-top: 6px; display: flex; gap: 8px; align-items: center;" ]
        [ HH.span [ HP.style "font-family: 'Fira Code', monospace; font-size: 9px; color: #f57c00;" ] [ HH.text "\x26a0 May be outdated" ]
        , if isReplying then HH.text "" else replyBtn
        ]
    _ ->
      HH.div
        [ HP.style "margin-top: 6px; display: flex; gap: 8px;" ]
        [ if isReplying then HH.text "" else replyBtn ]

renderReplyInput :: forall m. State -> H.ComponentHTML Action () m
renderReplyInput state =
  HH.div
    [ HP.style "margin-top: 8px; padding: 8px; background: #f9f9f9; border: 1px solid #e0e0e0; border-radius: 4px;" ]
    [ HH.textarea
        [ HP.style "width: 100%; min-height: 60px; font-family: 'Fira Code', monospace; font-size: 11px; border: 1px solid #ccc; border-radius: 3px; padding: 6px; box-sizing: border-box; resize: vertical;"
        , HP.value state.replyText
        , HP.placeholder "Your reply..."
        , HE.onValueInput UpdateReplyText
        ]
    , HH.div
        [ HP.style "margin-top: 6px; display: flex; gap: 8px;" ]
        [ HH.span
            [ HP.style "font-family: 'Fira Code', monospace; font-size: 9px; color: #fff; background: #2563eb; cursor: pointer; padding: 2px 8px; border-radius: 2px;"
            , HE.onClick \_ -> SubmitReply
            ]
            [ HH.text "Send" ]
        , HH.span
            [ HP.style "font-family: 'Fira Code', monospace; font-size: 9px; color: #999; cursor: pointer; padding: 2px 8px; border: 1px solid #ccc; border-radius: 2px;"
            , HE.onClick \_ -> CancelReply
            ]
            [ HH.text "Cancel" ]
        ]
    ]

-- =============================================================================
-- Helpers
-- =============================================================================

type AnnotationThread =
  { root :: Loader.V2Annotation
  , replies :: Array Loader.V2Annotation
  }

buildThreads :: Array Loader.V2Annotation -> Array AnnotationThread
buildThreads anns =
  let
    roots = Array.filter (\a -> a.supersedes == Nothing) anns
    replyMap :: Map.Map Int (Array Loader.V2Annotation)
    replyMap = foldl (\acc a -> case a.supersedes of
      Just sid -> Map.insertWith (<>) sid [a] acc
      Nothing -> acc
    ) Map.empty anns
    collectChain :: Int -> Array Loader.V2Annotation
    collectChain rootId =
      let direct = fromMaybe [] (Map.lookup rootId replyMap)
      in direct <> Array.concatMap (\r -> collectChain r.id) direct
  in roots <#> \root -> { root, replies: collectChain root.id }

groupThreadsByKind :: Array AnnotationThread -> Array { kind :: String, threads :: Array AnnotationThread }
groupThreadsByKind threads =
  let grouped = foldl (\acc t ->
        let k = t.root.kind
            existing = fromMaybe [] (Map.lookup k acc)
        in Map.insert k (Array.snoc existing t) acc
      ) Map.empty threads
  in Map.toUnfoldable grouped <#> \(Tuple k ts) -> { kind: k, threads: ts }

statusBorderColor :: String -> String
statusBorderColor = case _ of
  "confirmed" -> "#4caf50"
  "rejected"  -> "#e53935"
  "stale"     -> "#f57c00"
  _           -> "#bdbdbd"

sourceTag :: String -> String
sourceTag "ai" = " (ai)"
sourceTag "human" = " (human)"
sourceTag s = if s == "" then "" else " (" <> s <> ")"

splitSentences :: String -> Array String
splitSentences text =
  let parts = SC.split (Pattern ". ") text
      len = Array.length parts
  in parts # Array.mapWithIndex (\i s ->
    if i < len - 1 then s <> "." else s)
    # Array.filter (\s -> SCU.length s > 0)

-- | Render text with declaration names as clickable inline references
annotateText :: forall m. String -> Array String -> Array (H.ComponentHTML Action () m)
annotateText text declNames =
  let
    matches = findTextMatches text declNames
    go :: Int -> Array { pos :: Int, name :: String } -> Array (H.ComponentHTML Action () m)
    go cursor remaining = case Array.uncons remaining of
      Nothing ->
        let rest = SCU.drop cursor text
        in if SCU.length rest > 0 then [HH.text rest] else []
      Just { head: m, tail: ms } ->
        let
          before = SCU.take (m.pos - cursor) (SCU.drop cursor text)
          nameLen = SCU.length m.name
          beforeEls = if SCU.length before > 0 then [HH.text before] else []
          matchEl = HH.span
            [ HP.style "padding: 1px 4px; border-radius: 3px; background: #e8e4d8; border: 1px solid #d8d0bc; cursor: pointer; font-family: 'Fira Code','SF Mono', monospace; font-size: 10px;"
            , HE.onClick \_ -> ClickDeclaration m.name
            ]
            [ HH.text m.name ]
        in beforeEls <> [matchEl] <> go (m.pos + nameLen) ms
  in go 0 matches

findTextMatches :: String -> Array String -> Array { pos :: Int, name :: String }
findTextMatches text names =
  let
    candidates = names
      # Array.filter (\n -> SCU.length n >= 4)
      # Array.mapMaybe (\n -> case SCU.indexOf (Pattern n) text of
          Just pos -> Just { pos, name: n }
          Nothing -> Nothing)
      # Array.sortBy (comparing _.pos)
    removeOverlaps = foldl (\acc m ->
      case Array.last acc of
        Nothing -> [m]
        Just prev ->
          if m.pos < prev.pos + SCU.length prev.name
          then acc
          else Array.snoc acc m
    ) [] candidates
  in Array.take 6 removeOverlaps

-- =============================================================================
-- Action Handlers
-- =============================================================================

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  Receive input -> do
    H.modify_ _ { lastInput = input }

  ConfirmAnnotation annId -> do
    H.raise (AnnotationStatusChanged annId "confirmed")

  DisputeAnnotation annId -> do
    H.raise (AnnotationStatusChanged annId "rejected")

  StartReply annId -> do
    H.modify_ _ { replyingTo = Just annId, replyText = "" }

  CancelReply -> do
    H.modify_ _ { replyingTo = Nothing, replyText = "" }

  UpdateReplyText text -> do
    H.modify_ _ { replyText = text }

  SubmitReply -> do
    state <- H.get
    case state.replyingTo of
      Nothing -> pure unit
      Just targetId -> do
        let input = state.lastInput
            targetAnn = Array.find (\a -> a.id == targetId) input.annotations
            kind = fromMaybe "summary" (targetAnn <#> _.kind)
        when (SCU.length state.replyText > 0) do
          H.modify_ _ { replyingTo = Nothing, replyText = "" }
          H.raise $ AnnotationReplyCreated
            { targetType: "module"
            , targetId: input.moduleName
            , kind
            , value: state.replyText
            , supersedes: targetId
            }

  ToggleThreadCollapse rootId -> do
    state <- H.get
    let newCollapsed = if Set.member rootId state.collapsedThreads
          then Set.delete rootId state.collapsedThreads
          else Set.insert rootId state.collapsedThreads
    H.modify_ _ { collapsedThreads = newCollapsed }

  ClickDeclaration declName -> do
    H.raise (DeclarationClicked declName)
