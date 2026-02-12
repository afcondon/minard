// Curated collection of PureScript type signatures for visual grammar exploration
// Each signature chosen to exercise a different rendering challenge

const SIGNATURES = [

  // ─── 1. SIMPLE VALUES ─────────────────────────────────────────
  {
    category: "simple",
    name: "maxHeaderSize",
    module: "Node.HTTP",
    kind: "value",
    sig: "Int"
  },
  {
    category: "simple",
    name: "pid",
    module: "Node.Process",
    kind: "value",
    sig: "Pid"
  },

  // ─── 2. PURE FUNCTIONS ────────────────────────────────────────
  {
    category: "pure_function",
    name: "head",
    module: "Data.NonEmpty",
    kind: "value",
    sig: "forall a. NonEmptyArray a -> a"
  },
  {
    category: "pure_function",
    name: "decayAlpha",
    module: "Hylograph.Simulation",
    kind: "value",
    sig: "Number -> Number -> Number -> Number -> Number"
  },
  {
    category: "pure_function",
    name: "dropWhile",
    module: "Data.String",
    kind: "value",
    sig: "String -> String -> (Boolean -> Char)"
  },

  // ─── 3. CONSTRAINED FUNCTIONS ─────────────────────────────────
  {
    category: "constrained",
    name: "sort",
    module: "Data.Array",
    kind: "value",
    sig: "forall a. Ord a => Array a -> Array a"
  },
  {
    category: "constrained",
    name: "fromFoldable",
    module: "Data.Map",
    kind: "value",
    sig: "forall f k v. Ord k => Foldable f => Map k v -> f (Tuple k v)"
  },
  {
    category: "constrained",
    name: "evalStateT",
    module: "Control.Monad.State",
    kind: "value",
    sig: "forall s m a. Functor m => m a -> s -> StateT s m a"
  },
  {
    category: "constrained",
    name: "experiment",
    module: "Control.Comonad.Store",
    kind: "value",
    sig: "forall f a w s. ComonadStore s w => Functor f => f a -> w a -> (f s -> s)"
  },

  // ─── 4. MULTI-CONSTRAINT ──────────────────────────────────────
  {
    category: "multi_constraint",
    name: "foldM",
    module: "Data.Foldable",
    kind: "value",
    sig: "forall f m a b. Foldable f => Monad m => m b -> f a -> b -> (m b -> a -> b)"
  },
  {
    category: "multi_constraint",
    name: "tailRecM3",
    module: "Control.Monad.Rec.Class",
    kind: "value",
    sig: "forall m a b c d. MonadRec m => m d -> c -> b -> a -> (m (Step Record ( a :: a, b :: b, c :: c ) d) -> c -> b -> a)"
  },
  {
    category: "multi_constraint",
    name: "biall",
    module: "Data.Bifoldable",
    kind: "value",
    sig: "forall t a b c. Bifoldable t => BooleanAlgebra c => c -> t a b -> (c -> b) -> (c -> a)"
  },

  // ─── 5. CLOSED RECORD TYPES ───────────────────────────────────
  {
    category: "record",
    name: "BrushExtent",
    module: "Hylograph.Brush.Types",
    kind: "type_synonym",
    sig: "Record ( x0 :: Number, y0 :: Number, x1 :: Number, y1 :: Number )"
  },
  {
    category: "record",
    name: "TypeClassInfo",
    module: "CE2.Data.Loader",
    kind: "type_synonym",
    sig: "Record ( id :: Int, name :: String, moduleName :: String, packageName :: String, methodCount :: Int, instanceCount :: Int )"
  },
  {
    category: "record",
    name: "PackState",
    module: "DataViz.Layout.Hierarchy.Pack",
    kind: "type_synonym",
    sig: "Record ( circles :: Map CircleId Circle, frontChain :: Array CircleId, nextId :: CircleId )"
  },
  {
    category: "record",
    name: "ChordLayout",
    module: "DataViz.Layout.Chord.Types",
    kind: "type_synonym",
    sig: "Record ( groups :: Array ChordGroup, chords :: Array Chord )"
  },

  // ─── 6. OPEN ROW / ROW-POLYMORPHIC ───────────────────────────
  {
    category: "row_poly",
    name: "setPosition",
    module: "Hylograph.Scene.Rules",
    kind: "value",
    sig: "forall r. Record ( x :: Number, y :: Number | r ) -> Record ( x :: Number, y :: Number | r ) -> Number -> Number"
  },
  {
    category: "row_poly",
    name: "D3_ID",
    module: "Hylograph.Kernel.D3.Simulation",
    kind: "type_synonym",
    sig: "( id :: NodeID | row )"
  },
  {
    category: "row_poly",
    name: "MouseEvents",
    module: "Halogen.HTML.Properties",
    kind: "type_synonym",
    sig: "( onDoubleClick :: MouseEvent, onClick :: MouseEvent, onMouseDown :: MouseEvent, onMouseEnter :: MouseEvent, onMouseLeave :: MouseEvent, onMouseMove :: MouseEvent, onMouseOver :: MouseEvent, onMouseOut :: MouseEvent, onMouseUp :: MouseEvent | r )"
  },
  {
    category: "row_poly",
    name: "onValueInput",
    module: "Halogen.HTML.Events",
    kind: "value",
    sig: "forall r i. IProp ( value :: String, onInput :: Event | r ) i -> (i -> String)"
  },
  {
    category: "row_poly",
    name: "SimulationNode",
    module: "Hylograph.ForceEngine.Simulation",
    kind: "type_synonym",
    sig: "Record (D3_ID + D3_XY + D3_VxyFxy + r)"
  },

  // ─── 7. EFFECT / MONADIC ──────────────────────────────────────
  {
    category: "effect",
    name: "scrollToElement",
    module: "Web.DOM",
    kind: "value",
    sig: "Effect Unit -> String"
  },
  {
    category: "effect",
    name: "createWriteStream'",
    module: "Node.FS.Stream",
    kind: "value",
    sig: "forall r trash. Union r trash WriteStreamOptions => Effect (Writable ()) -> Record r -> FilePath"
  },
  {
    category: "effect",
    name: "registerTransition",
    module: "Hylograph.Transition.Engine",
    kind: "value",
    sig: "Effect TransitionId -> TransitionSpec -> String -> Element -> ElementTransitionManager"
  },

  // ─── 8. HIGHER-KINDED ─────────────────────────────────────────
  {
    category: "higher_kinded",
    name: "foldMap",
    module: "Data.Foldable",
    kind: "value",
    sig: "forall a m. Monoid m => m -> f a -> (m -> a)"
  },
  {
    category: "higher_kinded",
    name: "oneOfMap",
    module: "Data.Foldable",
    kind: "value",
    sig: "forall f g a b. Foldable f => Plus g => g b -> f a -> (g b -> a)"
  },
  {
    category: "higher_kinded",
    name: "takeWhile",
    module: "Data.List.Lazy",
    kind: "value",
    sig: "forall f a. Applicative f => ListT f a -> ListT f a -> (Boolean -> a)"
  },

  // ─── 9. NESTED / COMPLEX ──────────────────────────────────────
  {
    category: "complex",
    name: "either",
    module: "Data.Either",
    kind: "value",
    sig: "forall a b c. c -> Either a b -> (c -> b) -> (c -> a)"
  },
  {
    category: "complex",
    name: "getFieldOptional'",
    module: "Data.Argonaut.Decode.Combinators",
    kind: "value",
    sig: "forall a. Either JsonDecodeError (Maybe a) -> String -> Object Json -> (Either JsonDecodeError a -> Json)"
  },
  {
    category: "complex",
    name: "combineRoutes",
    module: "Routing.Duplex",
    kind: "value",
    sig: "RouteDuplex' (Either left right) -> RouteDuplex' right -> RouteDuplex' left"
  },

  // ─── 10. DATA TYPE CONSTRUCTORS ───────────────────────────────
  {
    category: "constructor",
    name: "Nothing",
    module: "Data.Maybe",
    kind: "data",
    parentType: "Maybe a",
    sig: "Maybe a"
  },
  {
    category: "constructor",
    name: "Just",
    module: "Data.Maybe",
    kind: "data",
    parentType: "Maybe a",
    sig: "a -> Maybe a"
  },
  {
    category: "constructor",
    name: "Left",
    module: "Data.Either",
    kind: "data",
    parentType: "Either a b",
    sig: "a -> Either a b"
  },
  {
    category: "constructor",
    name: "Right",
    module: "Data.Either",
    kind: "data",
    parentType: "Either a b",
    sig: "b -> Either a b"
  },

  // ─── 11. TYPE CLASS MEMBERS ───────────────────────────────────
  {
    category: "class_member",
    name: "foldMap",
    module: "Data.Foldable",
    kind: "type_class",
    className: "Foldable f",
    sig: "forall a m. Monoid m => m -> f a -> (m -> a)"
  },
  {
    category: "class_member",
    name: "joinData",
    module: "Hylograph.HATS",
    kind: "type_class",
    className: "TreeDSL tree",
    sig: "forall datum. tree datum -> (tree datum -> datum) -> Array datum -> String -> String"
  },

  // ─── 12. NESTED RECORDS ────────────────────────────────────────
  {
    category: "nested_record",
    name: "BundleLink",
    moduleName: "DataViz.Layout.Hierarchy.EdgeBundle.Types",
    kind: "type_synonym",
    sig: "Record ( source :: String, target :: String, path :: Array RadialNode )"
  },
  {
    category: "nested_record",
    name: "IndexedTransition",
    moduleName: "Hylograph.Transition.Engine",
    kind: "type_synonym",
    sig: "Record ( index :: Int, state :: Record ( from :: Number, to :: Number, progress :: Number ) )"
  },
  {
    category: "nested_record",
    name: "EngineState",
    moduleName: "Hylograph.Scene.Engine",
    kind: "type_synonym",
    sig: "Record ( currentScene :: Maybe String, viewport :: Record ( width :: Number, height :: Number ), camera :: Record ( x :: Number, y :: Number, zoom :: Number ) )"
  },
  {
    category: "nested_record",
    name: "animatedLinkVertical",
    moduleName: "Hylograph.Transition.Animated",
    kind: "value",
    sig: "forall datum. Record ( toSourceX :: datum -> Number, toSourceY :: datum -> Number, toTargetX :: datum -> Number, toTargetY :: datum -> Number ) -> Record ( fromSourceX :: datum -> Number, fromSourceY :: datum -> Number, fromTargetX :: datum -> Number, fromTargetY :: datum -> Number ) -> datum -> String"
  },
  {
    category: "nested_record",
    name: "lookupGT",
    moduleName: "Data.Map",
    kind: "value",
    sig: "forall k v. Ord k => k -> Map k v -> Maybe (Record ( key :: k, value :: v ))"
  },

  // ─── 13. LARGE RECORD (stress test) ──────────────────────────
  {
    category: "large_record",
    name: "ResourceUsage",
    module: "Node.Process",
    kind: "type_synonym",
    sig: "Record ( userCPUTime :: Int, systemCPUTime :: Int, maxRSS :: Int, sharedMemorySize :: Int, unsharedDataSize :: Int, unsharedStackSize :: Int, minorPageFault :: Int, majorPageFault :: Int, swappedOut :: Int, fsRead :: Int, fsWrite :: Int, ipcSent :: Int, ipcReceived :: Int, signalsCount :: Int, voluntaryContextSwitches :: Int, involuntaryContextSwitches :: Int )"
  },
  {
    category: "large_record",
    name: "HandleConfig",
    module: "Hylograph.Scene.Handle",
    kind: "type_synonym",
    sig: "Record ( containerSelector :: String, nodesGroupId :: GroupId, linksGroupId :: Maybe GroupId )"
  },

  // ─── 14. DATA TYPES (ADTs) ──────────────────────────────────
  {
    category: "adt",
    name: "Maybe",
    module: "Data.Maybe",
    kind: "data",
    typeParams: ["a"],
    constructors: [
      { name: "Nothing", args: [] },
      { name: "Just", args: ["a"] }
    ]
  },
  {
    category: "adt",
    name: "Either",
    module: "Data.Either",
    kind: "data",
    typeParams: ["a", "b"],
    constructors: [
      { name: "Left", args: ["a"] },
      { name: "Right", args: ["b"] }
    ]
  },
  {
    category: "adt",
    name: "Ordering",
    module: "Data.Ordering",
    kind: "data",
    typeParams: [],
    constructors: [
      { name: "LT", args: [] },
      { name: "EQ", args: [] },
      { name: "GT", args: [] }
    ]
  },
  {
    category: "adt",
    name: "These",
    module: "Data.These",
    kind: "data",
    typeParams: ["a", "b"],
    constructors: [
      { name: "This", args: ["a"] },
      { name: "That", args: ["b"] },
      { name: "Both", args: ["a", "b"] }
    ]
  },
  {
    category: "adt",
    name: "Step",
    module: "Control.Monad.Rec.Class",
    kind: "data",
    typeParams: ["a", "b"],
    constructors: [
      { name: "Loop", args: ["a"] },
      { name: "Done", args: ["b"] }
    ]
  },
  {
    category: "adt",
    name: "List",
    module: "Data.List.Types",
    kind: "data",
    typeParams: ["a"],
    constructors: [
      { name: "Nil", args: [] },
      { name: "Cons", args: ["a", "List a"] }
    ]
  },
  {
    category: "adt",
    name: "NonEmpty",
    module: "Data.NonEmpty",
    kind: "data",
    typeParams: ["f", "a"],
    constructors: [
      { name: "NonEmpty", args: ["a", "f a"] }
    ]
  },
  {
    category: "adt",
    name: "AppPhase",
    module: "CE2.Component.AppShell",
    kind: "data",
    typeParams: [],
    constructors: [
      { name: "Loading", args: [] },
      { name: "Ready", args: [] },
      { name: "Error", args: ["String"] }
    ]
  },
  {
    category: "adt",
    name: "ScaleLevel",
    module: "CE2.Types",
    kind: "data",
    typeParams: [],
    constructors: [
      { name: "RegistryTimelineScale", args: [] },
      { name: "PackageSetScale", args: [] },
      { name: "ProjectDepsScale", args: [] },
      { name: "ProjectDepsTreemapScale", args: [] },
      { name: "ProjectOnlyScale", args: [] },
      { name: "ModuleSubtreeScale", args: [] },
      { name: "WithinModuleScale", args: [] }
    ]
  },
  {
    category: "adt",
    name: "AttributeValue",
    module: "Hylograph.Internal.Attribute",
    kind: "data",
    typeParams: [],
    constructors: [
      { name: "StringValue", args: ["String"] },
      { name: "NumberValue", args: ["Number"] },
      { name: "BooleanValue", args: ["Boolean"] }
    ]
  },
  {
    category: "adt",
    name: "Alignment",
    module: "DataViz.Layout.Sankey.Types",
    kind: "data",
    typeParams: [],
    constructors: [
      { name: "Justify", args: [] },
      { name: "Left", args: [] },
      { name: "Right", args: [] },
      { name: "Center", args: [] }
    ]
  },

  // ─── 15. RANK-N TYPES ──────────────────────────────────────
  {
    category: "rank_n",
    name: "runST",
    module: "Control.Monad.ST",
    kind: "value",
    sig: "forall a. (forall s. ST s a) -> a"
  },
  {
    category: "rank_n",
    name: "runExists",
    module: "Data.Exists",
    kind: "value",
    sig: "forall f r. (forall a. f a -> r) -> Exists f -> r"
  },
  {
    category: "rank_n",
    name: "hoistFree",
    module: "Control.Monad.Free",
    kind: "value",
    sig: "forall f g a. (forall b. f b -> g b) -> Free f a -> Free g a"
  },

  // ─── 16. KIND SIGNATURES ───────────────────────────────────
  {
    category: "kind_sig",
    name: "Proxy",
    module: "Type.Proxy",
    kind: "data",
    sig: "forall k. k -> Type"
  },
  {
    category: "kind_sig",
    name: "SProxy",
    module: "Type.Data.Symbol",
    kind: "foreign",
    sig: "Symbol -> Type"
  },
  {
    category: "kind_sig",
    name: "TypeEquals",
    module: "Type.Equality",
    kind: "class",
    sig: "Type -> Type -> Constraint"
  },

  // ─── 17. CLASS DEFINITIONS ─────────────────────────────────
  {
    category: "class_def",
    name: "Functor",
    module: "Data.Functor",
    kind: "class",
    typeParams: ["f"],
    superclasses: [],
    methods: [
      { name: "map", sig: "forall a b. (a -> b) -> f a -> f b" }
    ]
  },
  {
    category: "class_def",
    name: "Foldable",
    module: "Data.Foldable",
    kind: "class",
    typeParams: ["f"],
    superclasses: [],
    methods: [
      { name: "foldr", sig: "forall a b. (a -> b -> b) -> b -> f a -> b" },
      { name: "foldl", sig: "forall a b. (b -> a -> b) -> b -> f a -> b" },
      { name: "foldMap", sig: "forall a m. Monoid m => (a -> m) -> f a -> m" }
    ]
  },
  {
    category: "class_def",
    name: "Show",
    module: "Data.Show",
    kind: "class",
    typeParams: ["a"],
    superclasses: [],
    methods: [
      { name: "show", sig: "a -> String" }
    ]
  },
  {
    category: "class_def",
    name: "Apply",
    module: "Control.Apply",
    kind: "class",
    typeParams: ["f"],
    superclasses: ["Functor f"],
    methods: [
      { name: "apply", sig: "forall a b. f (a -> b) -> f a -> f b" }
    ]
  },
  {
    category: "class_def",
    name: "Bifunctor",
    module: "Data.Bifunctor",
    kind: "class",
    typeParams: ["f"],
    superclasses: [],
    methods: [
      { name: "bimap", sig: "forall a b c d. (a -> b) -> (c -> d) -> f a c -> f b d" }
    ]
  },
];

// Category metadata
const CATEGORIES = {
  simple:           { label: "Simple Values",        color: "#4e79a7" },
  pure_function:    { label: "Pure Functions",        color: "#4e79a7" },
  constrained:      { label: "Constrained",           color: "#f28e2b" },
  multi_constraint: { label: "Multi-Constraint",      color: "#e15759" },
  record:           { label: "Closed Records",        color: "#59a14f" },
  row_poly:         { label: "Row-Polymorphic",       color: "#76b7b2" },
  effect:           { label: "Effect / Monadic",      color: "#9333ea" },
  higher_kinded:    { label: "Higher-Kinded",         color: "#3b82f6" },
  complex:          { label: "Nested / Complex",      color: "#e15759" },
  constructor:      { label: "Constructors",          color: "#59a14f" },
  class_member:     { label: "Class Members",         color: "#f28e2b" },
  nested_record:    { label: "Nested Records",         color: "#2d6a4f" },
  large_record:     { label: "Large Records",         color: "#222" },
  adt:              { label: "Data Types (ADTs)",     color: "#b45309" },
  rank_n:           { label: "Rank-N Types",          color: "#7c3aed" },
  kind_sig:         { label: "Kind Signatures",       color: "#475569" },
  class_def:        { label: "Class Definitions",     color: "#4338ca" },
};
