-- | Curated collection of PureScript type signatures for visual testing.
-- | Ported from type-sig-viz/public/signatures.js (17 categories).
module CE2.SigTest.Signatures where

type TestSig =
  { idx :: Int
  , category :: String
  , name :: String
  , sig :: String
  }

signatures :: Array TestSig
signatures =
  -- 1. Simple Values
  [ { idx: 1,  category: "simple",           name: "maxHeaderSize",     sig: "Int" }
  , { idx: 2,  category: "simple",           name: "pid",              sig: "Pid" }

  -- 2. Pure Functions
  , { idx: 3,  category: "pure_function",    name: "head",             sig: "forall a. NonEmptyArray a -> a" }
  , { idx: 4,  category: "pure_function",    name: "decayAlpha",       sig: "Number -> Number -> Number -> Number -> Number" }
  , { idx: 5,  category: "pure_function",    name: "dropWhile",        sig: "String -> String -> (Boolean -> Char)" }

  -- 3. Constrained Functions
  , { idx: 6,  category: "constrained",      name: "sort",             sig: "forall a. Ord a => Array a -> Array a" }
  , { idx: 7,  category: "constrained",      name: "fromFoldable",     sig: "forall f k v. Ord k => Foldable f => Map k v -> f (Tuple k v)" }
  , { idx: 8,  category: "constrained",      name: "evalStateT",       sig: "forall s m a. Functor m => m a -> s -> StateT s m a" }
  , { idx: 9,  category: "constrained",      name: "experiment",       sig: "forall f a w s. ComonadStore s w => Functor f => f a -> w a -> (f s -> s)" }

  -- 4. Multi-Constraint
  , { idx: 10, category: "multi_constraint", name: "foldM",            sig: "forall f m a b. Foldable f => Monad m => m b -> f a -> b -> (m b -> a -> b)" }
  , { idx: 11, category: "multi_constraint", name: "tailRecM3",        sig: "forall m a b c d. MonadRec m => m d -> c -> b -> a -> (m (Step Record ( a :: a, b :: b, c :: c ) d) -> c -> b -> a)" }
  , { idx: 12, category: "multi_constraint", name: "biall",            sig: "forall t a b c. Bifoldable t => BooleanAlgebra c => c -> t a b -> (c -> b) -> (c -> a)" }

  -- 5. Closed Records
  , { idx: 13, category: "record",           name: "BrushExtent",      sig: "Record ( x0 :: Number, y0 :: Number, x1 :: Number, y1 :: Number )" }
  , { idx: 14, category: "record",           name: "TypeClassInfo",    sig: "Record ( id :: Int, name :: String, moduleName :: String, packageName :: String, methodCount :: Int, instanceCount :: Int )" }
  , { idx: 15, category: "record",           name: "PackState",        sig: "Record ( circles :: Map CircleId Circle, frontChain :: Array CircleId, nextId :: CircleId )" }
  , { idx: 16, category: "record",           name: "ChordLayout",     sig: "Record ( groups :: Array ChordGroup, chords :: Array Chord )" }

  -- 6. Open Row / Row-Polymorphic
  , { idx: 17, category: "row_poly",         name: "setPosition",      sig: "forall r. Record ( x :: Number, y :: Number | r ) -> Record ( x :: Number, y :: Number | r ) -> Number -> Number" }
  , { idx: 18, category: "row_poly",         name: "onValueInput",     sig: "forall r i. IProp ( value :: String, onInput :: Event | r ) i -> (i -> String)" }

  -- 7. Effect / Monadic
  , { idx: 19, category: "effect",           name: "scrollToElement",  sig: "Effect Unit -> String" }
  , { idx: 20, category: "effect",           name: "createWriteStream'", sig: "forall r trash. Union r trash WriteStreamOptions => Effect (Writable ()) -> Record r -> FilePath" }
  , { idx: 21, category: "effect",           name: "registerTransition", sig: "Effect TransitionId -> TransitionSpec -> String -> Element -> ElementTransitionManager" }

  -- 8. Higher-Kinded
  , { idx: 22, category: "higher_kinded",    name: "foldMap",          sig: "forall a m. Monoid m => m -> f a -> (m -> a)" }
  , { idx: 23, category: "higher_kinded",    name: "oneOfMap",         sig: "forall f g a b. Foldable f => Plus g => g b -> f a -> (g b -> a)" }
  , { idx: 24, category: "higher_kinded",    name: "takeWhile",        sig: "forall f a. Applicative f => ListT f a -> ListT f a -> (Boolean -> a)" }

  -- 9. Nested / Complex
  , { idx: 25, category: "complex",          name: "either",           sig: "forall a b c. c -> Either a b -> (c -> b) -> (c -> a)" }
  , { idx: 26, category: "complex",          name: "getFieldOptional'", sig: "forall a. Either JsonDecodeError (Maybe a) -> String -> Object Json -> (Either JsonDecodeError a -> Json)" }
  , { idx: 27, category: "complex",          name: "combineRoutes",    sig: "RouteDuplex' (Either left right) -> RouteDuplex' right -> RouteDuplex' left" }

  -- 10. Data Type Constructors
  , { idx: 28, category: "constructor",      name: "Nothing",          sig: "Maybe a" }
  , { idx: 29, category: "constructor",      name: "Just",             sig: "a -> Maybe a" }
  , { idx: 30, category: "constructor",      name: "Left",             sig: "a -> Either a b" }
  , { idx: 31, category: "constructor",      name: "Right",            sig: "b -> Either a b" }

  -- 11. Rank-N Types
  , { idx: 32, category: "rank_n",           name: "runST",            sig: "forall a. (forall s. ST s a) -> a" }
  , { idx: 33, category: "rank_n",           name: "runExists",        sig: "forall f r. (forall a. f a -> r) -> Exists f -> r" }
  , { idx: 34, category: "rank_n",           name: "hoistFree",        sig: "forall f g a. (forall b. f b -> g b) -> Free f a -> Free g a" }

  -- 12. Nested Records
  , { idx: 35, category: "nested_record",    name: "BundleLink",       sig: "Record ( source :: String, target :: String, path :: Array RadialNode )" }
  , { idx: 36, category: "nested_record",    name: "IndexedTransition", sig: "Record ( index :: Int, state :: Record ( from :: Number, to :: Number, progress :: Number ) )" }
  , { idx: 37, category: "nested_record",    name: "EngineState",      sig: "Record ( currentScene :: Maybe String, viewport :: Record ( width :: Number, height :: Number ), camera :: Record ( x :: Number, y :: Number, zoom :: Number ) )" }
  , { idx: 38, category: "nested_record",    name: "animatedLinkVertical", sig: "forall datum. Record ( toSourceX :: datum -> Number, toSourceY :: datum -> Number, toTargetX :: datum -> Number, toTargetY :: datum -> Number ) -> Record ( fromSourceX :: datum -> Number, fromSourceY :: datum -> Number, fromTargetX :: datum -> Number, fromTargetY :: datum -> Number ) -> datum -> String" }
  , { idx: 39, category: "nested_record",    name: "lookupGT",         sig: "forall k v. Ord k => k -> Map k v -> Maybe (Record ( key :: k, value :: v ))" }

  -- 13. Large Record (stress test)
  , { idx: 40, category: "large_record",     name: "ResourceUsage",    sig: "Record ( userCPUTime :: Int, systemCPUTime :: Int, maxRSS :: Int, sharedMemorySize :: Int, unsharedDataSize :: Int, unsharedStackSize :: Int, minorPageFault :: Int, majorPageFault :: Int, swappedOut :: Int, fsRead :: Int, fsWrite :: Int, ipcSent :: Int, ipcReceived :: Int, signalsCount :: Int, voluntaryContextSwitches :: Int, involuntaryContextSwitches :: Int )" }
  , { idx: 41, category: "small_record",     name: "HandleConfig",     sig: "Record ( containerSelector :: String, nodesGroupId :: GroupId, linksGroupId :: Maybe GroupId )" }

  -- 14. Bare Open Rows
  , { idx: 42, category: "bare_row",         name: "D3_ID",            sig: "( id :: NodeID | row )" }
  , { idx: 43, category: "bare_row",         name: "MouseEvents",      sig: "( onDoubleClick :: MouseEvent, onClick :: MouseEvent, onMouseDown :: MouseEvent, onMouseEnter :: MouseEvent, onMouseLeave :: MouseEvent, onMouseMove :: MouseEvent, onMouseOver :: MouseEvent, onMouseOut :: MouseEvent, onMouseUp :: MouseEvent | r )" }

  -- 15. Row Combination (+ operator)
  , { idx: 44, category: "row_combination",  name: "SimulationNode",   sig: "Record (D3_ID + D3_XY + D3_VxyFxy + r)" }

  -- 16. Kind Signatures
  , { idx: 45, category: "kind_sig",         name: "Proxy",            sig: "forall k. k -> Type" }
  , { idx: 46, category: "kind_sig",         name: "SProxy",           sig: "Symbol -> Type" }
  , { idx: 47, category: "kind_sig",         name: "TypeEquals",       sig: "Type -> Type -> Constraint" }

  -- 17. Halogen / Complex Effect
  , { idx: 48, category: "halogen",          name: "handleAction",     sig: "forall m. MonadAff m => Action -> HalogenM State Action () Output m Unit" }
  , { idx: 49, category: "halogen",          name: "component",        sig: "forall q m. MonadAff m => Component q Input Output m" }
  , { idx: 50, category: "halogen",          name: "mkEval",           sig: "forall state query action slots input output m. EvalSpec state query action slots input output m -> HalogenQ query action input -> HalogenM state action slots output m Unit" }
  ]
