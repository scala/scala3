package dotty.tools.dotc.config

object Config {

  final val cacheMembersNamed = true
  final val cacheAsSeenFrom = true
  final val cacheMemberNames = true
  final val cacheImplicitScopes = true
  final val cacheMatchReduced = true

  final val checkCacheMembersNamed = false

  /** When updating a constraint bound, check that the constrained parameter
   *  does not appear at the top-level of either of its bounds.
   */
  final val checkConstraintsNonCyclic = false

  /** Make sure none of the bounds of a parameter in an OrderingConstraint
   *  contains this parameter at its toplevel (i.e. as an operand of a
   *  combination of &'s and |'s.). The check is performed each time a new bound
   *  is added to the constraint.
   */
  final val checkConstraintsSeparated = false

  /** Check that each constraint resulting from a subtype test
   *  is satisfiable.
   */
  final val checkConstraintsSatisfiable = false

  /** Check that each constraint is fully propagated. i.e.
   *  If P <: Q then the upper bound of P is a subtype of the upper bound of Q
   *  and the lower bound of Q is a subtype of the lower bound of P.
   */
  final val checkConstraintsPropagated = false

  /** Check that constraints of globally committable typer states are closed.
   *  NOTE: When enabled, the check can cause CyclicReference errors because
   *  it traverses all elements of a type. Such failures were observed when
   *  compiling all of dotty together (source seems to be in GenBCode which
   *  accesses javac's settings.)
   *
   *  It is recommended to turn this option on only when chasing down
   *  a TypeParamRef instantiation error. See comment in Types.TypeVar.instantiate.
   */
  final val debugCheckConstraintsClosed = false

  /** Check that no type appearing as the info of a SymDenotation contains
   *  skolem types.
   */
  final val checkNoSkolemsInInfo = false

  /** Check that Name#toString is not called directly from backend by analyzing
   *  the stack trace of each toString call on names. This is very expensive,
   *  so not suitable for continuous testing. But it can be used to find a problem
   *  when running a specific test.
   */
  final val checkBackendNames = false

  /** Type comparer will fail with an assert if the upper bound
   *  of a constrained parameter becomes Nothing. This should be turned
   *  on only for specific debugging as normally instantiation to Nothing
   *  is not an error condition.
   */
  final val failOnInstantiationToNothing = false

  /** Enable noDoubleDef checking if option "-YnoDoubleDefs" is set.
    * The reason to have an option as well as the present global switch is
    * that the noDoubleDef checking is done in a hotspot, and we do not
    * want to incur the overhead of checking an option each time.
    */
  final val checkNoDoubleBindings = true

  /** Check positions for consistency after parsing */
  final val checkPositions = true

  /** Check that typed trees don't point to untyped ones */
  final val checkTreesConsistent = false

  /** Show subtype traces for all deep subtype recursions */
  final val traceDeepSubTypeRecursions = false

  /** When explaining subtypes and this flag is set, also show the classes of the compared types. */
  final val verboseExplainSubtype = false

  /** If this flag is set, take the fast path when comparing same-named type-aliases and types */
  final val fastPathForRefinedSubtype = true

  /** If this flag is set, and we compute `T1[X1]` & `T2[X2]` as a new
   *  upper bound of a constrained parameter, try to align the arguments by computing
   *  `S1 =:= S2` (which might instantiate type parameters).
   *  This rule is contentious because it cuts the constraint set.
   *
   *  For more info, see the comment in `TypeComparer#glbArgs`.
   */
  final val alignArgsInAnd = true

  /** If this flag is set, higher-kinded applications are checked for validity
   */
  final val checkHKApplications = false

  /** If this flag is set, method types are checked for valid parameter references
   */
  final val checkMethodTypes = false

  /** If this flag is set, it is checked that TypeRefs don't refer directly
   *  to themselves.
   */
  final val checkTypeRefCycles = false

  /** If this flag is set, we check that types assigned to trees are error types only
   *  if some error was already reported. There are complicicated scenarios where this
   *  is not true. An example is TestNonCyclic in posTwice. If we remove the
   *  first (unused) import `import dotty.tools.dotc.core.Types.Type` in `CompilationUnit`,
   *  we end up assigning a CyclicReference error type to an import expression `annotation`
   *  before the cyclic reference is reported. What happens is that the error was reported
   *  as a result of a completion in a not-yet committed typerstate. So we cannot enforce
   *  this in all circumstances. But since it is almost always true it is useful to
   *  keep the Config option for debugging.
   */
  final val checkUnreportedErrors = false

  /** If this flag is set, it is checked that class type parameters are
   *  only references with NoPrefix or ThisTypes as prefixes. This option
   *  is usually disabled, because there are still some legitimate cases where
   *  this can arise (e.g. for pos/Map.scala, in LambdaType.integrate).
   */
  final val checkTypeParamRefs = false

  /** The recursion depth for showing a summarized string */
  final val summarizeDepth = 2

  /** Check that variances of lambda arguments match the
   *  variance of the underlying lambda class.
   */
  final val checkLambdaVariance = false

  /** Check that certain types cannot be created in erasedTypes phases.
   *  Note: Turning this option on will get some false negatives, since it is
   *  possible that And/Or types are still created during erasure as the result
   *  of some operation on an existing type.
   */
  final val checkUnerased = false

  /** In `derivedSelect`, rewrite
   *
   *      (S & T)#A  -->  S#A & T#A
   *      (S | T)#A  -->  S#A | T#A
   *
   *  Not sure whether this is useful. Preliminary measurements show a slowdown of about
   *  7% for the build when this option is enabled.
   */
  final val splitProjections = false

  /** If this flag is on, always rewrite an application `S[Ts]` where `S` is an alias for
   *  `[Xs] -> U` to `[Xs := Ts]U`.
   *  Turning this flag on was observed to give a ~6% speedup on the JUnit test suite.
   */
  final val simplifyApplications = true

  /** If set, prints a trace of all symbol completions */
  final val showCompletions = false

  /** If set, enables tracing */
  final val tracingEnabled = true

  /** Initial capacity of uniques HashMap.
   *  Note: This MUST BE a power of two to work with util.HashSet
   */
  final val initialUniquesCapacity = 65536

  /** How many recursive calls to NamedType#underlying are performed before logging starts. */
  final val LogPendingUnderlyingThreshold = 50

  /** How many recursive calls to isSubType are performed before logging starts. */
  final val LogPendingSubTypesThreshold = 50

  /** How many recursive calls to findMember are performed before logging names starts
   *  Note: this threshold has to be chosen carefully. Too large, and programs
   *  like tests/pos/IterableSelfRec go into polynomial (or even exponential?)
   *  compile time slowdown. Too small and normal programs will cause the compiler  to
   *  do inefficient operations on findMember. The current value is determined
   *  so that (1) IterableSelfRec still compiles in reasonable time (< 10sec) (2) Compiling
   *  dotty itself only causes small pending names lists to be generated (we measured
   *  at max 6 elements) and these lists are never searched with contains.
   */
  final val LogPendingFindMemberThreshold = 9

  /** When in IDE, turn StaleSymbol errors into warnings instead of crashing */
  final val ignoreStaleInIDE = true
}
