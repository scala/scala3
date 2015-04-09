package dotty.tools.dotc.config

object Config {

  final val cacheMembersNamed = true
  final val cacheAsSeenFrom = true
  final val useFingerPrints = true
  final val cacheMemberNames = true
  final val cacheImplicitScopes = true

  final val checkCacheMembersNamed = false

  /** When updating a connstraint bound, check that the constrained parameter
   *  does not appear at the top-level of either of its bounds.
   */
  final val checkConstraintsNonCyclic = false

  /** Like `checkConstraintsNonCyclic`, but all constrained parameters
   *  are tested for direct or indirect dependencies, each time a
   *  constraint is added in TypeComparer.
   */
  final val checkConstraintsNonCyclicTrans = false

  /** Check that each constraint resulting from a subtype test
   *  is satisfiable.
   */
  final val checkConstraintsSatisfiable = false

  /** Check that each constraint is fully propagated. i.e.
   *  If P <: Q then the upper bound of P is a subtype of the upper bound of Q
   *  and the lower bound of Q is a subtype of the lower bound of P.
   */
  final val checkConstraintsPropagated = false

  /** Type comparer will fail with an assert if the upper bound
   *  of a constrained parameter becomes Nothing. This should be turned
   *  on only for specific debugging as normally instantiation to Nothing
   *  is not an error consdition.
   */
  final val failOnInstantiationToNothing = false

  /** Enable noDoubleDef checking if option "-YnoDoubleDefs" is set.
    * The reason to have an option as well as the present global switch is
    * that the noDoubleDef checking is done in a hotspot, and we do not
    * want to incur the overhead of checking an option each time.
    */
  final val checkNoDoubleBindings = true

  /** Show subtype traces for all deep subtype recursions */
  final val traceDeepSubTypeRecursions = false

  /** When explaining subtypes and this flag is set, also show the classes of the compared types. */
  final val verboseExplainSubtype = true

  /** If this flag is set, take the fast path when comparing same-named type-aliases and types */
  final val fastPathForRefinedSubtype = true

  /** When set, use new signature-based matching.
   *  Advantantage of doing so: It's supposed to be faster
   *  Disadvantage: It might hide inconsistencies, so while debugging it's better to turn it off
   */
  final val newMatch = false

  /** The recursion depth for showing a summarized string */
  final val summarizeDepth = 2

  /** Check that variances of lambda arguments match the
   *  variance of the underlying lambda class.
   */
  final val checkLambdaVariance = false

  /** Check that certain types cannot be created in erasedTypes phases */
  final val checkUnerased = true
}
