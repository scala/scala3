package dotty.tools.dotc.config

object Config {

  final val cacheMembersNamed = true
  final val cacheAsSeenFrom = true
  final val useFingerPrints = true
  final val cacheMemberNames = true
  final val cacheImplicitScopes = true

  final val checkCacheMembersNamed = false

  final val checkConstraintsNonCyclic = true

  final val flagInstantiationToNothing = false

  /** Throw an exception if a deep subtype recursion is detected */
  final val flagDeepSubTypeRecursions = true

  /** Show subtype traces for all deep subtype recursions */
  final val traceDeepSubTypeRecursions = false

  final val verboseExplainSubtype = true

  /** When set, use new signature-based matching.
   *  Advantantage of doing so: It's supposed to be faster
   *  Disadvantage: It might hide inconsistencies, so while debugging it's better to turn it off
   */
  final val newMatch = false
}