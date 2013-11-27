package dotty.tools.dotc.config

object Config {

  final val cacheMembersNamed = true
  final val cacheAsSeenFrom = true
  final val useFingerPrints = true
  final val cacheMemberNames = true

  /** When set, use new signature-based matching.
   *  Advantantage of doing so: It's supposed to be faster
   *  Disadvantage: It might hide inconsistencies, so while debugging it's better to turn it off
   */
  final val newMatch = false
}