package scala.runtime

import language.experimental.captureChecking

/** A type constructor for a case in a match type.
 *
 *  @tparam Pat the pattern type to match against the scrutinee of the match type
 *  @tparam Body the result type produced when the scrutinee matches `Pat`
 */
final abstract class MatchCase[Pat, +Body]
