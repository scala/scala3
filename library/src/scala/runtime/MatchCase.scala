package scala.runtime

import language.experimental.captureChecking

/** A type constructor for a case in a match type. */
final abstract class MatchCase[Pat, +Body]
