// Under unsafe nulls, a value of type `Null` must not be treated as known to be non-null.
// If it were, `.nn` on it would be reported as unnecessary, and a type test against a
// reference type would be reported as matching everything rather than everything but null.
// Both diagnostics are compile-time symptoms of the same defect that
// `tests/explicit-nulls/run/unsafe-nulls-null-scrutinee.scala` observes at run time.

import scala.language.unsafeNulls

object Test:
  val n: Null = null

  val s = n.nn // no warning: the qualifier really can be null

  def typeTest = null match
    case _: AnyRef => 1
    case _ => -1 // warn
