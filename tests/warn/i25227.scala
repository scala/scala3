//> using options -Wunused:all

import scala.language.strictEquality

object Givens:
  given CanEqual[Any, Null] = CanEqual.derived
  given CanEqual[Null, Any] = CanEqual.derived

object Test:
  import Givens.given  // no warn: used by `case null =>`

  def m(value: Any): Boolean = value match
    case null => true
    case _ => false

object TestUnused:
  import Givens.given  // warn: not used here

  def m(value: Any): Boolean = value match
    case _: String => true
    case _ => false
