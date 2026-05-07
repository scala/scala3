// SIP-80 §Overload resolution: overloads that differ only by an extra
// default-valued parameter (a common binary-compatibility evolution pattern)
// resolve `#X` against the parameter type they share.
//
// Both overloads can be called with one argument (the second has a default
// for `b`). `narrowBySize` keeps both, but their first parameter type is
// the same, so SIP-80's `pretypeArgs` extension pre-types `#Cat` with that
// common formal — `narrowMostSpecific` then picks the no-default overload.
import scala.language.experimental.hashCompanionShorthand

sealed trait Animal
object Animal:
  case object Cat extends Animal
  case object Dog extends Animal

object SameFirstFormal:
  def bar(a: Animal): Int                         = 1
  def bar(a: Animal, b: Animal = Animal.Dog): Int = 2

  // Both overloads applicable by arity. Shared formal at position 0 is
  // `Animal`, so `#Cat` resolves there.
  val r: Int = bar(#Cat)

  // Same shape with a slightly different default expression.
  def baz(a: Animal): String                = a.toString
  def baz(a: Animal, sep: String = "-"): String = a.toString
  val s: String = baz(#Dog)

  // Common formal at position 0 with a shorthand followed by an explicit
  // second arg should also work — the 2-arg overload is selected.
  val r2: Int = bar(#Cat, Animal.Dog)
