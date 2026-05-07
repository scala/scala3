import scala.language.experimental.hashCompanionShorthand

// Same-arity overloads with disjoint companion members require explicit
// disambiguation in v1 (the SIP defers smarter resolution to a follow-up).
object Overload:

  sealed trait Color
  object Color:
    case object Red  extends Color
    case object Blue extends Color

  sealed trait Direction
  object Direction:
    case object North extends Direction
    case object South extends Direction

  def f(c: Color): String     = "color: " + c
  def f(d: Direction): String = "dir: " + d
  def f(i: Int): String       = "int: " + i

  // Disambiguate via ascription — `#Red` is in a target-typed position.
  val r1: String = f((#Red: Color))
  val r2: String = f((#North: Direction))

  // Or via a non-overloaded wrapper; `#X` then sees a clean expected type.
  def fc(c: Color): String     = f(c)
  def fd(d: Direction): String = f(d)
  val r3: String = fc(#Red)
  val r4: String = fd(#North)
