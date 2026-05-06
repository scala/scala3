import scala.language.experimental.targetTypedCompanionShorthand

// Overload disambiguation requires a known target type for `.X`. The SIP
// recommends resolving relative selections after overload selection. This
// initial implementation requires the user to disambiguate explicitly when
// the call is overloaded — either by ascribing the argument or by preferring
// a non-overloaded helper. The cases below exercise the supported variants.
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

  // Disambiguate via ascription — `.Red` is parsed in a target-typed position.
  val r1: String = f((.Red: Color))
  val r2: String = f((.North: Direction))

  // Or via a non-overloaded wrapper; relative scoping then sees a clean pt.
  def fc(c: Color): String     = f(c)
  def fd(d: Direction): String = f(d)
  val r3: String = fc(.Red)
  val r4: String = fd(.North)
