import scala.language.experimental.targetTypedCompanionShorthand

// SIP-80 recommends resolving `.X` after overload selection; the initial
// implementation requires a single-candidate target type. Overloaded calls
// without disambiguation report "expected type ... cannot be determined".
object Overload:

  sealed trait Color
  object Color:
    case object Red extends Color

  sealed trait Direction
  object Direction:
    case object North extends Direction

  def f(c: Color): String     = ""
  def f(d: Direction): String = ""

  val bad: String = f(.Red) // error
