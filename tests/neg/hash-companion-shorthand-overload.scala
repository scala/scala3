import scala.language.experimental.hashCompanionShorthand

// SIP-80 v1: same-arity overloads with disjoint formals at the `#X` position
// are not auto-disambiguated. The user supplies an ascription or a wrapper.
object OverloadNeg:

  sealed trait Color
  object Color:
    case object Red extends Color

  sealed trait Direction
  object Direction:
    case object North extends Direction

  def f(c: Color): String     = ""
  def f(d: Direction): String = ""

  val bad: String = f(#Red) // error
