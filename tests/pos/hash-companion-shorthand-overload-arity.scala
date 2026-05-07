// SIP-80: when overload arity narrows to a single candidate, `#X` resolves
// against that candidate's formal parameter type. This is the
// arity-disambiguation path — narrowBySize picks the unique candidate before
// SIP-80 logic runs.
import scala.language.experimental.hashCompanionShorthand

object Arity:

  sealed trait Color
  object Color:
    case object Red  extends Color
    case object Blue extends Color

  def foo(arg: Color): String              = "1: " + arg
  def foo(arg: Color, arg2: Int): String   = "2: " + arg + "," + arg2

  // 1 arg arity → first overload wins; #Red typed with pt=Color.
  val r1: String = foo(#Red)
  // 2 args arity → second overload wins; #Red typed with pt=Color.
  val r2: String = foo(#Red, 5)
