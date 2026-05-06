// SIP-80: when overload arity narrows to a single candidate, `.X` can be
// resolved against that candidate's formal parameter type. This is the
// arity-disambiguation path — no special overload logic needed beyond what
// the typer already does.
import scala.language.experimental.targetTypedCompanionShorthand

sealed trait Color
object Color:
  case object Red  extends Color
  case object Blue extends Color

object Arity:
  def foo(arg: Color): String          = "1: " + arg
  def foo(arg: Color, arg2: Int): String = "2: " + arg + "," + arg2

  // 1 arg arity → first overload wins; .Red typed with pt=Color → Color.Red.
  val r1: String = foo(.Red)
  // 2 args arity → second overload wins; .Red typed with pt=Color → Color.Red.
  val r2: String = foo(.Red, 5)
