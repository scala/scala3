// SIP-80 companion-inference: when overload arity narrows to a single
// candidate, the bare identifier resolves against that candidate's formal
// parameter type.
import scala.language.experimental.companionScopeInference

object Arity:

  sealed trait Color
  object Color:
    case object Red  extends Color
    case object Blue extends Color

  def foo(arg: Color): String              = "1: " + arg
  def foo(arg: Color, arg2: Int): String   = "2: " + arg + "," + arg2

  // 1 arg arity → first overload wins; `Red` typed with pt=Color.
  val r1: String = foo(Red)
  // 2 args arity → second overload wins; `Red` typed with pt=Color.
  val r2: String = foo(Red, 5)
