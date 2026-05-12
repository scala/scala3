// Shadowing counterpart of `tests/pos/companion-inference-varargs.scala`.
// A local of wrong type shadows the companion member in a vararg call;
// normal resolution wins and the type mismatch surfaces at the call.
import scala.language.experimental.companionScopeInference

sealed trait Color
object Color:
  case object Red   extends Color
  case object Blue  extends Color

object Vararg:
  def palette(colors: Color*): Int = colors.size

  // Local with the same name as a companion case object.
  val Red = "the letter R"

  val n: Int = palette(Red, Blue) // error
