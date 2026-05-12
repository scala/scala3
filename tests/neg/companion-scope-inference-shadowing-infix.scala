// Shadowing counterpart of `tests/pos/companion-inference-infix.scala`.
// In infix-RHS position the expected type is the operator's parameter
// type. If a local of wrong type shadows the companion member, normal
// resolution wins and the operator's argument has the wrong type.
import scala.language.experimental.companionScopeInference

object Infix:

  sealed trait Color
  object Color:
    case object Red  extends Color
    case object Blue extends Color

  extension (c: Color)
    infix def matchesColor(other: Color): Boolean = c == other

  val c: Color = Color.Red

  // Local with the same name as Color.Red.
  val Red = "the letter R"

  val eq: Boolean = c matchesColor Red // error
