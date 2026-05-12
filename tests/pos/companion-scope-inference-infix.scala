// Because there is no sigil, `c mix Red` parses cleanly (it always did).
// Companion inference resolves `Red` against the RHS's expected type.
import scala.language.experimental.companionScopeInference

object Infix:

  sealed trait Color
  object Color:
    case object Red  extends Color
    case object Blue extends Color

  extension (c: Color)
    infix def matchesColor(other: Color): Boolean = c == other
    infix def or(other: Color): Color = c
    def |+|(other: Color): Color      = other

  val c: Color = Color.Red

  // Custom infix method — RHS expected type is Color, so `Red` resolves.
  val eq2: Boolean   = c matchesColor Red
  val combined: Color = c or Blue

  // Operator-syntax custom: `c |+| Red`.
  val res: Color = c |+| Red

  // Method-call form.
  val res2: Color = c.|+|(Red)
  val eq3: Boolean = c.matchesColor(Red)
