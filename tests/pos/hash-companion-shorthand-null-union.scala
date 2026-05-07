// SIP-80: `T | Null` (and `Null | T`) is special-cased so the shorthand
// resolves against `T`'s companion. `Null` has no useful companion members
// and `T | Null` is the common nullable-reference idiom.
import scala.language.experimental.hashCompanionShorthand

object NullUnion:

  sealed trait Color
  object Color:
    case object Red   extends Color
    case object Blue  extends Color
    case object Green extends Color

  // RHS of typed val.
  val c1: Color | Null = #Red
  val c2: Null | Color = #Blue

  // Argument position.
  def paint(c: Color | Null): String = if c == null then "null" else c.toString
  val s1: String = paint(#Green)
  val s2: String = paint(null)

  // Conditional with nullable result type.
  def maybeColor(b: Boolean): Color | Null = if b then #Red else null

  // Pattern position — the scrutinee type provides the pt.
  val c: Color | Null = Color.Red
  val s: String = c match
    case #Red   => "r"
    case #Blue  => "b"
    case #Green => "g"
    case null   => "null"

  // Nested (rare but should work).
  val c3: (Color | Null) | Null = #Red
