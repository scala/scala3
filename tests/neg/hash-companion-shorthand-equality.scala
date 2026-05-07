// SIP-80 §Equality: `==` is defined on `Any`, so the right operand's expected
// type is `Any` — which has no member `Red`. The shorthand therefore does not
// help with equality comparisons. Users should write `Color.Red` instead.
import scala.language.experimental.hashCompanionShorthand

object Equality:
  sealed trait Color
  object Color:
    case object Red extends Color

  val c: Color = Color.Red

  val bad: Boolean = c == #Red // error
