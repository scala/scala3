// SIP-80 §Equality: `==` is defined on `Any`, so the right operand's expected
// type is `Any` — no useful companion. Users continue to write the fully
// qualified form, or define a typed comparison method.
import scala.language.experimental.companionScopeInference

object Equality:
  sealed trait Color
  object Color:
    case object Red extends Color

  val c: Color = Color.Red

  val bad: Boolean = c == Red // error
