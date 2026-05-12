// Shadowing counterpart of `tests/pos/companion-inference-patterns.scala`.
// In pattern position a capitalised local stable reference shadows the
// companion member, so `case Red` matches the local value rather than
// `Color.Red`. The match then becomes ill-typed because the local has the
// wrong type.
import scala.language.experimental.companionScopeInference

object Patterns:

  sealed trait Color
  object Color:
    case object Red  extends Color
    case object Blue extends Color

  // Local with the same name as a companion case object, capitalised so it
  // counts as a stable reference in pattern position (lower-case would
  // bind a fresh variable).
  val Red: String = "the letter R"

  def name(c: Color): String = c match
    case Red  => "red"   // error
    case Blue => "blue"
