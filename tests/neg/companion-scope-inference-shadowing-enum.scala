// Shadowing counterpart of `tests/pos/companion-inference-enum.scala`.
// A local of unrelated type with the same name as an enum case wins normal
// resolution; the val initialisation then has the wrong type.
import scala.language.experimental.companionScopeInference

object EnumSupport:

  enum Color:
    case Red, Blue, Green

  // Local with the same name as an enum case.
  val Red = 0

  val c: Color = Red // error
