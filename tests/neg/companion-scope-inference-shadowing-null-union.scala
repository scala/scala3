// Shadowing counterpart of `tests/pos/companion-inference-null-union.scala`.
// `T | Null` peels to `T`, but if a local of unrelated type shadows the
// companion member, normal resolution wins and produces a type mismatch
// — the peel never gets a chance to help.
import scala.language.experimental.companionScopeInference

object NullUnion:

  sealed trait Color
  object Color:
    case object Red  extends Color
    case object Blue extends Color

  // Local of unrelated type with the same name as Color.Red.
  val Red = false

  val c: Color | Null = Red // error
