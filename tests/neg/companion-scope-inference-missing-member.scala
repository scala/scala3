import scala.language.experimental.companionScopeInference

object MissingMember:

  sealed trait Color
  object Color:
    case object Red  extends Color
    case object Blue extends Color

  val ok: Color = Red

  // `Purple` is not in scope and not in `Color`'s companion.
  val bad: Color = Purple // error
