import scala.language.experimental.targetTypedCompanionShorthand

object MissingMember:

  sealed trait Color
  object Color:
    case object Red  extends Color
    case object Blue extends Color

  val ok: Color = .Red

  // `Purple` is not a member of `Color`'s companion.
  val bad: Color = .Purple // error
