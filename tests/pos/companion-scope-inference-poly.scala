// SIP-80: when a generic call's type parameter is constrainable from the
// surrounding expected type, type inference flows through the result and
// the bare-ident arguments resolve against the inferred parameter.
import scala.language.experimental.companionScopeInference

sealed trait Color
object Color:
  case object Red  extends Color
  case object Blue extends Color

object Inference:
  val xs: Seq[Color]    = Seq(Red, Blue)
  val ls: List[Color]   = List(Red, Blue, Red)
  val opt: Option[Color] = Some(Blue)
