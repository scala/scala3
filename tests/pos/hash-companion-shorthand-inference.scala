// SIP-80: when a generic call's type parameter is constrainable from the
// surrounding expected type, type inference flows through the result and
// the shorthand arguments resolve against the inferred parameter.
// E.g. `Seq.apply[A](xs: A*): Seq[A]` here gets `A = Color` from the val's
// declared type, so `#Red`/`#Blue` are typed against `Color`.
import scala.language.experimental.hashCompanionShorthand

sealed trait Color
object Color:
  case object Red  extends Color
  case object Blue extends Color

object Inference:
  val xs: Seq[Color]    = Seq(#Red, #Blue)
  val ls: List[Color]   = List(#Red, #Blue, #Red)
  val opt: Option[Color] = Some(#Blue)
