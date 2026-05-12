// Shadowing counterpart of `tests/pos/companion-inference-poly.scala`.
// `Seq(Red, Blue): Seq[Color]` works when `Red`/`Blue` aren't in scope —
// the outer expected type pins `Seq.apply[A]`'s `A` to `Color`. If both
// names are shadowed by locals of an unrelated type, normal resolution
// wins; the inferred element type is the local's type, and the outer
// val's declared type rejects the result.
import scala.language.experimental.companionScopeInference

sealed trait Color
object Color:
  case object Red  extends Color
  case object Blue extends Color

object Shadow:
  val Red = "r"
  val Blue = "b"

  val xs: Seq[Color] = Seq(Red, Blue) // error // error
