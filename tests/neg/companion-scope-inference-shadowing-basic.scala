// Shadowing counterpart of `tests/pos/companion-inference.scala`.
// A local `val Red = "..."` of the wrong type wins normal resolution, so
// companion scope inference does NOT fire (it is a fallback). The user sees a
// type-mismatch on the `Red` argument.
import scala.language.experimental.companionScopeInference

object Basic:

  sealed trait Color
  object Color:
    case object Red   extends Color
    case object Blue  extends Color

  sealed trait Geometry
  object Geometry:
    case object Triangle extends Geometry
    case object Circle   extends Geometry

  final case class Shape(geometry: Geometry, color: Color)

  // Local of unrelated type with the same name as a companion member.
  val Red = "the letter R"

  val s: Shape = Shape(Circle, Red) // error
