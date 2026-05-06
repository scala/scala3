import scala.language.experimental.targetTypedCompanionShorthand

sealed trait Color
object Color:
  case object Red   extends Color
  case object Blue  extends Color
  case object Green extends Color

sealed trait Geometry
object Geometry:
  case object Triangle extends Geometry
  case object Circle   extends Geometry

final case class Shape(geometry: Geometry, color: Color)

object Test:
  def describe(s: Shape): String = s match
    case Shape(.Triangle, .Red)  => "warning"
    case Shape(.Circle,   .Red)  => "stop"
    case Shape(.Circle,   .Blue) => "info"
    case _                       => "unknown"

  def main(args: Array[String]): Unit =
    val stop:  Shape = Shape(.Circle, .Red)
    val info:  Shape = Shape(.Circle, .Blue)
    val warn:  Shape = Shape(.Triangle, .Red)
    val other: Shape = Shape(.Triangle, .Green)
    println(describe(stop))
    println(describe(info))
    println(describe(warn))
    println(describe(other))

    // Bare RHS form.
    val c: Color = .Red
    println(c)
