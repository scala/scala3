import scala.language.experimental.targetTypedCompanionShorthand

object Patterns:

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

  // Pattern position — single relative selector.
  def name(c: Color): String = c match
    case .Red   => "red"
    case .Blue  => "blue"
    case .Green => "green"

  // `|` alternatives.
  def warm(c: Color): Boolean = c match
    case .Red | .Green => true
    case _             => false

  // Nested in extractor pattern: the parameter type of `Shape.apply` is
  // `Geometry` for the first arg and `Color` for the second.
  def kind(s: Shape): String = s match
    case Shape(.Triangle, _)        => "triangle"
    case Shape(.Circle, .Red)       => "stop sign"
    case Shape(.Circle, .Blue | .Green) => "wheel"
    case _                           => "other"

  // Combined with bound name: `c @ .Red` is a Bind that wraps the relative
  // pattern, identical to `c @ Color.Red`.
  def label(c: Color): String = c match
    case x @ .Red => "got " + x.toString
    case _        => "other"
