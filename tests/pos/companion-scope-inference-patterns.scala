import scala.language.experimental.companionScopeInference

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

  // Pattern position — capitalised identifiers are stable references.
  // Lower-case binders are unaffected (rule preserved).
  def name(c: Color): String = c match
    case Red   => "red"
    case Blue  => "blue"
    case Green => "green"

  // `|` alternatives.
  def warm(c: Color): Boolean = c match
    case Red | Green => true
    case _           => false

  // Nested in extractor pattern: each sub-pattern's expected type comes from
  // the corresponding extractor parameter.
  def kind(s: Shape): String = s match
    case Shape(Triangle, _)            => "triangle"
    case Shape(Circle, Red)            => "stop sign"
    case Shape(Circle, Blue | Green)   => "wheel"
    case _                             => "other"
