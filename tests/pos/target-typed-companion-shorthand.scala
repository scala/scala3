import scala.language.experimental.targetTypedCompanionShorthand

object Basic:

  sealed trait Color
  object Color:
    case object Red   extends Color
    case object Blue  extends Color
    case object Green extends Color

  sealed trait Geometry
  object Geometry:
    case object Triangle extends Geometry
    case object Circle   extends Geometry
    case object Square   extends Geometry

  final case class Shape(geometry: Geometry, color: Color)

  // Argument position — direct.
  val s1: Shape = Shape(.Circle, .Red)

  // Mixed positional / explicit.
  val s2: Shape = Shape(Geometry.Triangle, .Blue)
  val s3: Shape = Shape(.Square, Color.Green)

  // RHS of typed val.
  val c: Color = .Red
  val g: Geometry = .Square

  // Parenthesised.
  val c2: Color = (.Red)

  // `if` arms — both branches share the expected type.
  def pick(b: Boolean): Color = if b then .Red else .Blue

  // Inside lambda body with target type.
  val mk: Boolean => Color = b => if b then .Red else .Blue

  // Named argument: the name fixes the expected type.
  val s4: Shape = Shape(color = .Red, geometry = .Circle)

  // Using clause.
  def withColor(using c: Color): Color = c
  given Color = Color.Blue
  val c4: Color = withColor(using .Green)
