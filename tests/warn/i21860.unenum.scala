trait Figure
sealed trait Corners { self: Figure => }

sealed abstract class Shape extends Figure
object Shape:
  case object Triangle extends Shape with Corners
  case object Square   extends Shape with Corners
  case object Circle   extends Shape
  case object Ellipsis extends Shape

def hasCorners(s: Shape): Boolean = s match
  case hasCorners: Corners => true //  <--- reported as `Unreachable case`
  case _                   => false

class Test:
  def test(): Unit =
    println(hasCorners(Shape.Circle))
