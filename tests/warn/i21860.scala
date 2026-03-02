trait Figure
sealed trait Corners { self: Figure => }

enum Shape extends Figure:
  case Triangle extends Shape with Corners
  case Square   extends Shape with Corners
  case Circle   extends Shape
  case Ellipsis extends Shape

def hasCorners(s: Shape): Boolean = s match
  case hasCorners: Corners => true //  <--- reported as `Unreachable case`
  case _                   => false

class Test:
  def test(): Unit =
    println(hasCorners(Shape.Circle))
