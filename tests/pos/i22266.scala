sealed trait NonPolygon
sealed trait Polygon

sealed trait SymmetryAspect
sealed trait RotationalSymmetry      extends SymmetryAspect
sealed trait MaybeRotationalSymmetry extends SymmetryAspect

enum Shape:
  case Circle   extends Shape with NonPolygon with RotationalSymmetry
  case Triangle extends Shape with Polygon with MaybeRotationalSymmetry
  case Square   extends Shape with Polygon with RotationalSymmetry

object Shape:

  def hasPolygon(
    rotationalSyms: Vector[Shape & RotationalSymmetry],
    maybeSyms: Vector[Shape & MaybeRotationalSymmetry]
  ): Boolean =
    val all = rotationalSyms.concat(maybeSyms)
    all.exists:
      case _: Polygon => true
      case _          => false
