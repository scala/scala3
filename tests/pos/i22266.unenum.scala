sealed trait NonPolygon
sealed trait Polygon

sealed trait SymmetryAspect
sealed trait RotationalSymmetry      extends SymmetryAspect
sealed trait MaybeRotationalSymmetry extends SymmetryAspect

sealed abstract class Shape

object Shape:
  case object Circle   extends Shape with NonPolygon with RotationalSymmetry
  case object Triangle extends Shape with Polygon with MaybeRotationalSymmetry
  case object Square   extends Shape with Polygon with RotationalSymmetry

  def hasPolygon(
    rotationalSyms: Vector[Shape & RotationalSymmetry],
    maybeSyms: Vector[Shape & MaybeRotationalSymmetry]
  ): Boolean =
    val all = rotationalSyms.concat(maybeSyms)
    all.exists:
      case _: Polygon => true
      case _          => false
