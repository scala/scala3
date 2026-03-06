package benchmark

case class IdentityPoint(x: Double, y: Double, z: Double){
  private def square(v: Double): Double = v * v
  private def round(v: Double): Double = (v * 100).toInt / 100.0

  infix def squareDistance(that: IdentityPoint): Double =
    square(that.x - x) + square(that.y - y) + square(that.z - z)

  infix def l1Distance(that: IdentityPoint): Double =
    (that.x - x).abs + (that.y - y).abs + (that.z - z).abs
    
  override def toString = s"Point(${round(x)}, ${round(y)}, ${round(z)})"
}