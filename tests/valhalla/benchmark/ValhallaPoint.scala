package benchmark
// import scala.annotation.valhalla

// @valhalla
case class ValhallaPoint(x: Double, y: Double, z: Double){//} extends AnyVal{
  private def square(v: Double): Double = v * v
  private def round(v: Double): Double = (v * 100).toInt / 100.0

  infix def squareDistance(that: ValhallaPoint): Double =
    square(that.x - x) + square(that.y - y) + square(that.z - z)

  infix def l1Distance(that: ValhallaPoint): Double =
    (that.x - x).abs + (that.y - y).abs + (that.z - z).abs
    
  override def toString = s"ValhallaPoint(${round(x)}, ${round(y)}, ${round(z)})"
}
