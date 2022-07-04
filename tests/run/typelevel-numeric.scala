
import math.Numeric

abstract class MathLib[N : Numeric] {
  def dotProduct(xs: Array[N], ys: Array[N]): N
}

object MathLib {

  inline def apply[N](implicit n: Numeric[N]) = new MathLib[N] {
    import n.*
    def dotProduct(xs: Array[N], ys: Array[N]): N = {
      require(xs.length == ys.length)
      var i = 0
      var s: N = n.zero
      while (i < xs.length) {
        s = s + xs(i) * ys(i)
        i += 1
      }
      s
    }
  }
}

object Test extends App {

  val mlib = MathLib.apply[Double](scala.math.Numeric.DoubleIsFractional)

  val xs = Array(1.1, 1.1)
  val ys = Array(2.1, -3.1)
  val p = mlib.dotProduct(xs, ys)
  println(p)
}
