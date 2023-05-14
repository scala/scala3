trait MathSig {
  def sqrt(x: Double): Double
}

trait MathSpec extends MathSig {
  val epsilon = 0.00001
  abstract override def sqrt(x: Double) = {
    require(x >= 0)
    super.sqrt(x)
  } ensuring { result =>
    (x * x - result).abs < epsilon
  }
}

trait MathImpl extends MathSig {
  def sqrt(x: Double): Double =
    ???
}

object Math extends MathImpl
               with MathSpec