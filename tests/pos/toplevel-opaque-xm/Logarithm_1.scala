package logs

opaque type Logarithm = Double

object Logarithm {

  // These are the ways to lift to the logarithm type
  def apply(d: Double): Logarithm = math.log(d)

  def safe(d: Double): Option[Logarithm] =
    if (d > 0.0) Some(math.log(d)) else None

  // This is the first way to unlift the logarithm type
  def exponent(l: Logarithm): Double = l


  given AnyRef {
    // This is the second way to unlift the logarithm type
    extension (x: Logarithm) def toDouble: Double = math.exp(x)
    extension (x: Logarithm) def + (y: Logarithm) = Logarithm(math.exp(x) + math.exp(y))
    extension (x: Logarithm) def * (y: Logarithm): Logarithm = Logarithm(x + y)
  }
}
