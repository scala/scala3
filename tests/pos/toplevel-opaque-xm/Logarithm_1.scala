package logs

opaque type Logarithm = Double

implicit object Logarithm {

  // These are the ways to lift to the logarithm type
  def apply(d: Double): Logarithm = math.log(d)

  def safe(d: Double): Option[Logarithm] =
    if (d > 0.0) Some(math.log(d)) else None

  // This is the first way to unlift the logarithm type
  def exponent(l: Logarithm): Double = l

  // Extension methods define opaque types' public APIs

  // This is the second way to unlift the logarithm type
  def (x: Logarithm) toDouble: Double = math.exp(x)
  def (x: Logarithm) + (y: Logarithm) = Logarithm(math.exp(x) + math.exp(y))
  def (x: Logarithm) * (y: Logarithm): Logarithm = Logarithm(x + y)
}
