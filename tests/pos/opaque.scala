object opaquetypes {
  opaque type Logarithm = Double

  object Logarithm {

    // These are the ways to lift to the logarithm type
    def apply(d: Double): Logarithm = math.log(d)

    def safe(d: Double): Option[Logarithm] =
      if (d > 0.0) Some(math.log(d)) else None

    // This is the first way to unlift the logarithm type
    def exponent(l: Logarithm): Double = l

    // Extension methods define opaque types' public APIs
    implicit class LogarithmOps(val `this`: Logarithm) extends AnyVal {
      // This is the second way to unlift the logarithm type
      def toDouble: Double = math.exp(`this`)
      def +(that: Logarithm): Logarithm = Logarithm(math.exp(`this`) + math.exp(that))
      def *(that: Logarithm): Logarithm = Logarithm(`this` + that)
    }
  }
}
