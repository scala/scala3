import Predef.{any2stringadd as _, *}
object opaquetypes {
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
    extension (x: Logarithm) def toDouble: Double = math.exp(x)
    extension (x: Logarithm) def + (y: Logarithm) = Logarithm(math.exp(x) + math.exp(y))
    extension (x: Logarithm) def * (y: Logarithm): Logarithm = Logarithm(x + y)
  }
}
object usesites {
  import opaquetypes.*
  val l = Logarithm(1.0)
  val l2 = Logarithm(2.0)
  val l3 = l * l2
  val l4 = l + l2  // currently requires any2stringadd to be disabled because
                   // as a contextual implicit this takes precedence over the
                   // implicit scope implicit LogarithmOps.
                   // TODO: Remove any2stringadd
  val d = Logarithm.toDouble(l3)
  val l5: Logarithm = (1.0).asInstanceOf[Logarithm]
}
