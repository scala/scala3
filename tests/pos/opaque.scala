import Predef.{any2stringadd => _, _}
object opaquetypes {
  opaque type Logarithm = Double

  object Logarithm {

    // These are the ways to lift to the logarithm type
    def apply(d: Double): Logarithm = math.log(d)

    def safe(d: Double): Option[Logarithm] =
      if (d > 0.0) Some(math.log(d)) else None

    // This is the first way to unlift the logarithm type
    def exponent(l: Logarithm): Double = l

    assert(exponent(LL) == 1.0)
  }

  // Extension methods define opaque types' public APIs
  implicit class LogarithmOps(val `this`: Logarithm) extends AnyVal {
    // This is the second way to unlift the logarithm type
    def toDouble: Double = math.exp(`this`)
    def +(that: Logarithm): Logarithm = Logarithm(math.exp(`this`) + math.exp(that))
    def *(that: Logarithm): Logarithm = Logarithm(`this` + that)
  }

  val LL: Logarithm = Logarithm(1)
}
object usesites {
  import opaquetypes._
  val l = Logarithm(1.0)
  val l2 = Logarithm(2.0)
  val l3 = l * l2
  val l4 = l + l2  // currently requires any2stringadd to be disabled because
                   // as a contextual implicit this takes precedence over the
                   // implicit scope implicit LogarithmOps.
                   // TODO: Remove any2stringadd
  val d = l3.toDouble
  val l5: Logarithm = (1.0).asInstanceOf[Logarithm]
}
