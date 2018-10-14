import Predef.{any2stringadd => _, _}
object opaquetypes {
  type Logarithm = Double

  implicit object Logarithm {

    // These are the ways to lift to the logarithm type
    def apply(d: Double): Logarithm = ???

    def safe(d: Double): Option[Logarithm] = ???

    // This is the first way to unlift the logarithm type
    def exponent(l: Logarithm): Double = ???

    // Extension methods define opaque types' public APIs

    // This is the second way to unlift the logarithm type
    def toDouble(this x: Logarithm): Double = ???
    def +++(this x: Logarithm)(y: Logarithm) = ???
    def ***(this x: Logarithm)(y: Logarithm): Logarithm = ???
  }
}
object usesites {
  import opaquetypes._
  val l = Logarithm(1.0)
  val l2 = Logarithm(2.0)
  val l3 = l *** l2
  val l4 = l +++ l2  // currently requires any2stringadd to be disabled because
                   // as a contextual implicit this takes precedence over the
                   // implicit scope implicit LogarithmOps.
                   // TODO: Remove any2stringadd
  val d = l3.toDouble
  val l5: Logarithm = (1.0).asInstanceOf[Logarithm]
}
