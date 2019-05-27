object opaquetypes {

  opaque val x: Int = 1 // error

  opaque class Foo // error

  opaque object Foo // error

  opaque type T // error

  opaque type U <: String // error

  opaque type Fix[F[_]] = F[Fix[F]] // error: cyclic

  opaque type O = String

  val s: O = "" // should now be OK

  object O {
    val s: O = "" // should be OK
  }

  def foo() = {
    opaque type X = Int   // error
  }
}

object logs {
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

object Test {
  import logs._
  val l = Logarithm(2.0)
  val d: Double = l       // error: found: Logarithm, required: Double
  val l2: Logarithm = 1.0 // error: found: Double, required: Logarithm
  l * 2                   // error: found: Int(2), required: Logarithm
  l / l2                  // error: `/` is not a member fo Logarithm
}
