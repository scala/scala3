package scala


object ConversionFunction {

  opaque type ConversionFunction[+F <: Nothing => Any] = F

  def apply[F <: Nothing => Any](f: F): ConversionFunction[F] = f
  def get[F <: Nothing => Any](using f: ConversionFunction[F]): F = f

}

type ConversionFunction[+F <: Nothing => Any] =
  ConversionFunction.ConversionFunction[F]

object Test {

  {
    given ConversionFunction[Int => String] = ConversionFunction(_.toString)
    // val a: String = 3
    val a: String = ConversionFunction.get[3 => String].apply(3)
  }

  trait X {
    type T
    def t: T
  }
  val x: X = ???

  {
    given ConversionFunction[(x: X) => x.T] = ConversionFunction((x: X) => x.t)
    // val a: x.T = x
    val a: x.T = ConversionFunction.get[(x: X) => x.T].apply(x)
  }

  {
    given ConversionFunction[(x: X) => x.T] = ConversionFunction(_.t)
    // val a: x.T = x
    val a: x.T = ConversionFunction.get[(x: X) => x.T].apply(x)
  }

}
