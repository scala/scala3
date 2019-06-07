object AnyKinds {

  val x: AnyKind = 2   // error

  def f[X <: AnyKind]: Any = {
    type T0 = AnyKind[String] // error
    type T1 = X[Int]  // error
    type T2 = X { type F = Int } // error
    type T3 <: List & AnyKind // error // error
    type T4 <: Int & AnyKind // error
    val x: X = ???   // error
  }

  f[Int]      // OK
  f[[X] =>> X] // OK
  f[Nothing]  // OK

  def g[X <: Any]: Any = {
    f[X]      // OK
  }
  g[Int]      // OK
  g[Nothing]  // OK

  trait X[F[_] <: AnyKind] { type L = F[Int]; def a: L = ??? } // error: cannot be used as a value type

}