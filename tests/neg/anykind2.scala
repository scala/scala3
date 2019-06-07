object AnyKinds {

  def f[X <: AnyKind]: Any = {
    g[X]             // error
    g[AnyKind]       // error
  }

  f[Int]      // OK
  f[[X] =>> X] // OK
  f[Nothing]  // OK

  def g[X <: Any]: Any = {
    f[X]      // OK
  }
  g[Int]      // OK
  g[List]     // error
  g[Nothing]  // OK

  1.asInstanceOf[AnyKind] // error

}