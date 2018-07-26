abstract class C0[X, Y, Z, W](x: X, y: Y, z: Z, w: W) {
  def f: Unit = {
    println(x)
    println(y)
    println(z)
    println(w)
  }
}

object Test {
  type T0[X, Y, Z, A, B, C] = C0[X, Y, Z, A]
  type T1[A, B, C, D] = T0[A, B, C, D, Nothing, Any]
  type T2[E, F] = T1[E, F, E, F]
  type T3[X, Y, Z] = T2[X, Z]
  type T4[X, Y, Z, W] = T3[X, W, Y]
  type T5[A, B] = T4[A, B, (Int, Int), Unit]
  type T6 = T5[Int, String]

  class D extends T6(1, "a", 2, "b")

  def main(args: Array[String]) = {
    (new D).f
  }
}
