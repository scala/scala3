object Test {
  val x: T[Int] = ??? //2      // error
  val y: Int = 1 // x         // error
  val a: Int = T.f(x)    // ok
  val b: T[Int] = T.g(y) // ok
}
