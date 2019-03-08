object Test {
  val x: T[Int] = ???
  val y: Int = 1
  val a: Int = T.f(x)
  val b: T[Int] = T.g(y)
}
