object Test {
  type L[X] = X match {
    case Int => L[X]
  }
  type LL[X] = X match {
    case Int => LL[LL[X]]
  }
  def a: L[Boolean] = ???
  // def b: L[Int] = ??? // times out
  def g[X]: L[X] = ???
  // val x: Int = g[Int] // times out

  def aa: LL[Boolean] = ???
  def bb: LL[Int] = ???   // error: recursion limit exceeded with  reduce type  LazyRef(Test.LL[Int]) match ...
  def gg[X]: LL[X] = ???
  val xx: Int = gg[Int]   // error: recursion limit exceeded with  reduce type  LazyRef(Test.LL[Int]) match ...
}
