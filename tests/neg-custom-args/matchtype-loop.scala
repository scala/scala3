object Test {
  type L[X] = X match {
    case Int => L[X]
  }
  type LL[X] = X match {  // error: recursion limit exceeded
    case Int => LL[LL[X]]
  }
  def a: L[Boolean] = ???
  def b: L[Int] = ???
  def g[X]: L[X] = ???
  val x: Int = g[Int]     // error: found: L[Int], required: Int

  def aa: LL[Boolean] = ???
  def bb: LL[Int] = ???   // error: recursion limit exceeded with  reduce type  LazyRef(Test.LL[Int]) match ... // error
  def gg[X]: LL[X] = ???
  val xx: Int = gg[Int]   // error: recursion limit exceeded with  reduce type  LazyRef(Test.LL[Int]) match ...
}
