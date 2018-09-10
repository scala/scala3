object Test {
  type L[X] = X match {
    case Int => L[X]
  }
  type LL[X] = X match {
    case Int => LL[LL[X]]
  }
  def a: L[Boolean] = ???
  def b: L[Int] = ???
  def g[X]: L[X] = ???
  def g[X]: L[X] = ???   // error: found: L[Int], required: Int

  def aa: LL[Boolean] = ???
  def bb: LL[Int] = ???   // error: recursion limit exceeded with  subtype LazyRef(Test.LL[Int]) <:< Int
  def gg[X]: LL[X] = ???
  val xx: Int = gg[Int]   // error: recursion limit exceeded with  subtype LazyRef(Test.LL[Int]) <:< Int
}
