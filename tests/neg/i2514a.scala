object Foo {
  def foo(): Int = {
    val f: given Int => Int = given (x: Int) => 2 * x
    f given 2
  }

  val f = implicit (x: Int) => x

  (given (x: Int) => x): (given Int => Int) // error: no implicit argument found
}
