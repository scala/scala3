object Foo {
  def foo(): Int = {
    val f: implicit Int => Int = implicit (x: Int) => 2 * x
    f(2)
  }

  val f = implicit (x: Int) => x

  (implicit (x: Int) => x): (implicit Int => Int) // error: no implicit argument found
}
