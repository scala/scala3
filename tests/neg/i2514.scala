object Foo {
  def foo(): Int = {
    val f: implicit Int => Int = (implicit x: Int) => 2 * x // error // error
    f(2)
  }

  val f = (implicit x: Int) => x // error // error

  ((implicit x: Int) => x): (implicit Int => Int) // error // error // error
}
