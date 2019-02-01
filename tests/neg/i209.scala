
case class Foo(a: Int) {
  private def copy(i: Int): Foo = Foo(2 * i)
}

object Test {
  val foo = Foo(2)
  foo.copy(3) // error
}
