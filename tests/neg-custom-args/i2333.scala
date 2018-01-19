@deprecated("bla", "2.11.0") class Foo {
  def this(x: Int) = this()
}

object Test {
  new Foo // error: deprecated
  new Foo(1) // error: deprecated
}