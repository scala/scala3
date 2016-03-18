// testing the limits of parameter type inference

object Test {
  def mapX(f: Char => Char): String = ???
  def mapX[U](f: U => U): U = ???
  mapX(x => x) // error: missing parameter type

  def foo(f: Char => Char): Unit = ???
  def foo(f: Int => Int): String = ???
  foo(x => x) // error: missing parameter type

  def bar(f: (Char, Char) => Unit): Unit = ???
  def bar(f: Char => Unit) = ???
  bar((x, y) => ())
  bar (x => ())

}
