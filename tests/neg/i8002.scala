class Foo
  def f(x: Int, y: Int = 1) = ???

object bar extends Foo
  f()  // error: missing argument