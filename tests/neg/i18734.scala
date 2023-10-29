case class Foo(a: Int, b: Int)

object Bar:
  val x = 1
  val y = 2

  Foo(1 2) // error

  Foo(x y) // error

  Foo(1 b = 2) // error

  // Or
  Foo(
    a = 1
    b = 2 // error
  )

  val f: (Int, Int) => Int = (x y) => x + y // error

  def bar[X, Y](x: X, y: Y) = ???

  bar[Int String](1 2) // error // error


