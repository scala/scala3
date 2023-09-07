object Test:
  def f(xs: Int*) = xs.sum
  def g() = 1

  val x1 = f  // error
  val x2 = g  // error
  val x3 = java.nio.file.Paths.get(".").toRealPath // error

// #14567
case class Foo(x: Int)(xs: String*)
def test = Foo(3)  // error
