sealed case class Foo protected (i: Int, j: Int)

final class Bar(n: Int) extends Foo(n, n)

class Other:
  def foo  = Foo(1, 2)       // error
  def foo2 = Foo.apply(1, 2) // error
