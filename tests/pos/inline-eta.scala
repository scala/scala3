class Foo(x: Int)

object A:
  inline def bar(x: Int): Int = x
  val g1 = bar
  val g2: Int => Int = bar

  def foo(xs: List[Int]) =
    xs.map(Foo.apply) // use the `inline def apply` constructor proxy
