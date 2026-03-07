// Regression test for https://github.com/mbovel/dotty/issues/33

case class Foo(a: Int, b: {e: Int with a + e > 10})

object Test:
  val f = Foo(1, 10)
