//> using options -experimental

import scala.annotation.unroll

class Unrolled {
  final def foo(x: Int)[T](
    s: T,
  ): String = "" + x + s
}

class TestV1 {
  def test(u: Unrolled): Unit =
    assert(u.foo(0)("foo").startsWith("0foo"))
}
