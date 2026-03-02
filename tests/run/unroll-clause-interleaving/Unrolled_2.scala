//> using options -experimental

import scala.annotation.unroll

class Unrolled {
  final def foo(x: Int)[T](
    s: T,
    @unroll y: Boolean = true,
  ): String = "" + x + s + y
}

class TestV2 {
  def test(u: Unrolled): Unit =
    assert(u.foo(0)("foo").startsWith("0footrue"))
    assert(u.foo(0)("foo", false).startsWith("0foofalse"))
}
