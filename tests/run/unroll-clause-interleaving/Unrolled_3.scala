//> using options -experimental

import scala.annotation.unroll

class Unrolled {
  final def foo(x: Int)[T](
    s: T,
    @unroll y: Boolean = true,
    @unroll i: Int = 0,
  ): String = "" + x + s + y + i
}

class TestV3 {
  def test(u: Unrolled): Unit =
    assert(u.foo(0)("foo").startsWith("0footrue0"))
    assert(u.foo(0)("foo", false).startsWith("0foofalse0"))
    assert(u.foo(0)("foo", false, 1).startsWith("0foofalse1"))
}
