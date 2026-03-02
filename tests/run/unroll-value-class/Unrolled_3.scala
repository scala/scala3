//> using options -experimental

import scala.annotation.unroll

class Unrolled(val x: Int) extends AnyVal {
  final def foo(
    s: String,
    @unroll y: Boolean = true,
    @unroll i: Int = 0
  ): String = "" + x + s + y + i
}

class TestV3:
  def test(u: Unrolled): Unit =
    assert(u.foo("foo").startsWith("0footrue0"))
    assert(u.foo("foo", false).startsWith("0foofalse0"))
    assert(u.foo("foo", false, 1).startsWith("0foofalse1"))
