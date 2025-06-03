//> using options -experimental

import scala.annotation.unroll

class Unrolled(val x: Int) extends AnyVal {
  final def foo(
    s: String,
    @unroll y: Boolean = true,
  ): String = "" + x + s + y
}

class TestV2:
  def test(u: Unrolled): Unit =
    assert(u.foo("foo").startsWith("0footrue"))
    assert(u.foo("foo", false).startsWith("0foofalse"))
