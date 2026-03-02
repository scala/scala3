//> using options -experimental

import scala.annotation.unroll

class Unrolled(val x: Int) extends AnyVal {
  final def foo(
    s: String,
  ): String = "" + x + s
}

class TestV1:
  def test(u: Unrolled): Unit =
    assert(u.foo("foo").startsWith("0foo"))
