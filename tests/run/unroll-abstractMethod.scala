//> using options -experimental

import scala.annotation.unroll

trait Unrolled {
  def foo(s: String, n: Int = 1, @unroll b: Boolean = true): String
}

abstract class UnrolledBase {
  def foo(s: String, n: Int = 1, @unroll b: Boolean = true): String
}

class UnrolledCls extends Unrolled {
  /** infers `override` even though Unrolled.foo is actually implemented. */
  def foo(s: String, n: Int = 1, @unroll b: Boolean = true) = s + n + b
}

object UnrolledObject extends UnrolledBase {
  /** infers `override` even though Unrolled.foo is actually implemented. */
  def foo(s: String, n: Int = 1, @unroll b: Boolean = true) = s + n + b
}

@main def Test: Unit = {
  // val cls = new UnrolledCls
  // assert(cls.foo("foo") == "foo1true")
  // assert(UnrolledObject.foo("foo") == "foo1true")
  ()
}
