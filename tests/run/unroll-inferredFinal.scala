//> using options -experimental

import scala.annotation.unroll

object UnrolledObj {
  // final is not needed because objects can't be extended
  def foo(s: String, @unroll y: Boolean = true): String = s + y
}

// final inferred for constructor
class UnrolledClass(s: String, @unroll y: Boolean = true):
  override def toString = s"UnrolledClass($s,$y)"


@main def Test: Unit =
  assert(UnrolledObj.foo("foo") == "footrue")
  assert(new UnrolledClass("foo").toString == "UnrolledClass(foo,true)")
