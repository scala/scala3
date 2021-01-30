import annotation.targetName

object A:
  def f(x: => String): Int = x.length
  @targetName("f2") def f(x: => Int): Int = x

import A.*

trait T:
  def f(x: => String): Int
  @targetName("f2") def f(x: => Int): Int

class C:
  def f(x: => String): Int = x.length
  @targetName("f2") def f(x: => Int): Int = x

object B1 extends C, T:
  @targetName("f2") override def f(x: => Int): Int = x + 1

object B2 extends C, T:
  override def f(x: => String): Int = x.length + 1

@targetName("fooString") def foo(ps: String*) : Unit = println(s"strings: $ps")
@targetName("fooInt") def foo(ps: Int*) : Unit = println(s"ints: $ps")
@targetName("fooAny") def foo(ps: Any*) : Unit = println(s"objs: $ps")

@main def Test =
  assert(f("abc") == f(3))
  assert(B2.f("abc") == 4)
  val t: T = B1
  assert(t.f(2) == 3)
  foo("hello")
  foo(1, 2)
  foo(1, "hi")


