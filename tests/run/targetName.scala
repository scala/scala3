import annotation.targetName

object A:
  def f(x: => String): Int = x.length
  @targetName("f2") def f(x: => Int): Int = x

import A._
@main def Test =
  assert(f("abc") == f(3))

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
