import scala.annotation.targetName

class A:
  @targetName("ff") def f(x: => Int): Int = x
  def f(x: => String): Int = x.length


class B extends A:
  @targetName("ff") override def f(x: => Int): Int = x // OK
  override def f(x: => String): Int = x.length // OK

