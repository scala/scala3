import scala.annotation.targetName

class A with
  @targetName("ff") def f(x: => Int): Int = x
  def f(x: => String): Int = x.length


class B extends A with
  @targetName("ff") override def f(x: => Int): Int = x // OK
  override def f(x: => String): Int = x.length // OK

