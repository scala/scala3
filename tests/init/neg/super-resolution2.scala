abstract class A:
  val n: Int
  def foo(): Int = n

trait B:
  val m: Int
  def foo(): Int = m

class N extends A with B:
  override def foo(): Int = 10 * super.foo()

  class Inner:
    N.super[A].foo()
    N.super.foo()

  new Inner

  val m = 30 // error
  val n = 40 // error
