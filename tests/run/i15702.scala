abstract class A:
  val n: Int
  def foo(): Int = n

trait B:
  val m: Int
  def foo(): Int = m

trait M extends A with B:
  val a: Int
  override def foo(): Int = a + super.foo()

trait N extends A with B:
  val b: Int
  override def foo(): Int = b * super.foo()

  class Inner:
    println(N.super[A].foo())
    println(N.super.foo())
    println(foo())

object C extends A with M with N:
  val a = 10
  val b = 10
  val m = 10
  val n = 10
  new Inner()

@main def Test = C