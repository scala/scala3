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

class C extends A with M with N:
  foo()
  val a = 10 // error
  val b = 20 // error
  val m = 30 // error
  val n = 40

