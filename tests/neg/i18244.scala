import scala.annotation.*

class A:
  def foo: Int = 1
class B extends A:
  @targetName("foo") private[this] def bla: Int = 2 // error
class C extends A:
  @targetName("foo") private def bla: Int = 2 // error

@main def Test =
  val b = new B
  println(b.foo)
