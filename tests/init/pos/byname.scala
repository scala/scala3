trait T:
  def bar(i: => Int): Int

class A extends T:
  override def bar(i: => Int): Int = i + 1

class B extends T:
  override def bar(i: => Int): Int = i + 2

object A:
  val a: T = if ??? then new A else new B
  def foo(b: List[Int]) = a.bar(b match {
    case x :: xs => 1
    case Nil => 0
  })

  val f = foo(Nil)
