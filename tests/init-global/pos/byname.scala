trait T:
  def bar(i: => Int): Int

class A extends T:
  override def bar(i: => Int): Int = i + 1

class B extends T:
  override def bar(i: => Int): Int = i

object A:
  val a: T = if ??? then new A else new B
  def foo(b: List[Int]): Int = a.bar(b match {
    case head :: rest => head + foo(rest) + a.bar(head)
    case Nil => 0
  })

  val f = foo(Nil)
