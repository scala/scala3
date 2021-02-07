sealed class Foo(val value: Int)

object Foo {
  def unapplySeq(foo: Foo): Seq[Int] = List(foo.value)
}

def foo(x: Foo): Unit =
  x match {
    case Foo(x, _*) => assert(x == 3)
  }
