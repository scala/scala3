
trait Foo[T]:
  def foo(v: T): Unit

given myFooOfInt:
  Foo[Int] with
  def foo(v: Int): Unit = ???

given myFooOfLong:
  Foo[Long] = new Foo[Long] {
    def foo(v: Long): Unit = ???
  }
