class Foo

object Test {
  def foo[T](x: T)(implicit ev: T): T = ???

  class Fn[T] {
    def invoke(implicit ev: T): T = ???
  }

  def bar[T](x: T): Fn[T] = ???

  def test: Unit = {
    implicit val evidence: Foo = new Foo
    foo(new Foo)
    bar(new Foo).invoke
  }
}
