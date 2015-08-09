class Foo

object Test {
  def foo[T](x: T)(implicit ev: T): T = ???

  def test: Unit = {
    implicit val evidence: Foo = new Foo
    foo(new Foo)
  }
}
