object Test {
  class Foo(val bar: Bar)
  class Bar(baz0: => Baz) {
    lazy val baz = baz0
  }
  class Baz(val foo: Foo)

  implicit def mkFoo(implicit bar: Bar): Foo = new Foo(bar)
  implicit def mkBar(implicit baz: => Baz): Bar = new Bar(baz)
  implicit def mkBaz(implicit foo: Foo): Baz = new Baz(foo)

  def main(args: Array[String]): Unit = {
    val foo: Foo = implicitly[Foo]
    assert(foo.bar.baz.foo eq foo)
  }
}
