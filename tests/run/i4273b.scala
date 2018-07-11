class Foo(val config: String) {
  case class Bar()
}

case class Baz(val foo: Foo, bar: foo.Bar)

object Test {
  def main(args: Array[String]): Unit = {
    val foo1 = new Foo("deep")
    val foo2 = new Foo("deep")
    val baz1 = Baz(foo1, foo1.Bar())
    val baz2 = Baz(foo2, foo2.Bar())

    // equality
    assert(baz1 != baz2)

    // pattern matching
    baz1 match {
      case Baz(foo, bar) =>
        bar : foo.Bar
    }

    // copy
    baz1.copy(bar = baz1.foo.Bar())
    baz2.copy(bar = baz2.foo.Bar())
  }
}
