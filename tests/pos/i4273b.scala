class Foo(val config: String) {
  case class Bar()
}

case class Baz(foo: Foo, bar: foo.Bar)

object Test {
  val foo1 = new Foo("deep")
  val baz1 = new Baz(foo1, new foo1.Bar())

  baz1 match {
    case Baz(foo, bar) =>
      bar : foo.Bar                // ok
  }
}
