class Foo(val config: String) {
  case class Bar()
}

case class Baz(foo: Foo, bar: foo.Bar)

object GetBaz {
  def unapply(baz: Baz): Option[Baz] = Some(baz)
}

object Test {
  val foo1 = new Foo("deep")
  val foo2 = new Foo("deep")
  val baz1 = new Baz(foo1, new foo1.Bar())
  val baz2 = new Baz(foo2, new foo2.Bar())

  baz1 match {
    case GetBaz(foo, bar) =>
      bar : foo.Bar                // ok
      baz2 match {
        case GetBaz(foo2, bar2) =>
          bar2 : foo.Bar                // error
      }
  }
}
