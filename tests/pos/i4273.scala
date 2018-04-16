trait Foo {
  def config: String

  case class Bar() {
    def doThings: String = config //Do whatever here
  }
}

case class Baz(foo: Foo, bar: foo.Bar)
case class Baz2(val foo: Foo, bar: foo.Bar)
case class Baz3(foo: Foo)(val bar: foo.Bar)
