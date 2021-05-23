class Foo(implicit val foo: Foo)

object Test extends App {
  implicit object Bar extends Foo  // error
  Bar.foo
}