object Test {
  import scala.util.Not

  class Foo
  class Bar
  implicit def foo: Foo = ???
  implicitly[Foo]
  implicitly[Not[Foo]] // error
  implicitly[Not[Bar]]
}
