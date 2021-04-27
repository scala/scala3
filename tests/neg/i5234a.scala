object Test {
  import scala.util.NotGiven

  class Foo
  class Bar
  implicit def foo: Foo = ???
  implicitly[Foo]
  implicitly[NotGiven[Foo]] // error
  implicitly[NotGiven[Bar]]
}
