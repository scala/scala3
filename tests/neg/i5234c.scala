object Test {
  import scala.util.NotGiven

  class Foo
  implicit def foo: Foo = ???

  def foo[T](implicit ev: NotGiven[T]) = ???
  foo[Foo] // error
}
