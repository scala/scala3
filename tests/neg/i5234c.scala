object Test {
  import implicits.Not

  class Foo
  implicit def foo: Foo = ???

  def foo[T](implicit ev: Not[T]) = ???
  foo[Foo] // error
}
