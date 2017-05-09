class Foo[A]

object Test {
  def foo[T](x: Foo[T]) = x

  foo((new Foo[Int]: Foo[_]))
}


