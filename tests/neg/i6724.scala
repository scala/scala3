enum Foo[T] {
  case Bar(s: String)
  case Baz extends Foo[Int]
}

object Main {
  def f(foo: Foo.Baz): Foo[_] = foo // error
}
