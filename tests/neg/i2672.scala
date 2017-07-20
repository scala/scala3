class Foo[T]

trait Prepend {
  type Out
}

object Test {
  def foo()(implicit ev: Prepend): Foo[ev.Out] = ???

  def test: Unit = {
    foo(): Foo[Any] // error: found: Prepend => Foo[_] required: Foo[Any]

  }

  implicit val p: Prepend = ???
}
