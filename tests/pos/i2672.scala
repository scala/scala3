class Foo[+T]

trait Prepend {
  type Out
}

object Test {
  def foo()(implicit ev: Prepend): Foo[ev.Out] = ???

  def test: Unit = {
    foo(): Foo[Any]
  }

  implicit val p: Prepend = ???
}
