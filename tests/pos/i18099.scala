class Foo[A]

object Test {
  class Bar[F[_]](val underlying: Any) extends AnyVal {
    def foo = new Foo[F[Any]] {}
  }
}
