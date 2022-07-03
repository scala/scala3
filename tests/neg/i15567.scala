object Test:
  object Foo { def apply[A]: Foo.type = this }
  Foo() // error

  case class Bar() { def apply[A]: Bar = this }
  Bar()() // error

  case class Qux() { def apply[F[_]]: Qux = this }
  Qux()() // error
