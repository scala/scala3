import scala.quoted._

class Foo {
  transparent def foo: Unit = ~Foo.impl // error
  object Bar {
    transparent def foo: Unit = ~Foo.impl // error
  }
}

object Foo {
  class Baz {
    transparent def foo: Unit = ~impl // error
  }
  object Quox {
    transparent def foo: Unit = ~Foo.impl
  }
  def impl: Expr[Unit] = '()
}
