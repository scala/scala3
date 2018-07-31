import scala.quoted._

class Foo {
  transparent def foo: Unit = ~Foo.impl
  object Bar {
    transparent def foo: Unit = ~Foo.impl
  }
}

object Foo {
  class Baz {
    transparent def foo: Unit = ~impl
  }
  object Quox {
    transparent def foo: Unit = ~Foo.impl
  }
  def impl: Expr[Unit] = '()
}
