import scala.quoted._

class Foo {
  rewrite def foo: Unit = ~Foo.impl
  object Bar {
    rewrite def foo: Unit = ~Foo.impl
  }
}

object Foo {
  class Baz {
    rewrite def foo: Unit = ~impl
  }
  object Quox {
    rewrite def foo: Unit = ~Foo.impl
  }
  def impl: Expr[Unit] = '()
}
