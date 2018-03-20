import scala.quoted._

class Foo {
  inline def foo: Unit = ~Foo.impl // error
  object Bar {
    inline def foo: Unit = ~Foo.impl // error
  }
}

object Foo {
  class Baz {
    inline def foo: Unit = ~impl // error
  }
  object Quox {
    inline def foo: Unit = ~Foo.impl
  }
  def impl: Expr[Unit] = '()
}
