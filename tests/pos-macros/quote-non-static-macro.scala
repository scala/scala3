import scala.quoted.*

class Foo {
  inline def foo: Unit = ${Foo.impl}
  object Bar {
    inline def foo: Unit = ${Foo.impl}
  }
}

object Foo {
  class Baz {
    inline def foo: Unit = ${impl}
  }
  object Quox {
    inline def foo: Unit = ${Foo.impl}
  }
  def impl(using Quotes): Expr[Unit] = '{}
}
