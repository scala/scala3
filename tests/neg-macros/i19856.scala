import scala.quoted.*

def test(using Quotes): Any =
  class Foo {
    class IdxWrapper
    def foo(using Type[IdxWrapper]): Expr[Any] =
      '{ new IdxWrapper } // error
  }
  ()
