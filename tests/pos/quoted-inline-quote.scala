import scala.quoted._
class Foo {
  inline def foo(x: Expr[String])(given QuoteContext) = '{ println(${x}) }

  given QuoteContext = ???
  foo('{"abc"})
}
