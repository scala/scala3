import scala.quoted._
class Foo {
  inline def foo(x: Expr[String])(using QuoteContext) = '{ println(${x}) }

  given QuoteContext = ???
  foo('{"abc"})
}
