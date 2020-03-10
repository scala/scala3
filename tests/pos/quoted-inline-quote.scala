import scala.quoted._
class Foo(using QuoteContext) {
  inline def foo(x: Expr[String])(using QuoteContext) = '{ println(${x}) }

  foo('{"abc"})
}
