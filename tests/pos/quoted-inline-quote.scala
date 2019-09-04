import scala.quoted._
class Foo {
  inline def foo(x: Expr[String]) given QuoteContext = '{ println(${x}) }

  given as QuoteContext = ???
  foo('{"abc"})
}
