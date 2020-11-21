import scala.quoted._
class Foo(using Quotes) {
  inline def foo(x: Expr[String])(using Quotes) = '{ println(${x}) }

  foo('{"abc"})
}
