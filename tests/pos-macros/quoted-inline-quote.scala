import scala.quoted.*
class Foo(using Quotes) {
  inline def foo(x: Expr[String])(using Quotes) = '{ println(${x}) }

  foo('{"abc"})
}
