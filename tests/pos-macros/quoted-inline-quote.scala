import scala.quoted._
class Foo(using s: Scope) {
  inline def foo(using s: Scope)(x: s.Expr[String]) = '{ println(${x}) }

  foo('{"abc"})
}
