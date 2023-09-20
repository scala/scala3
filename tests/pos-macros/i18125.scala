//> using options -experimental

import scala.quoted.*

final class Foo[T](ns: T)

def foo(using Quotes)(x: Expr[Any]): Unit =
  x match
    case '{ new Foo($y: b) } =>
    case '{ new Foo($y: List[b]) } =>
    case '{ type b; new Foo($y: b) } =>

