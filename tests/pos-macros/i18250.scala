import scala.quoted.*

def test(x: Expr[Any])(using Quotes): Unit =
  x match
    case '{ type t; type u <: t; () } =>
    case '{ type t <: Comparable[t]; () } =>
