//> using options -experimental

import scala.quoted.*

def test2(x: Expr[Any])(using Quotes) =
  x match
    case '{ $_ : F[t, t]; () } => // warn // error
    case '{ type u <: Comparable[`u`]; $_ : F[u, u] } =>

type F[T, U <: Comparable[U]]
