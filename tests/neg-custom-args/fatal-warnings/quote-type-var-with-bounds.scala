// scalac: -deprecation

import scala.quoted.*

class C[T <: Int]
class D[T >: Null <: String]
class E[T <: Int, U <: Int]
class F[T <: Int, U <: String]

def test[T: Type](e: Expr[Any])(using Quotes) =
  e match
    case '{ $x: t } =>
    case '{ $x: C[t] } => // error
    case '{ $x: D[t] } => // error
    case '{ type t <: Int; $x: C[t] } =>

    case '{ $x: E[t, t] } => // error

    case '{ $x: F[t, t] } => // error // error
    case '{ type t <: Int; $x: F[t, t] } => // error

  Type.of[T] match
    case '[ C[t] ] => // will have an error with SIP-53
    case '[ D[t] ] => // will have an error with SIP-53
    case '[ E[t, t] ] => // will have an error with SIP-53
    case '[ F[t, t] ] => // error // will have a second error with SIP-53
