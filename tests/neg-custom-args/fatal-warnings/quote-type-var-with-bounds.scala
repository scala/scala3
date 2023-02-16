import scala.quoted.*

class C[T <: Int]
class D[T >: Null <: String]

def test(e: Expr[Any])(using Quotes) =
  e match
    case '{ $x: t } =>
    case '{ $x: C[t] } => // error
    case '{ $x: D[t] } => // error
    case '{ type t <: Int; $x: C[`t`] } =>
