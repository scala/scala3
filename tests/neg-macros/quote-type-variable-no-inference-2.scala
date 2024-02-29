import scala.quoted.*

def test2(x: Expr[Any])(using Quotes) =
  x match
    case '{ $_ : F[t, t]; () } => // warn // error
    case '{ type u <: Int & Double; $_ : F[u, u] } =>

type F[X <: Int, Y <: Double]
