//> using options -experimental

import scala.quoted.*

def test(x: Type[?])(using Quotes) =
  x match
    case '[ F[t, t] ] => // warn // error
    case '[ type u <: Int & Double; F[u, u] ] =>

type F[x <: Int, y <: Double]
