package playground

import scala.quoted.*

inline def mcr(x: => Any): Any = ${mcrImpl('x)}

def mcrImpl(body: Expr[Any])(using ctx: Quotes): Expr[Any] =
  body match
    case '{$x: t} =>
      '{
        val tmp: t = $x
        println(tmp)
        tmp
      }
