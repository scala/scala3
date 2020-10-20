package playground

import scala.quoted._

inline def mcr(x: => Any): Any = ${mcrImpl('x)}

def mcrImpl(body: Expr[Any])(using ctx: QuoteContext): Expr[Any] =
  body match
    case '{$x: $T} =>
      '{
        val tmp: T = $x
        println(tmp)
        tmp
      }
