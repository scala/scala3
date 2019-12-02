package playground

import scala.quoted.{_, given}, scala.quoted.matching._

inline def mcr(x: => Any): Any = ${mcrImpl('x)}

def mcrImpl(body: Expr[Any])(given ctx: QuoteContext): Expr[Any] =
  body match
    case '{$x: $t} =>
      '{
        val tmp: $t = $x
        println(tmp)
        tmp
      }
