package playground

import scala.quoted.{_, given _}, scala.quoted.matching._

inline def mcr(x: => Any): Any = ${mcrImpl('x)}

def mcrImpl(body: Expr[Any]) with (ctx: QuoteContext) : Expr[Any] =
  body match
    case '{$x: $t} =>
      '{
        val tmp: $t = $x
        println(tmp)
        tmp
      }
