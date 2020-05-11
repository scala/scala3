package playground

import scala.quoted._

inline def mcr(x: => Any): Any = ${mcrImpl('x)}

def mcrImpl(using s: Scope)(body: s.Expr[Any]): s.Expr[Any] =
  body match
    case '{$x: $t} =>
      '{
        val tmp: $t = $x
        println(tmp)
        tmp
      }
