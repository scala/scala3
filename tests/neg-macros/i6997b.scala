package playground

import scala.quoted._

inline def mcr(x: => Any): Any = ${mcrImpl('x)}

def mcrImpl(using s: Scope)(body: s.Expr[Any]): s.Expr[Any] = {
  val '{$x: $t} = body // error
  '{
    val tmp: $t = $x.asInstanceOf[$t] // error // error
    println(tmp)
    tmp
  }
}