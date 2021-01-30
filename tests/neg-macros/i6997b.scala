package playground

import scala.quoted.*

inline def mcr(x: => Any): Any = ${mcrImpl('x)}

def mcrImpl(body: Expr[Any])(using ctx: Quotes): Expr[Any] = {
  val '{$x: $t} = body // error
  '{
    val tmp: $t = $x.asInstanceOf[$t] // error // error
    println(tmp)
    tmp
  }
}