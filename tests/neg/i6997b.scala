package playground

import scala.quoted.{_, given _}, scala.quoted.matching._

inline def mcr(x: => Any): Any = ${mcrImpl('x)}

def mcrImpl(body: Expr[Any]) with (ctx: QuoteContext) : Expr[Any] = {
  val '{$x: $t} = body // error
  '{
    val tmp: $t = $x.asInstanceOf[$t] // error // error
    println(tmp)
    tmp
  }
}