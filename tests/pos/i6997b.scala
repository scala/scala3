package playground

import scala.quoted._, scala.quoted.matching._
import delegate scala.quoted._

inline def mcr(x: => Any): Any = ${mcrImpl('x)}

def mcrImpl(body: Expr[Any]) given (ctx: QuoteContext): Expr[Any] = {
  val '{$x: $t} = body
  '{
    val tmp: $t = $x.asInstanceOf[$t]
    println(tmp)
    tmp
  }
}