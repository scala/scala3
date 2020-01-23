package playground

import scala.quoted._, scala.quoted.matching._
import scala.quoted.{given _}
import scala.tasty._

object macros {
  inline def mcr(x: => Any) = ${mcrImpl('x)}

  def mcrImpl(body: Expr[Any]) with (ctx: QuoteContext) : Expr[Any] = {
    import ctx.tasty.{_, given _}
    body.unseal match { case Block(_, _) => '{2} }
  }
}
