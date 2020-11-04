package playground

import scala.quoted._

object macros {
  inline def mcr(x: => Any) = ${mcrImpl('x)}

  def mcrImpl(body: Expr[Any])(using ctx: QuoteContext) : Expr[Any] = {
    import ctx.reflect._
    body.unseal match { case Block(_, _) => '{2} }
  }
}
