package playground

import scala.quoted._

object macros {
  inline def mcr(x: => Any) = ${mcrImpl('x)}

  def mcrImpl(body: Expr[Any])(using ctx: Quotes) : Expr[Any] = {
    import ctx.reflect._
    body.asTerm match { case Block(_, _) => '{2} }
  }
}
