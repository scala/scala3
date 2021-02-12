package playground

import scala.quoted.*

object macros {
  inline def mcr(x: => Any) = ${mcrImpl('x)}

  def mcrImpl(body: Expr[Any])(using ctx: Quotes) : Expr[Any] = {
    import ctx.reflect.*
    body.asTerm match { case Block(_, _) => '{2} }
  }
}
