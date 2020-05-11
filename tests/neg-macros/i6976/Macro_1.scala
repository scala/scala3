package playground

import scala.quoted._

object macros {
  inline def mcr(x: => Any) = ${mcrImpl('x)}

  def mcrImpl(using s: Scope)(body: s.Expr[Any]): s.Expr[Any] = {
    import s.tasty._
    body match { case Block(_, _) => '{2} }
  }
}
