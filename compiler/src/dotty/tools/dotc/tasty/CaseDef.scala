package dotty.tools.dotc.tasty

import dotty.tools.dotc.ast.Trees
import dotty.tools.dotc.ast.tpd.Tree
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Decorators.sourcePos
import dotty.tools.dotc.core.Names


object CaseDef {

  def apply(tree: Tree)(implicit ctx: Context): scala.tasty.CaseDef = Impl(tree, ctx)

  object CaseDef {
    def unapply(term: scala.tasty.CaseDef): Option[(scala.tasty.Pattern, Option[scala.tasty.Term], scala.tasty.Term)] = term match {
      case Impl(Trees.CaseDef(pat, guard, body), ctx) =>
        Some(Pattern(pat)(ctx), if (guard.isEmpty) None else Some(Term(guard)(ctx)), Term(body)(ctx))
      case _ => None
    }
  }

  private case class Impl(tree: Tree, ctx: Context) extends scala.tasty.CaseDef with Positioned {
    override def toString: String = this match {
      case CaseDef(pat, guard, body) => s"CaseDef($pat, $guard, $body)"
      case _ => s"CaseDef"
    }
  }
}
