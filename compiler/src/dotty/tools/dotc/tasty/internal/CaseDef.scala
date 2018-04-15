package dotty.tools.dotc.tasty
package internal

import dotty.tools.dotc.ast.Trees
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context

import scala.tasty.trees

object CaseDef {

  def apply(tree: tpd.Tree)(implicit ctx: Context): trees.CaseDef = Impl(tree, ctx)

  def unapplyCaseDef(tree: trees.Tree): Option[trees.CaseDef.Data] = tree match {
    case Impl(Trees.CaseDef(pat, guard, body), ctx) =>
      Some(Pattern(pat)(ctx), if (guard.isEmpty) None else Some(Term(guard)(ctx)), Term(body)(ctx))
    case _ => None
  }

  private case class Impl(tree: tpd.Tree, ctx: Context) extends trees.CaseDef with Positioned {
    override def toString: String = {
      import Toolbox.extractor
      val trees.CaseDef(pat, guard, body) = this
      s"CaseDef($pat, $guard, $body)"
    }
  }
}
