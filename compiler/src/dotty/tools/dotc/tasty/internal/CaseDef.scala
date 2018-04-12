package dotty.tools.dotc.tasty
package internal

import dotty.tools.dotc.ast.Trees
import dotty.tools.dotc.ast.tpd.Tree
import dotty.tools.dotc.core.Contexts.Context

import scala.tasty.patterns

object CaseDef {

  def apply(tree: Tree)(implicit ctx: Context): patterns.CaseDef = Impl(tree, ctx)

  def unapplyCaseDef(arg: patterns.CaseDef): Option[patterns.CaseDef.Data] = arg match {
    case Impl(Trees.CaseDef(pat, guard, body), ctx) =>
      Some(Pattern(pat)(ctx), if (guard.isEmpty) None else Some(Term(guard)(ctx)), Term(body)(ctx))
    case _ => None
  }

  private case class Impl(tree: Tree, ctx: Context) extends patterns.CaseDef with Positioned {
    override def toString: String = {
      import Toolbox.extractor
      val patterns.CaseDef(pat, guard, body) = this
      s"CaseDef($pat, $guard, $body)"
    }
  }
}
