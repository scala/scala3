package dotty.tools.dotc.tasty
package internal

import dotty.tools.dotc.ast.Trees
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context

import scala.tasty.trees

object CaseDef {

  def apply(tree: tpd.CaseDef)(implicit ctx: Context): trees.CaseDef = new Impl(tree)

  def unapplyCaseDef(arg: Impl): Option[trees.CaseDef.Data] = {
    implicit val ctx: Context = arg.ctx
    val Trees.CaseDef(pat, guard, body) = arg.tree
    Some(Pattern(pat), if (guard.isEmpty) None else Some(Term(guard)), Term(body))
  }

  private[tasty] class Impl(val tree: tpd.CaseDef)(implicit val ctx: Context) extends trees.CaseDef with Positioned {
    override def toString: String = {
      import Toolbox.extractor
      val trees.CaseDef(pat, guard, body) = this
      s"CaseDef($pat, $guard, $body)"
    }
  }
}
