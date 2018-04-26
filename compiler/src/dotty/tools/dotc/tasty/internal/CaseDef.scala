package dotty.tools.dotc.tasty
package internal

import dotty.tools.dotc.ast.Trees
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context

import scala.tasty.trees

object CaseDef {

  def apply(tree: tpd.CaseDef): trees.CaseDef = new Impl(tree)

  def unapplyCaseDef(arg: Impl)(implicit ctx: Context): Option[trees.CaseDef.Data] = {
    val Trees.CaseDef(pat, guard, body) = arg.tree
    Some(Pattern(pat), if (guard.isEmpty) None else Some(Term(guard)), Term(body))
  }

  private[tasty] class Impl(val tree: tpd.CaseDef) extends trees.CaseDef with Positioned {
    override def toString: String = "CaseDef"
  }
}
