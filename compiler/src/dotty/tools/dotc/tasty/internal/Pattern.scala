package dotty.tools.dotc.tasty
package internal

import dotty.tools.dotc.ast.{Trees, tpd}
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Names

import scala.tasty.trees
import scala.tasty.types

object Pattern {

  def apply(tree: tpd.Tree): trees.Pattern = new Impl(tree)

  def unapplyValue(arg: Impl)(implicit ctx: Context): Option[trees.Value.Data] = arg.tree match {
    case lit: tpd.Literal => Some(Term(lit))
    case ident: tpd.Ident => Some(Term(ident))
    case _ => None
  }

  def unapplyBind(arg: Impl)(implicit ctx: Context): Option[trees.Bind.Data] = arg.tree match {
    case Trees.Bind(name: Names.TermName, body) => Some(TermName(name), Pattern(body))
    case _ => None
  }

  def unapplyUnapply(arg: Impl)(implicit ctx: Context): Option[trees.Unapply.Data] = arg.tree match {
    case Trees.UnApply(fun, implicits, patterns) =>
      Some((Term(fun), implicits.map(Term(_)), patterns.map(Pattern(_))))
    case _ => None
  }

  def unapplyAlternative(arg: Impl)(implicit ctx: Context): Option[trees.Alternative.Data] = arg.tree match {
    case Trees.Alternative(patterns) => Some(patterns.map(Pattern(_)))
    case _ => None
  }

  def unapplyTypeTest(arg: Impl)(implicit ctx: Context): Option[trees.TypeTest.Data] = arg.tree match {
    case Trees.Typed(_, tpt) => Some(TypeTree(tpt))
    case _ => None
  }

  private[tasty] class Impl(val tree: tpd.Tree) extends trees.Pattern with Positioned {

    def tpe: types.Type = Type(tree.tpe)

    override def toString: String = "Pattern"
  }
}

