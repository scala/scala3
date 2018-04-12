package dotty.tools.dotc.tasty.internal

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.Trees
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Decorators.sourcePos

import scala.tasty.modifiers

object QualifiedModifier {

  def apply(tree: tpd.DefTree)(implicit ctx: Context): modifiers.Modifier = Impl(tree, ctx)

  def unapplyQualifiedPrivate(mod: modifiers.Modifier): Option[modifiers.QualifiedPrivate.Data] = mod match {
    case Impl(tree, ctx) =>
      implicit val c = ctx
      ???
    case _ => None
  }

  def unapplyQualifiedProtected(mod: modifiers.Modifier): Option[modifiers.QualifiedProtected.Data] = mod match {
    case Impl(tree, ctx) =>
      ???
    case _ => None
  }

  private case class Impl(tree: tpd.DefTree, ctx: Context) extends modifiers.Modifier with Positioned {

    override def toString: String = this match {
      case _ => s"###"
    }
  }
}
