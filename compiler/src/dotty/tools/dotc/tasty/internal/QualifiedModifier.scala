package dotty.tools.dotc.tasty
package internal

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags

import scala.tasty.modifiers

object QualifiedModifier {

  def apply(tree: tpd.DefTree)(implicit ctx: Context): Option[modifiers.Modifier] =
    if (tree.symbol.privateWithin.exists) Some(Impl(tree, ctx)) else None

  def unapplyQualifiedPrivate(mod: modifiers.Modifier): Option[modifiers.QualifiedPrivate.Data] = mod match {
    case Impl(tree, ctx) =>
      implicit val ctx_ = ctx
      if (tree.symbol.is(Flags.Protected)) None
      else Some(Type(tree.symbol.privateWithin.typeRef))
    case _ => None
  }

  def unapplyQualifiedProtected(mod: modifiers.Modifier): Option[modifiers.QualifiedProtected.Data] = mod match {
    case Impl(tree, ctx) =>
      implicit val ctx_ = ctx
      if (tree.symbol.is(Flags.Protected)) Some(Type(tree.symbol.privateWithin.typeRef))
      else None
    case _ => None
  }

  private case class Impl(tree: tpd.DefTree, ctx: Context) extends modifiers.Modifier with Positioned {

    override def toString: String = {
      import Toolbox.extractor
      this match {
        case modifiers.QualifiedPrivate(tpe) => s"QualifiedPrivate($tpe)"
        case modifiers.QualifiedProtected(tpe) => s"QualifiedProtected($tpe)"
      }
    }
  }
}
