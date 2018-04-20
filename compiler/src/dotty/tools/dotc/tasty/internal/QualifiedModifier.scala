package dotty.tools.dotc.tasty
package internal

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags

import scala.tasty.modifiers

object QualifiedModifier {

  def apply(tree: tpd.DefTree)(implicit ctx: Context): Option[modifiers.Modifier] =
    if (tree.symbol.privateWithin.exists) Some(new Impl(tree)) else None

  def unapplyQualifiedPrivate(arg: Impl): Option[modifiers.QualifiedPrivate.Data] = {
    implicit val ctx: Context = arg.ctx
    if (arg.tree.symbol.is(Flags.Protected)) None
    else Some(Type(arg.tree.symbol.privateWithin.typeRef))
  }

  def unapplyQualifiedProtected(arg: Impl): Option[modifiers.QualifiedProtected.Data] = {
    implicit val ctx: Context = arg.ctx
    if (arg.tree.symbol.is(Flags.Protected)) Some(Type(arg.tree.symbol.privateWithin.typeRef))
    else None
  }

  private[tasty] class Impl(val tree: tpd.DefTree)(implicit val ctx: Context) extends modifiers.Modifier with Positioned {

    override def toString: String = {
      import Toolbox.extractor
      this match {
        case modifiers.QualifiedPrivate(tpe) => s"QualifiedPrivate($tpe)"
        case modifiers.QualifiedProtected(tpe) => s"QualifiedProtected($tpe)"
      }
    }
  }
}
