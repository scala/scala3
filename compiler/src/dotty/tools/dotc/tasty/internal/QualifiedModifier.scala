package dotty.tools.dotc.tasty
package internal

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags

import scala.tasty.modifiers

object QualifiedModifier {

  def apply(tree: tpd.DefTree)(implicit ctx: Context): Option[modifiers.Qualified] =
    if (tree.symbol.privateWithin.exists) Some(new Impl(tree)) else None

  def unapplyQualifiedPrivate(arg: Impl)(implicit ctx: Context): Option[modifiers.QualifiedPrivate.Data] = {
    if (arg.tree.symbol.is(Flags.Protected)) None
    else Some(Type(arg.tree.symbol.privateWithin.typeRef))
  }

  def unapplyQualifiedProtected(arg: Impl)(implicit ctx: Context): Option[modifiers.QualifiedProtected.Data] = {
    if (arg.tree.symbol.is(Flags.Protected)) Some(Type(arg.tree.symbol.privateWithin.typeRef))
    else None
  }

  private[tasty] class Impl(val tree: tpd.DefTree) extends modifiers.Qualified {
    override def toString: String = "Qualified"
  }
}
