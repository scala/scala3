package dotty.tools.dotc
package transform

import core._
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.DenotTransformers.{SymTransformer, IdentityDenotTransformer}
import Contexts.Context
import Symbols._
import Scopes._
import Flags._
import StdNames._
import SymDenotations._
import Types._
import collection.mutable
import TreeTransforms._
import Decorators._
import ast.Trees._
import TreeTransforms._

/** Make private term members that are accessed from another class
 *  non-private by resetting the Private flag and expanding their name.
 *
 *  Also, make non-private any private parameter forwarders that forward to an inherited
 *  public or protected parameter accessor with the same name as the forwarder.
 *  This is necessary since private methods are not allowed to have the same name
 *  as inherited public ones.
 *
 *  See discussion in https://github.com/lampepfl/dotty/pull/784
 *  and https://github.com/lampepfl/dotty/issues/783
 */
class ExpandPrivate extends MiniPhaseTransform with IdentityDenotTransformer { thisTransform =>
  import ast.tpd._

  override def phaseName: String = "expandPrivate"

  override def checkPostCondition(tree: Tree)(implicit ctx: Context): Unit = {
    tree match {
      case t: DefDef =>
        val sym = t.symbol
        def hasWeakerAccess(other: Symbol) = {
          // public > protected > /* default */ > private
          if (sym.is(Private)) other.is(Private)
          else if (sym.is(Protected)) other.is(Protected | Private)
          else true // sym is private
        }
        val fail = sym.allOverriddenSymbols.findSymbol(x => !hasWeakerAccess(x))
        if (fail.exists) {
          assert(false, i"${sym.showFullName} has weaker access that superclass method ${fail.showFullName}")
        }
      case _ =>
    }
  }

  /** Make private terms accessed from different classes non-private.
   *  Note: this happens also for accesses between class and linked module class.
   *  If we change the scheme at one point to make static module class computations
   *  static members of the companion class, we should tighten the condition below.
   */
  private def ensurePrivateAccessible(d: SymDenotation)(implicit ctx: Context) =
    if (d.is(PrivateTerm) && d.owner != ctx.owner.enclosingClass)
      d.ensureNotPrivate.installAfter(thisTransform)

  override def transformIdent(tree: Ident)(implicit ctx: Context, info: TransformerInfo) = {
    ensurePrivateAccessible(tree.symbol)
    tree
  }

  override def transformSelect(tree: Select)(implicit ctx: Context, info: TransformerInfo) = {
    ensurePrivateAccessible(tree.symbol)
    tree
  }

  override def transformDefDef(tree: DefDef)(implicit ctx: Context, info: TransformerInfo) = {
    val sym = tree.symbol
    tree.rhs match {
      case Apply(sel @ Select(_: Super, _), _)
      if sym.is(PrivateParamAccessor) && sel.symbol.is(ParamAccessor) && sym.name == sel.symbol.name =>
        sym.ensureNotPrivate.installAfter(thisTransform)
      case _ =>
    }
    tree
  }
}
