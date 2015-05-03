package dotty.tools.dotc
package transform

import core._
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
 */
class ExpandPrivate extends MiniPhaseTransform with IdentityDenotTransformer { thisTransform =>
  import ast.tpd._

  override def phaseName: String = "expandPrivate"

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
}
