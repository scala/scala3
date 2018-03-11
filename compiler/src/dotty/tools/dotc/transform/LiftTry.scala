package dotty.tools.dotc
package transform

import MegaPhase._
import core.DenotTransformers._
import core.Symbols._
import core.Contexts._
import core.Types._
import core.Flags._
import core.Decorators._
import core.NameKinds.LiftedTreeName
import NonLocalReturns._
import util.Store

/** Lifts try's that might be executed on non-empty expression stacks
 *  to their own methods. I.e.
 *
 *      try body catch handler
 *
 *  is lifted to
 *
 *      { def liftedTree$n() = try body catch handler; liftedTree$n() }
 */
class LiftTry extends MiniPhase with IdentityDenotTransformer { thisPhase =>
  import ast.tpd._

  /** the following two members override abstract members in Transform */
  val phaseName: String = "liftTry"

  private var NeedLift: Store.Location[Boolean] = _
  private def needLift(implicit ctx: Context): Boolean = ctx.store(NeedLift)

  override def initContext(ctx: FreshContext) =
    NeedLift = ctx.addLocation(false)

  private def liftingCtx(p: Boolean)(implicit ctx: Context) =
    if (needLift == p) ctx else ctx.fresh.updateStore(NeedLift, p)

  override def prepareForApply(tree: Apply)(implicit ctx: Context) =
    if (tree.fun.symbol.is(Label)) ctx
    else liftingCtx(true)

  override def prepareForValDef(tree: ValDef)(implicit ctx: Context) =
    if (!tree.symbol.exists  ||
        tree.symbol.isSelfSym ||
        tree.symbol.owner == ctx.owner.enclosingMethod) ctx
    else liftingCtx(true)

  override def prepareForAssign(tree: Assign)(implicit ctx: Context) =
    if (tree.lhs.symbol.maybeOwner == ctx.owner.enclosingMethod) ctx
    else liftingCtx(true)

  override def prepareForReturn(tree: Return)(implicit ctx: Context) =
    if (!isNonLocalReturn(tree)) ctx
    else liftingCtx(true)

  override def prepareForTemplate(tree: Template)(implicit ctx: Context) =
    liftingCtx(false)

  override def transformTry(tree: Try)(implicit ctx: Context): Tree =
    if (needLift) {
      ctx.debuglog(i"lifting tree at ${tree.pos}, current owner = ${ctx.owner}")
      val fn = ctx.newSymbol(
        ctx.owner, LiftedTreeName.fresh(), Synthetic | Method,
        MethodType(Nil, tree.tpe.widenIfUnstable), coord = tree.coord)
      tree.changeOwnerAfter(ctx.owner, fn, thisPhase)
      Block(DefDef(fn, tree) :: Nil, ref(fn).appliedToNone)
    }
    else tree
}
