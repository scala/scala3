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
 *
 *  However, don't lift try's without catch expressions (try-finally).
 *  Lifting is needed only for try-catch expressions that are evaluated in a context
 *  where the stack might not be empty. `finally` does not attempt to continue evaluation
 *  after an exception, so the fact that values on the stack are 'lost' does not matter
 *  (copied from https://github.com/scala/scala/pull/922).
 */
class LiftTry extends MiniPhase with IdentityDenotTransformer { thisPhase =>
  import ast.tpd._

  val phaseName: String = "liftTry"

  // See tests/run/t2333.scala for an example where running after the group of LazyVals matters
  override def runsAfterGroupsOf: Set[String] = Set(LazyVals.name)

  private var NeedLift: Store.Location[Boolean] = _
  private def needLift(implicit ctx: Context): Boolean = ctx.store(NeedLift)

  override def initContext(ctx: FreshContext): Unit =
    NeedLift = ctx.addLocation(false)

  private def liftingCtx(p: Boolean)(implicit ctx: Context) =
    if (needLift == p) ctx else ctx.fresh.updateStore(NeedLift, p)

  override def prepareForApply(tree: Apply)(implicit ctx: Context): Context =
    liftingCtx(true)

  override def prepareForValDef(tree: ValDef)(implicit ctx: Context): Context =
    if (!tree.symbol.exists  ||
        tree.symbol.isSelfSym ||
        tree.symbol.owner == ctx.owner.enclosingMethod) ctx
    else liftingCtx(true)

  override def prepareForAssign(tree: Assign)(implicit ctx: Context): Context =
    if (tree.lhs.symbol.maybeOwner == ctx.owner.enclosingMethod) ctx
    else liftingCtx(true)

  override def prepareForReturn(tree: Return)(implicit ctx: Context): Context =
    if (!isNonLocalReturn(tree)) ctx
    else liftingCtx(true)

  override def prepareForTemplate(tree: Template)(implicit ctx: Context): Context =
    liftingCtx(false)

  override def transformTry(tree: Try)(implicit ctx: Context): Tree =
    if (needLift && tree.cases.nonEmpty) {
      ctx.debuglog(i"lifting tree at ${tree.span}, current owner = ${ctx.owner}")
      val fn = ctx.newSymbol(
        ctx.owner, LiftedTreeName.fresh(), Synthetic | Method,
        MethodType(Nil, tree.tpe.widenIfUnstable), coord = tree.span)
      tree.changeOwnerAfter(ctx.owner, fn, thisPhase)
      Block(DefDef(fn, tree) :: Nil, ref(fn).appliedToNone)
    }
    else tree
}
