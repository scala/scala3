package dotty.tools.dotc
package transform

import MegaPhase._
import core.DenotTransformers._
import core.Symbols._
import core.Contexts._
import core.Types._
import core.Decorators._
import core.Flags._
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
class LiftTry extends MiniPhase, IdentityDenotTransformer, RecordStackChange { thisPhase =>
  import ast.tpd._

  override def phaseName: String = LiftTry.name

  override def description: String = LiftTry.description

  private var NeedLift: Store.Location[Boolean] = _
  private def needLift(using Context): Boolean = ctx.store(NeedLift)

  override def initContext(ctx: FreshContext): Unit =
    NeedLift = ctx.addLocation(false)

  private def liftingCtx(p: Boolean)(using Context): Context =
    if (needLift == p) ctx else ctx.fresh.updateStore(NeedLift, p)

  protected def stackChange(using Context): Context = liftingCtx(true)

  override def prepareForDefDef(tree: DefDef)(using Context): Context =
    liftingCtx(false)

  override def prepareForTemplate(tree: Template)(using Context): Context =
    liftingCtx(false)

  override def transformTry(tree: Try)(using Context): Tree =
    if (needLift && tree.cases.nonEmpty) {
      report.debuglog(i"lifting tree at ${tree.span}, current owner = ${ctx.owner}")
      val fn = newSymbol(
        ctx.owner, LiftedTreeName.fresh(), Synthetic | Method,
        MethodType(Nil, tree.tpe.widenIfUnstable), coord = tree.span)
      tree.changeOwnerAfter(ctx.owner, fn, thisPhase)
      Block(DefDef(fn, tree) :: Nil, ref(fn).appliedToNone)
    }
    else tree
}
object LiftTry:
  val name = "liftTry"
  val description: String = "lift any try that might be executed on a non-empty expression stack"
