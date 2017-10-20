package dotty.tools.dotc
package transform

import TreeTransforms._
import core.DenotTransformers._
import core.Symbols._
import core.Contexts._
import core.Types._
import core.Flags._
import core.Decorators._
import core.NameKinds.LiftedTreeName
import NonLocalReturns._

/** Lifts try's that might be executed on non-empty expression stacks
 *  to their own methods. I.e.
 *
 *      try body catch handler
 *
 *  is lifted to
 *
 *      { def liftedTree$n() = try body catch handler; liftedTree$n() }
 */
class LiftTry extends MiniPhase with IdentityDenotTransformer { thisTransform =>
  import ast.tpd._

  /** the following two members override abstract members in Transform */
  val phaseName: String = "liftTry"

  val treeTransform = new Transform(needLift = false)
  val liftingTransform = new Transform(needLift = true)

  class Transform(needLift: Boolean) extends TreeTransform {
    def phase = thisTransform

    override def prepareForApply(tree: Apply)(implicit ctx: Context) =
      if (tree.fun.symbol.is(Label)) this
      else liftingTransform

    override def prepareForValDef(tree: ValDef)(implicit ctx: Context) =
      if (!tree.symbol.exists  ||
          tree.symbol.isSelfSym ||
          tree.symbol.owner == ctx.owner.enclosingMethod) this
      else liftingTransform

    override def prepareForAssign(tree: Assign)(implicit ctx: Context) =
      if (tree.lhs.symbol.maybeOwner == ctx.owner.enclosingMethod) this
      else liftingTransform

    override def prepareForReturn(tree: Return)(implicit ctx: Context) =
      if (!isNonLocalReturn(tree)) this
      else liftingTransform

    override def prepareForTemplate(tree: Template)(implicit ctx: Context) =
      treeTransform

    override def transformTry(tree: Try)(implicit ctx: Context, info: TransformerInfo): Tree =
      if (needLift) {
        ctx.debuglog(i"lifting tree at ${tree.pos}, current owner = ${ctx.owner}")
        val fn = ctx.newSymbol(
          ctx.owner, LiftedTreeName.fresh(), Synthetic | Method,
          MethodType(Nil, tree.tpe.widenIfUnstable), coord = tree.pos)
        tree.changeOwnerAfter(ctx.owner, fn, thisTransform)
        Block(DefDef(fn, tree) :: Nil, ref(fn).appliedToNone)
      }
      else tree
  }
}
