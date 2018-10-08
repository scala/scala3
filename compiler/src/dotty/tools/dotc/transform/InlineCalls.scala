package dotty.tools.dotc.transform

import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.typer.Inliner


class InlineCalls extends MacroTransform { thisPhase =>
  import tpd._

  override def phaseName: String = InlineCalls.name

  override def run(implicit ctx: Context): Unit =
    if (!ctx.settings.YnoInline.value) super.run

  override def transformPhase(implicit ctx: Context): Phase = thisPhase.next

  protected def newTransformer(implicit ctx: Context): Transformer =
    new InlineCallsTransformer

  class InlineCallsTransformer extends Transformer {
    override def transform(tree: Tree)(implicit ctx: Context): Tree = tree match {
      case _: RefTree | _: GenericApply[_] if Inliner.isInlineable(tree) && !ctx.reporter.hasErrors =>
        normalize(transform(Inliner.inlineCall(tree, tree.tpe.widen)))
      case _: MemberDef =>
        val newTree = super.transform(tree)
        newTree.symbol.defTree = newTree // update tree set in PostTyper or set for inlined members
        newTree
      case _ =>
        super.transform(tree)
    }
  }

  def normalize(tree: Tree)(implicit ctx: Context): Tree = tree match {
    case Inlined(call, bindings, expansion) if !call.isEmpty =>
      // TODO: Normalize when Inlined is created. We never use the full call, we always collapse it first.
      // Leave only a call trace consisting of
      //  - a reference to the top-level class from which the call was inlined,
      //  - the call's position
      // in the call field of an Inlined node.
      // The trace has enough info to completely reconstruct positions.
      // The minimization is done for two reasons:
      //  1. To save space (calls might contain large inline arguments, which would otherwise
      //     be duplicated
      //  2. To enable correct pickling (calls can share symbols with the inlined code, which
      //     would trigger an assertion when pickling).
      val callTrace = Ident(call.symbol.topLevelClass.typeRef).withPos(call.pos)
      cpy.Inlined(tree)(callTrace, bindings, expansion)
    case _ =>
      tree
  }
}

object InlineCalls {
  final val name = "inlineCalls"
}
