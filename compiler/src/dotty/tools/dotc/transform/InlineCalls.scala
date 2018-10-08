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
        transform(Inliner.inlineCall(tree, tree.tpe.widen))
      case _ =>
        super.transform(tree)
    }

  }
}

object InlineCalls {
  final val name = "inlineCalls"
}
