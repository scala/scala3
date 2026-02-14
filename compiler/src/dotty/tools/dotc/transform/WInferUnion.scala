package dotty.tools.dotc.transform

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.tpd.InferredTypeTree
import dotty.tools.dotc.core.Contexts.{Context, ctx}
import dotty.tools.dotc.core.Decorators.em
import dotty.tools.dotc.core.Types.OrType
import dotty.tools.dotc.report
import dotty.tools.dotc.reporting.ErrorMessageID
import dotty.tools.dotc.reporting.InferUnionWarning
import dotty.tools.dotc.transform.MegaPhase.MiniPhase

class WInferUnion extends MiniPhase {

  override def phaseName: String = WInferUnion.name

  override def description: String = WInferUnion.description

  override def isEnabled(using Context): Boolean = ctx.settings.Whas.inferUnion

  override def transformTypeApply(tree: tpd.TypeApply)(using Context): tpd.Tree =
    val inferredOrTypes = tree.args.find: tpt =>
      tpt.isInstanceOf[InferredTypeTree] && tpt.tpe.stripTypeVar.isInstanceOf[OrType]
    inferredOrTypes.foreach: tpt =>
      report.warning(
        InferUnionWarning(tpt.tpe.stripTypeVar),
        tpt.srcPos
      )
    tree
}

object WInferUnion:
  val name = "Winfer-union"
  val description = "check for type arguments inferred as union types"
