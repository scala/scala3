package dottyBench.tools.dotc.transform

import dottyBench.tools.dotc.ast.tpd
import dottyBench.tools.dotc.core.Contexts._
import dottyBench.tools.dotc.core.ContextOps._
import dottyBench.tools.dotc.typer.Docstrings

class CookComments extends MegaPhase.MiniPhase {
  override def phaseName: String = "cookComments"

  override def transformTypeDef(tree: tpd.TypeDef)(using Ctx, CState): tpd.Tree = {
    if (ctx.settings.YcookComments.value && tree.isClassDef) {
      val cls = tree.symbol
      val cookingCtx = ctx.localContext(tree, cls).setNewScope
      val template = tree.rhs.asInstanceOf[tpd.Template]
      val owner = template.self.symbol.orElse(cls)

      template.body.foreach { stat =>
        Docstrings.cookComment(stat.symbol, owner)(using cookingCtx)
      }

      Docstrings.cookComment(cls, cls)(using cookingCtx)
    }

    tree
  }
}


