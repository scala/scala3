package dotty.tools.dotc.transform

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.typer.Docstrings

class CookComments extends MegaPhase.MiniPhase {
  override def phaseName: String = "cookComments"

  override def transformTypeDef(tree: tpd.TypeDef)(implicit ctx: Context): tpd.Tree = {
    if (ctx.settings.YcookComments.value && tree.isClassDef) {
      val cls = tree.symbol
      val cookingCtx = ctx.localContext(tree, cls).setNewScope
      val template = tree.rhs.asInstanceOf[tpd.Template]
      val owner = template.self.symbol.orElse(cls)

      template.body.foreach { stat =>
        Docstrings.cookComment(stat.symbol, owner)(cookingCtx)
      }

      Docstrings.cookComment(cls, cls)(cookingCtx)
    }

    tree
  }
}


