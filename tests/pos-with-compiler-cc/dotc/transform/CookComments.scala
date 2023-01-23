package dotty.tools.dotc.transform

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.ContextOps._
import dotty.tools.dotc.typer.Docstrings

class CookComments extends MegaPhase.MiniPhase {

  override def phaseName: String = CookComments.name

  override def description: String = CookComments.description

  override def transformTypeDef(tree: tpd.TypeDef)(using Context): tpd.Tree = {
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

object CookComments:
  val name = "cookComments"
  val description: String = "cook the comments: expand variables, doc, etc."
