package dotty.tools
package dotc
package typer

import core._
import Contexts._, Symbols._, Decorators._, Comments._
import util.Positions._
import ast.tpd

trait Docstrings { self: Typer =>

  /**
   * Expands or cooks the documentation for `sym` in class `owner`.
   * The expanded comment will directly replace the original comment in the doc context.
   *
   * The expansion registers `@define` sections, and will replace `@inheritdoc` and variable
   * occurrences in the comments.
   *
   * If the doc comments contain `@usecase` sections, they will be typed.
   *
   * @param sym   The symbol for which the comment is being cooked.
   * @param owner The class for which comments are being cooked.
   */
  def cookComment(sym: Symbol, owner: Symbol)(implicit ctx: Context): Unit = {
    for {
      docbase <- ctx.docCtx
      comment <- docbase.docstring(sym).filter(!_.isExpanded)
    } {
      expandParentDocs(sym)
      docbase.docstring(sym).get.usecases.foreach { usecase =>
        enterSymbol(createSymbol(usecase.untpdCode))
        typedStats(usecase.untpdCode :: Nil, owner) match {
          case List(df: tpd.DefDef) => usecase.tpdCode = df
          case _ => ctx.error("`@usecase` was not a valid definition", usecase.codePos)
        }
      }
    }
  }

  private def expandParentDocs(sym: Symbol)(implicit ctx: Context): Unit =
    ctx.docCtx.foreach { docCtx =>
      docCtx.docstring(sym).foreach { cmt =>
        def expandDoc(owner: Symbol): Unit = if (!cmt.isExpanded) {
          val tplExp = docCtx.templateExpander
          tplExp.defineVariables(sym)

          val newCmt = cmt
            .expand(tplExp.expandedDocComment(sym, owner, _))
            .withUsecases

          docCtx.addDocstring(sym, Some(newCmt))
        }

        if (sym ne NoSymbol) {
          expandParentDocs(sym.owner)
          expandDoc(sym.owner)
        }
      }
    }
}
