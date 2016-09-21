package dotty.tools
package dotc
package typer

import core._
import Contexts._, Symbols._, Decorators._, Comments._
import util.Positions._
import ast.tpd

trait Docstrings { self: Typer =>

  /** The Docstrings typer will handle the expansion of `@define` and
    * `@inheritdoc` if there is a `DocContext` present as a property in the
    * supplied `ctx`.
    *
    * It will also type any `@usecase` available in function definitions.
    */
  def cookComments(syms: List[Symbol], owner: Symbol)(implicit ctx: Context): Unit =
    ctx.docCtx.foreach { docbase =>
      val relevantSyms = syms.filter(docbase.docstring(_).isDefined)
      relevantSyms.foreach { sym =>
        expandParentDocs(sym)
        val usecases = docbase.docstring(sym).map(_.usecases).getOrElse(Nil)

        usecases.foreach { usecase =>
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
