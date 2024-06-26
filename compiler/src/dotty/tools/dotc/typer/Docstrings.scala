package dotty.tools
package dotc
package typer

import core.*
import Contexts.*, Symbols.*, Decorators.*, Comments.{_, given}
import ast.tpd

object Docstrings {

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
  def cookComment(sym: Symbol, owner: Symbol)(using Context): Option[Comment] =
    ctx.docCtx.flatMap { docCtx =>
      expand(sym, owner)(using ctx)(using docCtx)
    }

  private def expand(sym: Symbol, owner: Symbol)(using Context)(using docCtx: ContextDocstrings): Option[Comment] =
    docCtx.docstring(sym).flatMap {
      case cmt if cmt.isExpanded =>
        Some(cmt)
      case _ =>
        expandComment(sym).map { expanded =>
          val typedUsecases = expanded.usecases.map { usecase =>
            ctx.typer.enterSymbol(ctx.typer.createSymbol(usecase.untpdCode))
            ctx.typer.typedStats(usecase.untpdCode :: Nil, owner)._1 match {
              case List(df: tpd.DefDef) =>
                usecase.typed(df)
              case _ =>
                report.error(em"`@usecase` was not a valid definition", ctx.source.atSpan(usecase.codePos))
                usecase
            }
          }

          val commentWithUsecases = expanded.copy(usecases = typedUsecases)
          docCtx.addDocstring(sym, Some(commentWithUsecases))
          commentWithUsecases
        }
    }

  private def expandComment(sym: Symbol, owner: Symbol, comment: Comment)(using Context)(using docCtx: ContextDocstrings): Comment = {
    val tplExp = docCtx.templateExpander
    tplExp.defineVariables(sym)
    val newComment = comment.expand(tplExp.expandedDocComment(sym, owner, _))
    docCtx.addDocstring(sym, Some(newComment))
    newComment
  }

  private def expandComment(sym: Symbol)(using Context)(using docCtx: ContextDocstrings): Option[Comment] =
    if (sym eq NoSymbol) None
    else
      for {
        cmt <- docCtx.docstring(sym) if !cmt.isExpanded
        _ = expandComment(sym.owner)
      }
      yield expandComment(sym, sym.owner, cmt)
}
