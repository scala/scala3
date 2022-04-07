package dotty.tools.scaladoc
package snippets

import scala.quoted._
import dotty.tools.scaladoc.tasty.SymOps._
import dotty.tools.dotc.core._
import dotty.tools.dotc.util.{ SourceFile => CSourceFile, NoSource }

class SnippetCompilerDataCollector[Q <: Quotes](val qctx: Q):
  import qctx.reflect._
  given qctx.type = qctx

  def getSourceFile(sym: Symbol): CSourceFile =
    given ctx: Contexts.Context = qctx.asInstanceOf[scala.quoted.runtime.impl.QuotesImpl].ctx
    sym match
      case csym: Symbols.Symbol => csym.source(using ctx)
      case _ =>
        report.warning(s"Can't cast symbol $sym to compiler symbol. This is a bug of snippet compiler, please create an issue on dotty repository.")
        NoSource

  def getSnippetCompilerData(sym: Symbol, originalSym: Symbol): SnippetCompilerData =
    val packageName = sym.packageName
    SnippetCompilerData(packageName, position(hackGetPositionOfDocstring(using qctx)(originalSym)))

  private def position(p: Option[qctx.reflect.Position]): SnippetCompilerData.Position =
    p.fold(SnippetCompilerData.Position(0, 0))(p => SnippetCompilerData.Position(p.startLine - 1, p.startColumn))

  private def hackGetPositionOfDocstring(using Quotes)(s: qctx.reflect.Symbol): Option[qctx.reflect.Position] =
    import dotty.tools.dotc.core.Comments.CommentsContext
    import dotty.tools.dotc
    given ctx: Contexts.Context = qctx.asInstanceOf[scala.quoted.runtime.impl.QuotesImpl].ctx
    val docCtx = ctx.docCtx.getOrElse {
      throw new RuntimeException(
        "DocCtx could not be found and documentations are unavailable. This is a compiler-internal error."
      )
    }
    s.pos.flatMap { pos =>
      docCtx.docstring(s.asInstanceOf[Symbols.Symbol]).map { docstring =>
        dotty.tools.dotc.util.SourcePosition(
          pos.sourceFile.asInstanceOf[dotty.tools.dotc.util.SourceFile],
          docstring.span
        ).asInstanceOf[qctx.reflect.Position]
      }
    }
