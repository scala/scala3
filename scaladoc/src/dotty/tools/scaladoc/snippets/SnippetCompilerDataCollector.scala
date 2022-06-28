package dotty.tools.scaladoc
package snippets

import scala.quoted._
import dotty.tools.scaladoc.tasty.SymOps._
import dotty.tools.dotc.core._
import dotty.tools.dotc.util.{ SourceFile => CSourceFile, NoSource }

object SnippetCompilerDataCollector:

  def getSourceFile(using Quotes)(sym: quotes.reflect.Symbol): CSourceFile =
    given ctx: Contexts.Context = quotes.asInstanceOf[scala.quoted.runtime.impl.QuotesImpl].ctx
    sym match
      case csym: Symbols.Symbol => csym.source(using ctx)
      case _ =>
        report.warning(s"Can't cast symbol $sym to compiler symbol. This is a bug of snippet compiler, please create an issue on dotty repository.")
        NoSource

  def getSnippetCompilerData(using Quotes)(sym: quotes.reflect.Symbol, originalSym: quotes.reflect.Symbol): SnippetCompilerData =
    val packageName = sym.packageName
    SnippetCompilerData(packageName, position(hackGetPositionOfDocstring(originalSym)))

  private def position(using Quotes)(p: Option[quotes.reflect.Position]): SnippetCompilerData.Position =
    p.fold(SnippetCompilerData.Position(0, 0))(p => SnippetCompilerData.Position(p.startLine - 1, p.startColumn))

  private def hackGetPositionOfDocstring(using Quotes)(s: quotes.reflect.Symbol): Option[quotes.reflect.Position] =
    import dotty.tools.dotc.core.Comments.CommentsContext
    import dotty.tools.dotc
    given ctx: Contexts.Context = quotes.asInstanceOf[scala.quoted.runtime.impl.QuotesImpl].ctx
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
        ).asInstanceOf[quotes.reflect.Position]
      }
    }
