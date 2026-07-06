package dotty.tools
package dotc
package typer

import ast.*
import Trees.*
import core.*, Contexts.*
import Constants.*
import parsing.Parsers.Parser
import parsing.Tokens
import util.SourceFile
import dotty.tools.dotc.reporting.{Diagnostic, Reporter}
import dotty.tools.dotc.printing.Formatting.ShownDef.Shown.runCtxShow


trait SpecStrings { this: Typer =>
  import tpd.*

  private def reportAsWarnings(reporter: Reporter)(using Context) =
    reporter.mapBufferedMessages:
      case err: Diagnostic.Error => Diagnostic.Warning(err.msg, err.pos)
      case dia => dia
    reporter.flush()

  private def extractBackquoted(strLit: untpd.Literal, startFrom: Int = 0)(using Context): List[untpd.Tree] =
    val Literal(Constant(str: String)) = strLit.runtimeChecked
    val opening = str.indexOf('`', from = startFrom)
    if opening >= 0 then
      val closing = str.indexOf('`', from = opening + 1)
      if closing >= 0 then
        val startOffset = strLit.span.start
        val nestedCtx = ctx.fresh.setNewTyperState()
        val parser = new Parser(
            ctx.compilationUnit.source,
            startFrom = startOffset + opening + 1,
            limit = startOffset + closing)(using nestedCtx)
        println(s"str starting at ${ctx.compilationUnit.source.content().slice(startOffset, startOffset + 10).mkString("[",",","]")}")
        println(s"starting at ${ctx.compilationUnit.source.content().slice(startOffset + opening + 1, startOffset + opening + 11).mkString("[",",","]")}")
        val expr = parser.expr()
        parser.accept(Tokens.EOF)
        if nestedCtx.reporter.hasErrors() then
          reportAsWarnings(nestedCtx.reporter)
          extractBackquoted(strLit, closing + 1)
        else
          val prefix = Literal(Constant(str.take(opening + 1)))
              .withSpan(strLit.span.withEnd(startOffset + opening + 1))
          val suffix = Literal(Constant(str.drop(closing)))
              .withSpan(strLit.span.withStart(startOffset + closing))
          untpd.Thicket(prefix, expr) :: extractBackquoted(suffix, 1)
      else strLit :: Nil
    else strLit :: Nil

  def processSpecSegment(tree: untpd.Tree)(using Context): List[untpd.Tree] =
    tree match
    case Thicket(strLit :: expr :: Nil) =>
      processSpecSegment(strLit).runtimeChecked match
        case _ :: Nil => tree :: Nil  // no change
        case prefix :+ last => prefix :+ untpd.Thicket(last :: expr :: Nil)
    case strLit: Literal =>
      extractBackquoted(strLit, 0)
}

