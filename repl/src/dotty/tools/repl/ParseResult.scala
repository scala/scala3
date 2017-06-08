package dotty.tools
package repl

import dotc.reporting.diagnostic.MessageContainer
import dotc.core.Contexts.Context
import dotc.parsing.Parsers
import dotc.util.SourceFile
import dotc.ast.untpd
import dotc.reporting._

sealed trait ParseResult
case class Trees(xs: Seq[untpd.Tree]) extends ParseResult
case class SyntaxErrors(errors: Seq[MessageContainer], ctx: Context) extends ParseResult
case object Newline extends ParseResult

sealed trait Command extends ParseResult
case object Quit extends Command

object ParseResult {

  def apply(sourceCode: String)(implicit ctx: Context): ParseResult = sourceCode match {
    case "" => Newline
    case ":quit" => Quit
    case _ => {
      val reporter = new StoreReporter(null) with UniqueMessagePositions with HideNonSensicalMessages
      implicit val myCtx = ctx.fresh.setReporter(reporter)

      val source = new SourceFile("<console>", sourceCode.toCharArray)
      val parser = new Parsers.Parser(source)(myCtx)

      val (_, stats) = parser.templateStatSeq

      if (reporter.hasErrors) SyntaxErrors(reporter.removeBufferedMessages, myCtx)
      else Trees(stats)
    }
  }

  def isIncomplete(sourceCode: String)(implicit ctx: Context): Boolean = sourceCode match {
    case "" | ":quit" => false
    case _ => {
      val reporter = new StoreReporter(null) with HideNonSensicalMessages
      var needsMore = false
      reporter.withIncompleteHandler(_ => _ => needsMore = true) {
        implicit val myCtx = ctx.fresh.setReporter(reporter)
        val source = new SourceFile("<console>", sourceCode.toCharArray)
        val parser = new Parsers.Parser(source)(myCtx)
        parser.templateStatSeq


        !reporter.hasErrors && needsMore
      }
    }
  }
}
