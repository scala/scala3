package dotty.tools
package repl

import dotc.reporting.diagnostic.MessageContainer
import dotc.core.Contexts.Context
import dotc.parsing.Parsers
import dotc.util.SourceFile
import dotc.ast.untpd
import dotc.reporting._

sealed trait ParseResult
case class Parsed(sourceCode: String, trees: Seq[untpd.Tree]) extends ParseResult
case class SyntaxErrors(errors: Seq[MessageContainer], ctx: Context) extends ParseResult
case object Newline extends ParseResult

sealed trait Command extends ParseResult
case class UnknownCommand(cmd: String) extends Command
case class Load(path: String) extends Command
case object Reset extends Command
case object Quit extends Command
case object Help extends Command {
  val text =
    """The REPL has several commands available:
      |
      |:help                    print this summary or command-specific help
      |:load <path>             interpret lines in a file
      |:quit                    exit the interpreter
      |:reset                   reset the repl to its initial state, forgetting all session entries
    """.stripMargin
}

object ParseResult {

  private[this] val CommandExtract = """(:[\S]+)\s*([\S]+)?""".r

  def apply(sourceCode: String)(implicit ctx: Context): ParseResult = sourceCode match {
    case "" => Newline
    case CommandExtract(cmd, arg) => cmd match {
      case ":quit"  => Quit
      case ":help"  => Help
      case ":reset" => Reset
      case ":load"  => Load(sourceCode.drop(5).trim)
      case _        => UnknownCommand(cmd)
    }
    case _ => {
      val reporter = new StoreReporter(null) with UniqueMessagePositions with HideNonSensicalMessages
      implicit val myCtx = ctx.fresh.setReporter(reporter)

      val source = new SourceFile("<console>", sourceCode.toCharArray)
      val parser = new Parsers.Parser(source)(myCtx)

      val (_, stats) = parser.templateStatSeq

      if (reporter.hasErrors) SyntaxErrors(reporter.removeBufferedMessages, myCtx)
      else Parsed(sourceCode, stats)
    }
  }

  def isIncomplete(sourceCode: String)(implicit ctx: Context): Boolean = sourceCode match {
    case "" => false
    case CommandExtract(_) => false
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
