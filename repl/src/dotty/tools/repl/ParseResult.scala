package dotty.tools
package repl

import dotc.reporting.diagnostic.MessageContainer
import dotc.core.Contexts.Context
import dotc.parsing.Parsers.Parser
import dotc.util.SourceFile
import dotc.ast.untpd
import dotc.reporting._

import results._

sealed trait ParseResult
case class Parsed(sourceCode: String, trees: List[untpd.Tree]) extends ParseResult
case class SyntaxErrors(errors: List[MessageContainer], trees: List[untpd.Tree]) extends ParseResult
case object Newline extends ParseResult
case object SigKill extends ParseResult

sealed trait Command extends ParseResult
case class UnknownCommand(cmd: String) extends Command
case class Load(path: String) extends Command
object Load {
  val command = ":load"
}
case class Type(expr: String) extends Command
object Type {
  val command = ":type"
}
case object Imports extends Command {
  val command = ":imports"
}
case object Reset extends Command {
  val command = ":reset"
}
case object Quit extends Command {
  val command = ":quit"
}
case object Help extends Command {
  val command = ":help"
  val text =
    """The REPL has several commands available:
      |
      |:help                    print this summary or command-specific help
      |:load <path>             interpret lines in a file
      |:quit                    exit the interpreter
      |:type <expression>       evaluate the type of the given expression
      |:reset                   reset the repl to its initial state, forgetting all session entries
    """.stripMargin
}

object ParseResult {

  private[this] val CommandExtract = """(:[\S]+)\s*(.*)""".r

  def apply(sourceCode: String)(implicit ctx: Context): ParseResult =
    sourceCode match {
      case "" => Newline
      case CommandExtract(cmd, arg) => cmd match {
        case Quit.command => Quit
        case Help.command => Help
        case Reset.command => Reset
        case Imports.command => Imports
        case Load.command => Load(arg)
        case Type.command => Type(arg)
        case _ => UnknownCommand(cmd)
      }
      case _ => {
        val source = new SourceFile("<console>", sourceCode.toCharArray)
        val parser = new Parser(source)

        val (_, stats) = parser.templateStatSeq()

        if (ctx.reporter.hasErrors)
          SyntaxErrors(ctx.reporter.asInstanceOf[StoreReporter].removeBufferedMessages, stats)
        else
          Parsed(sourceCode, stats)
      }
    }

  def isIncomplete(sourceCode: String)(implicit ctx: Context): Boolean =
    sourceCode match {
      case CommandExtract(_) | "" => false
      case _ => {
        val reporter = storeReporter
        var needsMore = false
        reporter.withIncompleteHandler(_ => _ => needsMore = true) {
          val source = new SourceFile("<console>", sourceCode.toCharArray)
          val parser = new Parser(source)(ctx.fresh.setReporter(reporter))
          parser.templateStatSeq()
          !reporter.hasErrors && needsMore
        }
      }
    }
}
