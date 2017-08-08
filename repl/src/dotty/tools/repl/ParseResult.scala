package dotty.tools
package repl

import dotc.reporting.diagnostic.MessageContainer
import dotc.core.Contexts.Context
import dotc.parsing.Parsers.Parser
import dotc.util.SourceFile
import dotc.ast.untpd
import dotc.reporting._

import results._

/** A parsing result from string input */
sealed trait ParseResult

/** An error free parsing resulting in a list of untyped trees */
case class Parsed(sourceCode: String, trees: List[untpd.Tree]) extends ParseResult

/** A parsing result containing syntax `errors` */
case class SyntaxErrors(sourceCode: String,
                        errors: List[MessageContainer],
                        trees: List[untpd.Tree]) extends ParseResult

/** Parsed result is simply a newline */
case object Newline extends ParseResult

/** `ctrl-c` obtained from input string */
case object SigKill extends ParseResult

/** A command is on the format:
 *
 *  ```none
 *  :commandName <optional arguments...>
 *  ```
 *  The `Command` trait denotes these commands
 */
sealed trait Command extends ParseResult

/** An unknown command that will not be handled by the REPL */
case class UnknownCommand(cmd: String) extends Command

/** `:load <path>` interprets a scala file as if entered line-by-line into
 *  the REPL
 */
case class Load(path: String) extends Command
object Load {
  val command = ":load"
}

/** To find out the type of an expression you may simply do:
 *
 * ```
 * scala> :type (1 * 54).toString
 * String
 * ```
 */
case class Type(expr: String) extends Command
object Type {
  val command = ":type"
}

/** `:imports` lists the imports that have been explicitly imported during the
 *  session
 */
case object Imports extends Command {
  val command = ":imports"
}

/** Reset the session to the initial state from when the repl program was
 *  started
 */
case object Reset extends Command {
  val command = ":reset"
}

/** `:quit` exits the repl */
case object Quit extends Command {
  val command = ":quit"
}

/** `:help` shows the different commands implemented by the Dotty repl */
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

  /** Extract a `ParseResult` from the string `sourceCode` */
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

        if (ctx.reporter.hasErrors) {
          SyntaxErrors(sourceCode,
                       ctx.reporter.asInstanceOf[StoreReporter].removeBufferedMessages,
                       stats)
        }
        else
          Parsed(sourceCode, stats)
      }
    }

  /** Check if the input is incomplete
   *
   *  This can be used in order to check if a newline can be inserted without
   *  having to evaluate the expression
   */
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
