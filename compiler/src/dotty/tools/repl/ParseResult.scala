package dotty.tools
package repl

import dotc.CompilationUnit
import dotc.ast.untpd
import dotc.core.Contexts._
import dotc.core.StdNames.str
import dotc.parsing.Parsers.Parser
import dotc.parsing.Tokens
import dotc.reporting.{Diagnostic, StoreReporter}
import dotc.util.SourceFile

import scala.annotation.internal.sharable

/** A parsing result from string input */
sealed trait ParseResult

/** An error free parsing resulting in a list of untyped trees */
case class Parsed(source: SourceFile, trees: List[untpd.Tree], reporter: StoreReporter) extends ParseResult

/** A parsing result containing syntax `errors` */
case class SyntaxErrors(sourceCode: String,
                        errors: List[Diagnostic],
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

/** An ambiguous prefix that matches multiple commands */
case class AmbiguousCommand(cmd: String, matchingCommands: List[String]) extends Command

/** `:load <path>` interprets a scala file as if entered line-by-line into
 *  the REPL
 */
case class Load(path: String) extends Command
object Load {
  val command: String = ":load"
}


/** Run the javap disassembler on the given target(s) */
case class JavapOf(args: String) extends Command
object JavapOf:
  val command: String = ":javap"

/** To find out the type of an expression you may simply do:
 *
 * ```
 * scala> :type (1 * 54).toString
 * String
 * ```
 */
case class TypeOf(expr: String) extends Command
object TypeOf {
  val command: String = ":type"
}

/**
 * A command that is used to display the documentation associated with
 * the given expression.
 */
case class DocOf(expr: String) extends Command
object DocOf {
  val command: String = ":doc"
}

/** `:imports` lists the imports that have been explicitly imported during the
 *  session
 */
case object Imports extends Command {
  val command: String = ":imports"
}

case class Settings(arg: String) extends Command
object Settings {
  val command: String = ":settings"
}

/** Reset the session to the initial state from when the repl program was
 *  started
 */
case class Reset(arg: String) extends Command
object Reset {
  val command: String = ":reset"
}

/** `:quit` exits the repl */
case object Quit extends Command {
  val command: String = ":quit"
  val alias: String = ":exit"
}

/** `:help` shows the different commands implemented by the Dotty repl */
case object Help extends Command {
  val command: String = ":help"
  val text: String =
    """The REPL has several commands available:
      |
      |:help                    print this summary
      |:load <path>             interpret lines in a file
      |:quit                    exit the interpreter
      |:type <expression>       evaluate the type of the given expression
      |:doc <expression>        print the documentation for the given expression
      |:imports                 show import history
      |:reset [options]         reset the repl to its initial state, forgetting all session entries
      |:settings <options>      update compiler options, if possible
      |:javap <path|class>      disassemble a file or class name
    """.stripMargin
}

object ParseResult {

  @sharable private val CommandExtract = """(:[\S]+)\s*(.*)""".r

  private def parseStats(using Context): List[untpd.Tree] = {
    val parser = new Parser(ctx.source)
    val stats = parser.blockStatSeq()
    parser.accept(Tokens.EOF)
    stats
  }

  private[repl] val commands: List[(String, String => ParseResult)] = List(
    Quit.command -> (_ => Quit),
    Quit.alias -> (_ => Quit),
    Help.command -> (_  => Help),
    Reset.command -> (arg  => Reset(arg)),
    Imports.command -> (_  => Imports),
    Load.command -> (arg => Load(arg)),
    TypeOf.command -> (arg => TypeOf(arg)),
    DocOf.command -> (arg => DocOf(arg)),
    Settings.command -> (arg => Settings(arg)),
    JavapOf.command -> (arg => JavapOf(arg))
  )

  def apply(source: SourceFile)(implicit state: State): ParseResult = {
    val sourceCode = source.content().mkString
    sourceCode match {
      case "" => Newline
      case CommandExtract(cmd, arg) => {
        val matchingCommands = commands.filter((command, _) => command.startsWith(cmd))
        matchingCommands match {
          case Nil => UnknownCommand(cmd)
          case (_, f) :: Nil => f(arg)
          case multiple => AmbiguousCommand(cmd, multiple.map(_._1))
        }
      }
      case _ =>
        inContext(state.context) {
          val reporter = newStoreReporter
          val stats = parseStats(using state.context.fresh.setReporter(reporter).withSource(source))

          if (reporter.hasErrors)
            SyntaxErrors(
              sourceCode,
              reporter.removeBufferedMessages,
              stats)
          else
            Parsed(source, stats, reporter)
        }
    }
  }

  def apply(sourceCode: String)(implicit state: State): ParseResult =
    apply(SourceFile.virtual(str.REPL_SESSION_LINE + (state.objectIndex + 1), sourceCode, maybeIncomplete = true))

  /** Check if the input is incomplete.
   *
   *  This can be used in order to check if a newline can be inserted without
   *  having to evaluate the expression.
   */
  def isIncomplete(sourceCode: String)(using Context): Boolean =
    sourceCode match {
      case CommandExtract(_) | "" => false
      case _ => {
        val reporter = newStoreReporter
        val source   = SourceFile.virtual("<incomplete-handler>", sourceCode, maybeIncomplete = true)
        val unit     = CompilationUnit(source, mustExist = false)
        val localCtx = ctx.fresh
                          .setCompilationUnit(unit)
                          .setReporter(reporter)
        var needsMore = false
        reporter.withIncompleteHandler((_, _) => needsMore = true) {
          parseStats(using localCtx)
        }
        !reporter.hasErrors && needsMore
      }
    }
}
