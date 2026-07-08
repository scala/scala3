package dotty.tools
package repl

import dotc.CompilationUnit
import dotc.ast.untpd
import dotc.core.Contexts.*
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
sealed trait Command extends ParseResult:
  def replayLine: Option[String]

/** An unknown command that will not be handled by the REPL */
case class UnknownCommand(cmd: String) extends Command:
  override def replayLine = None

case class Dep(dep: String) extends Command:
  override def replayLine = Some(s"${Dep.command} $dep")
object Dep {
  val command: String = ":dep"
}
/** An ambiguous prefix that matches multiple commands */
case class AmbiguousCommand(cmd: String, matchingCommands: List[String]) extends Command:
  override def replayLine = None

case class Save(path: String) extends Command:
  override def replayLine = None

object Save {
  val command: String = ":save"

  /** `:load` treats a file as a session only when it starts with this header.
   * any other file is a plain source loaded as one compilation unit.
   */
  val sessionHeader: String = "/* Scala REPL session */"

  /** Written before each saved entry so that `:load` can replay every entry as
   *  a single compilation unit.
   */
  val entrySeparator: String = "/* ---- entry ---- */"
}

/** `:load <path>` interprets a scala file as if entered line-by-line into
 *  the REPL
 */
case class Load(path: String) extends Command:
  override def replayLine = Some(s"${Load.command} $path")
object Load {
  val command: String = ":load"
}

/** `:require` is a deprecated alias for :jar`
 */
case class Require(path: String) extends Command:
  override def replayLine = Some(s"${Require.command} $path")
object Require {
  val command: String = ":require"
}

/** `:jar <path>` adds a jar to the classpath
 */
case class JarCmd(path: String) extends Command:
  override def replayLine = Some(s"${JarCmd.command} $path")
object JarCmd {
  val command: String = ":jar"
}

/** `:kind <type>` display the kind of a type. see also :help kind
 */
case class KindOf(expr: String) extends Command:
  override def replayLine = Some(s"${KindOf.command} $expr")
object KindOf {
  val command: String = ":kind"
}

/** To find out the type of an expression you may simply do:
 *
 * ```
 * scala> :type (1 * 54).toString
 * String
 * ```
 */
case class TypeOf(expr: String) extends Command:
  override def replayLine = Some(s"${TypeOf.command} $expr")
object TypeOf {
  val command: String = ":type"
}

/**
 * A command that is used to display the documentation associated with
 * the given expression.
 */
case class DocOf(expr: String) extends Command:
  override def replayLine = Some(s"${DocOf.command} $expr")
object DocOf {
  val command: String = ":doc"
}

/** `:imports` lists the imports that have been explicitly imported during the
 *  session
 */
case object Imports extends Command {
  override def replayLine = Some(command)
  val command: String = ":imports"
}

case class Settings(arg: String) extends Command:
  override def replayLine = Some(s"${Settings.command} $arg")
object Settings {
  val command: String = ":settings"
}

/** Reset the session to the initial state from when the repl program was
 *  started
 */
case class Reset(arg: String) extends Command:
  override def replayLine = Some(s"${Reset.command} $arg")
object Reset {
  val command: String = ":reset"
}

case class Replay(arg: String) extends Command:
  override def replayLine = None
object Replay {
  val command: String = ":replay"
}

/** `:sh <command line>` run a shell command (result is implicitly => List[String]) */
case class Sh(expr: String) extends Command:
  override def replayLine = Some(s"${Sh.command} $expr")
object Sh {
  val command: String = ":sh"
}

/** `:paste` is deprecated; JLine supports multiline editing so it is not needed */
case object Paste extends Command:
  override def replayLine = Some(command)
  val command: String = ":paste"

/** Toggle automatic printing of results */
case object Silent extends Command:
  override def replayLine = Some(command)
  val command: String = ":silent"

/** `:quit` exits the repl */
case object Quit extends Command {
  override def replayLine = None
  val command: String = ":quit"
  val alias: String = ":exit"
}

/** `:help` shows the different commands implemented by the Dotty repl */
case object Help extends Command {
  override def replayLine = Some(command)
  val command: String = ":help"
  val text: String =
    """The REPL has several commands available:
      |
      |:help                    print this summary
      |:save <path>             save replayable session to a file
      |:load <path>             interpret lines in a file
      |:quit                    exit the interpreter
      |:type <expression>       evaluate the type of the given expression
      |:doc <expression>        print the documentation for the given expression
      |:imports                 show import history
      |:reset [options]         clear the session and start fresh with the given compiler options
      |:replay [options]        reset, then re-run the session with the given compiler options
      |:settings <options>      update compiler options, if possible
      |:silent                  disable/enable automatic printing of results
      |:dep <group>::<artifact>:<version>     Resolve a dependency and make it available in the REPL
    """.stripMargin
}

object ParseResult {

  // Commands must start with ":" followed by a letter (e.g. :help, :load)
  // This ensures operators like :: or :+ are treated as Scala code, not commands
  @sharable private val CommandExtract = """(:[a-zA-Z]\S*)\s*(.*)""".r

  private def parseStats(using Context): List[untpd.Tree] = {
    val parser = new Parser(ctx.source)
    val stats = parser.blockStatSeq(outermost = true)
    parser.accept(Tokens.EOF)
    stats
  }

  private[repl] val commands: List[(String, String => ParseResult)] = List(
    Quit.command -> (_ => Quit),
    Quit.alias -> (_ => Quit),
    Help.command -> (_  => Help),
    Reset.command -> (arg  => Reset(arg)),
    Replay.command -> (arg => Replay(arg)),
    Imports.command -> (_  => Imports),
    JarCmd.command -> (arg => JarCmd(arg)),
    KindOf.command -> (arg => KindOf(arg)),
    Save.command -> (arg => Save(arg)),
    Load.command -> (arg => Load(arg)),
    Require.command -> (arg => Require(arg)),
    Dep.command -> (arg => Dep(arg)),
    TypeOf.command -> (arg => TypeOf(arg)),
    DocOf.command -> (arg => DocOf(arg)),
    Settings.command -> (arg => Settings(arg)),
    Sh.command -> (arg => Sh(arg)),
    Paste.command -> (_ => Paste),
    Silent.command -> (_ => Silent),
  )

  def apply(source: SourceFile)(using state: State): ParseResult = {
    val sourceCode = source.content().mkString
    sourceCode match {
      case "" => Newline
      case CommandExtract(cmd: String, arg: String) => {
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

  def apply(sourceCode: String)(using state: State): ParseResult =
    apply(SourceFile.virtual(str.REPL_SESSION_LINE + (state.objectIndex + 1), sourceCode))

  def isCommand(line: String): Boolean =
    line match
      case CommandExtract(_, _) => true
      case _ => false

  /** Check if the input is incomplete.
   *
   *  This can be used in order to check if a newline can be inserted without
   *  having to evaluate the expression.
   */
  def isIncomplete(sourceCode: String)(using Context): Boolean =
    sourceCode match {
      case CommandExtract(_, _) | "" => false
      case _ => {
        val reporter = newStoreReporter
        val source   = SourceFile.virtual("<incomplete-handler>", sourceCode)
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
