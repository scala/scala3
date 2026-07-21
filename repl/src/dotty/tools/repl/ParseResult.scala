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
import dotty.tools.directives.{ExtractorResult, UsingDirectiveDiagnostic, UsingDirectivesParser}

import scala.annotation.internal.sharable
import scala.annotation.tailrec

/** A parsing result from string input */
sealed trait ParseResult

/** An error free parsing resulting in a list of untyped trees */
case class Parsed(
  source: SourceFile,
  trees: List[untpd.Tree],
  reporter: StoreReporter,
  directiveDiagnostics: Seq[UsingDirectiveDiagnostic] = Nil
) extends ParseResult

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

/** A command on the first line followed by Scala code, e.g.
 *  ```none
 *  :dep <coords>
 *  println("Hello")
 *  ```
 *  The command is interpreted first, then code is evaluated.
 */
case class CommandThenCode(command: Command, code: ParseResult) extends ParseResult

/** Input that mixes `:` commands and `//> using` directives in a single block */
case object MixedCommandsAndDirectives extends ParseResult

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
      |
      |Scala CLI `//> using dep` directives are also supported and behave like `:dep`, e.g.:
      |  //> using dep <group>::<artifact>:<version>
      |Directives must appear before any Scala code in the input; code on following lines is
      |evaluated as usual. Other `//> using` directives are not (yet) supported in the REPL.
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

  /** Resolve a `:command` name (which may be a prefix) and its argument into a `Command`. */
  private def command(cmd: String, arg: String): Command =
    commands.filter((command, _) => command.startsWith(cmd)) match {
      case Nil => UnknownCommand(cmd)
      case (_, f) :: Nil =>
        f(arg) match {
          case matched: Command => matched
          case _ => UnknownCommand(cmd)
        }
      case multiple => AmbiguousCommand(cmd, multiple.map(_._1))
    }

  private def extractDirectives(sourceCode: String): ExtractorResult =
    UsingDirectivesParser.extractLines(sourceCode.toIndexedSeq)

  private val UsingDirectivePrefix = """^//>\s*using(?:\s|$)""".r

  /** Extract line bounds and trimmed content starting at offset.
   *  Returns `(lineEnd, line, trimmed)` where:
   *  - `lineEnd`: index of `'\n'` or `src.length`
   *  - `line`: the raw line content (without newline)
   *  - `trimmed`: `line` with leading whitespace stripped
   */
  private def extractLine(src: String, offset: Int): (Int, String, String) =
    val lineEnd = src.indexOf('\n', offset) match
      case -1 => src.length
      case nl => nl
    val line = src.substring(offset, lineEnd)
    val trimmed = line.stripLeading()
    (lineEnd, line, trimmed)

  /** Skip a block comment (with nesting). Returns offset after closing star-slash. */
  private def skipBlockComment(src: String, start: Int): Int =
    @tailrec
    def loop(off: Int, depth: Int): Int =
      if off >= src.length - 1 || depth == 0 then off
      else if src(off) == '/' && src(off + 1) == '*' then loop(off + 2, depth + 1)
      else if src(off) == '*' && src(off + 1) == '/' then loop(off + 2, depth - 1)
      else loop(off + 1, depth)
    loop(start + 2, 1)

  /** Find offset of first significant content (past blank lines, line comments, block comments). */
  private def skipLeadingCommentsAndBlanks(src: String): Int =
    @tailrec
    def scan(offset: Int): Int =
      if offset >= src.length then offset
      else
        val (lineEnd, line, trimmed) = extractLine(src, offset)

        if trimmed.isEmpty then
          scan(lineEnd + 1)
        else if trimmed.startsWith("/*") then
          scan(skipBlockComment(src, offset + line.indexOf("/*")))
        else if trimmed.startsWith("//") then
          scan(lineEnd + 1)
        else
          offset + line.indexOf(trimmed.head)
    scan(0)

  /** Detect if the leading prefix (before Scala code) contains both `:` commands
   *  and `//> using` directives. Skips blank lines, a leading shebang, line comments,
   *  and block comments (with nesting support matching the Scala compiler).
   */
  private def mixesCommandsAndDirectives(sourceCode: String): Boolean =
    @tailrec
    def scan(offset: Int, hasCommand: Boolean, hasDirective: Boolean): Boolean =
      if offset >= sourceCode.length then hasCommand && hasDirective
      else
        val (lineEnd, line, trimmed) = extractLine(sourceCode, offset)

        if trimmed.isEmpty then
          scan(lineEnd + 1, hasCommand, hasDirective)
        else if offset == 0 && trimmed.startsWith("#!") then
          // Shebang: skip only on the very first line (matches CommentExtractor)
          scan(lineEnd + 1, hasCommand, hasDirective)
        else if trimmed.startsWith("/*") then
          val afterBlock = skipBlockComment(sourceCode, offset + line.indexOf("/*"))
          scan(afterBlock, hasCommand, hasDirective)
        else if trimmed.startsWith("//") then
          if UsingDirectivePrefix.findFirstIn(trimmed).isDefined then
            if hasCommand then true else scan(lineEnd + 1, hasCommand, hasDirective = true)
          else
            scan(lineEnd + 1, hasCommand, hasDirective)
        else trimmed match
          case CommandExtract(_, _) =>
            if hasDirective then true else scan(lineEnd + 1, hasCommand = true, hasDirective)
          case _ =>
            hasCommand && hasDirective
    end scan

    scan(0, hasCommand = false, hasDirective = false)
  end mixesCommandsAndDirectives

  /** If `sourceCode`'s first significant line (after comments/blank lines) is a
   *  `:command`, split it into `(commandName, commandArg, remainingText)`.
   *  `remainingText` may be blank.
   */
  private def leadingCommand(sourceCode: String, extractedDirectives: ExtractorResult): Option[(String, String, String)] =
    if extractedDirectives.directiveLines.nonEmpty then None // directive blocks are handled as Parsed input
    else
      val start = extractedDirectives.codeOffset
      if start >= sourceCode.length then None
      else
        val significant = sourceCode.substring(start)
        val (firstLineRaw, rest) = significant.indexOf('\n') match
          case -1 => (significant, "")
          case nl => (significant.substring(0, nl), significant.substring(nl + 1))
        val firstLine = if firstLineRaw.endsWith("\r") then firstLineRaw.dropRight(1) else firstLineRaw
        firstLine match
          case CommandExtract(cmd: String, arg: String) => Some((cmd, arg, rest))
          case _ => None

  def apply(source: SourceFile)(using state: State): ParseResult = {
    val sourceCode = source.content().mkString
    sourceCode match {
      case "" => Newline
      case _ if mixesCommandsAndDirectives(sourceCode) => MixedCommandsAndDirectives
      case CommandExtract(cmd: String, arg: String) => command(cmd, arg)
      case _ =>
        val extracted = extractDirectives(sourceCode)
        leadingCommand(sourceCode, extracted) match {
          case Some((cmd, arg, rest)) =>
            if rest.exists(!_.isWhitespace) then CommandThenCode(command(cmd, arg), apply(rest))
            else command(cmd, arg)
          case None =>
            inContext(state.context) {
              val reporter = newStoreReporter
              val stats = parseStats(using state.context.fresh.setReporter(reporter).withSource(source))

              if (reporter.hasErrors)
                SyntaxErrors(
                  sourceCode,
                  reporter.removeBufferedMessages,
                  stats)
              else
                Parsed(source, stats, reporter, extracted.diagnostics)
            }
        }
    }
  }

  def apply(sourceCode: String)(using state: State): ParseResult =
    apply(SourceFile.virtual(str.REPL_SESSION_LINE + (state.objectIndex + 1), sourceCode))

  def isCommand(line: String): Boolean =
    line match
      case CommandExtract(_, _) => true
      case _ => false

  /** True if `sourceCode` contains one or more leading `//> using` directives and no code yet. */
  private def onlyDirectivesSoFar(sourceCode: String, extractedDirectives: ExtractorResult): Boolean =
    extractedDirectives.directiveLines.nonEmpty && extractedDirectives.codeOffset >= sourceCode.length

  /** True if `sourceCode` is a single `:command` line and no code yet. */
  private def onlyCommandSoFar(sourceCode: String, extractedDirectives: ExtractorResult): Boolean =
    leadingCommand(sourceCode, extractedDirectives) match
      case Some((_, _, rest)) => rest.forall(_.isWhitespace)
      case None => false

  /** A lone leading directive block or a single `:command` line with no trailing
   *  code yet. During a paste we keep reading so following code joins this input;
   *  on a single interactive ENTER (nothing pending) it is submitted as-is. */
  private[repl] def awaitsTrailingCode(sourceCode: String): Boolean =
    awaitsTrailingCode(sourceCode, extractDirectives(sourceCode))

  private def awaitsTrailingCode(sourceCode: String, extractedDirectives: ExtractorResult): Boolean =
    !sourceCode.endsWith("\n") && (onlyDirectivesSoFar(sourceCode, extractedDirectives) || onlyCommandSoFar(sourceCode, extractedDirectives))

  /** Whether JLine should accept the current buffer as a complete submission. */
  private[repl] def shouldAcceptLine(sourceCode: String, hasPendingInput: Boolean)(using Context): Boolean =
    val extractedDirectives = extractDirectives(sourceCode)
    if awaitsTrailingCode(sourceCode, extractedDirectives) then !hasPendingInput
    else !isIncomplete(sourceCode, extractedDirectives)

  /** The trailing code after any leading `:command` lines. Only this part decides
   *  whether the whole input is complete: a `:command` is complete on its own, and
   *  parsing its line as Scala would raise a spurious error (`:` is illegal) that
   *  would otherwise make unfinished trailing code look complete and submit early.
   */
  private def codeAfterLeadingCommands(sourceCode: String, extractedDirectives: ExtractorResult): String =
    // After the first command there are no directives (would have been detected earlier),
    // so subsequent steps skip comments/blanks without re-running extractDirectives.
    @tailrec
    def skipCommands(src: String, extracted: ExtractorResult): String =
      leadingCommand(src, extracted) match
        case Some((_, _, rest)) =>
          val offset = skipLeadingCommentsAndBlanks(rest)
          val significant = if offset < rest.length then rest.substring(offset) else ""
          skipCommands(significant, ExtractorResult.empty)
        case None => src
    skipCommands(sourceCode, extractedDirectives)

  /** Check if the input is incomplete.
   *
   *  This can be used in order to check if a newline can be inserted without
   *  having to evaluate the expression.
   */
  def isIncomplete(sourceCode: String)(using Context): Boolean =
    isIncomplete(sourceCode, extractDirectives(sourceCode))

  private def isIncomplete(sourceCode: String, extractedDirectives: ExtractorResult)(using Context): Boolean =
    sourceCode match {
      case "" => false
      case CommandExtract(_, _) => false
      case _ =>
        val code = codeAfterLeadingCommands(sourceCode, extractedDirectives)
        code.nonEmpty && {
          val reporter = newStoreReporter
          val source   = SourceFile.virtual("<incomplete-handler>", code)
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
