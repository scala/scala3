package dotty.tools.repl

import scala.language.unsafeNulls

import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.parsing.Scanners.Scanner
import dotty.tools.dotc.parsing.Tokens._
import dotty.tools.dotc.printing.SyntaxHighlighting
import dotty.tools.dotc.reporting.Reporter
import dotty.tools.dotc.util.SourceFile
import org.jline.reader
import org.jline.reader.Parser.ParseContext
import org.jline.reader._
import org.jline.reader.impl.LineReaderImpl
import org.jline.reader.impl.history.DefaultHistory
import org.jline.terminal.TerminalBuilder
import org.jline.utils.AttributedString

final class JLineTerminal extends java.io.Closeable {
  // import java.util.logging.{Logger, Level}
  // Logger.getLogger("org.jline").setLevel(Level.FINEST)

  private val terminal =
    TerminalBuilder.builder()
    .dumb(dumbTerminal) // fail early if not able to create a terminal
    .build()
  private val history = new DefaultHistory
  def dumbTerminal = Option(System.getenv("TERM")) == Some("dumb")

  private def blue(str: String)(using Context) =
    if (ctx.settings.color.value != "never") Console.BLUE + str + Console.RESET
    else str
  private def prompt(using Context)        = blue("\nscala> ")
  private def newLinePrompt(using Context) = blue("     | ")

  /** Blockingly read line from `System.in`
   *
   *  This entry point into JLine handles everything to do with terminal
   *  emulation. This includes:
   *
   *  - Multi-line support
   *  - Copy-pasting
   *  - History
   *  - Syntax highlighting
   *  - Auto-completions
   *
   *  @throws EndOfFileException This exception is thrown when the user types Ctrl-D.
   */
  def readLine(
    completer: Completer // provide auto-completions
  )(using Context): String = {
    import LineReader.Option._
    import LineReader._
    val userHome = System.getProperty("user.home")
    val lineReader = LineReaderBuilder
      .builder()
      .terminal(terminal)
      .history(history)
      .completer(completer)
      .highlighter(new Highlighter)
      .parser(new Parser)
      .variable(HISTORY_FILE, s"$userHome/.dotty_history") // Save history to file
      .variable(SECONDARY_PROMPT_PATTERN, "%M") // A short word explaining what is "missing",
                                                // this is supplied from the EOFError.getMissing() method
      .variable(LIST_MAX, 400)                  // Ask user when number of completions exceed this limit (default is 100).
      .variable(BLINK_MATCHING_PAREN, 0L)       // Don't blink the opening paren after typing a closing paren.
      .variable(WORDCHARS,
        LineReaderImpl.DEFAULT_WORDCHARS.filterNot("*?.[]~=/&;!#%^(){}<>".toSet)) // Finer grained word boundaries
      .option(INSERT_TAB, true)                 // At the beginning of the line, insert tab instead of completing.
      .option(AUTO_FRESH_LINE, true)            // if not at start of line before prompt, move to new line.
      .option(DISABLE_EVENT_EXPANSION, true)    // don't process escape sequences in input
      .build()

    lineReader.readLine(prompt)
  }

  def close(): Unit = terminal.close()

  /** Provide syntax highlighting */
  private class Highlighter(using Context) extends reader.Highlighter {
    def highlight(reader: LineReader, buffer: String): AttributedString = {
      val highlighted = SyntaxHighlighting.highlight(buffer)
      AttributedString.fromAnsi(highlighted)
    }
    def setErrorPattern(errorPattern: java.util.regex.Pattern): Unit = {}
    def setErrorIndex(errorIndex: Int): Unit = {}
  }

  /** Provide multi-line editing support */
  private class Parser(using Context) extends reader.Parser {

    /**
     * @param cursor     The cursor position within the line
     * @param line       The unparsed line
     * @param word       The current word being completed
     * @param wordCursor The cursor position within the current word
     */
    private class ParsedLine(
      val cursor: Int, val line: String, val word: String, val wordCursor: Int
    ) extends reader.ParsedLine {
      // Using dummy values, not sure what they are used for
      def wordIndex = -1
      def words = java.util.Collections.emptyList[String]
    }

    def parse(input: String, cursor: Int, context: ParseContext): reader.ParsedLine = {
      def parsedLine(word: String, wordCursor: Int) =
        new ParsedLine(cursor, input, word, wordCursor)
      // Used when no word is being completed
      def defaultParsedLine = parsedLine("", 0)

      def incomplete(): Nothing = throw new EOFError(
        // Using dummy values, not sure what they are used for
        /* line    = */ -1,
        /* column  = */ -1,
        /* message = */ "",
        /* missing = */ newLinePrompt)

      case class TokenData(token: Token, start: Int, end: Int)
      def currentToken: TokenData /* | Null */ = {
        val source = SourceFile.virtual("<completions>", input)
        val scanner = new Scanner(source)(using ctx.fresh.setReporter(Reporter.NoReporter))
        var lastBacktickErrorStart: Option[Int] = None

        while (scanner.token != EOF) {
          val start = scanner.offset
          val token = scanner.token
          scanner.nextToken()
          val end = scanner.lastOffset

          val isCurrentToken = cursor >= start && cursor <= end
          if (isCurrentToken)
            return TokenData(token, lastBacktickErrorStart.getOrElse(start), end)


          // we need to enclose the last backtick, which unclosed produces ERROR token
          if (token == ERROR && input(start) == '`') then
            lastBacktickErrorStart = Some(start)
          else
            lastBacktickErrorStart = None
        }
        null
      }

      def acceptLine = {
        val onLastLine = !input.substring(cursor).contains("\n")
        onLastLine && !ParseResult.isIncomplete(input)
      }

      context match {
        case ParseContext.ACCEPT_LINE if acceptLine =>
          // using dummy values, resulting parsed input is probably unused
          defaultParsedLine

        // In the situation where we have a partial command that we want to
        // complete we need to ensure that the :<partial-word> isn't split into
        // 2 tokens, but rather the entire thing is treated as the "word", in
        //   order to insure the : is replaced in the completion.
        case ParseContext.COMPLETE if
          ParseResult.commands.exists(command => command._1.startsWith(input)) =>
            parsedLine(input, cursor)

        case ParseContext.COMPLETE =>
          // Parse to find completions (typically after a Tab).
          def isCompletable(token: Token) = isIdentifier(token) || isKeyword(token)
          currentToken match {
            case TokenData(token, start, end) if isCompletable(token) =>
              val word = input.substring(start, end)
              val wordCursor = cursor - start
              parsedLine(word, wordCursor)
            case _ =>
              defaultParsedLine
          }

        case _ =>
          incomplete()
      }
    }
  }
}
