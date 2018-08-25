package dotty.tools.repl

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.parsing.Scanners.Scanner
import dotty.tools.dotc.parsing.Tokens._
import dotty.tools.dotc.printing.SyntaxHighlighting
import dotty.tools.dotc.reporting.Reporter
import dotty.tools.dotc.util.SourceFile
import org.jline.reader
import org.jline.reader.Parser.ParseContext
import org.jline.reader._
import org.jline.reader.impl.history.DefaultHistory
import org.jline.terminal.TerminalBuilder
import org.jline.utils.AttributedString

final class JLineTerminal extends java.io.Closeable {
  // import java.util.logging.{Logger, Level}
  // Logger.getLogger("org.jline").setLevel(Level.FINEST)

  private val terminal = TerminalBuilder.builder()
    .build()
  private val history = new DefaultHistory

  private def blue(str: String) = Console.BLUE + str + Console.RESET
  private val prompt        = blue("scala> ")
  private val newLinePrompt = blue("     | ")

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
  )(implicit ctx: Context): String = {
    import LineReader.Option._
    import LineReader._
    val lineReader = LineReaderBuilder.builder()
      .terminal(terminal)
      .history(history)
      .completer(completer)
      .highlighter(new Highlighter)
      .parser(new Parser)
      .variable(SECONDARY_PROMPT_PATTERN, "%M") // A short word explaining what is "missing",
                                                // this is supplied from the EOFError.getMissing() method
      .variable(LIST_MAX, 400)                  // Ask user when number of completions exceed this limit (default is 100).
      .variable(BLINK_MATCHING_PAREN, 0L)       // Don't blink the opening paren after typing a closing paren.
      .option(INSERT_TAB, true)                 // At the beginning of the line, insert tab instead of completing.
      .option(AUTO_FRESH_LINE, true)            // if not at start of line before prompt, move to new line.
      .build()

    lineReader.readLine(prompt)
  }

  def close() = terminal.close()

  /** Provide syntax highlighting */
  private class Highlighter extends reader.Highlighter {
    def highlight(reader: LineReader, buffer: String): AttributedString = {
      val highlighted = SyntaxHighlighting(buffer).mkString
      AttributedString.fromAnsi(highlighted)
    }
  }

  /** Provide multi-line editing support */
  private class Parser(implicit ctx: Context) extends reader.Parser {

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

    def parse(line: String, cursor: Int, context: ParseContext): reader.ParsedLine = {
      def parsedLine(word: String, wordCursor: Int) =
        new ParsedLine(cursor, line, word, wordCursor)
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
        val source = new SourceFile("<completions>", line)
        val scanner = new Scanner(source)(ctx.fresh.setReporter(Reporter.NoReporter))
        while (scanner.token != EOF) {
          val start = scanner.offset
          val token = scanner.token
          scanner.nextToken()
          val end = scanner.lastOffset

          val isCurrentToken = cursor >= start && cursor <= end
          if (isCurrentToken)
            return TokenData(token, start, end)
        }
        null
      }

      context match {
        case ParseContext.ACCEPT_LINE =>
          // ENTER means SUBMIT when
          //   - cursor is at end (discarding whitespaces)
          //   - and, input line is complete
          val cursorIsAtEnd = line.indexWhere(!_.isWhitespace, from = cursor) < 0
          if (cursorIsAtEnd && !ParseResult.isIncomplete(line))
            defaultParsedLine // using dummy values, resulting parsed line is probably unused
          else
            incomplete()

        case ParseContext.COMPLETE =>
          // Parse to find completions (typically after a Tab).
          def isCompletable(token: Token) = isIdentifier(token) || isKeyword(token)
          currentToken match {
            case TokenData(token, start, end) if isCompletable(token) =>
              val word = line.substring(start, end)
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
