package dotty.tools.repl

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.parsing.Scanners.Scanner
import dotty.tools.dotc.parsing.Tokens._
import dotty.tools.dotc.printing.SyntaxHighlighting
import dotty.tools.dotc.reporting.Reporter
import dotty.tools.dotc.util.SourceFile
import org.jline.reader
import org.jline.reader.LineReader.Option
import org.jline.reader.Parser.ParseContext
import org.jline.reader._
import org.jline.reader.impl.history.DefaultHistory
import org.jline.terminal.TerminalBuilder
import org.jline.utils.AttributedString

final class JLineTerminal extends java.io.Closeable {
  // import java.util.logging.{Logger, Level}
  // Logger.getLogger("org.jline").setLevel(Level.FINEST)

  private val terminal = TerminalBuilder.terminal()
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
    val lineReader = LineReaderBuilder.builder()
      .terminal(terminal)
      .history(history)
      .completer(completer)
      .highlighter(new Highlighter)
      .parser(new Parser)
      .variable(LineReader.SECONDARY_PROMPT_PATTERN, "%M")
      .option(Option.INSERT_TAB, true)      // at the beginning of the line, insert tab instead of completing
      .option(Option.AUTO_FRESH_LINE, true) // if not at start of line before prompt, move to new line
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

    private class ParsedLine(
      val cursor: Int,    // The cursor position within the line
      val line: String,   // The unparsed line
      val word: String,   // The current word being completed
      val wordCursor: Int // The cursor position within the current word
    ) extends reader.ParsedLine {
      // Using dummy values, not sure what they are used for
      def wordIndex = -1
      def words = java.util.Collections.emptyList[String]
    }

    def parse(line: String, cursor: Int, context: ParseContext): reader.ParsedLine = {
      def parsedLine(word: String, wordCursor: Int) =
        new ParsedLine(cursor, line, word, wordCursor)

      def incomplete(): Nothing = throw new EOFError(
        // Using dummy values, not sure what they are used for
        /* line    = */ -1,
        /* column  = */ -1,
        /* message = */ "",
        /* missing = */ newLinePrompt)

      context match {
        case ParseContext.ACCEPT_LINE =>
          val lastLineOffset = line.lastIndexOfSlice(System.lineSeparator)
          if (cursor <= lastLineOffset || ParseResult.isIncomplete(line)) incomplete()
          else parsedLine("", 0)
            // using dummy values,
            // resulting parsed line is probably unused

        case ParseContext.COMPLETE =>
          // Parse to find completions (typically after a Tab).
          val source = new SourceFile("<completions>", line)
          val scanner = new Scanner(source)(ctx.fresh.setReporter(Reporter.NoReporter))

          // Looking for the current word being completed
          // and the cursor position within this word
          while (scanner.token != EOF) {
            val start = scanner.offset
            val token = scanner.token
            scanner.nextToken()
            val end = scanner.lastOffset

            val isCompletable =
              isIdentifier(token) || isKeyword(token) // keywords can start identifiers
            def isCurrentWord = cursor >= start && cursor <= end
            if (isCompletable && isCurrentWord) {
              val word = line.substring(start, end)
              val wordCursor = cursor - start
              return parsedLine(word, wordCursor)
            }
          }
          parsedLine("", 0) // no word being completed

        case _ =>
          incomplete()
      }
    }
  }
}
