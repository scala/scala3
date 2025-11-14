package dotty.tools
package repl

import scala.language.unsafeNulls

import dotc.core.Contexts.*
import dotc.parsing.Scanners.Scanner
import dotc.parsing.Tokens.*
import dotc.printing.SyntaxHighlighting
import dotc.reporting.Reporter
import dotc.util.SourceFile
import org.jline.reader
import org.jline.reader.Parser.ParseContext
import org.jline.reader.*
import org.jline.reader.impl.LineReaderImpl
import org.jline.reader.impl.history.DefaultHistory
import org.jline.terminal.TerminalBuilder
import org.jline.utils.AttributedString

class JLineTerminal extends java.io.Closeable {

  private val terminal =
    var builder = TerminalBuilder.builder()
    if System.getenv("TERM") == "dumb" then
      builder.dumb(true)
    builder.build()

  private val history = new DefaultHistory

  private def blue(str: String)(using Context) =
    if (ctx.settings.color.value != "never") Console.BLUE + str + Console.RESET
    else str

  protected def promptStr = "scala"
  private def prompt(using Context)        = blue(s"\n$promptStr> ")
  private def newLinePrompt(using Context) = "       "

  def readLine(
    completer: Completer
  )(using Context): String = {
    import LineReader.Option.*
    import LineReader.*
    val userHome = System.getProperty("user.home")

    val lineReader = LineReaderBuilder
      .builder()
      .terminal(terminal)
      .history(history)
      .completer(completer)
      .highlighter(new Highlighter)
      .parser(new Parser)
      .variable(HISTORY_FILE, s"$userHome/.dotty_history")
      .variable(SECONDARY_PROMPT_PATTERN, "%M")
      .variable(LIST_MAX, 400)
      .variable(BLINK_MATCHING_PAREN, 0L)
      .variable(
        WORDCHARS,
        LineReaderImpl.DEFAULT_WORDCHARS.filterNot("*?.[]~=/&;!#%^(){}<>".toSet)
      )
      .option(INSERT_TAB, true)
      .option(AUTO_FRESH_LINE, true)
      .option(DISABLE_EVENT_EXPANSION, true)
      .build()

    lineReader.readLine(prompt)
  }

  def close(): Unit = terminal.close()

  def handle(
    signal: org.jline.terminal.Terminal.Signal,
    handler: org.jline.terminal.Terminal.SignalHandler
  ): org.jline.terminal.Terminal.SignalHandler =
    terminal.handle(signal, handler)

  private class Highlighter(using Context) extends reader.Highlighter {
    def highlight(reader: LineReader, buffer: String): AttributedString = {
      val highlighted = SyntaxHighlighting.highlight(buffer)
      AttributedString.fromAnsi(highlighted)
    }
    def setErrorPattern(errorPattern: java.util.regex.Pattern): Unit = {}
    def setErrorIndex(errorIndex: Int): Unit = {}
  }

  private class Parser(using Context) extends reader.Parser {

    private class ParsedLine(
      val cursor: Int, val line: String, val word: String, val wordCursor: Int
    ) extends reader.ParsedLine {
      def wordIndex = -1
      def words: java.util.List[String] =
        java.util.Collections.emptyList[String]
    }

    def parse(
      input: String,
      cursor: Int,
      context: ParseContext
    ): reader.ParsedLine = {

      def parsedLine(word: String, wordCursor: Int) =
        new ParsedLine(cursor, input, word, wordCursor)

      def defaultParsedLine = parsedLine("", 0)

      def incomplete(): Nothing = throw new EOFError(
        -1, -1, "", newLinePrompt
      )

      case class TokenData(token: Token, start: Int, end: Int)

      def currentToken: TokenData | Null = {
        val source  = SourceFile.virtual("<completions>", input)
        val scanner = new Scanner(source)(using ctx.fresh.setReporter(Reporter.NoReporter))
        var lastBacktickErrorStart: Option[Int] = None

        while (scanner.token != EOF) {
          val start = scanner.offset
          val token = scanner.token
          scanner.nextToken()
          val end = scanner.lastOffset

          val isCurrentToken =
            cursor >= start && cursor <= end

          if (isCurrentToken)
            return TokenData(token, lastBacktickErrorStart.getOrElse(start), end)

          if (token == ERROR && input(start) == '`') then
            lastBacktickErrorStart = Some(start)
          else
            lastBacktickErrorStart = None
        }
        null
      }

      def acceptLine =
        !input.substring(cursor).contains(System.lineSeparator) &&
          !ParseResult.isIncomplete(input)

      context match {

  case ParseContext.ACCEPT_LINE if acceptLine =>
    defaultParsedLine

  // FIX: REPL commands starting with ":" must be treated as one word
  case ParseContext.COMPLETE if input.startsWith(":") =>
    parsedLine(input, cursor)

  case ParseContext.COMPLETE =>
    def isCompletable(token: Token) =
      isIdentifier(token) || isKeyword(token)

    currentToken match
      case TokenData(token, start, end) if isCompletable(token) =>
        val word       = input.substring(start, end)
        val wordCursor = cursor - start
        parsedLine(word, wordCursor)
      case _ =>
        defaultParsedLine

  case _ =>
    incomplete()
}

    }
  }
}
