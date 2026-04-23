package dotty.tools
package repl

import scala.language.unsafeNulls
import scala.io.AnsiColor

import java.io.{InputStream, InterruptedIOException}
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
import org.jline.terminal.Attributes
import org.jline.terminal.Attributes.ControlChar
import org.jline.terminal.TerminalBuilder
import org.jline.terminal.Terminal.Signal
import org.jline.utils.AttributedString
import org.jline.utils.NonBlockingReader

// `stdin` alternates between a background Ctrl-C monitor and the foreground
// wrapped `System.in` reader. These states track which side currently owns it.
private enum InputState:
  case Monitoring, ForegroundRead, Closed

class JLineTerminal extends java.io.Closeable {
  private val terminal =
    val builder = TerminalBuilder.builder()
    if System.getenv("TERM") == "dumb" then
      // Force dumb terminal if `TERM` is `"dumb"`.
      // Note: the default value for the `dumb` option is `null`, which allows
      // JLine to fall back to a dumb terminal. This is different than `true` or
      // `false` and can't be set using the `dumb` setter.
      // This option is used at https://github.com/jline/jline3/blob/894b5e72cde28a551079402add4caea7f5527806/terminal/src/main/java/org/jline/terminal/TerminalBuilder.java#L528.
      builder.dumb(true)
    builder.build()

  private val originalAttributes = terminal.getAttributes
  private val noIntrAttributes = new Attributes(originalAttributes)
  noIntrAttributes.setControlChar(ControlChar.VINTR, 0)
  terminal.setAttributes(noIntrAttributes)
  terminal.enterRawMode()

  private val history = new DefaultHistory
  @volatile private var monitoringThread: Thread | Null = null

  private val userLineReader =
    LineReaderBuilder
      .builder()
      .terminal(terminal)
      .parser(new SimpleParser())
      .build()

  bindCtrlCInterrupt(userLineReader)
  private val userInput = new UserInputStream(userLineReader, terminal.encoding())

  private def bindCtrlCInterrupt(lr: LineReader): Unit =
    lr.getKeyMaps.get(LineReader.MAIN).bind(
      new Widget { override def apply(): Boolean = throw new UserInterruptException("") },
      "\u0003"
    )

  private def magenta(str: String)(using Context) =
    // Deliberately do not use these properties on `Console` to avoid initializing it,
    // and thus capturing stdin/stdout/stderr state in its `Console.in/out/err` properties,
    // since the REPL may wish to change the std streams before giving control to the user.
    if (ctx.settings.color.value != "never") AnsiColor.MAGENTA + str + AnsiColor.RESET
    else str
  protected def promptStr = "scala"
  private def prompt(using Context)        = magenta(s"\n$promptStr> ")
  private def newLinePrompt(using Context) = "       "

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

    bindCtrlCInterrupt(lineReader)
    lineReader.readLine(prompt)
  }

  def close(): Unit =
    userInput.signalClosed()
    // Defensive: normally withMonitoringCtrlC joins and nulls the thread,
    // but if close() is called during an abnormal exit, clean up here.
    monitoringThread match
      case thread: Thread =>
        Thread.interrupted() // clear interrupt flag in case user code interrupted this thread
        thread.join()
      case null =>
    try terminal.setAttributes(originalAttributes)
    finally terminal.close()

  def userInputStream: InputStream =
    userInput

  /** Execute a block while monitoring for Ctrl-C keypresses.
   *  Calls the handler when Ctrl-C is detected during block execution.
   */
  def withMonitoringCtrlC[T](handler: () => Unit)(block: => T): T = {
    // If you change Ctrl+C handling in any way, make sure you manually check that
    // reading from both `System.in` and `Console.in` still works in embedded hosts.
    // In raw mode, SIGINT is not generated by the terminal (Ctrl-C is detected
    // by reading byte 3 from the raw stream). This handler is a fallback for
    // external signals, e.g. `kill -INT`.
    val previousHandler = terminal.handle(Signal.INT, _ => handler())
    val reader = terminal.reader()
    userInput.startMonitoring()
    val thread = new Thread(() =>
      while userInput.waitUntilActive() == InputState.Monitoring do
        val ch =
          try reader.read(100L)
          catch case _: Exception => -1

        if ch == NonBlockingReader.READ_EXPIRED then ()
        else if ch == NonBlockingReader.EOF then userInput.signalClosed()
        else if ch == 3 then handler()
        else userInput.enqueueChar(ch)
    , "REPL-CtrlC-Monitor")
    monitoringThread = thread
    thread.setDaemon(true)
    thread.start()

    try block
    finally {
      userInput.signalClosed()
      Thread.interrupted() // clear interrupted flag so join below doesn't explode
      thread.join()
      monitoringThread = null
      terminal.handle(Signal.INT, previousHandler)
    }
  }

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
      def words: java.util.List[String] = java.util.Collections.emptyList[String]
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
          if token == ERROR && input(start) == '`' then
            lastBacktickErrorStart = Some(start)
          else
            lastBacktickErrorStart = None
        }
        null
      }

      def acceptLine = {
        val onLastLine = !input.substring(cursor).contains(System.lineSeparator)
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

/** A `System.in` wrapper that lets the REPL monitor raw terminal input for Ctrl-C
 *  without stealing bytes from user code reading from `System.in` / `Console.in`.
 *
 *  The monitor thread peeks at terminal input while REPL code is running. Any
 *  non-Ctrl-C input it sees is buffered here so later `read()` calls from user
 *  code observe the same bytes instead of losing them to the monitor.
 */
private final class UserInputStream(
  userLineReader: LineReader,
  encoding: java.nio.charset.Charset
) extends InputStream {
  private var bytes = new Array[Byte](16)
  private var byteCount = 0
  private var state = InputState.ForegroundRead
 
  /** Blocks until the state is no longer ForegroundRead. Returns the active state. */
  def waitUntilActive(): InputState = synchronized {
    while state == InputState.ForegroundRead do wait()
    state
  }

  def enqueueChar(ch: Int): Unit = synchronized {
    val encoded = String.valueOf(ch.toChar).getBytes(encoding)
    enqueueBytes(encoded)
  }

  def signalClosed(): Unit = synchronized {
    state = InputState.Closed
    notifyAll()
  }

  def startMonitoring(): Unit = synchronized {
    byteCount = 0
    state = InputState.Monitoring
    notifyAll()
  }

  private def resumeMonitoring(): Unit = synchronized {
    if state != InputState.Closed then
      state = InputState.Monitoring
    notifyAll()
  }

  private def enqueueBytes(data: Array[Byte]): Unit = synchronized {
    ensureCapacity(byteCount + data.length)
    Array.copy(data, 0, bytes, byteCount, data.length)
    byteCount += data.length
  }

  private def pollByte(): Option[Int] = synchronized {
    if byteCount > 0 then
      val value = bytes(0) & 0xff
      removePrefix(1)
      Some(value)
    else if state == InputState.Closed then Some(-1)
    else
      state = InputState.ForegroundRead
      None
  }

  private def drainTo(buf: Array[Byte], offset: Int, maxLen: Int): Int = synchronized {
    val n = math.min(maxLen, byteCount)
    Array.copy(bytes, 0, buf, offset, n)
    removePrefix(n)
    n
  }

  private def ensureCapacity(required: Int): Unit =
    if required > bytes.length then
      var newSize = bytes.length
      while newSize < required do newSize *= 2
      val newBytes = new Array[Byte](newSize)
      Array.copy(bytes, 0, newBytes, 0, byteCount)
      bytes = newBytes

  private def removePrefix(n: Int): Unit =
    byteCount -= n
    if byteCount > 0 then
      Array.copy(bytes, n, bytes, 0, byteCount)

  private def readUserInputByte(): Int = {
    while true do
      pollByte() match
        case Some(value) => return value
        case None =>
          try
            val line = userLineReader.readLine("")
            val lineBytes = (line + System.lineSeparator()).getBytes(encoding)
            enqueueBytes(lineBytes)
          catch
            case _: EndOfFileException =>
              return -1
            case _: UserInterruptException =>
              throw new InterruptedIOException()
          finally
            resumeMonitoring()

    -1
  }

  override def read(): Int =
    readUserInputByte()

  override def read(bytes: Array[Byte], offset: Int, length: Int): Int =
    if length == 0 then 0
    else
      val first = read()
      if first == -1 then -1
      else
        bytes(offset) = first.toByte
        drainTo(bytes, offset + 1, length - 1) + 1
}

private final class SimpleParser extends reader.Parser {
  private class ParsedLine(val inputLine: String, val inputCursor: Int) extends reader.CompletingParsedLine {
    def word(): String = inputLine
    def wordCursor(): Int = inputCursor
    def wordIndex(): Int = 0
    def words(): java.util.List[String] = java.util.List.of(inputLine)
    def line(): String = inputLine
    def cursor(): Int = inputCursor
    def escape(candidate: CharSequence, complete: Boolean): CharSequence = candidate
    def rawWordCursor(): Int = inputCursor
    def rawWordLength(): Int = inputLine.length
  }

  def parse(input: String, cursor: Int, context: ParseContext): reader.ParsedLine =
    new ParsedLine(input, cursor)
}