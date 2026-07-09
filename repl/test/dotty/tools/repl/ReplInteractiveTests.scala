package dotty.tools
package repl

import scala.language.unsafeNulls

import java.io.{ByteArrayOutputStream, PipedInputStream, PipedOutputStream}
import java.nio.charset.StandardCharsets
import java.util.concurrent.{Executors, TimeUnit, TimeoutException}

import dotc.core.Contexts.Context
import org.junit.Assert.*
import org.junit.Test
import org.jline.reader.{Candidate, Completer, LineReader, ParsedLine}
import org.jline.terminal.TerminalBuilder

/** Interactive-mode accept-line behaviour for lone directives/commands and pastes. */
class ReplInteractiveTests:

  private lazy val context: Context = new ReplTest().initialState.context

  private def contextually[A](op: Context ?=> A): A = op(using context)

  @Test def `lone dep command is not incomplete`(): Unit = contextually:
    assertFalse(ParseResult.isIncomplete(":dep com.lihaoyi::os-lib:0.11.8"))

  @Test def `lone using directive is not incomplete`(): Unit = contextually:
    assertFalse(ParseResult.isIncomplete("//> using dep com.lihaoyi::os-lib:0.11.8"))

  @Test def `single-line dep accepts when nothing is pending`(): Unit = contextually:
    val input = ":dep com.lihaoyi::os-lib:0.11.8"
    assertTrue(ParseResult.shouldAcceptLine(input, hasPendingInput = false))

  @Test def `single-line directive accepts when nothing is pending`(): Unit = contextually:
    val input = "//> using dep com.lihaoyi::os-lib:0.11.8"
    assertTrue(ParseResult.awaitsTrailingCode(input))
    assertTrue(ParseResult.shouldAcceptLine(input, hasPendingInput = false))

  @Test def `lone directive defers when pasted code is already buffered`(): Unit = contextually:
    val input = "//> using dep com.lihaoyi::os-lib:0.11.8"
    assertFalse(ParseResult.shouldAcceptLine(input, hasPendingInput = true))

  @Test def `lone dep defers when pasted code is already buffered`(): Unit = contextually:
    val input = ":dep com.lihaoyi::os-lib:0.11.8"
    assertFalse(ParseResult.shouldAcceptLine(input, hasPendingInput = true))

  @Test def `directive with code accepts when nothing is pending`(): Unit = contextually:
    val input = "//> using dep com.lihaoyi::os-lib:0.11.8\nval x = 1"
    assertFalse(ParseResult.awaitsTrailingCode(input))
    assertTrue(ParseResult.shouldAcceptLine(input, hasPendingInput = false))

  private val noopCompleter = new Completer:
    def complete(reader: LineReader, line: ParsedLine, candidates: java.util.List[Candidate]): Unit = ()

  private def withTestTerminal[A](test: (JLineTerminal, PipedOutputStream) => A): A =
    val pos = new PipedOutputStream
    val pis = new PipedInputStream(pos)
    val sink = new ByteArrayOutputStream
    val terminal =
      TerminalBuilder.builder()
        .system(false)
        .streams(pis, sink)
        .dumb(true)
        .build()
    val jlt = new JLineTerminal(terminal)
    try test(jlt, pos)
    finally jlt.close()

  /** Feed `input` and assert `readLine` returns within `timeoutMs` (i.e. a single
   *  ENTER submitted it). Dumb terminals bind accept-line to LF, so ENTER is `\n`.
   */
  private def readPastedLine(input: String, timeoutMs: Long = 5000L): String =
    withTestTerminal: (term, pos) =>
      val executor = Executors.newSingleThreadExecutor: r =>
        val t = new Thread(r, "repl-interactive-test")
        t.setDaemon(true)
        t
      try
        val future = executor.submit(() => term.readLine(noopCompleter)(using context))
        pos.write(input.getBytes(StandardCharsets.UTF_8))
        pos.flush()
        try future.get(timeoutMs, TimeUnit.MILLISECONDS)
        catch
          case _: TimeoutException =>
            future.cancel(true)
            throw new AssertionError(
              s"readLine blocked awaiting a second ENTER on: ${input.replace("\n", "\\n")}")
      finally
        executor.shutdownNow()

  @Test def `single-line dep command submits on one ENTER`(): Unit =
    assertEquals(":dep com.lihaoyi::os-lib:0.11.8", readPastedLine(":dep com.lihaoyi::os-lib:0.11.8\n"))

  @Test def `single-line directive submits on one ENTER`(): Unit =
    assertEquals("//> using dep com.lihaoyi::os-lib:0.11.8", readPastedLine("//> using dep com.lihaoyi::os-lib:0.11.8\n"))

  @Test def `directive and code paste assembles into one submission`(): Unit =
    assertEquals(
      "//> using dep com.lihaoyi::os-lib:0.11.8\nval x = 1",
      readPastedLine("//> using dep com.lihaoyi::os-lib:0.11.8\nval x = 1\n"))

  /** What a single `readLine` submits for `input`, or `waitsForMore` if it keeps
   *  reading (the input was treated as incomplete). On timeout the piped input is
   *  closed so the blocked reader unwinds cleanly and never wedges the suite.
   */
  private val waitsForMore = "<waits-for-more>"
  private def submittedOrWaiting(input: String, timeoutMs: Long = 4000L): String =
    val pos = new PipedOutputStream
    val pis = new PipedInputStream(pos)
    val sink = new ByteArrayOutputStream
    val terminal =
      TerminalBuilder.builder().system(false).streams(pis, sink).dumb(true).build()
    val jlt = new JLineTerminal(terminal)
    val executor = Executors.newSingleThreadExecutor: r =>
      val t = new Thread(r, "repl-interactive-test")
      t.setDaemon(true)
      t
    try
      val future = executor.submit(() => jlt.readLine(noopCompleter)(using context))
      pos.write(input.getBytes(StandardCharsets.UTF_8))
      pos.flush()
      try future.get(timeoutMs, TimeUnit.MILLISECONDS)
      catch case _: TimeoutException =>
        pos.close() // EOF lets the blocked reader finish so cleanup does not hang
        try future.get(2000, TimeUnit.MILLISECONDS) catch case _: Exception => ()
        waitsForMore
    finally
      executor.shutdownNow()
      try jlt.close() catch case _: Throwable => ()

  @Test def `command then incomplete code is not submitted as one line`(): Unit =
    val incomplete = ":settings -deprecation\nif true then"
    assertNotEquals(incomplete, submittedOrWaiting(incomplete + "\n"))

  @Test def `code with incomplete trailing expression is not submitted as one line`(): Unit =
    val incomplete = "val x = 5\nif x == 5 then"
    assertNotEquals(incomplete, submittedOrWaiting(incomplete + "\n"))
