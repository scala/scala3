package dotty.tools
package repl

import scala.language.unsafeNulls

import java.io.{ByteArrayOutputStream, PipedInputStream, PipedOutputStream}

import org.junit.Assert.*
import org.junit.Test
import org.jline.terminal.TerminalBuilder
import org.jline.utils.NonBlockingReader

class JLineTerminalTests:

  /** Build a `JLineTerminal` backed by piped streams we control, so the test
   *  works in CI (no TTY) and lets us verify the terminal's input is alive
   *  without depending on `System.in`.
   */
  private def withTestTerminal(test: (JLineTerminal, PipedOutputStream) => Unit): Unit =
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

  /** Regression for the JLine 4.x change where `terminal.reader().close()`
   *  inside `withMonitoringCtrlC`'s finally block closes the terminal's shared
   *  input reader, causing the next `readLine()` to receive immediate EOF and
   *  the REPL to exit silently after the user's first command.
   */
  @Test def `withMonitoringCtrlC keeps the terminal reader alive`(): Unit =
    withTestTerminal: (term, _) =>
      // Sanity: empty pipe returns READ_EXPIRED (-2), not EOF (-1).
      assertEquals(
        "expected READ_EXPIRED on empty open pipe before monitoring",
        NonBlockingReader.READ_EXPIRED, term.peekTerminalReader(20L))

      term.withMonitoringCtrlC(() => ())(())

      // The reader must still be open after withMonitoringCtrlC; otherwise
      // the next REPL `readLine()` would observe EOF and the REPL would exit.
      assertEquals(
        "terminal reader was closed by withMonitoringCtrlC",
        NonBlockingReader.READ_EXPIRED, term.peekTerminalReader(20L))

      // And it must survive multiple round-trips, mirroring successive REPL commands.
      term.withMonitoringCtrlC(() => ())(())
      assertEquals(
        "terminal reader was closed after a second withMonitoringCtrlC",
        NonBlockingReader.READ_EXPIRED, term.peekTerminalReader(20L))

  /** Sanity check: bytes written to the pipe before monitoring are visible
   *  to the terminal reader after monitoring. This guards against a
   *  regression where the terminal's input is silently disconnected.
   */
  @Test def `terminal reader receives bytes after withMonitoringCtrlC`(): Unit =
    withTestTerminal: (term, pos) =>
      term.withMonitoringCtrlC(() => ())(())
      pos.write('x'.toInt)
      pos.flush()
      assertEquals(
        "terminal reader did not receive byte after withMonitoringCtrlC",
        'x'.toInt, term.peekTerminalReader(500L))
