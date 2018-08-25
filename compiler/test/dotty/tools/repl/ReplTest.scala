package dotty.tools
package repl

import vulpix.TestConfiguration

import java.io.{ByteArrayOutputStream, PrintStream}

import dotty.tools.dotc.reporting.MessageRendering
import org.junit.{After, Before}


class ReplTest private (out: ByteArrayOutputStream) extends ReplDriver(
  Array("-classpath", TestConfiguration.basicClasspath, "-color:never"),
  new PrintStream(out)
) with MessageRendering {

  def this() = this(new ByteArrayOutputStream)

  /** Get the stored output from `out`, resetting the buffer */
  def storedOutput(): String = {
    val output = stripColor(out.toString)
    out.reset()
    output
  }

  /** Make sure the context is new before each test */
  @Before def init(): Unit =
    resetToInitial()

  /** Reset the stored output */
  @After def cleanup: Unit =
    storedOutput()

  def fromInitialState[A](op: State => A): A =
    op(initState)

  implicit class TestingState(state: State) {
    def andThen[A](op: State => A): A = op(state)
  }
}
