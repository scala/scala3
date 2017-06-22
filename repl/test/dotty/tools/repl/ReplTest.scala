package dotty.tools
package repl

import java.io.{ OutputStream, PrintStream, ByteArrayOutputStream }
import org.junit.Before
import org.junit.Assert.fail

import dotc.reporting.diagnostic.MessageContainer

sealed class NullPrintStream extends ByteArrayOutputStream {
  override def write(b: Int) = ()
  override def write(b: Array[Byte], off: Int, len: Int) = ()
  override def writeTo(out: OutputStream) = ()
}

class ReplTest extends Repl(
  Array(
    // TODO: get rid of this!
    "-classpath",
      List("../out/bootstrap/dotty-library-bootstrapped/scala-0.2/classes",
           "../interfaces/target/classes").mkString(":")
  ),
  None,
  new PrintStream(new NullPrintStream)
) {

  val initState = State(0, 0, Nil)

  def onErrors(xs: Seq[MessageContainer]): Unit =
    fail(s"Expected no errors, got: \n${ xs.map(_.message).mkString("\n") }")

  /** Make sure the context is new before each test */
  @Before def init = {
    myCtx = initializeCtx
    compiler = new ReplCompiler(myCtx)
  }
}

