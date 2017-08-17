package dotty.tools
package repl

import dotty.Jars
import java.io.{ OutputStream, PrintStream, ByteArrayOutputStream }
import org.junit.{ Before, After }
import dotc.core.Contexts.Context
import org.junit.Assert.fail

import dotc.reporting.diagnostic.MessageContainer

sealed class NullPrintStream extends ByteArrayOutputStream {
  override def write(b: Int) = ()
  override def write(b: Array[Byte], off: Int, len: Int) = ()
  override def writeTo(out: OutputStream) = ()
}

sealed class StoringPrintStream extends PrintStream(new NullPrintStream) {
  private[this] var sb = new StringBuilder

  override def println(obj: Object) =
    println(obj.toString)

  override def println(str: String) = {
    sb.append(str)
    sb += '\n'
  }

  def flushStored(): String = {
    val str = sb.toString
    sb = new StringBuilder
    str
  }
}

class ReplTest extends ReplDriver(
  Array("-classpath", List(Jars.dottyLib, Jars.dottyInterfaces).mkString(":")),
  new StoringPrintStream
) {

  /** Fail if there are errors */
  def onErrors(xs: Seq[MessageContainer]): Unit =
    fail(s"Expected no errors, got: \n${ xs.map(_.message).mkString("\n") }")

  /** Get the stored output from `out`, resetting the `StoringPrintStream` */
  def storedOutput(): String =
    out.asInstanceOf[StoringPrintStream].flushStored()

  protected implicit def toParsed(expr: String): implicit Context => Parsed = {
    ParseResult(expr) match {
      case pr: Parsed => pr
      case pr =>
        throw new java.lang.AssertionError(s"Expected parsed, got: $pr")
    }
  }

  protected def withState[A](op: implicit (State, Context) => A): A =
    fromState(initState)(op)

  protected def fromState[A](state: State)(op: implicit (State, Context) => A): A = {
    implicit val nState = state.newRun(compiler, rootCtx)
    implicit val runCtx = nState.run.runContext
    op
  }

  /** Make sure the context is new before each test */
  @Before def init(): Unit =
    resetToInitial()

  /** Reset the stored output */
  @After def cleanup: Unit =
    storedOutput()
}

object ReplTest {
  implicit class SetEquals(val xs: Iterable[String]) extends AnyVal {
    def ===(other: Iterable[String]): Unit = {
      val sortedXs = xs.toVector.sorted
      val sortedOther = other.toVector.sorted

      val nonMatching = sortedXs
        .zip(sortedOther)
        .filterNot((a, b) => a == b)
        .map { (a, b) =>
          s"""expected element: "$a" got "$b""""
        }

      if (nonMatching.nonEmpty)
        fail(s"\nNot all elements matched:\n${ nonMatching.mkString("\n") }")
    }
  }
}
