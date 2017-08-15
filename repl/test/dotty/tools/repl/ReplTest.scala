package dotty.tools
package repl

import java.io.{ OutputStream, PrintStream, ByteArrayOutputStream }
import org.junit.{ Before, After }
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
  Array(
    // TODO: get rid of this!
    "-classpath",
      List("../out/bootstrap/dotty-library-bootstrapped/scala-0.3/classes",
           "../interfaces/target/classes").mkString(":")
  ),
  new StoringPrintStream
) {

  /** Fail if there are errors */
  def onErrors(xs: Seq[MessageContainer]): Unit =
    fail(s"Expected no errors, got: \n${ xs.map(_.message).mkString("\n") }")

  /** Get the stored output from `out`, resetting the `StoringPrintStream` */
  def storedOutput(): String =
    out.asInstanceOf[StoringPrintStream].flushStored()

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
