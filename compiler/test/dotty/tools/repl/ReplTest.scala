package dotty.tools
package repl

import dotty.Jars
import java.io.{ OutputStream, PrintStream, ByteArrayOutputStream }
import org.junit.{ Before, After }
import dotc.core.Contexts.Context
import dotc.reporting.MessageRendering
import org.junit.Assert.fail

import dotc.reporting.diagnostic.MessageContainer
import results.Result

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
  Array("-classpath", List(Jars.dottyLib, Jars.dottyInterfaces).mkString(":"), "-color:never"),
  new StoringPrintStream
) with MessageRendering {

  /** Get the stored output from `out`, resetting the `StoringPrintStream` */
  def storedOutput(): String =
    stripColor(out.asInstanceOf[StoringPrintStream].flushStored())

  implicit class StringToParsed(expr: String) {
    def toParsed(implicit state: State): Parsed = {
      implicit val ctx = state.run.runContext
      ParseResult(expr) match {
        case pr: Parsed => pr
        case pr =>
          throw new java.lang.AssertionError(s"Expected parsed, got: $pr")
      }
    }
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
    def andThen[A](op: State => A): A =
      op(state.newRun(compiler, rootCtx))
  }
}


object ReplTest {

  /** Fail if there are errors */
  def onErrors(xs: Seq[MessageContainer]): Nothing = throw new AssertionError(
    s"Expected no errors, got: \n${ xs.map(_.message).mkString("\n") }"
  )

  implicit class TestingStateResult(val res: Result[State]) extends AnyVal {
    def stateOrFail: State = res.fold(onErrors, x => x)
  }

  implicit class TestingStateResultTuple[A](val res: Result[(A, State)]) extends AnyVal {
    def stateOrFail: State = res.fold(onErrors, x => x._2)
  }

  implicit class SetEquals(val xs: Iterable[String]) extends AnyVal {
    def ===(other: Iterable[String]): Unit = {
      val sortedXs = xs.toVector.sorted
      val sortedOther = other.toVector.sorted

      val nonMatching = sortedXs
        .zip(sortedOther)
        .filterNot{ case (a, b) => a == b }
        .map { case (a, b) =>
          s"""expected element: "$a" got "$b""""
        }

      if (nonMatching.nonEmpty)
        fail(s"\nNot all elements matched:\n${ nonMatching.mkString("\n") }")
    }
  }
}
