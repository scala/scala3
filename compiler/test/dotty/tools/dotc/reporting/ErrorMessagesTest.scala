package dotty.tools
package dotc
package reporting

import diagnostic._
import core.Contexts.Context

import scala.collection.mutable

import org.junit.Assert._

trait ErrorMessagesTest extends DottyTest {

  class Report(messages: List[Message], ictx: Context) {
    def expect(f: (Context, List[Message]) => Unit): Unit = {
      f(ictx, messages)
    }

    def expectNoErrors: Unit = this.isInstanceOf[FailedReport]
  }

  class FailedReport extends Report(Nil, null) {
    override def expect(f: (Context, List[Message]) => Unit) =
      fail("""|
              |Couldn't capture errors from compiled sources, this can happen if
              |there are no errors or the compiler crashes.""".stripMargin)
  }

  class CapturingReporter extends Reporter
  with UniqueMessagePositions with HideNonSensicalMessages {
    private[this] val buffer = new mutable.ListBuffer[Message]
    private[this] var capturedContext: Context = _

    def doReport(m: MessageContainer)(implicit ctx: Context) = {
      capturedContext = ctx
      buffer append m.contained
    }

    def toReport: Report =
      if (capturedContext eq null)
        new FailedReport
      else {
        val xs = buffer.reverse.toList
        buffer.clear()

        val ctx = capturedContext
        capturedContext = null

        new Report(xs, ctx)
      }
  }

  ctx = freshReporter(ctx)

  private def freshReporter(ctx: Context) =
    ctx.fresh.setReporter(new CapturingReporter)

  def checkMessages(source: String): Report = {
    checkCompile("frontend", source) { (_,ictx) => () }
    val rep = ctx.reporter.asInstanceOf[CapturingReporter].toReport
    ctx = freshReporter(ctx)
    rep
  }

  def messageCount(expected: Int, messages: List[Message]): Unit =
    assertEquals(
      expected,
      messages.length
    )
}
