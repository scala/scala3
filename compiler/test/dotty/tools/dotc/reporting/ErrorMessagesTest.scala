package dotty.tools
package dotc
package reporting

import diagnostic._
import core.Contexts.Context

import scala.collection.mutable

import org.junit.Assert._

trait ErrorMessagesTest extends DottyTest {

  ctx = freshReporter(ctx)

  private def freshReporter(ctx: Context) =
    ctx.fresh.setReporter(new CapturingReporter)

  class Report(messages: List[Message], ictx: Context) {
    def expect(f: (Context, List[Message]) => Unit): Unit = {
      f(ictx, messages)
    }

    def expectNoErrors: Unit =
      assert(this.isInstanceOf[EmptyReport], "errors found when not expected")
  }

  class EmptyReport extends Report(Nil, null) {
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
      buffer append m.contained()
    }

    def toReport: Report =
      if (capturedContext eq null)
        new EmptyReport
      else {
        val xs = buffer.reverse.toList
        buffer.clear()

        val ctx = capturedContext
        capturedContext = null

        new Report(xs, ctx)
      }
  }

  def checkMessagesAfter(checkAfterPhase: String)(source: String): Report = {
    checkCompile(checkAfterPhase, source) { (_,ictx) => () }
    val rep = ctx.reporter.asInstanceOf[CapturingReporter].toReport
    ctx = freshReporter(ctx)
    rep
  }

  def assertMessageCount(expected: Int, messages: List[Message]): Unit =
    assertEquals(messages.mkString,
      expected,
      messages.length
    )
}
