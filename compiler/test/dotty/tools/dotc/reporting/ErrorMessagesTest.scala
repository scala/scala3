package dotty.tools
package dotc
package reporting

import core.Contexts.Context
import scala.collection.mutable
import org.junit.Assert._

trait ErrorMessagesTest extends DottyTest {

  protected def newContext = {
    val rep = new StoreReporter(null)
              with UniqueMessagePositions with HideNonSensicalMessages
    initialCtx.setReporter(rep).setSetting(ctx.settings.color, "never")
  }

  class Report(messages: List[Message], ictx: Context) {
    def expect(f: (Context, List[Message]) => Unit): Unit =
      f(ictx, messages)

    def expectNoErrors: Unit =
      assert(this.isInstanceOf[EmptyReport], "errors found when not expected")
  }

  class EmptyReport extends Report(Nil, null) {
    override def expect(f: (Context, List[Message]) => Unit) =
      fail("""|
              |Couldn't capture errors from compiled sources, this can happen if
              |there are no errors or the compiler crashes.""".stripMargin)
  }

  def checkMessagesAfter(checkAfterPhase: String)(source: String): Report = {
    ctx = newContext
    val runCtx = checkCompile(checkAfterPhase, source) { (_, _) => () }

    if (!runCtx.reporter.hasErrors) new EmptyReport
    else {
      val rep = runCtx.reporter.asInstanceOf[StoreReporter]
      val msgs = rep.removeBufferedMessages(using runCtx).map(_.msg).reverse
      new Report(msgs, runCtx)
    }
  }

  def assertMessageCount(expected: Int, messages: List[Message]): Unit =
    assertEquals(messages.mkString,
      expected,
      messages.length
    )
}
