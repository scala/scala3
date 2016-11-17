package dotty.tools
package dotc
package reporting

import org.junit.Assert._
import org.junit.Test

import core.Contexts._

import test.DottyTest

import diagnostic.{ Message, MessageContainer, ExtendMessage }

class TestMessageLaziness extends DottyTest {
  ctx = ctx.fresh.setReporter(new NonchalantReporter)

  class NonchalantReporter(implicit ctx: Context) extends Reporter
  with UniqueMessagePositions with HideNonSensicalMessages {
    def doReport(m: MessageContainer)(implicit ctx: Context) = ???

    override def report(m: MessageContainer)(implicit ctx: Context) = ()
  }

  case class LazyError() extends Message(1000) {
    throw new Error("Didn't stay lazy.")

    val kind = "Test"
    val msg = "Please don't blow up"
    val explanation = ""
  }

  @Test def assureLazy =
    ctx.error(LazyError())

  @Test def assureLazyExtendMessage =
    ctx.strictWarning(LazyError())
}
