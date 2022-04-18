package dotty.tools
package dotc
package reporting

import org.junit.Test
import core.Contexts._

class TestMessageLaziness extends DottyTest {
  ctx = ctx.fresh.setReporter(new NonchalantReporter)

  class NonchalantReporter(using Context) extends Reporter
  with UniqueMessagePositions with HideNonSensicalMessages {
    def doReport(dia: Diagnostic)(using Context) = ???

    override def report(dia: Diagnostic)(using Context) = ()
  }

  case class LazyError() extends Message(ErrorMessageID.LazyErrorId) {
    val kind = MessageKind.NoKind
    def msg = throw new Error("Didn't stay lazy.")
    def explain = ""
  }

  @Test def assureLazy =
    report.error(LazyError())

  @Test def assureLazyExtendMessage =
    report.errorOrMigrationWarning(LazyError(), from = config.SourceVersion.future)
}
