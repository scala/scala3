package dotty.tools
package dotc
package reporting

import core.Contexts.ContextRenamed
import diagnostic.MessageContainer
import diagnostic.messages.Error

/**
 * This class implements a Reporter that throws all errors and sends warnings and other
 * info to the underlying reporter.
 */
class ThrowingReporter(reportInfo: Reporter) extends Reporter {
  def doReport(m: MessageContainer)(implicit ctx: ContextRenamed): Unit = m match {
    case _: Error => throw m
    case _ => reportInfo.doReport(m)
  }
}
