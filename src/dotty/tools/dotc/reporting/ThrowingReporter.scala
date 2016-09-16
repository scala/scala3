package dotty.tools
package dotc
package reporting

import core.Contexts.Context
import collection.mutable
import diagnostic.Message
import Reporter._

/**
 * This class implements a Reporter that throws all errors and sends warnings and other
 * info to the underlying reporter.
 */
class ThrowingReporter(reportInfo: Reporter) extends Reporter {
  def doReport(m: Message)(implicit ctx: Context): Unit = m match {
    case _: Error => throw m
    case _ => reportInfo.doReport(m)
  }
}
