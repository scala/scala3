package dotty.tools
package dotc
package reporting

import core.Contexts.Context
import collection.mutable
import Reporter._

/**
 * This class implements a Reporter that throws all errors and sends warnings and other
 * info to the underlying reporter.
 */
class ThrowingReporter(reportInfo: Reporter) extends Reporter {
  def doReport(d: Diagnostic)(implicit ctx: Context): Boolean = d match {
    case _: Error => throw d
    case _ => reportInfo.doReport(d)
  }
}
