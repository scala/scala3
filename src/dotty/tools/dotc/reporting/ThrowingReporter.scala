package dotty.tools
package dotc
package reporting

import core.Contexts.Context
import collection.mutable

/**
 * This class implements a Reporter that stores all messages
 */
class ThrowingReporter(reportInfo: Reporter) extends Reporter {
  protected def doReport(d: Diagnostic)(implicit ctx: Context): Unit =
    if (d.severity == Severity.ERROR) throw d else reportInfo.report(d)
}
