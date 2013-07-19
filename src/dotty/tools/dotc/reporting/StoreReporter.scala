package dotty.tools
package dotc
package reporting

import core.Contexts.Context
import collection.mutable
import Reporter.Diagnostic

/**
 * This class implements a Reporter that stores all messages
 */
class StoreReporter(ctx: Context) extends Reporter(ctx) {

  val infos = new mutable.ListBuffer[Diagnostic]

  protected def doReport(d: Diagnostic)(implicit ctx: Context): Unit =
    infos += d

  def replay(implicit ctx: Context) =
    infos foreach ctx.reporter.report

}
