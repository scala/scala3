package dotty.tools
package dotc
package reporting

import core.Contexts.Context
import collection.mutable
import Reporter.Diagnostic
import config.Printers._

/**
 * This class implements a Reporter that stores all messages
 */
class StoreReporter extends Reporter {

  val infos = new mutable.ListBuffer[Diagnostic]

  protected def doReport(d: Diagnostic)(implicit ctx: Context): Unit = {
    typr.println(s">>>> StoredError: ${d.msg}") // !!! DEBUG
    infos += d
  }

  override def flush()(implicit ctx: Context) =
    infos foreach ctx.reporter.report
}
