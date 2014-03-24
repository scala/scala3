package dotty.tools
package dotc
package reporting

import core.Contexts.Context
import collection.mutable
import config.Printers._

/**
 * This class implements a Reporter that stores all messages
 */
class StoreReporter extends Reporter {

  private var infos: mutable.ListBuffer[Diagnostic] = null

  protected def doReport(d: Diagnostic)(implicit ctx: Context): Unit = {
    typr.println(s">>>> StoredError: ${d.msg}") // !!! DEBUG
    if (infos == null) infos = new mutable.ListBuffer
    infos += d
  }

  override def flush()(implicit ctx: Context) =
    if (infos != null) infos foreach ctx.reporter.report
}
