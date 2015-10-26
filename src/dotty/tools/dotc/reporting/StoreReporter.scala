package dotty.tools
package dotc
package reporting

import core.Contexts.Context
import collection.mutable
import Reporter.{Diagnostic, Error, Warning}
import config.Printers._

/**
 * This class implements a Reporter that stores all messages
 */
class StoreReporter(outer: Reporter) extends Reporter {

  private var infos: mutable.ListBuffer[Diagnostic] = null

  def doReport(d: Diagnostic)(implicit ctx: Context): Boolean = {
    typr.println(s">>>> StoredError: ${d.msg}") // !!! DEBUG
    if (infos == null) infos = new mutable.ListBuffer
    infos += d
    true
  }

  override def hasPending: Boolean = infos != null && {
    infos exists {
      case d: Error => true
      case d: Warning => true
      case _ => false
    }
  }

  override def flush()(implicit ctx: Context) =
    if (infos != null) {
      infos foreach ctx.reporter.report
      infos = null
    }

  override def errorsReported = hasErrors || outer.errorsReported
}
