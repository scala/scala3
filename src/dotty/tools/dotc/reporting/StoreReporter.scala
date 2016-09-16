package dotty.tools
package dotc
package reporting

import core.Contexts.Context
import collection.mutable
import Reporter.{Error, Warning}
import config.Printers.typr
import diagnostic.Message
import diagnostic.basic._

/**
 * This class implements a Reporter that stores all messages
 */
class StoreReporter(outer: Reporter) extends Reporter {

  private var infos: mutable.ListBuffer[Message] = null

  def doReport(m: Message)(implicit ctx: Context): Unit = {
    typr.println(s">>>> StoredError: ${m.message}") // !!! DEBUG
    if (infos == null) infos = new mutable.ListBuffer
    infos += m
  }

  override def hasPending: Boolean = infos != null && {
    infos exists {
      case _: Error => true
      case _: Warning => true
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
