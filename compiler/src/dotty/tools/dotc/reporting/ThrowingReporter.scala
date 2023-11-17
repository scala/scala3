package dotty.tools
package dotc
package reporting

import core.Contexts.*
import Diagnostic.Error

/**
 * This class implements a Reporter that throws all errors as UnhandledError exceptions
 * and sends warnings and other info to the underlying reporter.
 */
class ThrowingReporter(reportInfo: Reporter) extends Reporter {
  def doReport(dia: Diagnostic)(using Context): Unit = dia match {
    case dia: Error => throw UnhandledError(dia)
    case _ => reportInfo.doReport(dia)
  }
}

class UnhandledError(val diagnostic: Error) extends Exception:
  override def getMessage = diagnostic.message

