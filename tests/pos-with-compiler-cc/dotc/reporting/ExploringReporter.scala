package dotty.tools
package dotc
package reporting

import scala.language.unsafeNulls

import collection.mutable
import core.Contexts.Context
import Diagnostic._

/** A re-usable Reporter used in Contexts#test */
class ExploringReporter extends StoreReporter(null, fromTyperState = false):
  infos = new mutable.ListBuffer[Diagnostic]

  override def hasUnreportedErrors: Boolean =
    infos.exists(_.isInstanceOf[Error])

  override def removeBufferedMessages(using Context): List[Diagnostic] =
    try infos.toList finally reset()

  def reset(): Unit = infos.clear()

end ExploringReporter