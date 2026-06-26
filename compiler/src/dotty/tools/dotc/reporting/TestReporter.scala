package dotty.tools
package dotc
package reporting

import collection.mutable
import Diagnostic.*

/** A re-usable Reporter used in Contexts#test */
class TestingReporter extends StoreReporter(null, fromTyperState = false):
  infos = new mutable.ListBuffer[Diagnostic]
  override def hasUnreportedErrors: Boolean = infos.nn.exists(_.isInstanceOf[Error])
  def reset(): Unit = infos.nn.clear()
