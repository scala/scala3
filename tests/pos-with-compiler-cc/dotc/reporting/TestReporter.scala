package dotty.tools
package dotc
package reporting

import scala.language.unsafeNulls

import collection.mutable
import Diagnostic._

/** A re-usable Reporter used in Contexts#test */
class TestingReporter extends StoreReporter(null, fromTyperState = false):
  infos = new mutable.ListBuffer[Diagnostic]
  override def hasUnreportedErrors: Boolean = infos.exists(_.isInstanceOf[Error])
  def reset(): Unit = infos.clear()
