package dotty.tools
package dotc
package reporting

import scala.language.unsafeNulls

import collection.mutable.ListBuffer
import core.Contexts.Context
import Diagnostic.*

/** A re-usable Reporter used in Contexts#test */
class ExploringReporter extends StoreReporter(null, fromTyperState = false):
  infos = ListBuffer.empty[Diagnostic]

  override def hasUnreportedErrors: Boolean =
    infos.exists(_.isInstanceOf[Error])

  override def removeBufferedMessages(using Context): List[Diagnostic] =
    try infos.toList finally reset()

  override def mapBufferedMessages(f: Diagnostic => Diagnostic)(using Context): Unit =
    infos.mapInPlace(f)

  def reset(): Unit = infos.clear()

end ExploringReporter
