package dotty.tools
package dotc
package reporting

import scala.collection.mutable
import util.{SourcePosition, SourceFile}
import Reporter.Severity.{Value => Severity}
import core.Contexts.Context

/**
 * This trait implements `isHidden` do that multiple messages per position
 * are suppressed, unless they are of increasing severity.
 */
trait UniqueMessagePositions extends Reporter {

  private val positions =
    new mutable.HashMap[(SourceFile, Int), Severity]

  /** Logs a position and returns true if it was already logged.
   *  @note  Two positions are considered identical for logging if they have the same point.
   */
  override def isHidden(severity: Severity, pos: SourcePosition)(implicit ctx: Context): Boolean =
    pos.exists && {
      positions get (ctx.source, pos.point) match {
        case Some(level) if level >= severity => true
        case _ => positions((ctx.source, pos.point)) = severity; false
      }
    }

  override def reset(): Unit = {
    super.reset()
    positions.clear()
  }
}
