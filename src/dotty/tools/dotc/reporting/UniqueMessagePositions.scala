package dotty.tools
package dotc
package reporting

import scala.collection.mutable
import util.{SourcePosition, SourceFile}
import core.Contexts.Context

/**
 * This trait implements `isHidden` do that multiple messages per position
 * are suppressed, unless they are of increasing severity.
 */
trait UniqueMessagePositions extends Reporter {

  private val positions = new mutable.HashMap[(SourceFile, Int), Severity]

  /** Logs a position and returns true if it was already logged.
   *  @note  Two positions are considered identical for logging if they have the same point.
   */
  override def isHidden(d: Diagnostic)(implicit ctx: Context): Boolean =
    super.isHidden(d) || {
      d.pos.exists && {
        positions get (ctx.source, d.pos.point) match {
          case Some(s) if s.level >= d.severity.level => true
          case _ => positions((ctx.source, d.pos.point)) = d.severity; false
        }
      }
    }
}
