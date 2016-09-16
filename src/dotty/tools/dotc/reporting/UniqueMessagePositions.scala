package dotty.tools
package dotc
package reporting

import scala.collection.mutable
import util.{SourcePosition, SourceFile}
import core.Contexts.Context
import diagnostic.Message

/**
 * This trait implements `isHidden` so that multiple messages per position
 * are suppressed, unless they are of increasing severity.
 */
trait UniqueMessagePositions extends Reporter {

  private val positions = new mutable.HashMap[(SourceFile, Int), Int]

  /** Logs a position and returns true if it was already logged.
   *  @note  Two positions are considered identical for logging if they have the same point.
   */
  override def isHidden(m: Message)(implicit ctx: Context): Boolean =
    super.isHidden(m) || {
      m.pos.exists && {
        positions get (ctx.source, m.pos.point) match {
          case Some(level) if level >= m.level => true
          case _ => positions((ctx.source, m.pos.point)) = m.level; false
        }
      }
    }
}
