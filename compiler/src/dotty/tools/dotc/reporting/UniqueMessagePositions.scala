package dotty.tools
package dotc
package reporting

import scala.collection.mutable
import util.SourceFile
import core.Contexts._

/** This trait implements `isHidden` so that multiple messages per position
  * are suppressed, unless they are of increasing severity. */
trait UniqueMessagePositions extends Reporter {

  private val positions = new mutable.HashMap[(SourceFile, Int), Int]

  /** Logs a position and returns true if it was already logged.
   *  @note  Two positions are considered identical for logging if they have the same point.
   */
  override def isHidden(dia: Diagnostic)(using Context): Boolean =
    super.isHidden(dia) || {
      dia.pos.exists && !ctx.settings.YshowSuppressedErrors.value && {
        var shouldHide = false
        for (pos <- dia.pos.start to dia.pos.end)
          positions get (ctx.source, pos) match {
            case Some(level) if level >= dia.level => shouldHide = true
            case _ => positions((ctx.source, pos)) = dia.level
          }
        shouldHide
      }
    }
}
