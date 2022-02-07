package dotty.tools
package dotc
package reporting

import scala.collection.mutable
import util.SourceFile
import core.Contexts._

/** This trait implements `isHidden` so that multiple messages per position
  * are suppressed, unless they are of increasing severity. */
trait UniqueMessagePositions extends Reporter {

  private val positions = new mutable.HashMap[(SourceFile, Integer), Diagnostic]

  /** Logs a position and returns true if it was already logged.
   *  @note  Two positions are considered identical for logging if they have the same point.
   */
  override def isHidden(dia: Diagnostic)(using Context): Boolean =
    extension (dia1: Diagnostic) def hides(dia2: Diagnostic): Boolean =
      if dia2.msg.showAlways then dia1.msg.getClass == dia2.msg.getClass
      else dia1.level >= dia2.level
    super.isHidden(dia) || {
      dia.pos.exists
      && !ctx.settings.YshowSuppressedErrors.value
      && {
        var shouldHide = false
        for (pos <- dia.pos.start to dia.pos.end)
          positions get (ctx.source, pos) match {
            case Some(dia1) if dia1.hides(dia) => shouldHide = true
            case _ => positions((ctx.source, pos)) = dia
          }
        shouldHide
      }
    }
}
