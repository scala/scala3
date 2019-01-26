/*
 * Dotty (https://dotty.epfl.ch/)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package dotty.tools
package dotc
package reporting

import scala.collection.mutable
import util.SourceFile
import core.Contexts.Context
import diagnostic.MessageContainer

/** This trait implements `isHidden` so that multiple messages per position
  * are suppressed, unless they are of increasing severity. */
trait UniqueMessagePositions extends Reporter {

  private val positions = new mutable.HashMap[(SourceFile, Int), Int]

  /** Logs a position and returns true if it was already logged.
   *  @note  Two positions are considered identical for logging if they have the same point.
   */
  override def isHidden(m: MessageContainer)(implicit ctx: Context): Boolean =
    super.isHidden(m) || {
      m.pos.exists && !ctx.settings.YshowSuppressedErrors.value && {
        var shouldHide = false
        for (pos <- m.pos.start to m.pos.end) {
          positions get (ctx.source, pos) match {
            case Some(level) if level >= m.level => shouldHide = true
            case _ => positions((ctx.source, pos)) = m.level
          }
        }
        shouldHide
      }
    }
}
