/*
 * Dotty (https://dotty.epfl.ch/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 */

package dotty.tools.backend.sjs

import dotty.tools.dotc.core._
import Contexts._
import Phases._

/** Generates Scala.js IR files for the compilation unit. */
class GenSJSIR extends Phase {
  def phaseName: String = "genSJSIR"

  override def isRunnable(implicit ctx: Context): Boolean =
    super.isRunnable && ctx.settings.scalajs.value

  def run(implicit ctx: Context): Unit =
    new JSCodeGen().run()
}
