package dotty.tools.backend.sjs

import dotty.tools.dotc.core._
import Contexts._
import Phases._

/** Generates Scala.js IR files for the compilation unit. */
class GenSJSIR extends Phase {
  def phaseName: String = "genSJSIR"

  override def isRunnable(using Context): Boolean =
    super.isRunnable && ctx.settings.scalajs.value

  def run(using Context): Unit =
    new JSCodeGen().run()
}
