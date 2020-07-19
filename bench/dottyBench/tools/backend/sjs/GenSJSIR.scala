package dottyBench.tools.backend.sjs

import dottyBench.tools.dotc.core._
import Contexts._
import Phases._

/** Generates Scala.js IR files for the compilation unit. */
class GenSJSIR extends Phase {
  def phaseName: String = "genSJSIR"

  override def isRunnable(using Ctx): Boolean =
    super.isRunnable && ctx.settings.scalajs.value

  def run(using Ctx, CState): Unit =
    new JSCodeGen().run()
}
