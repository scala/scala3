package dotty.tools.backend.sjs

import dotty.tools.dotc.core.*
import Contexts.*
import Phases.*

/** Generates Scala.js IR files for the compilation unit. */
class GenSJSIR extends Phase {

  override def phaseName: String = GenSJSIR.name

  override def description: String = GenSJSIR.description

  override def isEnabled(using Context): Boolean =
    ctx.settings.scalajs.value

  override def isRunnable(using Context): Boolean =
    super.isRunnable && !ctx.usedBestEffortTasty

  override protected def run(using Context): Unit =
    new JSCodeGen().run()
}

object GenSJSIR:
  val name: String = "genSJSIR"
  val description: String = "generate .sjsir files for Scala.js"
