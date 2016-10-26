package dotty.tools.backend.sjs

import dotty.tools.dotc.core._
import Contexts._
import Phases._

/** Generates Scala.js IR files for the compilation unit. */
class GenSJSIR extends Phase {
  def phaseName: String = "genSJSIR"

  def run(implicit ctx: Context): Unit = {
    new JSCodeGen().run()
  }
}
