package dotty.tools
package dottydoc
package core

import dotc.CompilationUnit
import dotc.core.Contexts.Context
import dotc.core.Phases.Phase
import model.{Package, Entity}

/** TODO: re-write to `DocMiniPhase` */
class PrintPhase extends Phase {
  def phaseName = "docPrintPhase"

  var currentRun = 0
  override def run(implicit ctx: Context): Unit = ()

  override def runOn(units: List[CompilationUnit])(implicit ctx: Context): List[CompilationUnit] = {
    val compUnits = super.runOn(units)
    val packages = ctx.base.packages[Package].toMap

    val outputDir = {
      val out = ctx.settings.DocOutput.value
      if (out.last == '/') out.dropRight(1)
      else out
    }
    if (!ctx.settings.YDocNoWrite.value) (new util.OutputWriter).write(packages, outputDir)

    compUnits
  }
}
