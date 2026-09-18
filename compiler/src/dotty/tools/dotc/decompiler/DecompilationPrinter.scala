package dotty.tools.dotc
package decompiler

import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Phases.Phase

import scala.io.Codec
import scala.quoted.runtime.impl.QuotesImpl

/** Phase that prints the trees in all loaded compilation units.
 *
 *  @author Nicolas Stucki
 */
class DecompilationPrinter extends Phase {

  override def phaseName: String = "decompilationPrinter"

  override protected def run(using Context): Unit =
    if ctx.settings.outputDir.isDefault then
      printed().foreach(System.out.println)
    else
      val outputDir = ctx.settings.outputDir.value
      val outputFile = outputDir.getOrCreateFile("decompiled.scala")
      outputFile.writeLines(printed(), Codec.UTF8, append = true)

  private def printed()(using Context): List[String] = {
    val unit = ctx.compilationUnit
    val unitFile = unit.source.toString.replace("\\", "/").replace(".class", ".tasty")
    List(
      s"/** Decompiled from $unitFile */",
      QuotesImpl.showDecompiledTree(unit.tpdTree)
    )
  }
}
