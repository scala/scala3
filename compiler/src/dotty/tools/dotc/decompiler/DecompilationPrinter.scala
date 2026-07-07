package dotty.tools.dotc
package decompiler

import java.io.{OutputStream, PrintStream}
import java.nio.charset.StandardCharsets

import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Phases.Phase

import scala.quoted.runtime.impl.QuotesImpl

/** Phase that prints the trees in all loaded compilation units.
 *
 *  @author Nicolas Stucki
 */
class DecompilationPrinter extends Phase {

  override def phaseName: String = "decompilationPrinter"

  override protected def run(using Context): Unit =
    if (ctx.settings.outputDir.isDefault) printToOutput(System.out)
    else {
      val outputDir = ctx.settings.outputDir.value
      var os: OutputStream|Null = null
      var ps: PrintStream|Null = null
      try {
        os = outputDir.fileNamed("decompiled.scala").output(append = true)
        ps = new PrintStream(os, /* autoFlush = */ false, StandardCharsets.UTF_8.name)
        printToOutput(ps)
      }
      finally {
        if (os ne null) os.close()
        if (ps ne null) ps.close()
      }
    }

  private def printToOutput(out: PrintStream)(using Context): Unit = {
    val unit = ctx.compilationUnit
    val unitFile = unit.source.toString.replace("\\", "/").replace(".class", ".tasty")
    out.println(s"/** Decompiled from $unitFile */")
    out.println(QuotesImpl.showDecompiledTree(unit.tpdTree))
  }
}
