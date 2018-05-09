package dotty.tools.dotc
package decompiler

import java.io.{OutputStream, PrintStream}

import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.core.tasty.TastyPrinter
import dotty.tools.dotc.printing.DecompilerPrinter
import dotty.tools.io.{File, Path}

/** Phase that prints the trees in all loaded compilation units.
 *
 *  @author Nicolas Stucki
 */
class DecompilationPrinter extends Phase {

  override def phaseName: String = "decompilationPrinter"

  override def run(implicit ctx: Context): Unit = {
    val outputDir = ctx.settings.outputDir.value
    if (outputDir == ".") printToOutput(System.out)
    else {
      var os: OutputStream = null
      var ps: PrintStream = null
      try {
        os = File(outputDir + "/decompiled.scala").outputStream(append = true)
        ps = new PrintStream(os)
        printToOutput(ps)
      } finally {
        if (os ne null) os.close()
        if (ps ne null) ps.close()
      }
    }
  }

  private def printToOutput(out: PrintStream)(implicit ctx: Context): Unit = {
    val unit = ctx.compilationUnit
    val pageWidth = ctx.settings.pageWidth.value
    val printLines = ctx.settings.printLines.value

    out.println(s"/** Decompiled from $unit */")
    val printer = new DecompilerPrinter(ctx)
    out.println(printer.toText(unit.tpdTree).mkString(pageWidth, printLines))

    if (ctx.settings.printTasty.value) {
      out.println("/*")
      new TastyPrinter(unit.pickled.head._2).printContents()
      out.println("*/")
    }
  }
}
