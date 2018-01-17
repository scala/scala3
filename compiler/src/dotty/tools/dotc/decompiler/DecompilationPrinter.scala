package dotty.tools.dotc
package decompiler

import java.io.{OutputStream, PrintStream}

import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.core.tasty.TastyPrinter
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
        os = File(outputDir + ".decompiled").outputStream()
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

    val doubleLine = "=" * pageWidth
    val line = "-" * pageWidth

    out.println(doubleLine)
    out.println(unit.source)
    out.println(line)

    out.println(unit.tpdTree.show)
    out.println(line)

    if (ctx.settings.printTasty.value) {
      new TastyPrinter(unit.pickled.head._2).printContents()
      out.println(line)
    }
  }
}
