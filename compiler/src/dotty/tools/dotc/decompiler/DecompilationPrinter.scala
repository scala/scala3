package dotty.tools.dotc
package decompiler

import java.io.{OutputStream, PrintStream}

import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.core.tasty.TastyPrinter
import dotty.tools.dotc.tastyreflect.ReflectionImpl
import dotty.tools.io.File

/** Phase that prints the trees in all loaded compilation units.
 *
 *  @author Nicolas Stucki
 */
class DecompilationPrinter extends Phase {

  override def phaseName: String = "decompilationPrinter"

  override def run(implicit ctx: Context): Unit = {
    if (ctx.settings.outputDir.isDefault) printToOutput(System.out)
    else {
      val outputDir = ctx.settings.outputDir.value
      var os: OutputStream = null
      var ps: PrintStream = null
      try {
        os = File(outputDir.fileNamed("decompiled.scala").path).outputStream(append = true)
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
    if (ctx.settings.printTasty.value) {
      new TastyPrinter(unit.pickled.head._2).printContents()
    } else {
      val unitFile = unit.source.toString.replace("\\", "/").replace(".class", ".tasty")
      out.println(s"/** Decompiled from $unitFile */")
      out.println(new ReflectionImpl(ctx).showSourceCode.showTree(unit.tpdTree)(ctx))
    }
  }
}
