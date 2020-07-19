package dottyBench.tools.dotc
package decompiler

import java.io.{OutputStream, PrintStream}

import scala.io.Codec

import dottyBench.tools.dotc.core.Contexts._
import dottyBench.tools.dotc.core.Phases.Phase
import dottyBench.tools.dotc.core.tasty.TastyPrinter
import dottyBench.tools.dotc.tastyreflect.ReflectionImpl
import dottyBench.tools.io.File

/** Phase that prints the trees in all loaded compilation units.
 *
 *  @author Nicolas Stucki
 */
class DecompilationPrinter extends Phase {

  override def phaseName: String = "decompilationPrinter"

  override def run(using Ctx, CState): Unit =
    if (ctx.settings.outputDir.isDefault) printToOutput(System.out)
    else {
      val outputDir = ctx.settings.outputDir.value
      var os: OutputStream = null
      var ps: PrintStream = null
      try {
        os = File(outputDir.fileNamed("decompiled.scala").path)(Codec.UTF8).outputStream(append = true)
        ps = new PrintStream(os, /* autoFlush = */ false, "UTF-8")
        printToOutput(ps)
      }
      finally {
        if (os ne null) os.close()
        if (ps ne null) ps.close()
      }
    }

  private def printToOutput(out: PrintStream)(using Ctx, CState): Unit = {
    val unit = ctx.compilationUnit
    if (ctx.settings.printTasty.value)
      println(new TastyPrinter(unit.pickled.head._2).printContents())
    else {
      val unitFile = unit.source.toString.replace("\\", "/").replace(".class", ".tasty")
      out.println(s"/** Decompiled from $unitFile */")
      out.println(ReflectionImpl.showTree(unit.tpdTree)(using combinedContext))
    }
  }
}
