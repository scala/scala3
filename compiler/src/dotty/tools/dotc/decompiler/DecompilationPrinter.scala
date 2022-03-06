package dotty.tools.dotc
package decompiler

import scala.language.unsafeNulls

import java.io.{OutputStream, PrintStream}
import java.nio.charset.StandardCharsets

import scala.io.Codec

import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.io.File

import scala.quoted.runtime.impl.QuotesImpl

/** Phase that prints the trees in all loaded compilation units.
 *
 *  @author Nicolas Stucki
 */
class DecompilationPrinter extends Phase {

  override def phaseName: String = "decompilationPrinter"

  override def run(using Context): Unit =
    if (ctx.settings.outputDir.isDefault) printToOutput(System.out)
    else {
      val outputDir = ctx.settings.outputDir.value
      var os: OutputStream = null
      var ps: PrintStream = null
      try {
        os = File(outputDir.fileNamed("decompiled.scala").path)(Codec.UTF8).outputStream(append = true)
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
