package dotty.tools.dotc
package decompiler

import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.core.tasty.TastyPrinter

/** Phase that prints the trees in all loaded compilation units.
 *
 *  @author Nicolas Stucki
 */
class DecompilationPrinter extends Phase {

  override def phaseName: String = "decompilationPrinter"

  override def run(implicit ctx: Context): Unit = {
    val unit = ctx.compilationUnit

    val pageWidth = ctx.settings.pageWidth.value

    val doubleLine = "=" * pageWidth
    val line = "-" * pageWidth

    println(doubleLine)
    println(unit.source)
    println(line)

    println(unit.tpdTree.show)
    println(line)

    if (ctx.settings.printTasty.value) {
      new TastyPrinter(unit.pickled.head._2).printContents()
      println(line)
    }
  }
}
