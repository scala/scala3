package dotty.tools.dotc
package decompiler

import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Phases.Phase

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

    val code = unit.tpdTree.show
    println(if (ctx.useColors) printing.SyntaxHighlighting(code) else code)
    println(line)
  }
}
