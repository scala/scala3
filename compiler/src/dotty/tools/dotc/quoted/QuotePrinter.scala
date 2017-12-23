package dotty.tools.dotc.quoted

import java.io.PrintStream

import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Phases.Phase

/** Pretty prints the compilation unit to an output stream */
class QuotePrinter(out: PrintStream) extends Phase {

  override def phaseName: String = "quotePrinter"

  override def run(implicit ctx: Context): Unit = {
    val unit = ctx.compilationUnit
    out.print(unit.tpdTree.show)
  }
}
