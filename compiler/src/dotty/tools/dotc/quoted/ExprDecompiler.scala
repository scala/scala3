package dotty.tools.dotc.quoted

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Phases.Phase

/** Compiler that takes the contents of a quoted expression `expr` and outputs it's tree. */
class ExprDecompiler(output: tpd.Tree => Context => Unit) extends ExprCompiler(null) {
  override def phases: List[List[Phase]] = List(
    List(new ExprFrontend(putInClass = false)), // Create class from Expr
    List(new QuoteTreeOutput(output))
  )

  class QuoteTreeOutput(output: tpd.Tree => Context => Unit) extends Phase {
    override def phaseName: String = "quotePrinter"
    override def run(implicit ctx: Context): Unit = output(ctx.compilationUnit.tpdTree)(ctx)
  }
}
