package dotty.tools.dotc.quoted

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.ContextRenamed
import dotty.tools.dotc.core.Phases.Phase

/** Compiler that takes the contents of a quoted expression (or type) and outputs it's tree. */
class QuoteDecompiler(output: tpd.Tree => ContextRenamed => Unit) extends QuoteCompiler {
  override def phases: List[List[Phase]] = List(
    List(new QuotedFrontend(putInClass = false)), // Create class from Expr
    List(new RefreshNames),
    List(new QuoteTreeOutput(output))
  )

  class QuoteTreeOutput(output: tpd.Tree => ContextRenamed => Unit) extends Phase {
    override def phaseName: String = "quoteOutput"
    override def run(implicit ctx: ContextRenamed): Unit = output(ctx.compilationUnit.tpdTree)(ctx)
  }
}
