/*
 * Dotty (https://dotty.epfl.ch/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 */

package dotty.tools.dotc.quoted

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Phases.Phase

/** Compiler that takes the contents of a quoted expression (or type) and outputs it's tree. */
class QuoteDecompiler(output: tpd.Tree => Context => Unit) extends QuoteCompiler {
  override def phases: List[List[Phase]] = List(
    List(new QuotedFrontend(putInClass = false)), // Create class from Expr
    List(new RefreshNames),
    List(new QuoteTreeOutput(output))
  )

  class QuoteTreeOutput(output: tpd.Tree => Context => Unit) extends Phase {
    override def phaseName: String = "quoteOutput"
    override def run(implicit ctx: Context): Unit = output(ctx.compilationUnit.tpdTree)(ctx)
  }
}
