package dotty.tools.dotc.quoted

import java.io.PrintStream

import dotty.tools.dotc.core.Phases.Phase

/** Compiler that takes the contents of a quoted expression `expr` and produces outputs
 *  the pretty printed code.
 */
class ExprDecompiler(out: PrintStream) extends ExprCompiler(null) {
  override def phases: List[List[Phase]] = List(
    List(new ExprFrontend(putInClass = false)), // Create class from Expr
    List(new QuotePrinter(out)) // Print all loaded classes
  )
}
