package scala.quoted
package staging

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.util.NoSource

/** Compilation unit containing the contents of a quoted expression */
private class ExprCompilationUnit(val exprBuilder: Quotes => Expr[?]) extends CompilationUnit(NoSource)
