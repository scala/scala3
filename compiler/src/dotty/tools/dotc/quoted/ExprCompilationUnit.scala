package dotty.tools.dotc.quoted

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.util.NoSource

import scala.quoted._

/* Compilation unit containing the contents of a quoted expression */
class ExprCompilationUnit(val exprBuilder: QuoteContext => Expr[_]) extends CompilationUnit(NoSource)
