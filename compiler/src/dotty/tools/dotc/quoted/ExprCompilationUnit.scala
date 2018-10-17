package dotty.tools.dotc.quoted

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.util.NoSource

import scala.quoted.{Expr, StagingContext}

/* Compilation unit containing the contents of a quoted expression */
class ExprCompilationUnit(val expr: StagingContext => Expr[Any]) extends CompilationUnit(NoSource)
