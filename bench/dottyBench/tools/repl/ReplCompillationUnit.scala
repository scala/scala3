package dottyBench.tools.repl

import dottyBench.tools.dotc.CompilationUnit
import dottyBench.tools.dotc.util.SourceFile


class ReplCompilationUnit(source: SourceFile) extends CompilationUnit(source):
  override def isSuspendable: Boolean = false
