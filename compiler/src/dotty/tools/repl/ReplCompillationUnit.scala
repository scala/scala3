package dotty.tools.repl

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.util.SourceFile


class ReplCompilationUnit(source: SourceFile) extends CompilationUnit(source) with
  override def isSuspendable: Boolean = false
