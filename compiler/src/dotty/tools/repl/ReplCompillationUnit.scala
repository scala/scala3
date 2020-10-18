package dotty.tools
package repl

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.util.SourceFile


class ReplCompilationUnit(source: SourceFile) extends CompilationUnit(source):
  override def isSuspendable: Boolean = false
