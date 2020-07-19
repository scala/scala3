package dottyBench.tools.dotc.fromtasty

import dottyBench.tools.dotc.CompilationUnit
import dottyBench.tools.dotc.util.NoSource

class TASTYCompilationUnit(val className: String) extends CompilationUnit(NoSource) {
  override def toString: String = s"class file $className"
}
