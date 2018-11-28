package dotty.tools.dotc.fromtasty

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.util.NoSource

class TASTYCompilationUnit(val className: String) extends CompilationUnit(NoSource) {
  override def toString: String = s"class file $className"
}
