package dotty.tools.dotc.fromtasty

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.util.NoSource
import dotty.tools.io.File

class TASTYCompilationUnit(val className: String) extends CompilationUnit(NoSource) {
  override def toString: String = s"class file $className"
}
object TASTYCompilationUnit {
  def apply(className: String)(implicit ctx: Context): Option[TASTYCompilationUnit] = {
    if (File(className).exists) {
      ctx.inform(s"Ignoring $className: cannot create a `TASTYCompilationUnit` for a source file.")
      None
    } else {
      Some(new TASTYCompilationUnit(className))
    }
  }
}
