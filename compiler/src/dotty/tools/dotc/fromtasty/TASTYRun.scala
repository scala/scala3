package dotty.tools
package dotc
package fromtasty

import core.Contexts._

class TASTYRun(comp: Compiler, ictx: Context) extends Run(comp, ictx) {
  override def compile(classNames: List[String]) = {
    val units = classNames.flatMap(TASTYCompilationUnit(_)(ictx))
    compileUnits(units)
  }
}
