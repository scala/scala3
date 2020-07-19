package dottyBench.tools
package dotc
package fromtasty

import core.Contexts._

class TASTYRun(comp: Compiler, ictx: Context) extends Run(comp, ictx) {
  override def compile(classNames: List[String]): Unit = {
    val units = classNames.map(new TASTYCompilationUnit(_))
    compileUnits(units)
  }
}
