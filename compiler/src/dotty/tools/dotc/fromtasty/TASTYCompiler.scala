package dotty.tools
package dotc
package fromtasty

import core._
import Contexts._
import Phases.Phase
import dotty.tools.dotc.transform.Pickler

class TASTYCompiler extends Compiler {

  override def phases: List[List[Phase]] = {
    val backendPhases = super.phases.dropWhile {
      case List(_: Pickler) => false
      case _ => true
    }.tail
    List(new ReadTastyTreesFromClasses) :: backendPhases
  }

  override def newRun(implicit ctx: Context): Run = {
    reset()
    new TASTYRun(this, ctx)
  }
}
