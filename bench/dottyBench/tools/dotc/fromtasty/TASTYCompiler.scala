package dottyBench.tools
package dotc
package fromtasty

import core._
import Contexts._
import Phases.Phase
import dottyBench.tools.dotc.transform._

class TASTYCompiler extends Compiler {

  override protected def frontendPhases: List[List[Phase]] =
    List(new ReadTasty) :: Nil

  override def newRun(using Context): Run = {
    reset()
    given CState = currentContext.cstate
    withMode(Mode.ReadPositions) {
      new TASTYRun(this, currentContext)
    }
  }
}
