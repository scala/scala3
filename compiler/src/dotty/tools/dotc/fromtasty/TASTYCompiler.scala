package dotty.tools
package dotc
package fromtasty

import core.*
import Contexts.*
import Phases.Phase

class TASTYCompiler extends Compiler {

  override protected def frontendPhases: List[List[Phase]] =
    List(new ReadTasty) :: Nil

  override def newRun(using Context): Run = {
    reset()
    new TASTYRun(this, ctx.addMode(Mode.ReadPositions))
  }
}
