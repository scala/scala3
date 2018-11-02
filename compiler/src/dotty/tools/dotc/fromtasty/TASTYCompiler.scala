package dotty.tools
package dotc
package fromtasty

import core._
import Contexts._
import Phases.Phase
import dotty.tools.dotc.transform._

class TASTYCompiler extends Compiler {

  override protected def frontendPhases: List[List[Phase]] =
    List(new ReadTastyTreesFromClasses) :: Nil

  override protected def picklerPhases: List[List[Phase]] =
    super.picklerPhases.map(_.filterNot(_.isInstanceOf[Pickler])) // No need to repickle

  override def newRun(implicit ctx: ContextRenamed): Run = {
    reset()
    new TASTYRun(this, ctx.addMode(Mode.ReadPositions))
  }
}
