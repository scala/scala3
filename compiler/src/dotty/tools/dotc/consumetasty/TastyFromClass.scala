package dotty.tools.dotc.consumetasty

import dotty.tools.dotc.Run
import dotty.tools.dotc.core.Mode
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.fromtasty._

import scala.tasty.file.TastyConsumer

class TastyFromClass(consumer: TastyConsumer) extends TASTYCompiler {

  override protected def frontendPhases: List[List[Phase]] =
    List(new ReadTasty) :: // Load classes from tasty
    Nil

  override protected def picklerPhases: List[List[Phase]] = Nil
  override protected def transformPhases: List[List[Phase]] = Nil

  override protected def backendPhases: List[List[Phase]] =
    List(new TastyConsumerPhase(consumer)) ::  // Print all loaded classes
    Nil

  override def newRun(implicit ctx: Context): Run = {
    reset()
    new TASTYRun(this, ctx.fresh.addMode(Mode.ReadPositions).addMode(Mode.ReadComments))
  }
}
