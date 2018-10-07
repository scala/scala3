package dotty.tools.dotc.consumetasty

import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.tastyreflect.TastyImpl

import scala.tasty.file.TastyConsumer

class TastyConsumerPhase(consumer: TastyConsumer) extends Phase {

  override def phaseName: String = "tastyConsumer"

  override def run(implicit ctx: Context): Unit = {
    val tasty = new TastyImpl(ctx)
    consumer(tasty)(ctx.compilationUnit.tpdTree)
  }

}
