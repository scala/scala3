package dotty.tools.dotc.consumetasty

import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.tastyreflect.ReflectionImpl

import scala.tasty.file.TastyConsumer

class TastyConsumerPhase(consumer: TastyConsumer) extends Phase {

  override def phaseName: String = "tastyConsumer"

  override def run(implicit ctx: Context): Unit = {
    val reflect = ReflectionImpl(ctx)
    consumer(reflect)(ctx.compilationUnit.tpdTree.asInstanceOf[reflect.Tree])
  }

}
