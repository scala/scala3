/*
 * Dotty (https://dotty.epfl.ch/)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package dotty.tools.dotc.consumetasty

import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.tastyreflect.ReflectionImpl

import scala.tasty.file.TastyConsumer

class TastyConsumerPhase(consumer: TastyConsumer) extends Phase {

  override def phaseName: String = "tastyConsumer"

  override def run(implicit ctx: Context): Unit = {
    val reflect = new ReflectionImpl(ctx)
    consumer(reflect)(ctx.compilationUnit.tpdTree)
  }

}
