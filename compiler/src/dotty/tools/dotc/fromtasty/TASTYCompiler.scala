/*
 * Dotty (https://dotty.epfl.ch/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 */

package dotty.tools
package dotc
package fromtasty

import core._
import Contexts._
import Phases.Phase
import dotty.tools.dotc.transform._

class TASTYCompiler extends Compiler {

  override protected def frontendPhases: List[List[Phase]] =
    List(new ReadTasty) :: Nil

  override def newRun(implicit ctx: Context): Run = {
    reset()
    new TASTYRun(this, ctx.addMode(Mode.ReadPositions))
  }
}
