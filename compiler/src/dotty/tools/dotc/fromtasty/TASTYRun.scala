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

import core.Contexts._

class TASTYRun(comp: Compiler, ictx: Context) extends Run(comp, ictx) {
  override def compile(classNames: List[String]): Unit = {
    val units = classNames.map(new TASTYCompilationUnit(_))
    compileUnits(units)
  }
}
