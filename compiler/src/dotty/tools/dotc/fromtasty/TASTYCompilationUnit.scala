/*
 * Dotty (https://dotty.epfl.ch/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 */

package dotty.tools.dotc.fromtasty

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.util.NoSource

class TASTYCompilationUnit(val className: String) extends CompilationUnit(NoSource) {
  override def toString: String = s"class file $className"
}
