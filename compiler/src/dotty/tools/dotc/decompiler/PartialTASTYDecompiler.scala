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

package dotty.tools.dotc.decompiler

import dotty.tools.dotc.core.Phases.Phase

/** Partial TASTYDecompiler that doesn't execute the backendPhases
  * allowing to control decompiler output by manually running it
  * on the CompilationUnits
 */
class PartialTASTYDecompiler extends TASTYDecompiler {
  override protected def backendPhases: List[List[Phase]] = Nil
}
