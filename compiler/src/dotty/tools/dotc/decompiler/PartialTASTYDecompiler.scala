package dotty.tools.dotc.decompiler

import dotty.tools.dotc.core.Phases.Phase

/** Partial TASTYDecompiler that doesn't execute the backendPhases
  * allowing to control decompiler output by manually running it
  * on the CompilationUnits
 */
class PartialTASTYDecompiler extends TASTYDecompiler {
  override protected def backendPhases: List[List[Phase]] = Nil
}
