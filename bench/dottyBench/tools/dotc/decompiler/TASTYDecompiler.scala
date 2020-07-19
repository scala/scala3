package dottyBench.tools.dotc.decompiler

import dottyBench.tools.dotc.fromtasty._
import dottyBench.tools.dotc.core.Phases.Phase

/** Compiler from tasty to user readable high text representation
 *  of the compiled scala code.
 *
 * @author Nicolas Stucki
 */
class TASTYDecompiler extends TASTYCompiler {

  override protected def frontendPhases: List[List[Phase]] =
    List(new ReadTasty) :: // Load trees from TASTY files
    Nil

  override protected def picklerPhases: List[List[Phase]] = Nil
  override protected def transformPhases: List[List[Phase]] = Nil

  override protected def backendPhases: List[List[Phase]] =
    List(new DecompilationPrinter) ::  // Print all loaded classes
    Nil
}
