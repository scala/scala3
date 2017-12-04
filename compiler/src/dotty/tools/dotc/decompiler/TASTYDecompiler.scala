package dotty.tools.dotc.decompiler

import dotty.tools.dotc.fromtasty._
import dotty.tools.dotc.core.Phases.Phase

/** Compiler from tasty to user readable high text representation
 *  of the compiled scala code.
 *
 * @author Nicolas Stucki
 */
class TASTYDecompiler extends TASTYCompiler {
  override def phases: List[List[Phase]] = List(
    List(new ReadTastyTreesFromClasses), // Load classes from tasty
    List(new DecompilationPrinter)       // Print all loaded classes
  )
}
