package dotty.tools.dotc.decompiler

import dotty.tools.dotc.fromtasty.*
import dotty.tools.dotc.core.Phases.Phase

/** Compiler from tasty to user readable high text representation
 *  of the compiled scala code.
 *
 * @author Nicolas Stucki
 */
class TASTYDecompiler extends TASTYCompiler {

  override protected def frontendPhases: Vector[Vector[Phase]] =
    Vector(new ReadTasty) +: // Load trees from TASTY files
    Vector()

  override protected def picklerPhases: Vector[Vector[Phase]] = Vector()
  override protected def transformPhases: Vector[Vector[Phase]] = Vector()

  override protected def backendPhases: Vector[Vector[Phase]] =
    Vector(new DecompilationPrinter) +:  // Print all loaded classes
    Vector()
}
