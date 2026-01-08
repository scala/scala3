package dotty.tools.debug

import dotty.tools.dotc.Compiler
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.transform.ElimByName

/**
  * The expression compiler powers the debug console in Metals and the IJ Scala plugin,
  * enabling evaluation of arbitrary Scala expressions at runtime (even macros).
  * It produces class files that can be loaded by the running Scala program,
  * to compute the evaluation output.
  * 
  * To do so, it extends the Compiler with 3 phases:
  *  - InsertExpression: parses and inserts the expression in the original source tree
  *  - ExtractExpression: extract the typed expression and places it in the new expression class
  *  - ResolveReflectEval: resolves local variables or inaccessible members using reflection calls
  */
class ExpressionCompiler(config: ExpressionCompilerConfig) extends Compiler:

  override protected def frontendPhases: List[List[Phase]] =
    val parser :: others = super.frontendPhases: @unchecked
    parser :: List(InsertExpression(config)) :: others

  override protected def transformPhases: List[List[Phase]] =
    val store = ExpressionStore()
    // the ExtractExpression phase should be after ElimByName and ExtensionMethods, and before LambdaLift
    val transformPhases = super.transformPhases
    val index = transformPhases.indexWhere(_.exists(_.phaseName == ElimByName.name))
    val (before, after) = transformPhases.splitAt(index + 1)
    (before :+ List(ExtractExpression(config, store))) ++ (after :+ List(ResolveReflectEval(config, store)))
