package dotty.tools.debug

import dotty.tools.dotc.Compiler
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.transform.ElimByName

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
