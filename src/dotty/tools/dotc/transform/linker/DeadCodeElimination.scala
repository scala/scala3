package dotty.tools.dotc
package transform
package linker

import scala.language.postfixOps
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.transform.TreeTransforms._

class DeadCodeElimination extends MiniPhaseTransform {
  import tpd._

  def phaseName: String = "dce"

  private var callGraph: CallGraph = _
  private var buildCallGraphPhase: BuildCallGraph = _
  private var exception: Tree = _
  private var exportAnnotation: ClassSymbol = _

  override def prepareForUnit(tree: tpd.Tree)(implicit ctx: Context): TreeTransform = {
    buildCallGraphPhase = ctx.phaseOfClass(classOf[BuildCallGraph]).asInstanceOf[BuildCallGraph]
    callGraph = buildCallGraphPhase.getCallGraph
    exception = Throw(New(ctx.requiredClassRef("dotty.runtime.DeadCodeEliminated"), Nil))
    exportAnnotation = defn.ExportAnnot
    this
  }

  override def transformUnit(tree: tpd.Tree)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
    callGraph = null
    tree
  }

  override def transformDefDef(tree: tpd.DefDef)(implicit ctx: Context, info: TransformerInfo): Tree = {
    val sym = tree.symbol
    if (sym.is(Label) || keepAsNew(sym) || sym.isConstructor || callGraph.isReachableMethod(sym)) tree
    else {
      assert(!sym.hasAnnotation(exportAnnotation))
      tpd.cpy.DefDef(tree)(rhs = exception)
    }
  }

//  override def transformTypeDef(tree: TypeDef)(implicit ctx: Context, info: TransformerInfo): Tree = {
//    val sym = tree.symbol
//    if (keepAsNew(sym) || callGraph.isReachableClass(sym) || callGraph.isReachableClassOf(sym)) tree
//    else tpd.EmptyTree
//  }

  // TODO
//  override def transformApply(tree: Apply)(implicit ctx: Context, info: TransformerInfo): Tree = {
//    val tpe = tree.tpe
//    if (!tpe.widenDealias.isInstanceOf[MethodicType] && tree.fun.symbol.isPrimaryConstructor) tree
//    else exception.ensureConforms(tpe)
//  }

  private def keepAsNew(sym: Symbol)(implicit ctx: Context): Boolean =
    sym.initial.validFor.firstPhaseId > buildCallGraphPhase.period.phaseId
}
