package dotty.tools.dotc
package transform
package linker

import scala.language.postfixOps

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.transform.TreeTransforms._

class DeadCodeElimination extends MiniPhaseTransform {
  import tpd._

  def phaseName: String = "dce"

  private var reachableSet: Set[Symbol] = _
  private var reachableClassesSet: Set[Symbol] = _
  private var classOfs: Set[Symbol] = _
  private var keepAfter: BuildCallGraph = _
  private var exception: Tree = _

  override def prepareForUnit(tree: tpd.Tree)(implicit ctx: Context): TreeTransform = {
    keepAfter = ctx.phaseOfClass(classOf[BuildCallGraph]).asInstanceOf[BuildCallGraph]
    val callGraph = keepAfter.getCallGraph
    reachableSet = callGraph.reachableMethods.map(x => x.call.termSymbol)
    reachableClassesSet = callGraph.reachableTypes.flatMap(x => x.tp.classSymbol :: x.tp.baseClasses)
    classOfs = callGraph.classOfs
    exception = Throw(New(ctx.requiredClassRef("dotty.runtime.DeadCodeEliminated"), Nil))
    this
  }

  override def transformUnit(tree: tpd.Tree)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
    reachableSet = null
    tree
  }

  override def transformDefDef(tree: tpd.DefDef)(implicit ctx: Context, info: TransformerInfo): Tree = {
    val sym = tree.symbol
    val keepAsNew = sym.initial.validFor.firstPhaseId > keepAfter.period.phaseId
    if (sym.isConstructor || keepAsNew || reachableSet.contains(sym)) tree
    else tpd.cpy.DefDef(tree)(rhs = exception)
  }

  override def transformTypeDef(tree: TypeDef)(implicit ctx: Context, info: TransformerInfo): Tree = {
    val sym = tree.symbol
    val keepAsNew = sym.initial.validFor.firstPhaseId > keepAfter.period.phaseId
    if (keepAsNew || reachableClassesSet(sym) || classOfs(sym)) tree
    else tpd.EmptyTree
  }

  // TODO
//  override def transformApply(tree: Apply)(implicit ctx: Context, info: TransformerInfo): Tree = {
//    val tpe = tree.tpe
//    if (!tpe.widenDealias.isInstanceOf[MethodicType] && tree.fun.symbol.isPrimaryConstructor) tree
//    else exception.ensureConforms(tpe)
//  }
}
