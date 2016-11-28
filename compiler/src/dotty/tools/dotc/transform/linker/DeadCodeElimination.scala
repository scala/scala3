package dotty.tools.dotc
package transform
package linker

import scala.language.postfixOps
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.transform.SymUtils._
import dotty.tools.dotc.transform.TreeTransforms._
import dotty.tools.dotc.transform.linker.callgraph.CallGraph

object DeadCodeElimination {
  def isPhaseRequired(implicit ctx: Context): Boolean = ctx.settings.linkDCE.value
}

class DeadCodeElimination extends MiniPhaseTransform {
  import tpd._

  def phaseName: String = "dce"

  private var doTransform: Boolean = _
  private var callGraph: CallGraph = _
  private var buildCallGraphPhase: BuildCallGraph = _
  private var exception: Tree = _
  private var entryPointAnnotation: ClassSymbol = _
  private var doNotDCEAnnotation: ClassSymbol = _
  private var assertReachable: ClassSymbol = _
  private var assertNotReachable: ClassSymbol = _

  override def prepareForUnit(tree: tpd.Tree)(implicit ctx: Context): TreeTransform = {
    if (DeadCodeElimination.isPhaseRequired) {
      buildCallGraphPhase = ctx.phaseOfClass(classOf[BuildCallGraph]).asInstanceOf[BuildCallGraph]
      callGraph = buildCallGraphPhase.getCallGraph
      exception = Throw(New(ctx.requiredClassRef("dotty.runtime.DeadCodeEliminated"), Nil))
      entryPointAnnotation = defn.EntryPointAnnot
      doNotDCEAnnotation = ctx.requiredClassRef("scala.annotation.internal.link.DoNotDeadCodeEliminate").symbol.asClass
      assertReachable = ctx.requiredClassRef("scala.annotation.internal.link.AssertReachable").symbol.asClass
      assertNotReachable = ctx.requiredClassRef("scala.annotation.internal.link.AssertNotReachable").symbol.asClass
      doTransform = true
    } else {
      doTransform = false
    }
    this
  }

  override def transformUnit(tree: tpd.Tree)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
    callGraph = null
    tree
  }

  override def transformDefDef(tree: tpd.DefDef)(implicit ctx: Context, info: TransformerInfo): Tree = {
    if (!doTransform) {
      tree
    } else {
      val sym = tree.symbol
      val isReachableThroughCallGraph = callGraph.isReachableMethod(sym)

      if (isReachableThroughCallGraph && sym.hasAnnotation(assertNotReachable))
        ctx.error("@internal.link.AssertNotReachable annotation was used on a reachable member", tree.pos)
      else if (!isReachableThroughCallGraph && sym.hasAnnotation(assertReachable))
        ctx.error("@internal.link.AssertReachable annotation was used on a non reachable member", tree.pos)

      def isPotentiallyReachable = {
        isReachableThroughCallGraph || sym.is(Label) || sym.isConstructor || keepAsNew(sym) ||
          (sym.isSetter && callGraph.isReachableMethod(sym.getter))
      }

      if (isPotentiallyReachable || sym.hasAnnotation(doNotDCEAnnotation)) {
        tree
      } else {
        assert(!sym.hasAnnotation(entryPointAnnotation))
        tpd.cpy.DefDef(tree)(rhs = exception)
      }
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
