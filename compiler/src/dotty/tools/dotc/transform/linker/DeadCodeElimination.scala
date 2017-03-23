package dotty.tools.dotc
package transform
package linker

import scala.language.postfixOps
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.DenotTransformers.InfoTransformer
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.Types
import dotty.tools.dotc.core.Types.ClassInfo
import dotty.tools.dotc.transform.SymUtils._
import dotty.tools.dotc.transform.TreeTransforms._
import dotty.tools.dotc.transform.linker.callgraph.CallGraph

object DeadCodeElimination {
  def isPhaseRequired(implicit ctx: Context): Boolean =
    ctx.settings.linkDCE.value || ctx.settings.linkDCEAggressive.value
}

class DeadCodeElimination extends MiniPhaseTransform with InfoTransformer {
  import tpd._

  def phaseName: String = "dce"

  override def runsAfter: Set[Class[_ <: Phase]] = Set(classOf[CallGraphChecks])

  private var doTransform: Boolean = _
  private var callGraph: CallGraph = _
  private var buildCallGraphPhase: BuildCallGraph = _
  private var exception: Tree = _
  private var doNotDCEAnnotation: ClassSymbol = _
  private var aggressive: Boolean = _

  override def prepareForUnit(tree: tpd.Tree)(implicit ctx: Context): TreeTransform = {
    if (DeadCodeElimination.isPhaseRequired) {
      buildCallGraphPhase = ctx.phaseOfClass(classOf[BuildCallGraph]).asInstanceOf[BuildCallGraph]
      callGraph = buildCallGraphPhase.getCallGraph
      exception = Throw(New(ctx.requiredClassRef("dotty.runtime.DeadCodeEliminated"), Nil))
      doNotDCEAnnotation = ctx.requiredClassRef("scala.annotation.internal.link.DoNotDeadCodeEliminate").symbol.asClass
      aggressive = ctx.settings.linkDCEAggressive.value
      doTransform = true
    } else {
      doTransform = false
    }
    this
  }

  override def transformUnit(tree: tpd.Tree)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
    buildCallGraphPhase = null
    callGraph = null
    exception = null
    doNotDCEAnnotation = null
    doTransform = false
    tree
  }

  override def transformDefDef(tree: tpd.DefDef)(implicit ctx: Context, info: TransformerInfo): Tree = {
    val sym = tree.symbol
    if (!doTransform || doNotEliminate(sym)) tree
    else if (aggressive && !doNotEliminateAggressive(sym)) EmptyTree
    else tpd.cpy.DefDef(tree)(rhs = exception)
  }

  override def transformInfo(tp: Types.Type, sym: Symbol)(implicit ctx: Context): Types.Type = {
    if (!doTransform || !aggressive) tp
    else {
      tp match {
        case tp: ClassInfo =>
          val newDecls = tp.decls.filteredScope(x => doNotEliminate(x) || doNotEliminateAggressive(x))
          ClassInfo(tp.prefix, tp.cls, tp.classParents, newDecls, tp.selfInfo)
        case _ => tp
      }
    }
  }

  def doNotEliminate(sym: Symbol)(implicit ctx: Context): Boolean = {
    callGraph.isReachableMethod(sym) || sym.is(Label) || sym.isConstructor || keepAsNew(sym) ||
      (sym.isSetter && callGraph.isReachableMethod(sym.getter)) || sym.hasAnnotation(doNotDCEAnnotation)
  }

  def doNotEliminateAggressive(sym: Symbol)(implicit ctx: Context): Boolean = {
    sym.is(Synthetic) || sym.isType || sym.owner.is(Trait) || callGraph.outerMethods(sym)
  }

  private def keepAsNew(sym: Symbol)(implicit ctx: Context): Boolean =
    sym.initial.validFor.firstPhaseId > buildCallGraphPhase.period.phaseId
}
