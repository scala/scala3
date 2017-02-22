package dotty.tools.dotc
package transform
package linker

import scala.language.postfixOps
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.core.Phases.Phase
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

  override def runsAfter: Set[Class[_ <: Phase]] = Set(classOf[CallGraphChecks])

  private var doTransform: Boolean = _
  private var callGraph: CallGraph = _
  private var buildCallGraphPhase: BuildCallGraph = _
  private var exception: Tree = _
  private var doNotDCEAnnotation: ClassSymbol = _

  override def prepareForUnit(tree: tpd.Tree)(implicit ctx: Context): TreeTransform = {
    if (DeadCodeElimination.isPhaseRequired) {
      buildCallGraphPhase = ctx.phaseOfClass(classOf[BuildCallGraph]).asInstanceOf[BuildCallGraph]
      callGraph = buildCallGraphPhase.getCallGraph
      exception = Throw(New(ctx.requiredClassRef("dotty.runtime.DeadCodeEliminated"), Nil))
      doNotDCEAnnotation = ctx.requiredClassRef("scala.annotation.internal.link.DoNotDeadCodeEliminate").symbol.asClass
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
    tree
  }

  override def transformDefDef(tree: tpd.DefDef)(implicit ctx: Context, info: TransformerInfo): Tree = {
    if (!doTransform) {
      tree
    } else {
      val sym = tree.symbol
      def isPotentiallyReachable = {
        callGraph.isReachableMethod(sym) || sym.is(Label) || sym.isConstructor || keepAsNew(sym) ||
          (sym.isSetter && callGraph.isReachableMethod(sym.getter))
      }

      if (isPotentiallyReachable || sym.hasAnnotation(doNotDCEAnnotation)) {
        tree
      } else {
        tpd.cpy.DefDef(tree)(rhs = exception)
      }
    }
  }

  private def keepAsNew(sym: Symbol)(implicit ctx: Context): Boolean =
    sym.initial.validFor.firstPhaseId > buildCallGraphPhase.period.phaseId
}
