package dotty.tools.dotc
package transform
package linker

import scala.language.postfixOps
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.Trees
import dotty.tools.dotc.core.Constants._
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.transform.TreeTransforms._
import dotty.tools.dotc.transform.linker.callgraph.CallGraph

object CallGraphChecks {
  def isPhaseRequired(implicit ctx: Context): Boolean = ctx.settings.YlinkDCEChecks.value
}

class CallGraphChecks extends MiniPhaseTransform {
  import tpd._

  def phaseName: String = "dce"

  private var doYChecks: Boolean = _
  private var callGraph: CallGraph = _
  private var assertReachableAnnotation: ClassSymbol = _
  private var assertNotReachableAnnotation: ClassSymbol = _
  private var callGraphBoundsAnnotation: ClassSymbol = _

  override def prepareForUnit(tree: tpd.Tree)(implicit ctx: Context): TreeTransform = {
    if (DeadCodeElimination.isPhaseRequired) {
      val buildCallGraphPhase = ctx.phaseOfClass(classOf[BuildCallGraph]).asInstanceOf[BuildCallGraph]
      callGraph = buildCallGraphPhase.getCallGraph
      assertReachableAnnotation = ctx.requiredClassRef("scala.annotation.internal.link.AssertReachable").symbol.asClass
      assertNotReachableAnnotation = ctx.requiredClassRef("scala.annotation.internal.link.AssertNotReachable").symbol.asClass
      callGraphBoundsAnnotation = ctx.requiredClassRef("scala.annotation.internal.link.CallGraphBounds").symbol.asClass
      doYChecks = true
    } else {
      doYChecks = false
    }
    this
  }

  override def transformUnit(tree: tpd.Tree)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
    callGraph = null
    assertReachableAnnotation = null
    assertNotReachableAnnotation = null
    callGraphBoundsAnnotation = null
    tree
  }

  override def transformDefDef(tree: tpd.DefDef)(implicit ctx: Context, info: TransformerInfo): Tree = {
    if (doYChecks) {
      val sym = tree.symbol
      val isReachableThroughCallGraph = callGraph.isReachableMethod(sym)

      if (isReachableThroughCallGraph && sym.hasAnnotation(assertNotReachableAnnotation))
        ctx.error("@internal.link.AssertNotReachable annotation was used on a reachable member", tree.pos)
      else if (!isReachableThroughCallGraph && sym.hasAnnotation(assertReachableAnnotation))
        ctx.error("@internal.link.AssertReachable annotation was used on a non reachable member", tree.pos)

      val info = callGraph.getInfo
      sym.getAnnotation(callGraphBoundsAnnotation).foreach { ann =>
        def check(indexInAnnotation: Int, name: String, actual: Int): Unit = {
          ann.argument(indexInAnnotation) match {
            case Some(lit @ Trees.Literal(Constant(bound: Int))) =>
              if (bound <= 0)
                ctx.error(s"Invalid bound $name: bound must be positive ", lit.pos)
              else if (actual > bound)
                ctx.error(s"Too many $name: expected at most $bound but was $actual", lit.pos)
              // else if (actual < bound / 1.2)
              //   ctx.error(s"Bound is not tight for $name: bound is $bound and actually have $actual", lit.pos)
            case Some(arg) => ctx.error("Argument must be a literal integer", arg.pos)
            case _ => assert(false)
          }
        }

        check(0, "reachable classes", info.reachableClasses.size)
        check(1, "classes with reachable methods", info.classesWithReachableMethods.size)
        check(2, "reachable methods", info.reachableDefs.size)
      }
    }

    tree
  }

}
