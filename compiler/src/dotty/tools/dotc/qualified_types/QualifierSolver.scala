package dotty.tools.dotc.qualified_types

import dotty.tools.dotc.config.Printers
import dotty.tools.dotc.core.Contexts.{ctx, Context}
import dotty.tools.dotc.core.Decorators.i
import dotty.tools.dotc.core.Symbols.defn
import dotty.tools.dotc.core.Types.{Type, TypeMap, TypeVar}
import dotty.tools.dotc.printing.Showable
import dotty.tools.dotc.reporting.trace

import ENode.{Lambda, Op, OpApply}

class QualifierSolver(using Context):

  def implies(node1: ENode.Lambda, node2: ENode.Lambda): Boolean =
    trace(i"implies ${node1.showNoBreak}  -->  ${node2.showNoBreak}", Printers.qualifiedTypes):
      ctx.base.qualifiedTypesStats.record("QualifierSolver.implies"):
        require(node1.paramTps.length == 1)
        require(node2.paramTps.length == 1)
        val node1Inst = node1.normalizeTypes().asInstanceOf[ENode.Lambda]
        val node2Inst = node2.normalizeTypes().asInstanceOf[ENode.Lambda]
        val paramTp1 = node1Inst.paramTps.head
        val paramTp2 = node2Inst.paramTps.head
        if paramTp1 frozen_<:< paramTp2 then impliesCommonParams(node1Inst, node2Inst, node1Inst)
        else if paramTp2 frozen_<:< paramTp1 then impliesCommonParams(node1Inst, node2Inst, node2Inst)
        else false

  private def impliesCommonParams(node1: ENode.Lambda, node2: ENode.Lambda, mostPreciseNode: ENode.Lambda): Boolean =
    val paramRefs = mostPreciseNode.paramTps.zipWithIndex.map((tp, i) => ENodeParamRef(i, tp))
    impliesRec(node1.body.substEParamRefs(0, paramRefs), node2.body.substEParamRefs(0, paramRefs))

  private def impliesRec(node1: ENode, node2: ENode): Boolean =
    node1 match
      case OpApply(Op.Or, List(lhs, rhs)) =>
        return impliesRec(lhs, node2) && impliesRec(rhs, node2)
      case _ => ()

    ctx.base.qualifiedTypesStats.record("QualifiedTypes.impliesRec"):
      val assumptions = ENode.assumptions(node1) ++ ENode.assumptions(node2) ++ List(node1)
      val egraph = EGraph(ctx)
      impliesLeaf(
        egraph,
        assumptions.map(a => egraph.canonicalize(a.normalizeTypes())),
        egraph.canonicalize(node2.normalizeTypes())
      )

  protected def impliesLeaf(egraph: EGraph, assumptions: List[ENode], goal: ENode): Boolean =
    trace(
      i"impliesLeaf ${assumptions.map(_.showNoBreak).mkString(", ")}  -->  ${goal.showNoBreak}",
      Printers.qualifiedTypes
    ):
      ctx.base.qualifiedTypesStats.record("QualifiedTypes.impliesLeaf"):
        egraph.assertInvariants()
        for assumption <- assumptions do
          egraph.merge(assumption, egraph.trueNode)
        egraph.repair()
        egraph.equiv(goal, egraph.trueNode)

abstract class ExplainingQualifierSolver(using Context) extends QualifierSolver:
  def traceIndented[T](message: => String)(op: => T): T

  override protected def impliesLeaf(egraph: EGraph, assumptions: List[ENode], goal: ENode): Boolean =
    traceIndented(i"implies ${assumptions.map(_.showNoBreak).mkString(", ")}  -->  ${goal.showNoBreak}"):
      val res = super.impliesLeaf(egraph, assumptions, goal)
      // if !res then println(egraph.debugString())
      res
