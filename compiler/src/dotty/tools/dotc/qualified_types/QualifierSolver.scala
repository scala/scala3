package dotty.tools.dotc.qualified_types

import ENode.{Lambda, OpApply, Op}

import dotty.tools.dotc.config.Printers
import dotty.tools.dotc.core.Contexts.{ctx, Context}
import dotty.tools.dotc.core.Symbols.defn
import dotty.tools.dotc.core.Types.{Type, TypeVar, TypeMap}
import dotty.tools.dotc.core.Decorators.i
import dotty.tools.dotc.printing.Showable

class QualifierSolver(using Context):

  def implies(node1: ENode.Lambda, node2: ENode.Lambda) =
    require(node1.paramTps.length == 1)
    require(node2.paramTps.length == 1)
    val node1Inst = node1.normalizeTypes().asInstanceOf[ENode.Lambda]
    val node2Inst = node2.normalizeTypes().asInstanceOf[ENode.Lambda]
    val paramTp1 = node1Inst.paramTps.head
    val paramTp2 = node2Inst.paramTps.head
    if paramTp1 frozen_<:< paramTp2 then
      impliesRec(subsParamRefTps(node1Inst.body, node2Inst), node2Inst.body)
    else if paramTp2 frozen_<:< paramTp1 then
      impliesRec(node1Inst.body, subsParamRefTps(node2Inst.body, node1Inst))
    else
      false

  private def subsParamRefTps(node1Body: ENode, node2: ENode.Lambda): ENode =
    val paramRefs = node2.paramTps.zipWithIndex.map((tp, i) => ENodeParamRef(i, tp))
    node1Body.substEParamRefs(0, paramRefs)

  private def impliesRec(node1: ENode, node2: ENode): Boolean =
    node1 match
      case OpApply(Op.Or, List(lhs, rhs)) =>
        return impliesRec(lhs, node2) && impliesRec(rhs, node2)
      case _ => ()

    val assumptions = ENode.assumptions(node1) ++ ENode.assumptions(node2)
    val node1WithAssumptions = assumptions.foldLeft(node1)((acc, a) => OpApply(Op.And, List(acc, a.normalizeTypes())))
    impliesLeaf(EGraph(ctx), node1WithAssumptions, node2)

  protected def impliesLeaf(egraph: EGraph, enode1: ENode, enode2: ENode): Boolean =
    val node1Canonical = egraph.canonicalize(enode1)
    val node2Canonical = egraph.canonicalize(enode2)
    egraph.assertInvariants()
    egraph.merge(node1Canonical, egraph.trueNode)
    egraph.repair()
    egraph.equiv(node2Canonical, egraph.trueNode)

final class ExplainingQualifierSolver(
  traceIndented: [T] => (String) => (=> T) => T)(using Context) extends QualifierSolver:

  override protected def impliesLeaf(egraph: EGraph, enode1: ENode, enode2: ENode): Boolean =
    traceIndented(s"${enode1.showNoBreak}  -->  ${enode2.showNoBreak}"):
      val res = super.impliesLeaf(egraph, enode1, enode2)
      if !res then println(egraph.debugString())
      res
