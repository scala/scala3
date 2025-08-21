package dotty.tools.dotc.qualified_types

import ENode.{Lambda, OpApply, Op}

import dotty.tools.dotc.config.Printers
import dotty.tools.dotc.reporting.trace
import dotty.tools.dotc.core.Contexts.{ctx, Context}
import dotty.tools.dotc.core.Symbols.defn
import dotty.tools.dotc.core.Types.{Type, TypeVar, TypeMap}

class QualifierSolver(using Context):

  def implies(node1: ENode.Lambda, node2: ENode.Lambda) =
    trace(s"QualifierSolver.implies ${node1.body} -> ${node2.body}", Printers.qualifiedTypes):
      assert(node1.paramTps.length == 1)
      assert(node2.paramTps.length == 1)
      val paramTp1 = node1.paramTps.head
      val paramTp2 = node2.paramTps.head
      assert(paramTp1.index == 0)
      assert(paramTp2.index == 0)
      if paramTp1.underlying frozen_<:< paramTp2.underlying then
        impliesWithAssumptions(
          node1.body.substArgRefs(0, node2.paramTps),
          node2.body.instantiateTypeVars()
        )
      else if paramTp1.underlying frozen_<:< paramTp2.underlying then
        impliesWithAssumptions(
          node1.body.instantiateTypeVars(),
          node2.body.substArgRefs(0, node1.paramTps)
        )
      else
        false

  private def impliesWithAssumptions(node1: ENode, node2: ENode): Boolean =
    val assumptions = ENode.assumptions(node1) ++ ENode.assumptions(node2)
    val node1WithAssumptions =
      if assumptions.isEmpty then node1
      else assumptions.foldLeft(node1)((assumption, acc) => OpApply(Op.And, List(assumption, acc)))
    impliesRec(node1WithAssumptions, node2)

  private def impliesRec(node1: ENode, node2: ENode): Boolean =
    node1 match
      case OpApply(Op.Or, List(lhs, rhs)) =>
        return impliesRec(lhs, node2) && impliesRec(rhs, node2)
      case _ => ()

    node2 match
      case OpApply(Op.And, List(lhs, rhs)) =>
        return impliesRec(node1, lhs) && impliesRec(node1, rhs)
      case _ => ()

    val egraph = EGraph(ctx)
    val canonicalNode1 = egraph.canonicalize(node1)
    val canonicalNode2 = egraph.canonicalize(node2)
    trace(s"QualifierSolver.impliesRec $canonicalNode1 -> $canonicalNode2", Printers.qualifiedTypes):
      egraph.merge(canonicalNode1, egraph.trueNode)
      egraph.repair()
      egraph.equiv(canonicalNode2, egraph.trueNode)
