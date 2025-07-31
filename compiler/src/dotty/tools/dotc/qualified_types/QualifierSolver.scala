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
      val paramTp1 = node1.paramTps.head
      val paramTp2 = node2.paramTps.head
      if paramTp1.underlying frozen_<:< paramTp2.underlying then
        impliesRec(
          node1.body.mapTypes(SubstTypeMap(paramTp1, paramTp2)),
          node2.body.mapTypes(InstantiateMap())
        )
      else if paramTp1.underlying frozen_<:< paramTp2.underlying then
        impliesRec(
          node1.body.mapTypes(InstantiateMap()),
          node2.body.mapTypes(SubstTypeMap(paramTp2, paramTp1))
        )
      else
        false

  /** Instantiates type variables when possible.
    *
    * See `TypeMap.mapOverTypeVar`.
    */
  private class InstantiateMap extends TypeMap:
    override def mapOverTypeVar(tp: TypeVar): Type =
      val res = super.mapOverTypeVar(tp)
      if res eq tp then res else ENode.normalizeType(res)

    def apply(tp: Type): Type = mapOver(tp)

  private class SubstTypeMap(from: Type, to: Type) extends InstantiateMap:
    override def apply(tp: Type): Type = if tp eq from then to else mapOver(tp)

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
