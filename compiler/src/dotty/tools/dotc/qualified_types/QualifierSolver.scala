package dotty.tools.dotc.qualified_types

import dotty.tools.dotc.config.Printers
import dotty.tools.dotc.core.Contexts.{ctx, Context}
import dotty.tools.dotc.core.Decorators.i
import dotty.tools.dotc.core.Symbols.defn
import dotty.tools.dotc.core.Types.{Type, TypeMap, TypeVar}
import dotty.tools.dotc.printing.Showable
import dotty.tools.dotc.reporting.trace

import ENode.{Lambda, Op, OpApply}

/** Solves qualifier implications. A solver instance owns only per-scope state:
 *  the context facts it was created under and a memoization cache. All
 *  solving runs against the context of the current call, passed per method —
 *  a solver must never compute under its creation context, which may since
 *  have been left (e.g. a different typer state).
 */
class QualifierSolver(using Context):

  /** The facts and period this solver was created under. The memoization
   *  cache is only valid while both are unchanged — see
   *  [[QualifierSolver.reuse]].
   */
  private val createdFacts: QualifierContext = QualifierContext.facts
  private val createdPeriod = ctx.period

  /** Assumptions contributed by the context facts, computed once per solver
   *  (under the creation context, where the facts were recorded).
   */
  private val contextAssumptionsClosure: List[ENode] =
    val contextAssumptions = createdFacts.flatMap(_.toENode)
    contextAssumptions ++ contextAssumptions.flatMap(ENode.assumptions)

  /** Memoized [[impliesRec]] results. Sound because the facts are fixed per
   *  solver (`createdFacts`) and the remaining assumptions are derived from
   *  the nodes themselves. Only nodes without provisional types are memoized,
   *  so entries stay valid as type variables get instantiated.
   */
  private val impliesRecCache = collection.mutable.HashMap.empty[(ENode, ENode), Boolean]

  def implies(node1: ENode.Lambda, node2: ENode.Lambda)(using Context): Boolean =
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

  private def impliesCommonParams(node1: ENode.Lambda, node2: ENode.Lambda, mostPreciseNode: ENode.Lambda)(using
      Context): Boolean =
    // "Open" both lambdas by substituting their bound variables with fresh
    // `ENodeVar`s of kind `OpenedParam`, akin to the opening operation in
    // locally-nameless representations. This ensures the shared free
    // variables are never confused with bound variables in nested lambdas.
    val paramRefs = mostPreciseNode.paramTps.zipWithIndex.map((tp, i) =>
      ENodeVar.OpenedParam(i)(tp)
    )
    impliesRec(node1.body.substEParamRefs(0, paramRefs), node2.body.substEParamRefs(0, paramRefs))

  private def impliesRec(node1: ENode, node2: ENode)(using Context): Boolean =
    node1 match
      case OpApply(Op.Or, List(lhs, rhs)) =>
        return impliesRec(lhs, node2) && impliesRec(rhs, node2)
      case _ => ()

    ctx.base.qualifiedTypesStats.record("QualifiedTypes.impliesRec"):
      val key = (node1, node2)
      impliesRecCache.get(key) match
        case Some(res) =>
          ctx.base.qualifiedTypesStats.record("QualifierSolver.impliesRecCacheHit")(res)
        case None =>
          val res = impliesUncached(node1, node2)
          if isCacheable(node1) && isCacheable(node2) then impliesRecCache.update(key, res)
          res

  private def impliesUncached(node1: ENode, node2: ENode)(using Context): Boolean =
    val assumptions =
      ENode.assumptions(node1)
        ++ ENode.assumptions(node2)
        ++ contextAssumptionsClosure
        ++ List(node1)
    val egraph = EGraph(ctx)
    impliesLeaf(
      egraph,
      assumptions.map(a => egraph.canonicalize(a.normalizeTypes())),
      egraph.canonicalize(node2.normalizeTypes())
    )

  /** A node can be memoized only if none of its types is provisional: an
   *  uninstantiated type variable could later change both the node's hash
   *  and the answer.
   */
  private def isCacheable(node: ENode)(using Context): Boolean =
    var ok = true
    node.foreachType: tp =>
      if ok && tp.isProvisional then ok = false
    ok

  protected def impliesLeaf(egraph: EGraph, assumptions: List[ENode], goal: ENode)(using Context): Boolean =
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

object QualifierSolver:
  /** Reuse `prev` for a comparer being rebound to context `c` if its memoized
   *  state is still valid there: same period and the identical (`eq`) facts
   *  list. Returns `null` otherwise, forcing a fresh solver.
   */
  def reuse(prev: QualifierSolver | Null, c: Context): QualifierSolver | Null =
    if prev != null
      && prev.createdPeriod == c.period
      && (prev.createdFacts eq QualifierContext.facts(using c))
    then prev
    else null

abstract class ExplainingQualifierSolver(using Context) extends QualifierSolver:
  def traceIndented[T](message: => String)(op: => T): T

  override protected def impliesLeaf(egraph: EGraph, assumptions: List[ENode], goal: ENode)(using Context): Boolean =
    traceIndented(i"implies ${assumptions.map(_.showNoBreak).mkString(", ")}  -->  ${goal.showNoBreak}"):
      val res = super.impliesLeaf(egraph, assumptions, goal)
      // if !res then println(egraph.debugString())
      res
