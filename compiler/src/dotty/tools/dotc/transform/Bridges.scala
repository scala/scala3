package dotty.tools
package dotc
package transform

import core._
import Symbols._, Types._, Contexts._, Decorators._, Flags._, Scopes._, Phases._
import DenotTransformers._
import ast.untpd
import collection.{mutable, immutable}
import util.Spans.Span
import util.SrcPos

/** A helper class for generating bridge methods in class `root`. */
class Bridges(root: ClassSymbol, thisPhase: DenotTransformer)(using Context) {
  import ast.tpd._

  assert(ctx.phase == erasurePhase.next)
  private val preErasureCtx = ctx.withPhase(erasurePhase)
  private lazy val elimErasedCtx = ctx.withPhase(elimErasedValueTypePhase.next)

  private class BridgesCursor(using Context) extends OverridingPairs.Cursor(root) {

    override def isSubParent(parent: Symbol, bc: Symbol)(using Context) =
      true
      	// Never consider a bridge if there is a superclass that would contain it
      	// See run/t2857.scala for a test that would break with a VerifyError otherwise.

    /** Only use the superclass of `root` as a parent class. This means
     *  overriding pairs that have a common implementation in a trait parent
     *  are also counted. This is necessary because we generate bridge methods
     *  only in classes, never in traits.
     */
    override def parents = Array(root.superClass)

    override def exclude(sym: Symbol) =
      !sym.isOneOf(MethodOrModule) || super.exclude(sym)
  }

  //val site = root.thisType

  private var toBeRemoved = immutable.Set[Symbol]()
  private val bridges = mutable.ListBuffer[Tree]()
  private val bridgesScope = newScope
  private val bridgeTarget = MutableSymbolMap[Symbol]()

  def bridgePosFor(member: Symbol): SrcPos =
    (if (member.owner == root && member.span.exists) member else root).srcPos

  /** Add a bridge between `member` and `other`, where `member` overrides `other`
   *  before erasure, if the following conditions are satisfied.
   *
   *   - `member` and `other` have different signatures
   *   - there is not yet a bridge with the same name and signature in `root`.
   *
   *  The bridge has the erased info of `other` and forwards to `member`.
   *  Additionally, if `member` and `other` do have the same signature,
   *  but not the same type after erasure and before elimErasedValueTypes
   *  issue an error: A bridge would be needed yet it would clash with the member itself.
   *  See neg/i1905.scala
   */
  private def addBridgeIfNeeded(member: Symbol, other: Symbol) = {
    def bridgeExists =
      bridgesScope.lookupAll(member.name).exists(bridge =>
        bridgeTarget(bridge) == member && bridge.signature == other.signature)
    def info(sym: Symbol)(using Context) = sym.info
    def desc(sym: Symbol)= {
      val infoStr = info(sym)(using preErasureCtx) match {
        case ExprType(info) => i": $info"
        case info => info.show
      }
      i"$sym$infoStr in ${sym.owner}"
    }
    if (member.signature == other.signature) {
      if (!member.info.matches(other.info))
        report.error(em"""bridge generated for member ${desc(member)}
                      |which overrides ${desc(other)}
                      |clashes with definition of the member itself; both have erased type ${info(member)(using elimErasedCtx)}."""",
                  bridgePosFor(member))
    }
    else if (!bridgeExists)
      addBridge(member, other)
  }

  /** Generate bridge between `member` and `other`
   */
  private def addBridge(member: Symbol, other: Symbol) = {
    val bridge = other.copy(
      owner = root,
      flags = (member.flags | Method | Bridge | Artifact) &~
        (Accessor | ParamAccessor | CaseAccessor | Deferred | Lazy | Module),
      coord = bridgePosFor(member).span).enteredAfter(thisPhase).asTerm

    report.debuglog(
      i"""generating bridge from ${other.showLocated}: ${other.info}
             |to ${member.showLocated}: ${member.info} @ ${member.span}
             |bridge: ${bridge.showLocated} with flags: ${bridge.flagsString}""")

    bridgeTarget(bridge) = member
    bridgesScope.enter(bridge)

    if (other.owner == root) {
      root.delete(other)
      toBeRemoved += other
    }

    def bridgeRhs(argss: List[List[Tree]]) = {
      assert(argss.tail.isEmpty)
      val ref = This(root).select(member)
      if (member.info.isParameterless) ref // can happen if `member` is a module
      else Erasure.partialApply(ref, argss.head)
    }

    bridges += DefDef(bridge, bridgeRhs(_).withSpan(bridge.span))
  }

  /** Add all necessary bridges to template statements `stats`, and remove at the same
   *  time deferred methods in `stats` that are replaced by a bridge with the same signature.
   */
  def add(stats: List[untpd.Tree]): List[untpd.Tree] =
    val opc = new BridgesCursor()(using preErasureCtx)
    while opc.hasNext do
      if !opc.overriding.is(Deferred) then
        addBridgeIfNeeded(opc.overriding, opc.overridden)
      opc.next()
    if bridges.isEmpty then stats
    else stats.filterNot(stat => toBeRemoved contains stat.symbol) ::: bridges.toList
}
