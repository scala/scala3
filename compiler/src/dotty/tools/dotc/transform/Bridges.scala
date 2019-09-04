package dotty.tools
package dotc
package transform

import core._
import Symbols._, Types._, Contexts._, Decorators._, Flags._, Scopes._
import DenotTransformers._
import ast.untpd
import collection.{mutable, immutable}
import ShortcutImplicits._
import util.Spans.Span
import util.SourcePosition

/** A helper class for generating bridge methods in class `root`. */
class Bridges(root: ClassSymbol, thisPhase: DenotTransformer)(implicit ctx: Context) {
  import ast.tpd._

  assert(ctx.phase == ctx.erasurePhase.next)
  private val preErasureCtx = ctx.withPhase(ctx.erasurePhase)
  private val elimErasedCtx = ctx.withPhase(ctx.elimErasedValueTypePhase.next)

  private class BridgesCursor(implicit ctx: Context) extends OverridingPairs.Cursor(root) {

    /** Only use the superclass of `root` as a parent class. This means
     *  overriding pairs that have a common implementation in a trait parent
     *  are also counted. This is necessary because we generate bridge methods
     *  only in classes, never in traits.
     */
    override def parents = Array(root.superClass)

    override def exclude(sym: Symbol) =
      !sym.isOneOf(MethodOrModule) ||
      isImplicitShortcut(sym) ||
      super.exclude(sym)
  }

  //val site = root.thisType

  private[this] var toBeRemoved = immutable.Set[Symbol]()
  private val bridges = mutable.ListBuffer[Tree]()
  private val bridgesScope = newScope
  private val bridgeTarget = newMutableSymbolMap[Symbol]

  def bridgePosFor(member: Symbol): SourcePosition =
    (if (member.owner == root && member.span.exists) member else root).sourcePos

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
    def info(sym: Symbol)(implicit ctx: Context) = sym.info
    def desc(sym: Symbol)= {
      val infoStr = info(sym)(preErasureCtx) match {
        case ExprType(info) => i": $info"
        case info => info.show
      }
      i"$sym$infoStr in ${sym.owner}"
    }
    if (member.signature == other.signature) {
      if (!member.info.matches(other.info))
        ctx.error(em"""bridge generated for member ${desc(member)}
                      |which overrides ${desc(other)}
                      |clashes with definition of the member itself; both have erased type ${info(member)(elimErasedCtx)}."""",
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

    ctx.debuglog(
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
      val ref = This(root).select(member)
      if (member.info.isParameterless) ref // can happen if `member` is a module
      else ref.appliedToArgss(argss)
    }

    bridges += DefDef(bridge, bridgeRhs(_).withSpan(bridge.span))
  }

  /** Add all necessary bridges to template statements `stats`, and remove at the same
   *  time deferred methods in `stats` that are replaced by a bridge with the same signature.
   */
  def add(stats: List[untpd.Tree]): List[untpd.Tree] = {
    val opc = new BridgesCursor()(preErasureCtx)
    val ectx = ctx.withPhase(thisPhase)
    while (opc.hasNext) {
      if (!opc.overriding.is(Deferred)) {
        addBridgeIfNeeded(opc.overriding, opc.overridden)

        if (needsImplicitShortcut(opc.overriding)(ectx) && needsImplicitShortcut(opc.overridden)(ectx))
          // implicit shortcuts do not show up in the Bridges cursor, since they
          // are created only when referenced. Therefore we need to generate a bridge
          // for them specifically, if one is needed for the original methods.
          addBridgeIfNeeded(
            shortcutMethod(opc.overriding, thisPhase)(ectx),
            shortcutMethod(opc.overridden, thisPhase)(ectx))
      }
      opc.next()
    }
    if (bridges.isEmpty) stats
    else stats.filterNot(stat => toBeRemoved contains stat.symbol) ::: bridges.toList
  }
}
