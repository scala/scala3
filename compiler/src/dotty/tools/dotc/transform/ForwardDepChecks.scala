package dotty.tools
package dotc
package transform

import core.*
import Symbols.*, Types.*, Contexts.*, Flags.*, Decorators.*, reporting.*
import util.Spans.Span
import util.Store
import collection.immutable
import ast.tpd
import MegaPhase.MiniPhase

object ForwardDepChecks:

  import tpd.*

  val name: String = "forwardDepChecks"
  val description: String = "ensure no forward references to local vals"

  type LevelAndIndex = immutable.Map[Symbol, (LevelInfo, Int)]

  class OptLevelInfo {
    def levelAndIndex: LevelAndIndex = Map()
    def enterReference(sym: Symbol, span: Span): Unit = ()
  }

  /** A class to help in forward reference checking */
  class LevelInfo(outerLevelAndIndex: LevelAndIndex, stats: List[Tree])(using Context)
  extends OptLevelInfo {
    override val levelAndIndex: LevelAndIndex =
      stats.foldLeft(outerLevelAndIndex, 0) {(mi, stat) =>
        val (m, idx) = mi
        val m1 = stat match {
          case stat: MemberDef => m.updated(stat.symbol, (this, idx))
          case _ => m
        }
        (m1, idx + 1)
      }._1
    var maxIndex: Int = Int.MinValue
    var refSpan: Span = _
    var refSym: Symbol = _

    override def enterReference(sym: Symbol, span: Span): Unit =
      if (sym.exists && sym.owner.isTerm)
        levelAndIndex.get(sym) match {
          case Some((level, idx)) if (level.maxIndex < idx) =>
            level.maxIndex = idx
            level.refSpan = span
            level.refSym = sym
          case _ =>
        }
  }

  val NoLevelInfo: OptLevelInfo = new OptLevelInfo()

class ForwardDepChecks extends MiniPhase:
  import ForwardDepChecks.*
  import tpd.*

  override def phaseName: String = ForwardDepChecks.name

  override def description: String = ForwardDepChecks.description

  override def runsAfter: Set[String] = Set(ElimByName.name)

  private var LevelInfo: Store.Location[OptLevelInfo] = _
  private def currentLevel(using Context): OptLevelInfo = ctx.store(LevelInfo)

  override def initContext(ctx: FreshContext): Unit =
    LevelInfo = ctx.addLocation(NoLevelInfo)

  override def prepareForStats(trees: List[Tree])(using Context): Context =
    if (ctx.owner.isTerm)
      ctx.fresh.updateStore(LevelInfo, new LevelInfo(currentLevel.levelAndIndex, trees))
    else ctx

  override def transformValDef(tree: ValDef)(using Context): ValDef =
    val sym = tree.symbol
    if sym.exists && sym.owner.isTerm && !sym.is(Lazy) then
      currentLevel.levelAndIndex.get(sym) match
        case Some((level, symIdx)) if symIdx <= level.maxIndex =>
          report.error(ForwardReferenceExtendsOverDefinition(sym, level.refSym),
            ctx.source.atSpan(level.refSpan))
        case _ =>
    tree

  override def transformIdent(tree: Ident)(using Context): Ident = {
    currentLevel.enterReference(tree.symbol, tree.span)
    tree
  }

  override def transformApply(tree: Apply)(using Context): Apply = {
    if (isSelfConstrCall(tree)) {
      assert(currentLevel.isInstanceOf[LevelInfo], s"${ctx.owner}/" + i"$tree")
      val level = currentLevel.asInstanceOf[LevelInfo]
      if (level.maxIndex > 0) {
        // An implementation restriction to avoid VerifyErrors and lazyvals mishaps; see SI-4717
        report.debuglog("refsym = " + level.refSym)
        report.error("forward reference not allowed from self constructor invocation",
          ctx.source.atSpan(level.refSpan))
      }
    }
    tree
  }

  override def transformNew(tree: New)(using Context): New = {
    currentLevel.enterReference(tree.tpe.typeSymbol, tree.span)
    tree.tpe.dealias.foreachPart {
      case TermRef(_, s: Symbol) => currentLevel.enterReference(s, tree.span)
      case _ =>
    }
    tree
  }
end ForwardDepChecks
