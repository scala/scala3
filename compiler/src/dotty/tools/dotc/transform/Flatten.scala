package dotty.tools.dotc
package transform

import core.*
import DenotTransformers.SymTransformer
import Contexts.*
import Flags.*
import SymDenotations.SymDenotation
import Symbols.Symbol
import collection.mutable
import MegaPhase.MiniPhase
import util.Property
import util.Store

import scala.compiletime.uninitialized

/** Lift nested classes to toplevel */
class Flatten extends MiniPhase with SymTransformer {
  import ast.tpd.*

  override def phaseName: String = Flatten.name

  override def description: String = Flatten.description

  // private[this] and protected[this] modifiers must be dropped
  // before classes are lifted. Getters drop these modifiers.
  override def runsAfter: Set[String] = Set(Getters.name)

  override def changesMembers: Boolean = true // the phase removes inner classes

  private var LiftedDefs: Store.Location[mutable.ListBuffer[Tree] | Null] = uninitialized
  private def liftedDefs(using Context) = ctx.store(LiftedDefs)

  override def initContext(ctx: FreshContext): Unit =
    LiftedDefs = ctx.addLocation[mutable.ListBuffer[Tree] | Null](null)

  def transformSym(ref: SymDenotation)(using Context): SymDenotation =
    if (ref.isClass && !ref.is(Package) && !ref.owner.is(Package))
      ref.copySymDenotation(
        name = ref.flatName,
        owner = ref.enclosingPackageClass)
    else ref

  override def prepareForPackageDef(tree: PackageDef)(using Context): FreshContext =
    ctx.fresh.updateStore(LiftedDefs, new mutable.ListBuffer[Tree])

  private def liftIfNested(tree: Tree)(using Context) =
    if (ctx.owner.is(Package)) tree
    else {
      transformFollowing(tree).foreachInThicket(t => liftedDefs.nn += t)
      EmptyTree
    }

  override def transformStats(stats: List[Tree])(using Context): List[Tree] =
    import Flatten.{NestHost, NestMembers, initialTopLevelClass}
    if ctx.owner.is(Package) then
      // among top-level stats, find class defs with module and set class as host
      for d <- stats do
        if d.isInstanceOf[TypeDef] && !d.symbol.is(ModuleClass) then
          stats.find(_.symbol == d.symbol.scalacLinkedClass).foreach: module =>
            module.putAttachment(NestHost, d.symbol)
            val members = d.getAttachment(NestMembers).getOrElse(Nil)
            d.putAttachment(NestMembers, module.symbol :: members)

      val defs = liftedDefs
      if defs != null then
        // among lifted classes, make the original tlc a nest host, or if the tlc is a module, its companion if it exists
        for d <- defs do
          val sym = d.symbol
          val tlc = sym.initialTopLevelClass
          val cmp = if tlc.is(ModuleClass) then stats.find(_.symbol == tlc.scalacLinkedClass) else None
          cmp.orElse(stats.find(_.symbol == tlc)) match
            case Some(topTree) =>
              d.getAttachment(NestHost) match
                case Some(host) =>
                case None => d.putAttachment(NestHost, topTree.symbol)
              val members = topTree.getAttachment(NestMembers).map(sym :: _).getOrElse(sym :: Nil)
              topTree.putAttachment(NestMembers, members)
            case None => // error no top class in package statements
        val liftedStats = stats ++ defs
        defs.clear()
        liftedStats
      else stats
    else stats

  override def transformTypeDef(tree: TypeDef)(using Context): Tree =
    liftIfNested(tree)
}

object Flatten:
  val name: String = "flatten"
  val description: String = "lift all inner classes to package scope"

  val NestHost: Property.Key[Symbol] = Property.Key()
  val NestMembers: Property.Key[List[Symbol]] = Property.Key()

  extension (sym: SymDenotation) def initialTopLevelClass(using Context): Symbol =
    def topLevel(d: SymDenotation): Symbol =
      if d.isTopLevelClass then d.symbol
      else topLevel(d.owner.initial)
    val top = topLevel(sym.initial)
    if top.isClass then top else top.moduleClass
