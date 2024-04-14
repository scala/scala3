package dotty.tools.dotc
package ast

import tpd.*
import core.Contexts.*
import core.Symbols.*
import util.Property

import scala.collection.mutable

/**
  * It is safe to assume that the companion of a tree is in the same scope.
  * Therefore, when expanding MacroAnnotations, we will only keep track of
  * the trees in the same scope as the current transformed tree
  */
abstract class TreeMapWithTrackedStats extends TreeMapWithImplicits:

  import TreeMapWithTrackedStats.*

  /** Fetch the corresponding tracked tree for a given symbol */
  protected final def getTracked(sym: Symbol)(using Context): Option[MemberDef] =
    for trees <- ctx.property(TrackedTrees)
        tree <- trees.get(sym)
    yield tree

  /** Update the tracked trees */
  protected final def updateTracked(tree: Tree)(using Context): Tree =
    tree match
      case tree: MemberDef =>
        trackedTrees.update(tree.symbol, tree)
        tree
      case _ => tree
  end updateTracked

  /** Process a list of trees and give the priority to trakced trees */
  private final def withUpdatedTrackedTrees(stats: List[Tree])(using Context) =
    val trackedTrees = TreeMapWithTrackedStats.trackedTrees
    stats.mapConserve:
      case tree: MemberDef if trackedTrees.contains(tree.symbol) =>
        trackedTrees(tree.symbol)
      case stat => stat

  override def transform(tree: Tree)(using Context): Tree =
    tree match
      case PackageDef(_, stats) =>
        inContext(trackedDefinitionsCtx(stats)): // Step I: Collect and memoize all the definition trees
          // Step II: Transform the tree
          val pkg@PackageDef(pid, stats) = super.transform(tree): @unchecked
          // Step III: Reconcile between the symbols in syms and the tree
          cpy.PackageDef(pkg)(pid = pid, stats = withUpdatedTrackedTrees(stats))
      case block: Block =>
        inContext(trackedDefinitionsCtx(block.stats)): // Step I: Collect all the member definitions in the block
          // Step II: Transform the tree
          val b@Block(stats, expr) = super.transform(tree): @unchecked
          // Step III: Reconcile between the symbols in syms and the tree
          cpy.Block(b)(expr = expr, stats = withUpdatedTrackedTrees(stats))
      case TypeDef(_, impl: Template) =>
        inContext(trackedDefinitionsCtx(impl.body)): // Step I: Collect and memoize all the stats
          // Step II: Transform the tree
          val newTree@TypeDef(name, impl: Template) = super.transform(tree): @unchecked
          // Step III: Reconcile between the symbols in syms and the tree
          cpy.TypeDef(newTree)(rhs = cpy.Template(impl)(body = withUpdatedTrackedTrees(impl.body)))
      case _ => super.transform(tree)

end TreeMapWithTrackedStats

object TreeMapWithTrackedStats:
  private val TrackedTrees = new Property.Key[mutable.Map[Symbol, tpd.MemberDef]]

  /** Fetch the tracked trees in the cuurent context */
  private def trackedTrees(using Context): mutable.Map[Symbol, MemberDef] =
    ctx.property(TrackedTrees).get

  /** Build a context and track the provided MemberDef trees */
  private def trackedDefinitionsCtx(stats: List[Tree])(using Context): Context =
    val treesToTrack = stats.collect { case m: MemberDef => (m.symbol, m) }
    ctx.fresh.setProperty(TrackedTrees, mutable.Map(treesToTrack*))
