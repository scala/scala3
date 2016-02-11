package dotty.tools.dotc
package transform

import core._
import DenotTransformers.SymTransformer
import Phases.Phase
import Contexts.Context
import Flags._
import Symbols._
import SymDenotations.SymDenotation
import ast.Trees._
import collection.mutable
import Decorators._
import NameOps._
import TreeTransforms.{TreeTransform, MiniPhase}
import dotty.tools.dotc.transform.TreeTransforms.TransformerInfo

/** Remove companion objects that are empty */
class DropEmptyCompanions extends MiniPhase { thisTransform =>
  import ast.tpd._
  override def phaseName = "dropEmpty"
  val treeTransform = new Transform(Set())

  class Transform(dropped: Set[Symbol]) extends TreeTransform {
    def phase = thisTransform

    /** Is `tree` an empty companion object? */
    private def isEmptyCompanion(tree: Tree)(implicit ctx: Context) = tree match {
      case TypeDef(_, impl: Template) if
        tree.symbol.is(SyntheticModule) &&
        tree.symbol.companionClass.exists &&
        impl.body.forall(_.symbol.isPrimaryConstructor) =>
        //println(i"removing ${tree.symbol}")
        true
      case _ =>
        false
    }

    /** A transform which has all empty companion objects in `stats`
     *  recorded in its `dropped` set.
     */
    private def localTransform(stats: List[Tree])(implicit ctx: Context) =
      new Transform(stats.filter(isEmptyCompanion).map(_.symbol).toSet)

    override def prepareForTemplate(tree: Template)(implicit ctx: Context) =
      localTransform(tree.body)

    override def prepareForStats(trees: List[Tree])(implicit ctx: Context) =
      if (ctx.owner is Package) localTransform(trees) else this

    /** Symbol is a $lzy field representing a module */
    private def isLazyModuleVar(sym: Symbol)(implicit ctx: Context) =
      sym.name.isLazyLocal &&
      sym.owner.info.decl(sym.name.asTermName.nonLazyName).symbol.is(Module)

    /** Symbol should be dropped together with a dropped companion object.
     *  Such symbols are:
     *   - lzy fields pointing to modules,
     *   - vals and getters representing modules.
     */
    private def toDrop(sym: Symbol)(implicit ctx: Context): Boolean =
      (sym.is(Module) || isLazyModuleVar(sym)) &&
      dropped.contains(sym.info.resultType.typeSymbol)

    /** Tree should be dropped because it (is associated with) an empty
     *  companion object. Such trees are
     *   - module classes of empty companion objects
     *   - definitions of lazy module variables or assignments to them.
     *   - vals and getters for empty companion objects
     */
    private def toDrop(stat: Tree)(implicit ctx: Context): Boolean = stat match {
      case stat: TypeDef => dropped.contains(stat.symbol)
      case stat: ValOrDefDef => toDrop(stat.symbol)
      case stat: Assign => toDrop(stat.lhs.symbol)
      case _ => false
    }

    override def transformStats(stats: List[Tree])(implicit ctx: Context, info: TransformerInfo) =
      stats.filterNot(toDrop)
  }
}
